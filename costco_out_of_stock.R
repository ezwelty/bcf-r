library(magrittr)
source("functions.R")
source("passwords.R")

# ---- Load raw invoice data ----

token <- pma_login(pma_user, pma_pass)
invoices <- get_foodclub_invoices(overwrite = FALSE, token = token)
products <- get_foodclub_products(overwrite = FALSE, token = token)

# ---- Select Costco out of stock----

df <- invoices %>%
  dplyr::filter(
    account_id == "bcf_costco",
    key %>% grepl("^out_of_stock", .)
  ) %>%
  dplyr::filter(
    key %>% grepl("out_of_stock_for_user", .) %>% not()
  ) %>%
  dplyr::transmute(
    account_id, order_date,
    code = key %>% gsub("out_of_stock_", "", .)
  ) %>%
  dplyr::left_join(products, by = c("account_id", "code", "order_date")) %>%
  dplyr::mutate_all(
    function(x) {ifelse(x == "NULL", NA, x)}
  ) %>%
  dplyr::select(
    order_date, code, category, sub_category, manufacturer, description, size
  )

# ----- Results ----

# Count total orders
orders <- products %>%
  dplyr::filter(account_id == "bcf_costco") %>%
  dplyr::select(order_date, code) %>%
  dplyr::group_by(order_date, code) %>%
  dplyr::summarise_all(dplyr::first) %>%
  dplyr::group_by(code) %>%
  dplyr::summarise(orders = length(code))

# Count out-of-stock orders
out_orders <- df %>%
  dplyr::group_by(order_date, code) %>%
  dplyr::summarise_all(
    dplyr::first
  ) %>%
  dplyr::group_by(code) %>%
  dplyr::summarise(
    out_orders = length(code)
  ) %>%
  dplyr::arrange(-out_orders)

# Compile counts
counts <- orders %>%
  dplyr::right_join(out_orders, by = "code") %>%
  dplyr::mutate(out_percent = round(100 * (out_orders / orders)))
counts %>%
  readr::write_csv("costco_out_of_stock-orders_by_code.csv", na = "")

# List occurences
df %>%
  dplyr::group_by(order_date, code) %>%
  dplyr::summarise_all(
    dplyr::first
  ) %>%
  dplyr::left_join(counts, by = "code") %>%
  readr::write_csv("costco_out_of_stock-all_occurences.csv", na = "")
