library(magrittr)
source("functions.R")
source("passwords.R")

# Read all orders
token <- pma_login(pma_user, pma_pass)
# tax_exempt_users <- c("saramolton16@gmail.com", "bonaishalom")
tax_exempt_users <- c()
orders <- get_orders(overwrite = TRUE, token = token) %>%
  dplyr::mutate(
    tax_exempt = user_id %in% tax_exempt_users
  ) %>%
  dplyr::filter(
    account_id != "bcf_internal"
  )

# Save new rows to file
orders %>%
  dplyr::select(foodclub, order_date, account_id, user_id, price_paid, tax_paid, collected, sales, tax, food, tax_exempt) %>%
  dplyr::arrange(order_date, account_id, user_id) %>%
  dplyr::filter(order_date > as.Date("2018-09-16")) %>%
  write.table("new_orders.tsv", quote = FALSE, na = "", sep = "\t", row.names = FALSE, col.names = FALSE)

# Copy-paste new_orders.tsv into https://docs.google.com/spreadsheets/d/1UCTvRndnzphGTGPRsSX9Y6FuuZhMIMlF7XRTb1jGzk4/edit#gid=1616631951

# For testing:
orders <- get_foodclub_orders(overwrite = FALSE, token = token)
