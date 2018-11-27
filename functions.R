library(magrittr)

# Get elements by exact name only
`$` <- function(x, name) {
  name <- deparse(substitute(name))
  x[[name, exact = TRUE]]
}

# ---- Helpers -----

#' Parse hex to string
#'
#' @param x (character) Hexadecimal strings
#' @examples
#' parse_hex(c("4f7374617261", "7069636b6c6562726963"))
parse_hex <- function(x) {
  sapply(x, function(s) {
    seq(1, nchar(s), by = 2) %>%
      sapply(function(x) substr(s, x, x + 1)) %>%
      strtoi(base = 16L) %>%
      as.raw() %>%
      rawToChar()
  }, USE.NAMES = FALSE)
}

#' Format object for SQL query
#'
#' Strings are wrapped in single quotes (\code{"'x'"}) and \code{NA}, \code{NULL} converted to \code{"NULL"}.
#' @param x Object to format
#' @examples
#' format_sql(1)
#' format_sql("foodclub")
#' format_sql(NA)
#' format_sql(NULL)
format_sql <- function(x) {
  if (is.character(x)) {
    paste0("'", x, "'")
  } else if (is.null(x) || is.na(x)) {
    "NULL"
  } else {
    x
  }
}

# ---- Foodclub (website) ----

#' Login to Foodclub
#'
#' @param username (character) Username
#' @param password (character) Password
#' @return Authorization token
#' @examples
#' \dontrun{
#' foodclub_login("username", "password")
#' }
foodclub_login <- function(username, password) {
  url <- "https://foodclub.org/bouldercoopfood/login"
  response <- httr::POST(url, body = list(user_id = username, password = password, login = "Login"))
  index_url <- "https://foodclub.org/bouldercoopfood/index"
  if (httr::GET(index_url)$url == index_url) {
    return(httr::handle_find(url))
  } else {
    stop("Login failed")
  }
}

# ---- Foodclub (phpMyAdmin) ----

#' Login to Foodclub phpMyAdmin
#'
#' @param username (character) Username
#' @param password (character) Password
#' @return Authorization token
#' @examples
#' \dontrun{
#' pma_login("username", "password")
#' }
pma_login <- function(username, password) {
  url <- "https://foodclub.org/phpmyadmin/index.php"
  body <- list(pma_username = username, pma_password = password)
  response <- httr::POST(url, body = body)
  token <- response$url %>%
    httr::parse_url() %$% query %$% token
  if (response$status_code != 200 || is.null(token)) {
    stop("Login failed")
  }
  token
}

#' Run SQL query via Foodclub phpMyAdmin
#'
#' @param sql (character) SQL query
#' @param table (character) Database table (default: \code{NULL})
#' @param db (character) Database (default: \code{"foodclub"})
#' @param token (character) Authentication token (see \code{\link{pma_login}})
#' @param result (boolean) Whether to return the query result (\code{TRUE}) or a boolean success flag (\code{FALSE})
#' @return Query result as a \code{\code{data.frame}} or boolean success flag (\code{result = FALSE})
#' @examples
#' \dontrun{
#' token <- pma_login("username", "password")
#' pma_query("select * from private_bcf_goldenorganics", token = token, result = TRUE)
#' }
pma_query <- function(sql, table = NULL, db = "foodclub", token, result = FALSE) {
  url <- "https://foodclub.org/phpmyadmin/import.php"
  query <- list(
    token = token,
    table = table,
    db = db,
    pos = 0,
    goto = "tbl_sql.php",
    sql_query = sql,
    show_query = 0
  )
  xml <- httr::POST(url, query = query) %>%
    httr::content()
  success <- xml %>%
    xml2::xml_find_first("//div[@class = 'success']")
  if (length(success) > 0) {
    if (result) {
      xml %>%
        xml2::xml_find_first(xpath = "//table[@id = 'table_results']") %>%
        rvest::html_table(header = TRUE, fill = TRUE, trim = TRUE)
    } else {
      TRUE
    }
  } else {
    FALSE
  }
}

#' Get table via Foodclub phpMyAdmin
#'
#' May be slow for very large tables since the function works by scraping HTML.
#'
#' @param table (character) Database table
#' @param db (character) Database (default: \code{"foodclub"})
#' @param token (character) Authentication token (see \code{\link{pma_login}})
#' @param hex (character) Field names to parse from hex (see \code{\link{parse_hex}})
#' @param block (integer) Number of rows to process at a time
#' @return Database table as a \code{\link{data.frame}}
#' @examples
#' \dontrun{
#' token <- pma_login("username", "password")
#' pma_get_table("private_bcf_goldenorganics", token = token)
#' }
pma_get_table <- function(table, db = "foodclub", token, hex = c("user_id"), block = 1000) {
  # Count table rows
  sql <- paste0("SELECT COUNT(*) as rows FROM ", db, ".", table, ";")
  rows <- pma_query(sql = sql, db = db, token = token, result = TRUE)$rows
  # Read table in blocks
  starts <- seq(0, rows - 1, block)
  success <- TRUE
  dfs <- starts %>%
    lapply(function(i) {
      sql <- paste0("SELECT * FROM ", db, ".", table, " LIMIT ", i, ", ", block, ";")
      df <- pma_query(sql = sql, db = db, token = token, result = TRUE) %>%
        data.frame(fix.empty.names = TRUE)
      if (is.logical(df) && !df) {
        success <- FALSE
        break
      } else {
        df
      }
    })
  if (success) {
    df <- dfs %>%
      dplyr::bind_rows() %>%
      dplyr::filter(Var.2 == "Edit") %>% # Remove header rows
      dplyr::select(-dplyr::starts_with('Var.')) %>% # Remove non-data columns
      dplyr::mutate_at(.funs = parse_hex, .vars = intersect(hex, names(.)))
  } else {
    FALSE
  }
}

# Export cannot be accessed via URL
# pma_export_csv <- function(table, db = 'foodclub', token) {
#   url <- 'https://foodclub.org/phpmyadmin/export.php'
#   body <- list(
#     db = db, table = table, token = token,
#     single_table = TRUE, export_type = 'table', export_method = 'quick', quick_or_custom = 'custom',
#     allrows = 1, output_format = 'astext', filename_template = '@TABLE@', charset_of_file = 'utf-8',
#     compression = 'none', what = 'csv',
#     csv_null = '', csv_columns = 'something')
#   response <- httr::POST(url, httr::write_disk('temp.csv', overwrite = TRUE), body = body)
#   textarea <- xml %>%
#     xml2::xml_find_first(xpath = "//textarea[@name = 'sqldump']")
#   if (length(textarea) > 0) {
#     textarea %>%
#       xml2::xml_text()
#   } else {
#     return(FALSE)
#   }
# }

#' Import CSV via Foodclub phpMyAdmin
#'
#' Expects a CSV file with column names in the first row which all correspond to Foodclub database schema fields.
#'
#' @param path (character) Path to CSV file
#' @param table (character) Database table
#' @param db (character) Database (default: \code{"foodclub"}).
#' @param csv_replace (boolean) Whether to delete existing table rows before import (default: \code{TRUE})
#' @param csv_columns (character) Table fields corresponding to each column of the csv file (default: \code{names(read.csv(import_file)})
#' @param skip_queries (numeric) Number of lines to skip when reading \code{file} (default: \code{1})
#' @param token (character) Authentication token (see \code{\link{pma_login}})
#' @examples
#' \dontrun{
#' token <- pma_login("username", "password")
#' pma_import_csv(file.csv, "table", token = token)
#' }
pma_import_csv <- function(path, table, db = "foodclub", csv_replace = TRUE, csv_columns = names(read.csv(path)), skip_queries = 1, token) {
  csv_columns <- paste(csv_columns, collapse = ",")
  url <- "https://foodclub.org/phpmyadmin/import.php"
  body = c(
    mget(c("token", "table", "db", "csv_replace", "csv_columns", "skip_queries")),
    list(
      import_type = "table", format = "csv", charset_of_file = "utf-8",
      csv_terminated = ",", csv_enclosed = "\"", csv_escaped = "\"", csv_new_line = "auto",
      import_file = httr::upload_file(path))
  )
  response <- httr::POST(url, body = body)
  # Print query result
  httr::content(response) %>%
    xml2::xml_find_first("//div[@id='result_query']") %>%
    xml2::xml_text() %>%
    cat()
}

#' Write object to CSV for import via Foodclub phpMyAdmin
#'
#' As expected by phpMyAdmin, \code{NA} is written as NULL (without quotes) and row names are left out.
#'
#' @param x (coercible to data.frame) Object to write
#' @param path (character) Path to write to (default: \code{""}, output to console)
#' @examples
#' x <- data.frame(code = c("a", "b"), price = c(10, NA))
#' pma_write_import_csv(x)
pma_write_import_csv <- function(x, path = "") {
  write.csv(x, file = path, na = "NULL", row.names = FALSE)
}

# ---- Finances ----

#' Get Foodclub users (UNUSED)
get_foodclub_users <- function(overwrite = FALSE, token) {
  cache <- "users.rds"
  if (overwrite || !file.exists(cache)) {
    pma_get_table("custom_view_users_bouldercoopfood", token = token) %T>%
      saveRDS(cache)
  } else {
    readRDS(cache)
  }
}

#' Get Foodclub orders
get_foodclub_orders <- function(overwrite = FALSE, token) {
  cache <- "orders.rds"
  if (overwrite || !file.exists(cache)) {
    pma_get_table("custom_view_dw_archived_invoice_user_totals_bouldercoopfood", token = token) %T>%
      saveRDS(cache)
  } else {
    readRDS(cache)
  }
}

get_foodclub_products <- function(overwrite = FALSE, token) {
  cache <- "products.rds"
  if (overwrite || !file.exists(cache)) {
    pma_get_table("custom_view_archived_orders_bouldercoopfood", token = token, hex = c("user_id", "code")) %T>%
      saveRDS(cache)
  } else {
    readRDS(cache)
  }
}

get_foodclub_invoices <- function(overwrite = FALSE, token) {
  cache <- "invoices.rds"
  if (overwrite || !file.exists(cache)) {
    pma_get_table("custom_view_archived_invoice_entry_data_bouldercoopfood", token = token, hex = c("key")) %T>%
      saveRDS(cache)
  } else {
    readRDS(cache)
  }
}

#' Parse Foodclub orders
#' account_id | order_date | user_id | price_paid | tax_paid | collected | sales | tax | food | tax_exempt
parse_foodclub_orders <- function(orders) {

  ## Check for accounting changes
  if (any(orders$refunds != 0)) {
    stop("Non-zero refund")
  }

  ## Remove empty orders
  orders <- orders[orders$pretax != 0, ]

  ## Reallocate Costco true-ups
  # Early Costco orders lacked receipts, so percent fees were applied to match the charges on the debit card. Reallocate these fees to the price and tax paid by members:
  true_up_orders <- list(
    list(
      account_id = "bcf_costco",
      begin = as.Date("2017-02-08"),
      end = as.Date("2017-04-29")
    ),
    list(
      account_id = "bcf_costco_nf",
      begin = as.Date("2017-02-08"),
      end = as.Date("2017-04-29")
    )
  )
  for (x in true_up_orders) {
    ind <- with(orders, account_id == x$account_id & order_date >= x$begin & order_date <= x$end)
    true_ups <- with(orders[ind, ], (overall_order - order_subtotal) /  order_subtotal, 0)
    modified <- orders[ind, ] %>%
      dplyr::mutate(
        pretax = pretax * (1 + true_ups),
        tax = tax * (1 + true_ups),
        overall_order = pretax + tax,
        custom_fees = 0
      )
    err <- modified$overall_order - orders$overall_order[ind]
    if (any(err >= 0.01)) {
      stop("Change in order total larger than rounding error")
    }
    orders[ind, ] <- modified
  }

  ## Move custom fees to member fees
  # The first several months of orders used `custom_fees` for markups, rather than the standard `member_fees`. Reassign all custom fees as member fees:
  custom_fee_orders <- list(
    list(
      begin = as.Date("2017-02-08"),
      end = as.Date("2017-06-15")
    )
  )
  for (x in custom_fee_orders) {
    ind <- with(orders, order_date >= x$begin & order_date <= x$end)
    orders[ind, ] %<>%
      dplyr::mutate(
        member_fees = custom_fees,
        custom_fees = 0
      )
  }
  if (any(orders$custom_fees != 0)) {
    stop("Non-zero custom fees")
  }
  orders$custom_fees <- NULL

  ## Reassign misassigned orders
  # Foodclub accounts do not always map perfectly to BCF membership. Reassign misassigned orders to the correct account:
  misassigned_orders <- list(
    # Loren Matilsky used his personal Foodclub account for Ostara orders before moving out.
    list(
      from = "illorenzo",
      to = "Ostara",
      begin = as.Date("2017-02-08"),
      end = as.Date("2017-02-20")
    )
  )
  for (x in misassigned_orders) {
    ind <- with(orders, user_id == x$from & order_date >= x$begin & order_date <= x$end)
    orders$user_id[ind] <- x$to
  }

  ## Split out total-only orders
  # The first few orders did not seperate price components. Reconstruct wholesale price, tax, and markup for these orders:
  total_only_orders <- list(
    list(
      account_id = "bcf_goldenorganics",
      date = as.Date("2017-02-08"),
      markup = 0.1
    ),
    list(
      account_id = "bcf_costco",
      date = as.Date("2017-02-08"),
      tax = 0.0346
    ),
    list(
      account_id = "bcf_fiordilatte",
      date = as.Date("2017-02-13"),
      markup = 0.039803,
      tax_applied = 0.08995
    )
  )
  for (x in total_only_orders) {
    built_in <- sum(x$markup, x$tax)
    ind <- with(orders, account_id == x$account_id & order_date == x$date)
    xtax <- sum(x$tax, x$tax_applied)
    xmarkup <- sum(x$markup, x$markup_applied)
    modified <- orders[ind, ] %>%
      dplyr::mutate(
        pretax = pretax / (1 + built_in),
        tax = xtax * pretax,
        member_fees = xmarkup * (pretax + tax),
        overall_order = pretax + tax + member_fees
      )
    err <- modified$overall_order - orders$overall_order[ind]
    if (any(err >= 0.01)) {
      stop("Change in order total larger than rounding error")
    }
    orders[ind, ] <- modified
  }

  ## Move olive oil in mixed bcf_internal order to bcf_other (2018-04-16)
  mixed_orders <- list(
    list(
      user_id = "ingram",
      shares = 50
    ),
    list(
      user_id = "oragoldman",
      shares = 25
    )
  )
  ind <- with(orders, order_date == as.Date("2018-04-16") & account_id == "bcf_internal")
  temp <- orders[ind, ]
  temp$account_id <- "bcf_other"
  markup <- 0.10 # special case
  for (x in mixed_orders) {
    # Set bcf_internal to only member shares
    i <- with(orders, ind & user_id == x$user_id)
    orders[i, c("pretax", "invoice", "order_subtotal")] <- x$shares / (1 + markup)
    orders[i, c("member_fees")] <- x$shares * (markup / (1 + markup))
    orders[i, c("overall_order")] <- x$shares
    orders[i, c("tax")] <- 0
    # Set bcf_other to only olive oil
    j <- with(temp, user_id == x$user_id)
    temp[j, c("pretax", "tax", "invoice", "order_subtotal", "member_fees", "overall_order")] %<>%
      subtract(orders[i, c("pretax", "tax", "invoice", "order_subtotal", "member_fees", "overall_order")])
  }
  orders <- rbind(
      orders[!ind, ],
      orders[ind & orders$user_id %in% sapply(mixed_orders, "[[", "user_id"), ],
      temp)

  ## Move olive oil in unmixed bcf_internal order to bcf_other (2018-05-07)
  ind <- with(orders, order_date == as.Date("2018-05-07") & account_id == "bcf_internal")
  orders$account_id[ind] <- "bcf_other"

  ## Correct Chrysalis markup fiasco
  # Remove spurious Costco (non-food) order
  orders %<>%
    dplyr::filter(order_date != as.Date("2018-09-14"))
  # Fix Chrysalis member_fees (15% -> 10%) on 2018-08-30 Golden Organics and 2018-09-04 Costco
  ind <- which(with(orders, user_id == "Chrysalis" & order_date == as.Date("2018-08-30") & account_id == "bcf_goldenorganics"))
  orders$member_fees[ind] <- orders$order_subtotal[ind] * 0.10
  orders$overall_order[ind] <- orders$member_fees[ind] + orders$order_subtotal[ind]
  ind <- which(with(orders, user_id == "Chrysalis" & order_date == as.Date("2018-09-04") & account_id == "bcf_costco"))
  orders$member_fees[ind] <- orders$order_subtotal[ind] * 0.10
  orders$overall_order[ind] <- orders$member_fees[ind] + orders$order_subtotal[ind]

  ## Compute taxes paid and collected
  food_tax <- 0.0386
  nonfood_tax <- 0.08845
  orders %<>%
    dplyr::mutate(
      # price_paid: Pretax price paid to supplier (pretax)
      price_paid = pretax,
      # tax_paid: Tax paid to supplier
      tax_rate_paid = (tax / pretax) %>%
        # Costco
        # Before 2017-06-10: Tax paid on food (0.0346) and non-food (0.08445)
        # Since 2017-06-10: No tax paid (reimbursed for orders through 2017-07-20)
        replace(orders$account_id == "bcf_costco" & orders$order_date < as.Date("2017-06-10"), 0.0346) %>%
        replace(orders$account_id == "bcf_costco" & orders$order_date >= as.Date("2017-06-10"), 0) %>%
        replace(orders$account_id == "bcf_costco_nf" & orders$order_date < as.Date("2017-06-10"), 0.08445) %>%
        replace(orders$account_id == "bcf_costco_nf" & orders$order_date >= as.Date("2017-06-10"), 0) %>%
        # Frontier
        # Before 2017-08-31: Tax paid combination of food (0.0386) and non-food (0.08845)
        # Since 2017-08-31: No tax paid
        replace(orders$account_id == "bcf_frontiernaturalfoods" & orders$order_date >= as.Date("2017-08-31"), 0) %>%
        # All others: No tax paid
        replace(!orders$account_id %in% c("bcf_costco", "bcf_costco_nf", "bcf_frontiernaturalfoods"), 0),
      tax_paid = pretax * tax_rate_paid,
      # collected: Total collected from user (overall_order)
      collected = overall_order,
      # food: Fraction of food in order
      food = ifelse(
        account_id == "bcf_frontiernaturalfoods",
        ((tax / pretax) - nonfood_tax) / (food_tax - nonfood_tax),
        ifelse(account_id %in% c("bcf_costco_nf", "bcf_internal"), 0, 1)
      ),
      tax_rate_owed = food * food_tax + (1 - food) * nonfood_tax,
      # sales: Sales (price + markup) collected from user
      sales = collected / (1 + tax_rate_owed),
      # tax: Tax owed
      tax = sales * tax_rate_owed
    ) %>%
    dplyr::arrange(order_date, account_id, user_id) %>%
    dplyr::select(account_id, order_date, user_id, price_paid, tax_paid, collected, sales, tax, food)

  ## Set all but collected to zero for member shares
  ind <- orders$account_id == "bcf_internal"
  orders[ind, c("price_paid", "tax_paid", "sales", "tax", "food")] <- 0

  ## Adjust pretax value of olive oil sold from inventory
  inventory_orders <- list(
    list(
      order_date = as.Date("2018-05-07"),
      markup = 26.85 / 25
    )
  )
  for (x in inventory_orders) {
    ind <- with(orders, account_id == "bcf_other" & order_date == x$order_date)
    orders$price_paid[ind] %<>% divide_by(x$markup)
  }

  orders
}

#' Get Pre-Foodclub orders
get_prefoodclub_orders <- function() {
  read.csv("prefoodclub_orders.csv", stringsAsFactors = FALSE, na.strings = "") %>%
    dplyr::mutate(
      order_date = as.Date(order_date),
      user_id = ifelse(is.na(user_id), member, user_id)
    ) %>%
    dplyr::filter(price_paid > 0) %>%
    dplyr::select(-tax_rate_paid, -fee_rate, -supplier, -member)
}

get_orders <- function(...) {
  pre_orders <- get_prefoodclub_orders() %>%
    dplyr::mutate(foodclub = FALSE)
  orders <- get_foodclub_orders(...) %>%
    parse_foodclub_orders() %>%
    dplyr::mutate(foodclub = TRUE)
  dplyr::bind_rows(orders, pre_orders) %>%
    dplyr::arrange(account_id, order_date, user_id)
}
