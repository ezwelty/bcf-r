library(magrittr)
source("functions.R")
source("passwords.R")

# ---- Functions ----

#' Parse Golden Organics item sizes
#'
#' @examples
#' parse_sizes(c("1", "12 oz", "2 x 12 oz", "2 x 1 lb", "2 gal", "6 x #10", "6x108oz"))
parse_sizes <- function(sizes) {
  parts <- sizes %>%
    gsub("#|\\.$", "", .) %>%
    gsub("^([a-z])", "1 \\1", .) %>%
    gsub("([0-9])([a-z])", "\\1 \\2", .) %>%
    gsub("([0-9])$", "\\1 lb", .) %>%
    stringr::str_match("([0-9\\.]+)\\s*x*\\s*([0-9\\.]+)* ([a-z]+)")
  x <- ifelse(is.na(parts[, 3]), 1, parts[, 2]) %>%
    as.numeric()
  value <- ifelse(is.na(parts[, 3]), parts[, 2], parts[, 3]) %>%
    as.numeric()
  unit <- parts[, 4]
  new_value <- value
  new_unit <- unit
  # 16 oz = 1 lb
  oz = !is.na(unit) & unit == "oz"
  new_value[oz] %<>%
    divide_by(16) %>%
    round(digits = 3)
  new_unit[oz] <- "lb"
  # Results
  df <- data.frame(
    size = paste(new_value * x, new_unit) %>%
      replace(is.na(new_value), NA),
    description = paste(x, "x", value, unit) %>%
      replace(x == 1, NA),
    valid_split_increment = ifelse(x == 1, 0, new_value),
    stringsAsFactors = FALSE
  )
  unit_changed <- !is.na(df$description) & !is.na(new_unit) & new_unit != unit
  df$description[unit_changed] %<>% paste("/", new_value[unit_changed], new_unit[unit_changed])
  df
}

#' Parse Golden Organics item origin
#'
#' @examples
#' parse_origins(c("US", "F38"))
parse_origins <- function(origins) {
  replacements <- c(
    Dominica = "Dominican Republic",
    `Dom Repub` = "Dominican Republic",
    US = "USA",
    F38 = NA,
    `N. Dakota` = "North Dakota",
    `S. Dakota` = "South Dakota",
    Philppines = "Philippines",
    Philippine = "Philippines",
    Philipines = "Philippines",
    Spaon = "Spain",
    ua = "USA",
    Netherland = "Netherlands",
    Thailnd = "Thailand"
  )
  replace <- origins %in% names(replacements)
  origins[replace] %<>%
    replacements[.]
  origins
}

#' Parse Golden Organics item categories
#'
#' @examples
#' parse_categories(c("SPICE", "BAKING"))
parse_categories <- function(categories) {
  categories %>%
    stringr::str_to_title() %>%
    replace(. == 'Non Food', 'Non-Food') %>%
    replace(. == 'Sweetners', 'Sweeteners') %>%
    replace(. == 'Spice', 'Spices') %>%
    replace(. == 'Grains', 'Grain')
}

#' Read Golden Organics pricelist
#'
#' @examples
#' \dontrun{
#' read_pricelist("Commercial Pricelist March 2018.xlsx")
#' }
read_pricelist <- function(path) {
  if (grepl('\\.csv$', path)) {
    df <- path %>%
      readr::read_csv(skip = 10)
  } else {
    df <- path %>%
      readxl::read_excel(skip = 10)
  }
  df %<>%
    dplyr::mutate(
      category = NA
    )
  is_category <- df[, 2] %>%
    is.na()
  breaks <- is_category %>%
    which() %>%
    c(nrow(df) + 1)
  for (i in seq_len(length(breaks) - 1)) {
    df$category[breaks[i]:(breaks[i + 1] - 1)] <- unlist(df[breaks[i], 1])
  }
  df %<>%
    dplyr::filter(!is_category) %>%
    dplyr::rename(
      code = `Item ID`,
      description = `Item Description`,
      origin = Location,
      size = `Stocking U/M`,
      price = Commercial
    ) %>%
    dplyr::select(-`Price/lb`)
  sizes <- df$size %>%
    parse_sizes()
  df %<>%
    dplyr::mutate(
      size = (sizes$size),
      description = ifelse(is.na(sizes$description), description, paste0(description, " (", sizes$description, ")")) %>%
        gsub("[ ]+", " ", .),
      valid_split_increment = sizes$valid_split_increment,
      origin = parse_origins(origin),
      category = parse_categories(category)
    ) %>%
    dplyr::filter(gsub("[- ]", "", tolower(category)) != "non food")
  duplicates <- duplicated(df$code)
  if (any(duplicates)) {
    all_duplicates <- duplicates | duplicated(df$code, fromLast = TRUE)
    warning("Removing items with duplicate codes")
    print(df[all_duplicates, ])
    df %<>%
      dplyr::filter(!duplicates)
  }
  if (any(is.na(df$size))) {
    warning("Items missing size")
  }
  df
}

#' Build SQL query to update product database
#'
#' @param old (data.frame) Foodclub Golden Organics product database (see \code{\link{pma_get_table}})
#' @param new (data.frame) Golden Organics pricelist
#' @param token (character) Authorization token (see \code{\link{pma_login}})
#' @examples
#' \dontrun{
#' token <- pma_login("username", "passworld")
#' old <- pma_get_table("private_bcf_goldenorganics", token = token)
#' new <- read_pricelist("Commercial Pricelist March 2018.xlsx")
#' build_sql(old, new, token = token)
#' }
build_sql <- function(old, new, token) {
  update_sql <- function(i, j = NULL) {
    updates <- list()
    if (length(j) == 0) {
      # Out of stock
      if (is.na(old$num_available[i])) {
        updates["num_available"] <- 0
      }
    } else {
      # In stock
      if (!new$price[j] %in% as.numeric(old$price[i])) {
        updates["price"] <- new$price[j]
      }
      if (!new$size[j] %in% old$size[i]) {
        updates["size"] <- new$size[j]
      }
      if (!new$description[j] %in% old$description[i]) {
        updates["description"] <- new$description[j]
      }
      if (!new$category[j] %in% old$category[i]) {
        updates["category"] <- new$category[j]
      }
      if (!new$origin[j] %in% old$origin[i]) {
        updates["origin"] <- new$origin[j]
      }
      if (!is.na(old$num_available[i])) {
        updates["num_available"] <- NA
      }
    }
    if (length(updates) > 0) {
      updates %>%
        lapply(format_sql) %>%
        paste(names(.), ., sep = " = ", collapse = ", ") %>%
        paste("UPDATE private_bcf_goldenorganics SET", ., "WHERE code =", format_sql(old$code[i]))
    }
  }
  insert_sql <- function(j) {
    defaults <- list(
      valid_order_increment = 1
    )
    values <- c(defaults, new[j, ])
    values %>%
      lapply(format_sql) %>%
      {
        paste0(
          "INSERT INTO private_bcf_goldenorganics (",
          paste(names(.), collapse = ", "),
          ") VALUES (",
          paste(., collapse = ", "), ")")
      }
  }
  union(old$code, new$code) %>%
    lapply(function(code) {
      i = which(old$code == code)
      j = which(new$code == code)
      is_i <- length(i) == 1
      is_j <- length(j) == 1
      if (is_i & !is_j) {
        # UPDATE: Out of stock
        sql <- update_sql(i)
      } else if (is_i & is_j) {
        # UPDATE: In stock
        sql <- update_sql(i, j)
      } else if (!is_i & is_j) {
        # INSERT
        sql <- insert_sql(j)
      } else {
        stop(paste0(code, " - i: ", i, ", j: ", j))
      }
      sql %>%
        gsub("'NA'", "NULL", .)
    })
}

# ---- Update pricelist ----

token <- pma_login(pma_user, pma_pass)
old <- pma_get_table("private_bcf_goldenorganics", token = token) %>%
  dplyr::mutate_if(is.character, .funs = function(x) {x %>% replace(. == "NULL", NA)})
new <- read_pricelist("~/downloads/golden_pricelist.csv") %>%
  dplyr::select_if(.predicate = function(x) {!all(is.na(x))})
sql <- build_sql(old, new, token = token) %>%
  unlist()
sql %>%
  split(ceiling(seq_along(.) / 25)) %>%
  sapply(function(queries) {
    result <- queries %>%
      paste(collapse = ";") %>%
      pma_query(token = token)
    Sys.sleep(1)
    return(result)
  })
