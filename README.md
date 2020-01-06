# Boulder Cooperative Food (BCF) R utilities

## Installation and use

Clone the repository and enter the directory.

```bash
git clone https://github.com/ezwelty/bcf-r.git
cd bcf-r
```

Copy the template passwords file to `passwords.R` and update this new file with your credentials.

```bash
cp passwords.R.template passwords.R
```

Start R.

```bash
R
```

Once in R, install the required packages.

```r
c('magrittr', tibble', 'dplyr', 'httr', 'xml2', 'readr', 'readxl', 'stringr',
  'lubridate', 'rlang') %>%
  setdiff(row.names(installed.packages())) %>%
  install.packages()
```

## Update orders data

The script below loads archived invoices from the Foodclub database for a particular month and formats them for use in the Orders tab of the bookkeeping spreadsheet. The result is printed to the terminal window. Simply select and copy the output and paste it to the end of the sheet.

```r
# Variables (change as needed)
YEAR <- 2019  # Order year
MONTH <- 12  # Order month

# Load functions
source('functions.R')

# Get an authentication token from the Foodclub database
source('passwords.R')
token <- pma_login(pma_user, pma_pass)

# Pull orders data from the Foodclub database
df <- pma_get_table(
    table = 'custom_view_dw_archived_invoice_user_totals_bouldercoopfood',
    token = token
  )

# Massage orders data into desired format
orders <- df %>%
  # Fix errors in data
  clean_foodclub_orders() %>%
  # Format data
  format_foodclub_orders() %>%
  # Remove member shares
  dplyr::filter(account_id != 'bcf_internal') %>%
  # Add foodclub = TRUE column
  tibble::add_column(foodclub = TRUE, .before = 1) %>%
  # Sort by date, then supplier, then user
  dplyr::arrange(order_date, account_id, user_id) %>%
  # Filter by year and month
  dplyr::filter(
    lubridate::year(order_date) == YEAR,
    lubridate::month(order_date) == MONTH
  )

# Print results for easy cut and paste
orders %>%
  write.table(
    quote = FALSE, na = "", sep = "\t", row.names = FALSE, col.names = FALSE)
```

## Update Golden Organics pricelist

By request, Golden Organics emails a new pricelist (as an Excel spreadsheet) roughly every month. The script below updates the Foodclub product database by generating a list of SQL update commands. These should be reviewed manually before being run, since the pricelist can contain errors and inconsistencies that sneak past the automated fixes. Things to watch out for:

- Rows in the pricelist spreadsheet that precede the row with the column names must be skipped when reading the file. The number of rows can change, so set the variable `SKIP` accordingly.
- The column names can change, so set the respective variables (e.g. `CODE`, `DESCRIPTION`) accordingly.
- Product categories and origins can be incorrectly or differently spelled. Attempts are made to standardize these, but any updates should be scrutinized in the SQL commands.
- Product sizes are sometimes wrong, and this can cause problems if they make it into our database. Most item codes contain the weight in pounds (e.g. "choc5" is 5 lb of chocolate chips), so the script will warn of any inconsistencies between a product code and a weight in pounds (e.g. "choc1" with size "5 lb"). However, some of these are actually correct (e.g. "hon196" is 196 ounces of honey, so actually "12.25 lb"), so it is best to check for any updates to product size in the SQL commands.

Fix any issues like mispelled categories or erroneous product weight by editing the spreadsheet, then rerun the script starting from the step "Read new pricelist from file".

```r
# Variables (change as needed)
PATH <- 'pricelist.xlsx'  # Path to new pricelist
SKIP <- 7  # Number of rows before row with column names in pricelist
# Column names in pricelist
CODE <- 'Item ID'
DESCRIPTION <- 'Item Description'
ORIGIN <- 'Location'
SIZE <- 'Stocking U/M'
PRICE <- 'Commercial'

# Load functions
source('functions.R')

# Get an authentication token from the Foodclub database
source('passwords.R')
token <- pma_login(pma_user, pma_pass)

# Pull old pricelist from the Foodclub database
old <- pma_get_table(table = 'private_bcf_goldenorganics', token = token)

# Read new pricelist from file
new <- go_read_pricelist(
  path = PATH, skip = SKIP, code = CODE, description = DESCRIPTION,
  origin = ORIGIN, size = SIZE, price = PRICE
  )

# Generate SQL commands needed to update old pricelist to match new pricelist.
sql <- go_build_sql(old, new, token = token)

# Print and review SQL commands.
# If issues are found, fix them in the pricelist,
# then repeat from step "Read new pricelist from file".
sql

# Update pricelist on Foodclub database
sql %>%
  split(ceiling(seq_along(.) / 25)) %>%
  sapply(function(queries) {
    result <- queries %>%
      paste(collapse = ";") %>%
      pma_query(token = token)
    Sys.sleep(1)
    return(result)
  })
```

## Count Costco out-of-stock

```r
# Load functions
source('functions.R')

# Get an authentication token from the Foodclub database
source('passwords.R')
token <- pma_login(pma_user, pma_pass)

# Pull product-level invoice data from the Foodclub database
invoices <- pma_get_table(
  table = 'custom_view_archived_invoice_entry_data_bouldercoopfood',
  hex = c('key'), token = token)
products <- pma_get_table(
  table = 'custom_view_archived_orders_bouldercoopfood',
  hex = c('user_id', 'code'), token = token)

# Count total orders by product code
total_orders <- products %>%
  dplyr::filter(account_id == "bcf_costco") %>%
  dplyr::group_by(order_date, code) %>%
  dplyr::summarise(orders = 1) %>%
  dplyr::group_by(code) %>%
  dplyr::summarise(orders = sum(orders))

# Count out-of-stock orders by product code
oos_orders <- invoices %>%
  dplyr::filter(
    account_id == 'bcf_costco',
    grepl('^out_of_stock', key),
    !grepl('^out_of_stock_for_user', key)
  ) %>%
  dplyr::mutate(
    code = gsub('out_of_stock_', '', key)
  ) %>%
  dplyr::left_join(products, by = c('account_id', 'code', 'order_date')) %>%
  dplyr::group_by(order_date, code) %>%
  dplyr::summarise(oos_orders = 1) %>%
  dplyr::group_by(code) %>%
  dplyr::summarise(oos_orders = sum(oos_orders))

# Compile counts
total_orders %>%
  dplyr::right_join(oos_orders, by = 'code') %>%
  dplyr::mutate(oos_percent = round(100 * (oos_orders / orders))) %>%
  dplyr::arrange(-oos_percent, -orders)
```
