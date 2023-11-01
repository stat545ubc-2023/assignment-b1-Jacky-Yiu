Assignment_B1
================
Jacky Yiu
2023-10-31

## Prerequisites

Please install the following libraries

``` r
library(datateachr)
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.3     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ## ✔ ggplot2   3.4.3     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(testthat)
```

    ## 
    ## Attaching package: 'testthat'
    ## 
    ## The following object is masked from 'package:dplyr':
    ## 
    ##     matches
    ## 
    ## The following object is masked from 'package:purrr':
    ## 
    ##     is_null
    ## 
    ## The following objects are masked from 'package:readr':
    ## 
    ##     edition_get, local_edition
    ## 
    ## The following object is masked from 'package:tidyr':
    ## 
    ##     matches

``` r
library(dplyr)
```

## Exercise 1: Make a Function (25 points)

### Function Description:

During the tidying part of my MDA part 2, I try to tidy 2 columns with
comma-separated categorical values and a column with slash-separated
categorical values.

First I separate the delimiter-separated categorical values into
multiple rows using `separate_rows`.

Then, I adds a new column called `value` with the constant value “TURE.”

Next, it pivots the data so that each unique categorical values from the
separated column becomes a separate column in the resulting dataset,
with “TURE” indicating the presence of that categorical value and
“FALSE” as a fill value where the categorical value is absent.

Finally, I prefixes the column names with “column_name\_” to create a
tidy dataset where each column represents a specific Value and indicates
its presence or absence in each data point.

To handle NA, if the original value is “NA”, then “column_name_NA” will
be TRUE, and the rest of the newly generated column will be FALSE

the code chunk below is an example, the following operation turn the
column `amenities` into 8 different columns containing “TRUE” or
“FALSE”.

``` r
apartments_tidy <- apt_buildings %>%
  separate_rows(amenities, sep = " , ") %>% #first separating amenities into multiple rows using separate_rows
  mutate(value = TRUE) %>%  #adds a new column called "value" with the constant value "YES.", meaning the presence of the amenity
  pivot_wider(names_from = amenities, values_from = value, values_fill =list(value = FALSE), names_prefix = "amenities_")
```

### Before

in the original data, the column `amenities` contains comma separated
values, which is not tidy

``` r
glimpse(apt_buildings %>% 
  select(starts_with("amenities"))) 
```

    ## Rows: 3,455
    ## Columns: 1
    ## $ amenities <chr> "Outdoor rec facilities", "Outdoor pool", NA, NA, NA, NA, NA…

### After

amenities is separated into 8 separated columns, `amenities_NA` is
`TURE` when the original data is NA

``` r
glimpse(apartments_tidy %>% 
  select(starts_with("amenities"))) 
```

    ## Rows: 3,455
    ## Columns: 8
    ## $ `amenities_Outdoor rec facilities` <lgl> TRUE, FALSE, FALSE, FALSE, FALSE, F…
    ## $ `amenities_Outdoor pool`           <lgl> FALSE, TRUE, FALSE, FALSE, FALSE, F…
    ## $ amenities_NA                       <lgl> FALSE, FALSE, TRUE, TRUE, TRUE, TRU…
    ## $ `amenities_Indoor pool`            <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, …
    ## $ `amenities_Indoor recreation room` <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, …
    ## $ `amenities_Indoor exercise room`   <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, …
    ## $ amenities_Sauna                    <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, …
    ## $ `amenities_Child play area`        <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, …

I will turn the operation in to a function and name my function
`tidy_column_with_delimiter_seperated_values`

I will write the code and the documentation together in the next
section, please see the next section .

## Exercise 2: Document your Function (20 points)

``` r
#' Tidy Column with Delimiter-Separated Values
#'
#' This function separates a column with delimiter-separated values into multiple rows and pivots it to wide format, indicating the presence of each value.
#'
#' @param data A tibble/data frame containing the column to be tidied.
#' @param delimiter The delimiter used to separate values within the column.
#' @param column The name of the column that contains delimiter-separated values.
#'
#' @return A modified tibble/data frame where the specified column has been separated into multiple rows and pivoted to a wider format, showing the presence of each 'value.'
#' 
tidy_column_with_delimiter_seperated_values <- function(data, delimiter, column) {
  col_name <- deparse(substitute(column)) 
  stopifnot(col_name %in% colnames(data))
  stopifnot(is.character(data[[col_name]]))
  
  data %>%
    separate_rows({{ column }}, sep = delimiter) %>% 
    mutate(value = TRUE) %>%
    pivot_wider(names_from = {{ column }}, 
                values_from = value, 
                values_fill =  FALSE, 
                names_prefix = paste0(col_name , "_"))
}
```

## Exercise 3: Include examples (15 points)

### Example 1

Here, we create a different dataframe that use “;” as delimiter, and the
column name is “`Facilities`” .

``` r
example_1 <- data.frame(
  ID = 1:4,
  Facilities = c("Tennis;Spa;Pool", "Gym;Sauna", "Parking;Wifi", "Garden")
)

example_1
```

    ##   ID      Facilities
    ## 1  1 Tennis;Spa;Pool
    ## 2  2       Gym;Sauna
    ## 3  3    Parking;Wifi
    ## 4  4          Garden

We see that the function run successfully, it turn facilities in to 8
different columns containing Boolean values

``` r
tidy_column_with_delimiter_seperated_values(example_1, ";", Facilities)
```

    ## # A tibble: 4 × 9
    ##      ID Facilities_Tennis Facilities_Spa Facilities_Pool Facilities_Gym
    ##   <int> <lgl>             <lgl>          <lgl>           <lgl>         
    ## 1     1 TRUE              TRUE           TRUE            FALSE         
    ## 2     2 FALSE             FALSE          FALSE           TRUE          
    ## 3     3 FALSE             FALSE          FALSE           FALSE         
    ## 4     4 FALSE             FALSE          FALSE           FALSE         
    ## # ℹ 4 more variables: Facilities_Sauna <lgl>, Facilities_Parking <lgl>,
    ## #   Facilities_Wifi <lgl>, Facilities_Garden <lgl>

### Example 2

This time, we create a dataset with delimiter-separated values and
missing data

``` r
example_2 <- data.frame(
  ID = 1:4,
  Services = c("Cleaning,Laundry", "Room Service,Pool", NA, "Gym,Spa")
)
example_2
```

    ##   ID          Services
    ## 1  1  Cleaning,Laundry
    ## 2  2 Room Service,Pool
    ## 3  3              <NA>
    ## 4  4           Gym,Spa

The function work as expect, the data point with NA is TRUE in
`Services_NA` and FALSE in anywhere else.

``` r
tidy_column_with_delimiter_seperated_values(example_2, ",", Services)
```

    ## # A tibble: 4 × 8
    ##      ID Services_Cleaning Services_Laundry `Services_Room Service` Services_Pool
    ##   <int> <lgl>             <lgl>            <lgl>                   <lgl>        
    ## 1     1 TRUE              TRUE             FALSE                   FALSE        
    ## 2     2 FALSE             FALSE            TRUE                    TRUE         
    ## 3     3 FALSE             FALSE            FALSE                   FALSE        
    ## 4     4 FALSE             FALSE            FALSE                   FALSE        
    ## # ℹ 3 more variables: Services_NA <lgl>, Services_Gym <lgl>, Services_Spa <lgl>

## Exercise 4: Test the Function (25 points)

``` r
test_that("Testing tidy_column_with_delimiter_seperated_values function", {
  # Test 1: Vector with no NAs
  df_no_na <- data.frame(ID = 1:3, Amenities = c("Wifi,Pool,Gym", "Parking,Wifi", "Gym,Spa"))
  output_no_na <- tidy_column_with_delimiter_seperated_values(df_no_na, ",", Amenities)
  expect_true(all(c("Amenities_Wifi", "Amenities_Pool", "Amenities_Gym") %in% colnames(output_no_na)))
  expect_equal(ncol(output_no_na), 6) 

  # Test 2: Vector that has NAs
  df_with_na <- data.frame(ID = 1:4, Amenities = c("Wifi,Pool,Gym", NA, "Parking,Wifi", "Gym,Spa"))
  output_with_na <- tidy_column_with_delimiter_seperated_values(df_with_na, ",", Amenities)
  expect_true(all(c("Amenities_Wifi", "Amenities_Pool", "Amenities_NA") %in% colnames(output_with_na)))
  expect_identical(nrow(output_with_na), nrow(df_with_na)) 

  # Test 3: Vector of length 0
  df_empty <- data.frame(ID = numeric(0), Amenities = character(0))
  output_empty <- tidy_column_with_delimiter_seperated_values(df_empty, ",", Amenities)
  expect_equal(nrow(output_empty), 0)

  # Test 4: Test for passing a non-existence column to the function, should return Error
  df_nonexistent_column <- data.frame(ID = 1:3, Features = c("Forest,Lake", "Lake,Pool", "Gym,Spa"))
  expect_error(tidy_column_with_delimiter_seperated_values(df_nonexistent_column, ",", Nonexistent_Column))

})
```

    ## Test passed 😸
