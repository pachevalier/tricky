#' Count NA
#'
#' count_na count the number of NA in a vector
#'
#' @param x a vector
#'
#' @return a tibble
#' @export
#'
#' @examples
#' library("readxl")
#' read_excel(
#' path = system.file(
#' "extdata",
#'  "panorama.xlsx",
#' package = "tricky")
#' ) %>%
#' set_standard_names() %>%
#' .$ministere_porteur %>%
#' count_na()
#'
count_na <- function(x) {
  x %>%
    is.na() %>%
    factor() %>%
    forcats::fct_count()
  }

#' Detect NA
#'
#' Detect NA takes a tibble and return the number and share of missing values for each variable
#'
#' @param table name of the input variable
#'
#' @return a tibble with 3 columns (variable, n_missing, share_missing)
#' @export
#'
#' @examples
#' library("readxl")
#' read_excel(
#' path = system.file(
#' "extdata",
#' "panorama.xlsx",
#' package = "tricky")
#' ) %>%
#' set_standard_names() %>%
#' detect_na()

detect_na <- function(table) {
  table %>%
    plyr::ldply(
      .fun = count_na, .id = "variable"
      ) %>%
    dplyr::group_by_(~ variable) %>%
    dplyr::mutate_(
      .dots = list(
        "share_missing" = lazyeval::interp(~ 100 * x / sum(x), x = quote(n))
      )
    ) %>%
    dplyr::filter_(
      .dots = list(~ f == TRUE)
    ) %>%
    dplyr::select_(
      .dots = list(~ variable, "n_missing" = ~ n, ~ share_missing)
    )
}

#' Set standard names
#'
#' Many datasets have bad column names featuring accents, blank spaces, slash, etc.
#' Set standard names set standard names, ie names which can be used in programming.
#'
#' @param .data a tibble
#' @param prefix name of the prefix for names beginning with numbers
#'
#' @return a tibble with standardized column names
#' @export
#'
#' @examples
#'
#' library(readxl)
#' library(dplyr)
#'
#' read_excel(
#' path = system.file("extdata","panorama.xlsx", package = "tricky")
#' ) %>%
#' set_standard_names()
#'
set_standard_names <- function(.data, prefix = "var_") {
  stats::setNames(
    object = .data,
    nm = str_standardize(string = names(.data), prefix = prefix)
  )
}

#' Find keys
#'
#' find_keys look at all function in a table and returns the list of possible keys (ie variables which identifies an observation)
#'
#' @param table the name of the input table (either tibble or data.frame)
#'
#' @return a tibble with one column : keys
#' @export
#'
#' @examples
#' library("readr")
#' read_csv(system.file("extdata", "table_deputes.csv", package = "tricky")) %>%
#' find_keys()
#'
find_keys <- function(table) {

  table_n_distinct <- plyr::ldply(
    .data = table,
    .fun = dplyr::n_distinct
    )

  output_table <- table_n_distinct %>%
    dplyr::filter_(
      .dots = list(
        lazyeval::lazy_eval(
          ~ V1 == nrow(table),
          data = table_n_distinct
          )
        )
      ) %>%
    dplyr::select_(
      .dots = list("keys" = ~ .id)
      )

  if (nrow(output_table) == 0) {
    print("No key in the table")
  }
  else {
    return(output_table)
  }

}

#' Filter duplicates
#'
#' filter_dups_() takes a data frame and returns only duplicated rows
#'
#' @param .data a data frame
#' @param .groups a grouping variabme
#'
#' @return a data frame
#' @export
#'
#' @examples
#' table_test <- tibble::tibble(v1 = c("A", "A", "B", "C"), v2 = c("a", "b", "b", "c"))
#' filter_dups_(.data = table_test, .groups = ~ v1)
#' filter_dups_(.data = table_test, .groups = ~ v2)
#'
filter_dups_ <- function(.data, .groups) {
  dplyr::group_by_(
    .data = .data,
    .dots = list(lazyeval::lazy_eval(~ .groups, data = .data))
    ) %>%
    dplyr::filter_(.dots = list(~ n() > 1))
}
