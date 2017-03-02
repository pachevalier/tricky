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
#' plyr::ldply(.data = table_training, .fun = count_na)
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
#' dplyr::tbl(src = database_signauxfaibles, from = "table_training") %>%
#' dplyr::collect() %>%
#' detect_na()
#'
detect_na <- function(table) {
  table %>%
    plyr::ldply(.data = ., .fun = count_na, .id = "variable") %>%
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
      .dots = list(~variable, "n_missing" = ~n, ~share_missing)
    )

}

#' set standard names
#'
#' @param .data a tibble
#'
#' @return a tibble with standardized column names
#' @export
#'
#' @examples
#'
#' library(readxl)
#' library(dplyr)
#' read_excel(
#' path = "data-raw/DINSIC-Panorama_des_grands_projets_SI_20161116.xlsx"
#' ) %>%
#' set_standard_names()
#'
set_standard_names <- function(.data) {
  setNames(
    object = .data,
    nm = str_standardize(string = names(.data))
  )
}

#' find keys
#'
#' find_keys look at all function in a table and returns the list of possible keys (ie variables which identifies an observation)
#'
#' @param table the name of the input table (either tibble or data.frame)
#'
#' @return a tibble with one column : keys
#' @export
#'
#' @examples
#' read_csv("data-raw/table_deputes.csv") %>% find_keys()
#'
find_keys <- function(table) {
  output_table <- plyr::ldply(
    .data = table,
    .fun = dplyr::n_distinct) %>%
    dplyr::filter_(
      .dots = list(
        rlang::tidy_eval_rhs(
          f = ~ V1 == nrow(table),
          data = .
        )
      ),
      .data = .
    ) %>%
    dplyr::select_(
      .dots = list("keys" = ~ .id),
      .data = .
    )

  if (nrow(output_table) == 0) {
    print("No key in the table")
  }

  return(output_table)
}

