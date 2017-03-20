
#' is SIREN
#'
#' is_siren detect if the variable match the pattern of a SIRENE number
#'
#' @param var name of a variable
#'
#' @return a tibble with one boolean column
#' @export
#'
#' @examples
#' library("tibble")
#' table_datadetective <- tibble(
#'    v1 = c("123456789", "234567890"),
#'    v2 = c("1234567890123", "1234567890234")
#'    )
#'
#' is_siren(var = table_datadetective$v1)
#' is_siren(var = table_datadetective$v2)
#'
is_siren <- function(var) {
  tibble::tibble(x = all(
    stringr::str_detect(
      string = var,
      pattern = "^[[:digit:]]{9}$"
      )
   )
  )
}

#' Detect Siren
#'
#' detects if a data frame has a column of SIRENE identifier
#'
#' @param .data a data frame
#'
#' @return a tibble
#' @export
#'
#' @examples
#'
#' library("tibble")
#'
#' table_datadetective <- tibble::tibble(
#'   v1 = c("123456789", "234567890"),
#'   v2 = c("1234567890123", "1234567890234")
#'   )
#'
#' detect_siren(.data = table_datadetective)
#'
detect_siren <- function(.data) {

  plyr::ldply(
    .data = .data %>%
      dplyr::select_if(is.character),
    .fun = is_siren
  ) %>%
  dplyr::filter_(
    .dots = list(
      rlang::tidy_eval_rhs( ~ x == TRUE, data = .)
      )
    )

  }

