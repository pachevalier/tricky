#' Standardize
#'
#' Standardize removes accents, blank spaces, special caracters from a string
#'
#' @param string a string
#' @param prefix a string
#'
#' @return a standardized string
#' @export
#'
#' @examples
#' library(readxl)
#' library(dplyr)
#' read_excel(path = "data-raw/DINSIC-Panorama_des_grands_projets_SI_20161116.xlsx") %>%
#' names() %>%
#' str_standardize(.))
#'
str_standardize <- function(string, prefix = "var_") {
  string %>%
    stringr::str_trim(
      side = "both",
      string = . ) %>%
    stringr::str_to_lower(
      string = .
    ) %>%
    stringr::str_replace_all(
      pattern = "[ç]",
      replacement = "c",
      string = .) %>%
    stringr::str_replace_all(
      pattern = "[éèê]",
      replacement = "e",
      string = .) %>%
    stringr::str_replace_all(
      pattern = "[âà]",
      replacement = "a",
      string = .) %>%
    stringr::str_replace_all(
      pattern = "[ùû]",
      replacement = "u",
      string = .) %>%
    stringr::str_replace_all(
      string = .,
      pattern = "[ô]",
      replacement = "o"
    ) %>%
    stringr::str_replace_all(
      string = .,
      pattern = "[[:blank:]\\-\\/]",
      replacement = "_"
    ) %>%
    stringr::str_replace_all(
      string = .,
      pattern = "[\\[\\]\\(\\)]",
      replacement = ""
      ) %>%
    stringr::str_replace(
      string = .,
      pattern = "^([[:digit:]].*)",
      replacement = paste0(prefix, "\\1")
      )
}
