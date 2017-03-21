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
#' str_standardize("2017/07/07", prefix = "date_")
#' str_standardize(stringi::stri_trans_general("code externe de l'action", "latin-ascii"))
#'
str_standardize <- function(string, prefix = "var_") {
  string %>%
    stringr::str_trim(
      side = "both") %>%
    stringr::str_to_lower() %>%
    stringr::str_replace_all(
      pattern = "[ç]",
      replacement = "c") %>%
    stringr::str_replace_all(
      pattern = "[éèê]",
      replacement = "e") %>%
    stringr::str_replace_all(
      pattern = "[âà]",
      replacement = "a") %>%
    stringr::str_replace_all(
      pattern = "[ùû]",
      replacement = "u") %>%
    stringr::str_replace_all(
      pattern = "[ô]",
      replacement = "o"
    ) %>%
    stringr::str_replace_all(
      pattern = "[[:blank:]\\'\\-\\/]",
      replacement = "_"
    ) %>%
    stringr::str_replace_all(
      pattern = "[\\[\\]\\(\\)]",
      replacement = ""
      ) %>%
    stringr::str_replace(
      pattern = "^([[:digit:]].*)",
      replacement = paste0(prefix, "\\1")
      )
  }
