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
    forcats::fct_count() %>%
    dplyr::rename(missing = f)
  }




#' Standardize table names
#'
#' Names in tables may include blank caracters and many undesirable features.
#' str_standardize() makes it easy to standardize names and work with them inside R.
#'
#' @param x a tibble
#'
#' @return a vector of standardized names
#' @export
#'
#' @examples
#' library(readxl)
#' library(dplyr)
#' # source : http://www.data.gouv.fr/fr/datasets/panorama-des-grands-projets-si-de-letat-1/
#' read_excel(path = "raw-data/DINSIC-Panorama_des_grands_projets_SI_20161116.xlsx") %>%
#' setNames(str_standardize(.)) %>%
#' glimpse()
#'
str_standardize <- function(x) {
  x %>% names() %>%
    stringr::str_trim(side = "both") %>%
    stringr::str_replace_all(
      pattern = "[éèê]",
      replacement = "e",
      string = .) %>%
    stringr::str_replace_all(
      string = .,
      pattern = "[[:blank:]]",
      replacement = "_"
    ) %>%
    stringr::str_to_lower()
}

#' set standard names
#'
#' @param x a tibble
#'
#' @return a tibble with standardized column names
#' @export
#'
#' @examples
#'
#' library(readxl)
#' library(dplyr)
#' read_excel(
#' path = "raw-data/DINSIC-Panorama_des_grands_projets_SI_20161116.xlsx"
#' ) %>%
#' set_standard_names()
#'
set_standard_names <- function(x) {
  x %>% setNames(str_standardize(.))
  }
