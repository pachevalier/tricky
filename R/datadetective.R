library(magrittr)
library(rlang)

detect_siren <- function(tbl) {
  plyr::ldply(
    .data = dplyr::select_if(.tbl = tbl, .predicate = is.character),
    .fun = function(x) {
      all(
        stringr::str_detect(
        string = x,
        pattern = "^[[:digit:]]{9}$"
        )
      )
      }
    ) %>%
  dplyr::filter(!! quo(V1 == TRUE)) %>%
  dplyr::rename(
    "column" := .id,
    "is_siren" := V1
  ) %>%
  tibble::as_tibble()

}

tbl_datadetective <- tibble::tibble(
  x = c("123456789", "234567890"),
  y = c("1234567890123", "1234567890234")
  )
detect_siren(tbl = tbl_datadetective)
