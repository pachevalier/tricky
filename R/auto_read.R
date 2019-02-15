#' auto_read_tsv()
#'
#' @param file a file
#' @param ...
#'
#' @return a tibble
#' @export
#'
#' @examples
#' \dontrun{auto_read_tsv(file = "data-raw/depts2018.txt")}
#'
auto_read_tsv <- function(file, ...) {
  .encoding <- guess_encoding(file = file) %>% slice(1) %>% pull(encoding)
  cat("Detected encoding is ... ", .encoding, "\n")
  read_tsv(file = file, locale = locale(encoding = .encoding))
}
