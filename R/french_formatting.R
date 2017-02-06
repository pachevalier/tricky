#' Formatting numbers according to French conventions
#'
#' This function allows you to format numbers according to French conventions
#' @param x the number you need to format
#' @return a string
#' @keywords format
#' @export
#' @examples
#' french_formatting(11000.12)
#'
french_formatting <- function(x) {
  output <- stringr::str_trim(format(x, big.mark = " ", decimal.mark = ","))
  return(output)
}
