#' Formatting percentages
#'
#' This function allows you to format percentages
#' @param x the number you need to format
#' @return a string
#' @keywords format
#' @export
#' @examples
#' percent_formatting(0.012)
#'
#'
percent_formatting <- function(x) {
  paste( 100 * x, "%", sep = " ")
}
