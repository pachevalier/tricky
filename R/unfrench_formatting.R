#' Converts French formatted numbers into numbers
#'
#' This function allows you to extract the value of French formatted numbers
#' @param a string containing a number
#' @return a number
#' @keywords format
#' @export
#' @examples
#' unfrench_formatting(x = "0,12")
#'
unfrench_formatting <- function(x) {
  as.numeric(
    gsub(pattern = "[[:blank:]]", replacement = "", x =
           gsub(pattern = ",", replacement = ".", x = x)
         )
    )
}
