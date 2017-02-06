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
