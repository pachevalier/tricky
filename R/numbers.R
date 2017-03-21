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
#' @param x string containing a number
#'
#' @return a number
#' @keywords format
#' @export
#'
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

#' Format num
#'
#' @param x a number
#' @param locale a locale ("fr" or "en")
#' @param ... other parameter to format such scientific = TRUE/FALSE
#'
#' @return a formatted number
#' @export
#'
#' @examples
#' format_num(x = 1233.123, locale = "fr")
#' format_num(x = 1233.123, locale = "en")
#'
format_num <- function(x, locale, ...) {

  locales <- list(
    fr = list(
      big.mark = " ",
      decimal.mark = ","
    ),
    en = list(
      big.mark = ",",
      decimal.mark = "."
    )
  )

  format(
    x = x,
    big.mark = locales[[locale]][["big.mark"]],
    decimal.mark = locales[[locale]][["decimal.mark"]],
    ...
  )

}

