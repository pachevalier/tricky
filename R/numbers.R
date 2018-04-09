#' DEPRECATED Formatting numbers according to French conventions
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
percent_formatting <- function(x) {
  paste( 100 * x, "%", sep = " ")
}

#' DEPRECATED Converts French formatted numbers into numbers
#'
#' This function allows you to extract the value of French formatted numbers
#'
#' This function is deprecated use parse_French_numbers()
#'
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


#' parse_French_numbers()
#'
#' @param x a string to be converted to a number
#'
#' @return a double
#' @export
#'
#' @examples
#' parse_French_number(x = "1 123,123")
#'
parse_French_number <- function(x) {
  readr::parse_number(
    x = x,
    locale = readr::locale(
      grouping_mark = " ",
      decimal_mark = ","
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


#' Studendize
#'
#' Normalizing a numerical vector using Student's normalization
#'
#'
#' WARNING : This version doesn't handles vectors with missing values
#'
#' @param x a numeric vector
#'
#' @return a numeric vector
#' @export
#'
#' @examples
#'
#' studendize(x = rep(1,10))
#' studendize(x = 1:10) %>% mean()
#' studendize(x = 1:10) %>% sd()
#'
studendize <- function(x) {
  sdx <- stats::sd(x)

  if (sdx != 0) {
    (x - base::mean(x)) / stats::sd(x)
  }
  else {
    x
  }
}
