#' Compute the week of the year
#'
#' This function allows you to the week number in one year for a given date
#' @param date a date
#' @keywords date week
#' @export
#' @examples
#' week_of_the_year(date = today())
#' week_of_the_year(date = as.Date("2015-10-21"))
#' week_of_the_year(seq(as.Date("1990-12-31"), as.Date("2015-12-31"), by = "year"))
#'
week_of_the_year  <- function(date) {
  require("lubridate", quietly = TRUE)
  1 + ((yday(date) + as.numeric(factor(weekdays(as.Date(paste0(year(date),"-01-01"))), levels = c("lundi", "mardi", "mercredi", "jeudi", "vendredi", "samedi", "dimanche")))) - 2) %/%7
}



