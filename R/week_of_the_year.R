#' Compute the week of the year
#'
#' This function allows you to the week number in one year for a given date
#' @param a date
#' @return a number
#' @keywords date week
#' @export
#' @examples
#' library("lubridate")
#' week_of_the_year(date = today())
#' week_of_the_year(date = as.Date("2015-10-21"))
#' week_of_the_year(seq(as.Date("1990-12-31"), as.Date("2015-12-31"), by = "year"))
#'
week_of_the_year  <- function(date) {
  1 + ((lubridate::yday(date) +
          as.numeric(
            factor(
              weekdays(
                as.Date(paste0(lubridate::year(date), "-01-01"))),
              levels = c("lundi", "mardi", "mercredi",
                         "jeudi", "vendredi", "samedi", "dimanche"))
            )
        ) - 2) %/% 7
}
