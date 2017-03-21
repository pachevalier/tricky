

#' Extract all functions
#'
#' @param pkg name of a package
#'
#' @return a list of all functions in a package
#' @export
#'
#' @examples
#' extract_all_functions(pkg = "tricky")
#' extract_all_functions(pkg = "dplyr")
#' lsf.str("package:tricky")
#' ls("package:tricky")
#'
extract_all_functions <- function(pkg) {

  namespace <- readr::read_lines(
    file = system.file("NAMESPACE", package = pkg)
    )

  pat <- "export\\(([[:alnum:]\\_]+)\\)"

  sub(
      pattern = pat,
      replacement = "\\1",
      x = namespace[grepl(pattern = pat, x = namespace)]
    )

}

#' Find a function in a file
#'
#' @param function_name name of the function
#' @param file path to the file
#'
#' @return a tibble with two columns
#' @export
#'
#' @examples
#' \dontrun{
#' find_function_in_file(
#' function_name = "detect_na",
#' file = system.file("R", "tibbles.R", package = "tricky")
#' )
#' }
find_function_in_file <- function(function_name, file) {

  function_pattern <- paste0(
    gsub(
      x = function_name,
      pattern = "_",
      replacement = "\\_",
      fixed = TRUE),
    "\\("
  )

  tibble::tibble(
    file = file,
    find_function = any(
      grepl(
        pattern = function_pattern,
        x = readr::read_lines(file = file))
    )
  )

}

#' Find function in the current RStudio project
#'
#' @param function_name name of the project
#' @param files type of files : "R" or "Rmd"
#'
#' @return a tibble with two columns
#' @export
#'
#' @examples
#'
#' \dontrun{
#' find_function_in_project(function_name = "detect_na", file = "Rmd")
#' find_function_in_project(function_name = "detect_na", file = "R")
#' }
#'
find_function_in_project <- function(function_name, files) {

  if (files == "R") {
    pat_file <- "*.R$"
  }
  else if (files == "Rmd") {
    pat_file <- "*.Rmd$"
  }

  plyr::ldply(
    .data = list.files(
      path = rprojroot::find_rstudio_root_file(),
      recursive = TRUE,
      pattern = pat_file
    ),
    .fun = function(x) {
      find_function_in_file( function_name = function_name, file = x)
      }
  ) %>%
    dplyr::filter_(.dots = list( ~ find_function == TRUE))

}
