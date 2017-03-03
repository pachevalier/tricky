
# Tricky

***Tricky*** is an R package with miscellaneous R functions that are useful to me.

## Installation

    library("devtools")
    install_github("hadley/rlang")
    install_github("pachevalier/tricky")

## Functions

### Tibbles

* `set_standard_names()`: standardize names of tibble
* `detect_na()` : returns a tibble with number and share of NA for each variable
* `count_na()` : returns a tibble with number of NA and non NA for a variable
* `find_keys()` : detect potential keys in a dataset

### Numbers

* `format_num()` : format numbers according to a locale (fr or en)
* `french_formatting()` : deprecated, use format_num() instead of french_formatting
* `percent_formatting()`
* `unfrench_formatting()`

### Strings

* `str_standardize()` : returns a vector of standardized names (remove accents, blank spaces, special characters, etc)

### Dates

* `week_of_the_year()` : takes a date and returns the week number in the year.

