
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
* `detect_keys()` : detect potential keys in a dataset

### Numbers

* `french_formatting()`
* `percent_formatting()`
* `unfrench_formatting()`

### Strings

* `str_standardize()` : returns a vector of standardized names
