
# Tricky

A bag of useful R tricks 

[![Build Status](https://travis-ci.org/pachevalier/tricky.svg?branch=master)](https://travis-ci.org/pachevalier/tricky)

Contributions (pull requests and issues) are welcome.

## Installation

    library("devtools")
    install_github("pachevalier/tricky")

## Functions

### Tibbles

* `set_standard_names()`: standardize names of tibble
* `detect_na()` : returns a tibble with number and share of NA for each variable
* `count_na()` : returns a tibble with number of NA and non NA for a variable
* `find_keys()` : detect potential keys in a dataset

### Numbers

* `format_num()` : format numbers according to a locale (fr or en)
* `parse_French_number()` : extract numbers from string (wrapper for `readr::parse_number()`)
* `percent_formatting()` : format percentages

### Strings

* `str_standardize()` : returns a vector of standardized names (remove accents, blank spaces, special characters, etc)
