
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
* `french_formatting()` : deprecated, use format_num() instead of french_formatting
* `percent_formatting()` : 
* `unfrench_formatting()` : 

### Strings

* `str_standardize()` : returns a vector of standardized names (remove accents, blank spaces, special characters, etc)

### Dates

* `week_of_the_year()` : takes a date and returns the week number in the year.

### Networks

* `make_node_node_table_()` : transform a node-link table into a node-node table. 
