% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/networks.R
\name{make_node_node_table_}
\alias{make_node_node_table_}
\title{Make node node table}
\usage{
make_node_node_table_(.data, node, link)
}
\arguments{
\item{.data}{a data frame with one row per node, link}

\item{node}{name of the node variable}

\item{link}{name of the link variable}
}
\value{
a data frame with one row per link (node, node)
}
\description{
Make node node table
}
\examples{

library("tibble")

toy_data <- tibble::tibble(
node = c("A", "B", "C", "A", "B"),
link = c("a", "a", "b", "b", "b")
)

make_node_node_table_(
.data = toy_data,
node = "node",
link = "link"
)

}
