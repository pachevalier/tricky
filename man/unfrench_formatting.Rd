% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/numbers.R
\name{unfrench_formatting}
\alias{unfrench_formatting}
\title{DEPRECATED Converts French formatted numbers into numbers}
\usage{
unfrench_formatting(x)
}
\arguments{
\item{x}{string containing a number}
}
\value{
a number
}
\description{
This function allows you to extract the value of French formatted numbers
}
\details{
This function is deprecated use parse_French_numbers()
}
\examples{
unfrench_formatting(x = "0,12")

}
\keyword{format}
