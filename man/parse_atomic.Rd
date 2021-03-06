% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{parse_atomic}
\alias{parse_logical}
\alias{parse_integer}
\alias{parse_double}
\alias{parse_character}
\title{Parse logicals, integers, and reals}
\usage{
parse_logical(x, na = c("", "NA"), locale = default_locale(),
  trim_ws = TRUE)

parse_integer(x, na = c("", "NA"), locale = default_locale(),
  trim_ws = TRUE)

parse_double(x, na = c("", "NA"), locale = default_locale(),
  trim_ws = TRUE)

parse_character(x, na = c("", "NA"), locale = default_locale(),
  trim_ws = TRUE)
}
\arguments{
\item{x}{Character vector of values to parse.}
}
\description{
Use \code{parse_*()} if you have a character vector you want to parse. Use
\code{col_*()} in conjunction with a \code{read_*()} function to parse the
values as they're read in.
}
\examples{
parse_integer(c("1", "2", "3"))
parse_double(c("1", "2", "3.123"))
parse_number("$1,123,456.00")

# Use locale to override default decimal and grouping marks
es_MX <- locale("es", decimal_mark = ",")
parse_number("$1.123.456,00", locale = es_MX)

# Invalid values are replaced with missing values with a warning.
x <- c("1", "2", "3", "-")
parse_double(x)
# Or flag values as missing
parse_double(x, na = "-")
}
\seealso{
Other parsers: \code{\link{parse_factor}},
  \code{\link{parse_guess}}, \code{\link{parse_number}}
}
