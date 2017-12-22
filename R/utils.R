#' @importFrom tibble data_frame
#' @export
tibble::data_frame

skip <- function(x) {
  return(NULL)
}

parse_get <- function(collector) {
  if (inherits(collector,'collector_skip')) {
    return(skip)
  } else if( inherits(collector,'collector_logical')) {
    return(as.logical)
  } else if (inherits(collector,'collector_integer')) {
    return(as.integer)
  } else if (inherits(collector, 'collector_double')) {
    return(as.double)
  } else if (inherits(collector, 'collector_number')) {
    return(as.numeric)
  } else if (inherits(collector,'collector_character')) {
    return(as.character)
  } else if (inherits(collector,'collector_date')) {
    return(as.Date)
  } else if (inherits(collector, 'collector_datetime')) {
    return(as.POSIXct)
  } else if (inherits(collector, 'collector_time')) {
    return(as.character) ## Need TIME
  } else if (inherits(collector, 'collector_factor')) {
    return(as.factor)
  } else {
    return(skip)
  }
}

parse_ <- function(x, collector) {
  parse_get(collector = collector)(x)
}

parse <- function(x, collector) {
  if (is.character(collector)) {
    collector <- collector_find(collector)
  }

  parse_(x,collector)
}

collector <- function(type, ...) {
  structure(list(...), class=c(paste0('collector_',type), 'collector'))
}

is.collector <- function(x) inherits(x, "collector")

#' @export
print.collector <- function(x, ...) {
  cat("<", class(x)[1], ">\n", sep = "")
}

collector_find <- function(name) {
  if (is.na(name)) {
    return(col_character())
  }

  get(paste0("col_", name), envir = asNamespace("readr"))()
}

col_logical <- function() {
  collector("logical")
}

col_integer <- function() {
  collector("integer")
}

col_double <- function() {
  collector("double")
}

col_character <- function() {
  collector("character")
}

col_skip <- function() {
  collector("skip")
}

col_number <- function() {
  collector("number")
}


#' Parse a character vector.
#'
#' @param x Character vector of elements to parse.
#' @param collector Column specification.
#' @inheritParams read_delim
#' @inheritParams tokenizer_delim
#' @keywords internal
#' @export
#' @examples
#' x <- c("1", "2", "3", "NA")
#' parse_vector(x, col_integer())
#' parse_vector(x, col_double())
parse_vector <- function(x, collector, na = c("", "NA"), locale = default_locale(), trim_ws = TRUE) {
  if (is.character(collector)) {
    collector <- collector_find(collector)
  }

  warn_problems(parse_vector_(x, collector, na = na, locale_ = locale, trim_ws = trim_ws))
}

#' Parse logicals, integers, and reals
#'
#' Use `parse_*()` if you have a character vector you want to parse. Use
#' `col_*()` in conjunction with a `read_*()` function to parse the
#' values as they're read in.
#'
#' @name parse_atomic
#' @aliases NULL
#' @param x Character vector of values to parse.
#' @inheritParams tokenizer_delim
#' @inheritParams read_delim
#' @family parsers
#' @examples
#' parse_integer(c("1", "2", "3"))
#' parse_double(c("1", "2", "3.123"))
#' parse_number("$1,123,456.00")
#'
#' # Use locale to override default decimal and grouping marks
#' es_MX <- locale("es", decimal_mark = ",")
#' parse_number("$1.123.456,00", locale = es_MX)
#'
#' # Invalid values are replaced with missing values with a warning.
#' x <- c("1", "2", "3", "-")
#' parse_double(x)
#' # Or flag values as missing
#' parse_double(x, na = "-")
NULL

#' @rdname parse_atomic
#' @export
parse_logical <- function(x, na = c("", "NA"), locale = default_locale(), trim_ws = TRUE) {
  parse_vector(x, col_logical(), na = na, locale = locale, trim_ws = trim_ws)
}

#' @rdname parse_atomic
#' @export
parse_integer <- function(x, na = c("", "NA"), locale = default_locale(), trim_ws = TRUE) {
  parse_vector(x, col_integer(), na = na, locale = locale, trim_ws = trim_ws)
}

#' @rdname parse_atomic
#' @export
parse_double <- function(x, na = c("", "NA"), locale = default_locale(), trim_ws = TRUE) {
  parse_vector(x, col_double(), na = na, locale = locale, trim_ws = trim_ws)
}

#' @rdname parse_atomic
#' @export
parse_character <- function(x, na = c("", "NA"), locale = default_locale(), trim_ws = TRUE) {
  parse_vector(x, col_character(), na = na, locale = locale, trim_ws = trim_ws)
}


#' Parse numbers, flexibly
#'
#' This drops any non-numeric characters before or after the first number.
#' The grouping mark specified by the locale is ignored inside the number.
#'
#' @inheritParams parse_atomic
#' @inheritParams tokenizer_delim
#' @inheritParams read_delim
#' @family parsers
#' @export
#' @examples
#' parse_number("$1000")
#' parse_number("1,234,567.78")
parse_number <- function(x, na = c("", "NA"), locale = default_locale(), trim_ws = TRUE) {
  parse_vector(x, col_number(), na = na, locale = locale, trim_ws = trim_ws)
}


#' Parse using the "best" type
#'
#' `parse_guess()` returns the parser vector; `guess_parser()`
#' returns the name of the parser. These functions use a number of heuristics
#' to determine which type of vector is "best". Generally they try to err of
#' the side of safety, as it's straightforward to override the parsing choice
#' if needed.
#'
#' @inheritParams parse_atomic
#' @inheritParams tokenizer_delim
#' @inheritParams read_delim
#' @family parsers
#' @export
#' @examples
#' # Logical vectors
#' parse_guess(c("FALSE", "TRUE", "F", "T"))
#'
#' # Integers and doubles
#' parse_guess(c("1","2","3"))
#' parse_guess(c("1.6","2.6","3.4"))
#'
#' # Numbers containing grouping mark
#' guess_parser("1,234,566")
#' parse_guess("1,234,566")
#'
#' # ISO 8601 date times
#' guess_parser(c("2010-10-10"))
#' parse_guess(c("2010-10-10"))
parse_guess <- function(x, na = c("", "NA"), locale = default_locale(), trim_ws = TRUE) {
  parse_vector(x, guess_parser(x, locale), na = na, locale = locale, trim_ws = trim_ws)
}

#' @rdname parse_guess
#' @export
col_guess <- function() {
  collector("guess")
}

#' @rdname parse_guess
#' @export
guess_parser <- function(x, locale = default_locale()) {
  stopifnot(is.locale(locale))
  collectorGuess(x, locale)
}

#' Parse factors
#'
#' `parse_factor` is similar to [factor()], but will generate
#' warnings if elements of `x` are not found in `levels`.
#'
#' @param levels Character vector providing set of allowed levels. if `NULL`,
#'   will generate levels based on the unique values of `x`, ordered by order
#'   of appearance in `x`.
#' @param ordered Is it an ordered factor?
#' @param include_na If `NA` are present, include as an explicit factor to level?
#' @inheritParams parse_atomic
#' @inheritParams tokenizer_delim
#' @inheritParams read_delim
#' @family parsers
#' @export
#' @examples
#' parse_factor(c("a", "b"), letters)
#'
#' x <- c("cat", "dog", "caw")
#' levels <- c("cat", "dog", "cow")
#'
#' # Base R factor() silently converts unknown levels to NA
#' x1 <- factor(x, levels)
#'
#' # parse_factor generates a warning & problems
#' x2 <- parse_factor(x, levels)
#'
#' # Using an argument of `NULL` will generate levels based on values of `x`
#' x2 <- parse_factor(x, levels = NULL)
parse_factor <- function(x, levels, ordered = FALSE, na = c("", "NA"),
                         locale = default_locale(), include_na = TRUE, trim_ws = TRUE) {
  parse_vector(x, col_factor(levels, ordered, include_na), na = na, locale = locale, trim_ws = trim_ws)
}

#' @rdname parse_factor
#' @export
col_factor <- function(levels, ordered = FALSE, include_na = FALSE) {
  collector("factor", levels = levels, ordered = ordered, include_na = include_na)
}



# Silence R CMD check note
#' @importFrom tibble tibble
NULL

isFALSE <- function(x) identical(x, FALSE)

is.connection <- function(x) inherits(x, "connection")

`%||%` <- function(a, b) if (is.null(a)) b else a

is_syntactic <- function(x) make.names(x) == x

show_progress <- function() {
  isTRUE(getOption("readr.show_progress")) && # user disables progress bar
    interactive() && # an interactive session
    is.null(getOption("knitr.in.progress")) # Not actively knitting a document
}

deparse2 <- function(expr, ..., sep = "\n") {
  paste(deparse(expr, ...), collapse = sep)
}

is_integerish <- function(x) {
  floor(x) == x
}
