#' Pull Item
#'
#' Extracts `target` from `inlist`, using `collector` and `parse`.
#' Also checks to ensure that the incoming value is a list, and
#' has a start towards "modifying-on-reference" the original list item
#'
#' @param inlist A list object
#' @param target The targeted key to extract (a length 1 character vector or numeric vector)
#' @param collector A `collector` object to use when parsing`
#'
#' @return A list of "value" and "list".  "value" is the extracted value, "list" is
#' the resulting list object that should replace the original
#'
#' @keywords internal
pull_item <- function(inlist, target, collector) {
  if (!is.list(inlist)) {
    inlist <- as.list(inlist)
  }
  val <- parse(inlist[[target]], collector)
  # inlist[[target]] <- NULL # rudimentary start on modifiy-on-ref
  return(
    list(
      value = val
      , list = inlist
    )
  )
}

#' Process One Spec
#'
#' Processes a singluar "column" of the `spec` being processed.  It completes the following
#' in order:
#' - Extract the desired item
#' - Pull out the "value" object returned
#' - Ensure that the class of the value has been preserved appropriately
#' - Checks to see if any recursive calls are needed (`length(collector) > 0`)
#' - Calls any recursive calls necessary
#' - Sets names
#' - Returns a `tibble` if a `tibble` is not already present
#'
#' @param inlist A list object
#' @param col_name The column name that will be extracted from the list
#' @param collector The collector object that will be used to parse the list
#'
#' @return tibble or list of tibbles with the new columns
#'
#' @keywords internal
process_spec_one <- function(inlist, col_name, collector) {
  pulled <- pull_item(inlist, col_name, collector)
  value <- pulled[["value"]]
  parsed <- parse(value, collector) # this last (redundant) parse is meant to convert the class...

  if (length(collector) > 0) {
    # dispatch on sub-items
    parsed <- process_spec(parsed[[1]], col_spec(lapply(collector, identity)))
  }
  final <- setNames(list(parsed), col_name)

  # do not allow nested tibbles... (tibbles will not allow this)
  if (any(as.logical(lapply(final, tibble::is_tibble)))) {
    return(final)
  } else {
    return(as_tibble(final))
  }
}

#' Process Spec
#'
#' Processes a `spec` object against a provided list.  Does the
#' following:
#'
#' - Checks to see if the list has names
#'   - if so, uses the names of `spec$cols`
#'   - otherwise, tries to force names of `spec$cols` into numerics
#' - Uses `mapply` to apply `process_spec_one` to the list, one `spec` at a time.
#' - Uses `cascade_names` to take care of nested structures, cascade names, and bind columns together
#'
#' @param inlist A list object
#' @param spec A `col_spec` object
#'
#' @return A tibble with `spec` "processed"
#'
#' @keywords internal
process_spec <- function(inlist, spec) {
  if (all(as.logical( lapply(as.list(names(inlist)), function(x){ is.null(x) }) )) ) {
    nm <- as.numeric(names(spec$cols))
  } else {
    nm <- names(spec$cols)
  }
  proc <- mapply(
    FUN = process_spec_one, inlist = list(inlist)
    , col_name = as.list(nm)
    , collector = as.list(unname(spec$cols))
    , SIMPLIFY = FALSE
  )

  cascade_names(proc)
}

#' Spread List
#'
#' Spreads a list-column into additional columns by
#' combining `bind_cols` and `map_dfr`.  Also prints the
#' `col_spec` object
#'
#' @param tbl A tibble object or data.frame
#' @param inlist The list column (currently a length 1 character vector)
#' @param spec A `col_spec` object
#'
#' @return A tibble with `spec` spread into new columns
#'
#' @export
spread_list <- function(tbl, inlist, spec) {
  show_cols_spec(spec)
  tbl %>% bind_cols(map_dfr(.[[inlist]], process_spec, spec))
}
