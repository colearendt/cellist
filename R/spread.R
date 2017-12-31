#' @importFrom tidyr spread
#' @export
tidyr::spread


pull_item <- function(inlist, target, collector) {
  if (!is.list(inlist))
    inlist <- as.list(inlist)
  val <- parse(inlist[[target]], collector)
  inlist[[target]] <- NULL
  return(
    list(
      value=val
      ,list=inlist
    )
  )
}

process_spec_one <- function(inlist, col_name, collector) {
  pulled <- lapply(list(inlist), pull_item, col_name, collector)
  value <- lapply(pulled, function(x){x[["value"]]})
  parsed <- parse(value, collector) # this last (redundant) parse is meant to convert the class...
  setNames(list(parsed),col_name)
}

 process_spec_one_nol <- function(inlist, col_name, collector) {
   pulled <- pull_item(inlist, col_name, collector)
   value <- pulled[["value"]]
   parsed <- parse(value, collector) # this last (redundant) parse is meant to convert the class...

   if (length(collector) > 0) {
     # dispatch on sub-items
     parsed <- process_spec(parsed[[1]], col_spec(lapply(collector,identity)))
   }
   setNames(list(parsed),col_name)
 }

process_spec <- function(inlist, spec) {
  if (all(as.logical(
    lapply(as.list(names(inlist)), function(x){is.null(x)})
    )
  )
  ) {
    nm <- as.numeric(names(spec$cols))
  } else {
    nm <- names(spec$cols)
  }
  proc <- mapply(FUN=process_spec_one_nol, inlist=list(inlist)
                 , col_name=as.list(nm)
                 , collector=as.list(unname(spec$cols))
                 , SIMPLIFY=FALSE
  )

  dplyr::bind_cols(proc)
}

#' Spread List
#'
#' Spreads a list-column into additional columns
spread_list <- function(tbl, inlist, spec) {
  show_cols_spec(spec)
  tbl %>% bind_cols(map_dfr(.$list_col, process_spec, spec))
}
