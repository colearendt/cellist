#' @importFrom tidyr spread
#' @export
tidyr::spread


pull_item <- function(inlist, target, collector) {
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
   setNames(list(parsed),col_name)
 }

process_spec <- function(inlist, spec) {
  proc <- mapply(FUN=process_spec_one_nol, inlist=list(inlist)
                 , col_name=as.list(names(spec$cols))
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
