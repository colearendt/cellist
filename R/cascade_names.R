#' Cascade Names
#'
#' Cascade names takes data_frames in a nested lists and pulls
#' them up to the top level, appending names that it encounters
#' along the way
cascade_names <- function(list_or_df, prefix="") {
  UseMethod("cascade_names")
}

cascade_names.list <- function(list_or_df, prefix="") {
  if (is.null(names(list_or_df))) {
    nm <- ""
  } else {
    nm <- names(list_or_df)
  }
  obj <- lapply(list_or_df, cascade_names, prefix = paste0(prefix, nm))
  #  if (any(!as.character(lapply(list_or_df,class)) %in% c("data.frame","list"))) {
  #    return(obj)
  #  } else {
  return(bind_cols(obj))
  #  }
}

cascade_names.data.frame <- function(list_or_df, prefix="") {
  return(list_or_df %>% rename_all(.funs = list(function(x) {
    prep <- paste0(prefix, "_", x)
    prep <- stringr::str_replace_all(prep, "_{2,}", "_")
    prep <- stringr::str_replace_all(prep, "^_", "")
    return(prep)
  })))
}

cascade_names.default <- function(list_or_df, prefix="") {
  return(list_or_df)
}
