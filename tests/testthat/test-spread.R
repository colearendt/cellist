context("test-spread.R")

test_that("works with simple list of doubles", {
 tree <- tibble::data_frame(key=c(1,2)
                            ,list_col=list(
                              list("a"=1,"b"=2,"c"=3,"d"=4)
                              ,list("a"=5,"b"=6,"c"=7,"d"=8)
                              )
                            )
 spec <-col_spec(list(
   a=col_double()
   , b=col_double()
   , c=col_integer()
   )
   )
 pull_item <- function(inlist, target) {
   val <- inlist[[target]]
   inlist[[target]] <- NULL
   return(
     list(
       value=val
       ,list=inlist
       )
   )
 }

 process_spec_one <- function(list, col_name, collector) {
   pulled <- lapply(list, pull_item, col_name)
   value <- lapply(pulled, function(x){x[["value"]]})
   parsed <- parse(value, collector)
   setNames(list(parsed),col_name)
 }

 process_spec <- function(list, spec) {
   proc <- mapply(FUN=process_spec_one, list=list(list)
          , col_name=as.list(names(spec$cols))
          , collector=unname(spec$cols)
          , SIMPLIFY=FALSE
          )

   dplyr::bind_cols(proc)
 }

 process_spec(col, spec)

 process_col <- function(data, col, spec) {
   data %>%
 }

 output <- tibble::data_frame(key=c(1,2)
                              , a=c(1,5)
                              , b=c(2,6)
                              , c=c(3,7)
                              , d=c(4,8)
                              )

 # maybe worth creating callbacks for naming the columns...?
})

test_that("works with simple list of characters", {
  tree <- tibble::data_frame(key=c(1,2)
                             , list_col=list(
                               list("a","b","c")
                               ,list("d","e","f")
                               )
                             )

  output <- tibble::data_frame(key=c(1,2)
                               , output1=c("a","d")
                               , output2=c("b","e")
                               , output3=c("c","f")
                               )
})

test_that("works with nested tree", {
  tree <- tibble::data_frame(
    key = c(1,2)
    , list_col=list(
      list("a"=c(1,2)
           , "b"=c(3,4))
      , list("a"=c(5,6)
             ,"b"=c(7,8))
    )
  )
  print(tree)


  # tree %>% spread_tree(list_col,levels=1)
  # Parsed with column specification:
  #  cols(
  #    a = col_list(),
  #    b = col_list()
  #  )
  output_level1 <- tibble::data_frame(
    key=c(1,2)
    , a=list(c(1,2),c(5,6))
    , b=list(c(3,4),c(7,8))
  )
  print(output_level1)

  # output_level1 %>% spread_tree(c(a,b),levels=1)
  #  Parsed with column specification:
  #    cols(
  #      key = col_integer(),
  #      a_1 = col_integer(),
  #      a_2 = col_integer(),
  #      b_1 = col_integer(),
  #      b_2 = col_integer()
  #    )
  output_level2 <- tibble::data_frame(
    key=c(1,2)
    , a_1=c(1,3)
    , a_2=c(2,4)
    , b_1=c(5,7)
    , b_2=c(6,8)
  )
  print(output_level2)
})
