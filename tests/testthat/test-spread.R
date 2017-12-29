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
   , d=col_double()
   )
   )
  output <- tree %>% left_join(
   tibble::data_frame(key=c(1,2)
                      , a=c(1,5)
                      , b=c(2,6)
                      , d=c(4,8)
                      )
   , by =c("key")
 )
 expect_identical(
   tree %>% spread_list("list_col",spec)
   , output
 )


})

test_that("works with missing keys", {
  tree <- tibble::data_frame(key=c(1,2)
                             ,list_col=list(
                               list("a"=1,"b"=2,"c"=3)
                               ,list("a"=5,"c"=7,"d"=8)
                             )
  )
  spec <-col_spec(list(
    a=col_double()
    , b=col_double()
    , d=col_character()
    , x=col_integer()
  )
  )
  output <- tree %>% left_join(
    tibble::data_frame(key=c(1,2)
                       , a=c(1,5)
                       , b=c(2,NA)
                       , d=as.character(c(NA,8))
                       , x=as.integer(c(NA,NA))
                       )
    , by=c("key")
  )
  expect_identical(
    spread_list(tree, "list_col", spec)
    , output
    )



}
)

test_that("works with simple list of characters", {
  tree <- tibble::data_frame(key=c(1,2)
                             , list_col=list(
                               list(a="a",b="b",c="c")
                               ,list(a="d",b="e",c="f")
                               )
                             )

  spec <- col_spec(
    list(
      a = col_character()
      , b = col_character()
      , c = col_character()
    )
  )
  output <- tree %>% left_join(
    tibble::data_frame(key=c(1,2)
                       , a=c("a","d")
                       , b=c("b","e")
                       , c=c("c","f")
                       )
    , by = c("key")
  )
  expect_identical(spread_list(tree, "list_col",spec), output)

})

expect_lots <- function(object, expected) {
  expect_equal(names(object),names(expected))
  mapply(expect_equal, object, expected, SIMPLIFY=FALSE)
  invisible(object)
}

test_that("preserves nested list if desired", {
  tree <- tibble::data_frame(
    key = c(1,2)
    , list_col=list(
      list("a"=c(1,2)
           , "b"=c(3,4))
      , list("a"=c(5,6)
             ,"b"=c(7,8))
    )
  )

  spec <- col_spec(
    list(
      a = col_list()
      , b = col_list()
    )
  )

  output <- tree %>% left_join(tibble::tribble(
    ~key, ~a, ~b
    , 1, c(1,2), c(3,4)
    , 2, c(5,6), c(7,8)
  ), by="key")

  computed <- spread_list(tree, "list_col", spec)

  expect_lots(computed
                   , output)

})


test_that("spreads a nested tree", {
  skip("not working")
  tree <- tibble::data_frame(
    key = c(1,2)
    , list_col=list(
      list("a"=c(1,2)
           , "b"=c(3,4))
      , list("a"=c(5,6)
             ,"b"=c(7,8))
    )
  )
  spec <- col_spec(
    list(
      a = col_list(
        "1" = col_double()
        , "2" = col_double()
      )
    )
  )

  # output_level1 %>% spread_tree(c(a,b),levels=1)
  #  Parsed with column specification:
  #    cols(
  #      key = col_integer(),
  #      a_1 = col_integer(),
  #      a_2 = col_integer(),
  #      b_1 = col_integer(),
  #      b_2 = col_integer()
  #    )
#  output_level2 <- tibble::data_frame(
#    key=c(1,2)
#    , a_1=c(1,3)
#    , a_2=c(2,4)
#    , b_1=c(5,7)
#    , b_2=c(6,8)
#  )
})


#extract_item <- function(.list, .def) {
#  stopifnot(is.atomic(.def)
#            , length(.def)==1
#            , is.list(.list))
#  lapply(.list, function(x){x[[.def]]})
#}
