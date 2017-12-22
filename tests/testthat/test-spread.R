context("test-spread.R")

test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that("works with simple list of doubles", {
 tree <- tibble::data_frame(key=c(1,2)
                            ,list_col=list(
                              list(1,2,3,4)
                              ,list(5,6,7,8)
                              )
                            )
 output <- tibble::data_frame(key=c(1,2)
                              , output1=c(1,5)
                              , output2=c(2,6)
                              , output3=c(3,7)
                              , output4=c(4,8)
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
