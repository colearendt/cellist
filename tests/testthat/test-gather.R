context("test-gather.R")

test_that("works with simple list of doubles", {
  tree <- tibble::data_frame(key=c(1,2)
                             ,list_col=list(
                               list(1,2,3,4)
                               ,list(5,6,7,8)
                             )
  )

  output <- tibble::data_frame(
    key=c(rep(1,4), rep(2,4))
    , output = 1:8
  )
})

test_that("works with simple list of characters", {
  tibble::data_frame(key=c(1,2)
                             , list_col=list(
                               list("a","b","c")
                               ,list("d","e","f")
                             )
  )


# tree %>% gather_tree()
# Parsed with column specification:
#  cols(
#    output = col_character()
#  )
  tibble::data_frame(
    key=c(rep(1,3),rep(2,3))
    , output= c("a","b","c","d","e","f")
  )
})
