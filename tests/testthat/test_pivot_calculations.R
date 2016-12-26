library(testthat)
library(data.table)

source("../../R/helpers.R")
source("../../R/pt_helper_functions.R")

context("pivot table calculations")

test_that("pivot table calculations correct", {
  
  x <- data.table("col1" = c("a","a","b","b","c","c"),
                  "col2" = c("d","e","f","g","g","g"),
                  "col3" = c(1,1,1,1,1,1),
                  "col4" = c(1,2,3,4,5,6))
  
  pt_options <- new_pt_options(pt_row = "col1", 
                               pt_col = "col2", 
                               pt_metrics = "total3 = sum(col3), total4 = sum(col4)", 
                               pt_filter = "col1=='a'", 
                               pt_sort = "col1",
                               pt_sort_order = -1,
                               pt_row_subtotals = TRUE)
  
  g <- group_row_col_dt(copy(x), pt_options)
  
  expect_equal(sum(g$total3), sum(x$col3))
  expect_equal(sum(g$total4), sum(x$col4))
  expect_equal(nrow(g), nrow(x[,.N, by = c("col1","col2")]))
  expect_equal(x, group_row_col_dt(x, ""))
  
  f <- filter_dt(copy(x), pt_options)
  
  expect_equal(sum(f$col3), 2)
  expect_equal(nrow(f), 2)
  
  s <- sort_dt(copy(x), pt_options) 
  
  expect_equal(s$col1[1], tail(x$col1, 1))
  
  p <- group_pivot_dt(copy(x), pt_options)
  
  expect_equal(x[col1 == "a" & col2 == "d", sum(col3)], p[col1 == "a", total3__d])
  
  subtotal_g <- subtotal_dt(x, pt_options)
  
  expect_equal(nrow(subtotal_g[,.N, by = col1]), 1)
  
})