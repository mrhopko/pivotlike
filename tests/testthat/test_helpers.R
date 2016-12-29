library(data.table)
library(testthat)

source("../../R/helpers.R")
source("../../R/pt_helper_functions.R")

context("helpers")

test_that("helpers behave", {
  
  m_name <- c("m1", "m2")
  m_text <- c("sum(col1)", "sum(col2)")
  
  expect_equal(text_to_pt_metrics(m_name, m_text), "list(m1=sum(col1),m2=sum(col2))")
})
