library('testthat')
library('lubridate')

source("10_build_datatable.R")

fixed_colnums <- funct_fixed_colnums(rfs_int$x$data)[[1]]
date_colnums <- funct_fixed_colnums(rfs_int$x$data)[[2]]


test_that("check date columns and fixed columns are the same except the word 'fixed'", {
  expect_true(all(paste(names(rfs_int$x$data[,date_colnums]), 'fixed') == names(rfs_int$x$data[,fixed_colnums])))
})


test_that("check output data isnt datetime and charachter length is 10,  2052-06-17", {
  expect_true(all(!sapply(rfs_int$x$data[,date_colnums],is.POSIXct)))
  expect_true(all(sapply(rfs_int$x$data[,date_colnums],nchar)[,] == 10))
})

test_that("check dimensions fixed colnums = date colnums and dimensions of df", {
  expect_equal(length(fixed_colnums),length(date_colnums))
  expect_equal(dim(rfs_int$x$data[])[2],28)
  expect_gt(dim(rfs_int$x$data[])[1],1)
})


