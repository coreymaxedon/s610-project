## These tests source to the raw code file because testthat throws an error about
## zero-length variable names if we try to source it to the Rmd file. The raw code
## is everything from project.Rmd put into a plain R script.

context("Check Project Functions")
source("raw_code_for_tests.R")


## Test that divfun is converting data to probabilities correctly, 
## i.e. that all elements are in [0,1], and returns a matrix

n <- 5

x <- data.frame("test_V1" = runif(n, min = 0, max = 15),
                "test_V2" = runif(n, min = 0, max = 15),
                "test_V3" = runif(n, min = 0, max = 15))

test_that("divfun returns probabilities", {
  expect_true(all(divfun(x) >= 0) & all(divfun(x) <= 1))
})

test_that("divfun returns a matrix", {
  expect_equal(class(divfun(x)), "matrix")
})


## Test data generation function using some easy-ish numbers

qc = 0.25
qh = 0.75

model_1(j = 1, s = 3, qc = qc, qh =  qh, wjj =  (3/4))
data_gen(qc = 0.25, qh = 0.75, m = 4, n =3)

test1 = data_gen(qc = 0.25, qh = 0.75, m = 4, n =3)

onethree = choose(3,1) * (3/4) * (qc * qh^1)^2
twothree = choose(3,2) * (21/32) * (qc * qh^2)^1
test = matrix(c(0.25, 0.75, NA, NA,
                (1/16), (9/32), (21/32), NA,
                (1/64), onethree, twothree, (1 - onethree - twothree - (1/64))),
              ncol = 3, nrow = 4)

test_that("data_gen works", {
  expect_equal(test, test1)
})


## Test frobenious norm using easy numbers

A <- c(0, 1, 5, -3, 1)

test_that("frobenious norm works", {
  expect_equal(frobenious(A), 6)
})

