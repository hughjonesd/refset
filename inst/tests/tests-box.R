
library(testthat)

context("Boxing and unboxing")

test_that("boxing and unboxing work", {
  abc <- letters[1:3]
  bx <- box(abc)
  expect_equivalent(unbox(bx), letters[4:6])
  abc <- letters[4:6]
  expect_equivalent(unbox(bx), letters[4:6])
  
  names(abc) <- LETTERS[1:3]
  expect_equivalent(names(unbox(bx)), LETTERS[1:3])
})

test_that("boxing into a context works", {
  abc <- letters[1:3]
  bx <- box(abc)
  abc <- letters[4:6]
})