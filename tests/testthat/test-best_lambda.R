library(testthat)
library(glmnet)

context("Test the output of homework 2 best_lambda.")

test_that("You best_lambda() function works with cv.glmnet.", {
  
  data(iris)
  
  cvfit <- cv.glmnet(model.matrix(Sepal.Length ~ ., iris),as.matrix(iris$Sepal.Length), alpha = 0)
  
  my_lambda <- best_lambda(Sepal.Length ~ ., iris, lambdas = cvfit$lambda)
  
  expect_equivalent(cvfit$lambda.min, my_lambda$lambda, tolerance = 1e-2)
})