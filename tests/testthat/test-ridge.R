library(testthat)
library(MASS)

context("Test the output of homework 2 ridge.")

test_that("You ridge() function works in an easy case.", {
  
  data(iris)
  
  fit_my_ridge <- ridge(Sepal.Length ~ ., iris, lambda = .01)
  
  fit_lm <- lm.ridge(Sepal.Length  ~ ., iris,lambda = .01)
  
  expect_equivalent(coef(fit_lm), fit_my_ridge$coefficients,
                    tolerance = 1e-1)
})

test_that("You ridge() function works with contrasts.", {
  
  data(iris)
  
  fit_my_ridge <- ridge(Sepal.Length ~ ., iris, 
                            contrasts = list(Species = "contr.sum"))
  
  fit_lm <- lm.ridge(Sepal.Length  ~ ., iris, 
                           contrasts = list(Species = "contr.sum"))
  
  expect_equivalent(coef(fit_lm), fit_my_ridge$coefficients,
                    tolerance = 1e-1)
})

test_that("Your ridge() function works in a collinear case, test with lm.ridge.", {
  
  data(iris)
  iris$Sepal.Width_coll <- iris$Sepal.Width*1.5
  fit_my_ridge <- ridge(Sepal.Length ~ ., iris, lambda = .01)
  fit_lm <- lm.ridge(Sepal.Length  ~ ., iris,lambda = .01)
  expect_equivalent(coef(fit_lm), fit_my_ridge$coefficients,
                    tolerance = 1e-1)
})