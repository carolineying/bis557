library(testthat)

context("Test the output of homework 2.")

test_that("You gd_outsample() function works with contrasts.", {
  
  data(iris)
  
  fit_gd_os <- gd_outsample(Sepal.Length ~ ., iris, 
                                           contrasts = list(Species = "contr.sum"))
  
  fit_lm <- lm(Sepal.Length  ~ ., iris, contrasts = list(Species = "contr.sum"))
  
  expect_equivalent(fit_lm$coefficients, fit_gd_os$coefficients,
                    tolerance = 0.5)
  diff <- mean(fit_lm$coefficients - fit_gd_os$coefficients)
  print(paste('mean difference: ',diff, 'The sample result could be improved with different gamma and increased iterations. Also try larger datasets'))
})

