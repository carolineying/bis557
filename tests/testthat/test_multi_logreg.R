library(testthat)
library(palmerpenguins)

context("Test the output of homework 3.")

test_that("You multi_logreg function works for three class ~ 1.", {
  make_model_matrices <- function(formula, df){
    # create model matrix
    df_no_na <- model.frame(formula,df)
    X <- model.matrix(formula, df_no_na)
    # get dependent variable
    yname <- as.character(formula)[2]
    y <- matrix(df_no_na[,yname],ncol = 1)
    list(X = X, y = y)
  }
  
  data(penguins)
  form <- species ~ bill_length_mm + body_mass_g
  mat <- make_model_matrices(form,penguins)
  X <- mat$X
  y <- mat$y
  peng_spec <- multi_logreg(X,y)
  
  peng_new <- penguins
  peng_new$is_adelie <- ifelse(peng_new$species == "Adelie", 1, 0)
  peng_new$is_gentoo <- ifelse(peng_new$species == "Gentoo", 1, 0)
  peng_new$is_chinstrap <- ifelse(peng_new$species == "Chinstrap", 1, 0)
  
  glm_fit <- glm(is_adelie ~ bill_length_mm + body_mass_g, data = peng_new, family = binomial)
  
  #compare the coefficients
  expect_equivalent(peng_spec$coefficients[,1],glm_fit$coefficients, tolerance = 1e-2)
})

test_that("You multi_logreg function works for three class ~ 1.", {
  make_model_matrices <- function(formula, df){
    # create model matrix
    df_no_na <- model.frame(formula,df)
    X <- model.matrix(formula, df_no_na)
    # get dependent variable
    yname <- as.character(formula)[2]
    y <- matrix(df_no_na[,yname],ncol = 1)
    list(X = X, y = y)
  }
  data(penguins)
  form <- species ~ bill_length_mm + body_mass_g
  mat <- make_model_matrices(form,penguins)
  X <- mat$X
  y <- mat$y
  peng_spec <- multi_logreg(X,y)
  
  peng_new <- penguins
  peng_new$is_adelie <- ifelse(peng_new$species == "Adelie", 1, 0)
  peng_new$is_gentoo <- ifelse(peng_new$species == "Gentoo", 1, 0)
  peng_new$is_chinstrap <- ifelse(peng_new$species == "Chinstrap", 1, 0)
  
  glm_fit <- glm(is_gentoo ~ bill_length_mm + body_mass_g, data = peng_new, family = binomial)
  
  #compare the coefficients
  expect_equivalent(peng_spec$coefficients[,2],glm_fit$coefficients, tolerance = 1e-2)
})

test_that("You multi_logreg function works for three class ~ 1.", {
  make_model_matrices <- function(formula, df){
    # create model matrix
    df_no_na <- model.frame(formula,df)
    X <- model.matrix(formula, df_no_na)
    # get dependent variable
    yname <- as.character(formula)[2]
    y <- matrix(df_no_na[,yname],ncol = 1)
    list(X = X, y = y)
  }
  data(penguins)
  form <- species ~ body_mass_g
  mat <- make_model_matrices(form,penguins)
  X <- mat$X
  y <- mat$y
  
  peng_spec <- multi_logreg(X,y)
  peng_new <- penguins
  peng_new$is_adelie <- ifelse(peng_new$species == "Adelie", 1, 0)
  peng_new$is_gentoo <- ifelse(peng_new$species == "Gentoo", 1, 0)
  peng_new$is_chinstrap <- ifelse(peng_new$species == "Chinstrap", 1, 0)
  
  glm_fit <- glm(is_chinstrap ~ body_mass_g, data = peng_new, family = binomial)
  
  #compare the coefficients
  expect_equivalent(peng_spec$coefficients[,3],glm_fit$coefficients, tolerance = 1e-2)
})
