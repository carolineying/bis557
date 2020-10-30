## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(bis557)

## -----------------------------------------------------------------------------
form <- Sepal.Length ~ Sepal.Width
linear_model(formula = form,df = iris)

## -----------------------------------------------------------------------------
form <- Sepal.Length ~ Sepal.Width
gradient_descent(formula = form,df = iris)

