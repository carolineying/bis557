## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(bis557)
library(glmnet)

## -----------------------------------------------------------------------------
data(iris)
fit_my_os <- gd_outsample(Sepal.Length ~ ., iris)
fit_lm <- lm(Sepal.Length  ~ ., iris)
print(cbind(fit_my_os$coefficients, fit_lm$coefficients))

## -----------------------------------------------------------------------------
data(iris)
iris$Sepal.Width_coll <- iris$Sepal.Width
fit_my_ridge <- ridge(Sepal.Length ~ ., iris, lambda = .01)
fit_my_ridge$coefficients

## -----------------------------------------------------------------------------
data(iris)
cvfit <- cv.glmnet(model.matrix(Sepal.Length ~ ., iris),as.matrix(iris$Sepal.Length), alpha = 0)
mylambda <- best_lambda(Sepal.Length ~ ., iris, lambdas = cvfit$lambda)
c(cvfit$lambda.min,mylambda$lambda)

