## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(bis557)
library(casl)
library(reticulate)
library(stats)
use_condaenv("r-reticulate")

## -----------------------------------------------------------------------------
data(iris)
df_no_na <- model.frame(Sepal.Length ~ .,iris)
py$X <- model.matrix(Sepal.Length ~ ., df_no_na)
yname <- as.character(Sepal.Length ~ .)[2]
py$y <- matrix(df_no_na[,yname],ncol = 1)

## -----------------------------------------------------------------------------
fit_my_ridge <- ridge(Sepal.Length ~ ., iris, lambda = .01)
cbind(py$py_fit, fit_my_ridge$coefficients)

## -----------------------------------------------------------------------------
data(iris)
iris$Sepal.Width_coll <- iris$Sepal.Width*1.5+0.1
df_no_na <- model.frame(Sepal.Length ~ .,iris)
py$X <- model.matrix(Sepal.Length ~ ., iris)
yname <- as.character(Sepal.Length ~ .)[2]
py$y <- matrix(df_no_na[,yname],ncol = 1)

## -----------------------------------------------------------------------------
py_fit <- py$py_ridge_coll
fit_my_ridge <- ridge(Sepal.Length ~ ., iris, lambda = .01)
cbind(py$py_ridge_coll, fit_my_ridge$coefficients)

## -----------------------------------------------------------------------------
#simulate data
n <- 1e5
X <- matrix(c(rnorm(n, 10, 1), rnorm(n, 5, 1), rnorm(n, 2, 2)),nrow = n)
y <- rnorm(n,50,1)

# create batches
batch <- 100
b_size <- n/batch
beta <- matrix(rep(0,ncol(X)*batch), nrow = batch)

for (i in 1:batch){
  #create batches
  y_b <- y[(b_size*(i-1)+1):(b_size*i)]
  X_b <- X[(b_size*(i-1)+1):(b_size*i),]
  #read in python and fit model
  py$X_b <- X_b
  py$y_b <- y_b
  beta[i,] <- py$py_lm(X_b,y_b)
}
#compute mean
beta_final <- apply(beta,2,mean)
beta_final

lm(y~X-1)

## -----------------------------------------------------------------------------
#simulate data as page 192
n <- 1000
p <- 5
X <- matrix(rnorm(n * p), ncol = p)
beta <- c(3, 2, 1, rep(0, p - 3))
y <- X %*% beta + rnorm(n = n, sd = 0.1)
bhat <- casl_lenet(X, y, lambda = 0.01)
bhat

py$X <- X
py$y <- y
py$py_lasso(X,y,.01)

## -----------------------------------------------------------------------------
bhat <- casl_lenet(X, y, lambda = 0.1)
bhat

py$py_lasso(X,y,.1)

