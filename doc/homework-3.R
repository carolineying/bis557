## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(bis557)
library(dplyr)
library(palmerpenguins)


## -----------------------------------------------------------------------------
X <- matrix(c(1,1,0,1),ncol = 2)
p <- c(1e-5,0.8)

D <- diag(as.vector(p))
H_log <- t(X) %*% D %*% X
H_lin <- t(X) %*% X

max(svd(H_log)$d)/min(svd(H_log)$d)
max(svd(H_lin)$d)/min(svd(H_lin)$d)

## -----------------------------------------------------------------------------
set.seed(100)
n <- 5000; p <- 3
beta <- c(-1, 0.2, 0.1)
X <- cbind(1, matrix(rnorm(n * (p- 1)), ncol = p - 1))
eta <- X %*% beta
lambda <- exp(eta)
y <- rpois(n, lambda = lambda)
  
my_glm <- first_order_GLM(X,y,family = poisson(link = "log"), lr = "constant")
beta_glm <- glm(y ~ X[,-1], family = "poisson")
  
cbind(my_glm$coefficients, beta_glm$coefficients, beta)

## -----------------------------------------------------------------------------
my_glm_ad <- first_order_GLM(X,y,family = poisson(link = "log"), lr = "step", decay = 0.01)
cbind(my_glm_ad$coefficients, beta_glm$coefficients, beta)

## -----------------------------------------------------------------------------
make_model_matrices <- function(formula, df){
  # create model matrix
  df_no_na <- model.frame(formula,df)
  X <- model.matrix(formula, df_no_na)
  # get dependent variable
  yname <- as.character(formula)[2]
  y <- matrix(df_no_na[,yname],ncol = 1)
  list(X = X, y = y)
}

## ----message=FALSE------------------------------------------------------------
data(penguins)
form <- species ~ bill_length_mm + body_mass_g
mat <- make_model_matrices(form,penguins)
X <- mat$X
y <- mat$y
peng_spec <- multi_logreg(X,y)
  
peng_spec$coefficients
  
peng_new <- penguins
peng_new$is_adelie <- ifelse(peng_new$species == "Adelie", 1, 0)
peng_new$is_gentoo <- ifelse(peng_new$species == "Gentoo", 1, 0)
peng_new$is_chinstrap <- ifelse(peng_new$species == "Chinstrap", 1, 0)
  
  
glm(is_adelie ~ bill_length_mm + body_mass_g, data = peng_new, family = binomial)
glm(is_gentoo ~ bill_length_mm + body_mass_g, data = peng_new, family = binomial)
glm(is_chinstrap ~ bill_length_mm , data = peng_new, family = binomial,control=glm.control(maxit=15))


## -----------------------------------------------------------------------------
sum(peng_spec$pred_y != y)/length(y)

