library(testthat)

context("Test the output of homework 3.")

test_that("You first_order_glm function works on poisson log data.", {
  #initialize samples
  n <- 5000; p <- 3
  beta <- c(-1, 0.2, 0.1)
  X <- cbind(1, matrix(rnorm(n * (p- 1)), ncol = p - 1))
  eta <- X %*% beta
  lambda <- exp(eta)
  y <- rpois(n, lambda = lambda)
  
  # my glm and reference glm
  my_glm <- first_order_GLM(X,y,family = poisson(link = "log"), lr = "constant")
  
  beta_glm <- glm(y ~ X[,-1], family = "poisson")
  
  expect_equivalent(beta_glm$coefficients, my_glm$coefficients, tolerance = .2)
})


test_that("You first_order_glm function works with binomial.", {
  set.seed(100)
  n <- 1000; p <- 3
  beta <- c(0.2, 2, 1)
  X <- cbind(1, matrix(rnorm(n * (p- 1)), ncol = p - 1))
  mu <- 1 - pcauchy(X %*% beta)
  y <- as.numeric(runif(n) > mu)
  
  # my glm and reference glm
  my_glm <- first_order_GLM(X,y,family = binomial(link = "cauchit"), lr = "constant")
  
  beta_glm <- coef(glm.fit(X, y, family = binomial(link = "cauchit")))
  
  expect_equivalent(beta_glm, my_glm$coefficients, tolerance = .2)
})