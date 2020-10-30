#' @title first-order solution for the GLM maximum likelihood problem
#' @description This is a function taking X, y and required parameters,
#' and using gradient descent to fit a GLM
#' @param X numeric data matrix
#' @param y response vector
#' @param family the family of GLM
#' @param lr adaptive or constant learning rate, input "step" or "constant", default is constant
#' @param gamma gamma_k, the starting learning rate, default is 0.0001
#' @param iter max number of iterations, default is 1e5
#' @param tol tolerance, default is 1e-15
#' @param decay step decay rate if chosen step for lr, default is 0.00
#' @importFrom stats model.matrix model.frame
#' @examples
#' n <- 5000; p <- 3
#' beta <- c(-1, 0.2, 0.1)
#' X <- cbind(1, matrix(rnorm(n * (p- 1)), ncol = p - 1))
#' eta <- X %*% beta
#' lambda <- exp(eta)
#' y <- rpois(n, lambda = lambda)
#' first_order_GLM(X,y,family = poisson(link = "log"), lr = "constant")
#' 
#' @export

first_order_GLM <- function(X, y, family, lr = "constant" ,gamma = 0.0001, iter = 1e5, tol = 1e-15, decay = 0.00){
  
  # initialize beta
  beta <- rep(0, ncol(X))
  
  # performing gradient descent
  for(i in 1:iter){
    beta_old <- beta # keep old beta's for comparison
    eta <- X %*% beta # compute eta
    Ey <- family$linkinv(eta) # compute expected values for y
    grad <- t(X) %*% (y - Ey) # compute gradient
    # if adaptive learning rate
    if (lr == "step"){
      gamma <- gamma/(1+decay*i)
    }
    beta <- beta + gamma * grad
    if(sqrt(crossprod(beta - beta_old)) < tol){
      break
    } #check
  }
  
  # solve for beta
  ret <- (list(coefficients = beta))
  class(ret) <- "my_GLM"
  return(ret)
}

