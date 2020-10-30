#' @title multi-class logistic regression
#' @description This is a function taking X, y and fit a multi-class logistic regression
#' with reference to textbook p130
#' @param X numeric data matrix
#' @param y response vector
#' @param gamma gamma_k, the starting learning rate, default is 0.0001
#' @param iter max number of iterations, default is 1e5
#' @param tol tolerance, default is 1e-15
#' @importFrom stats model.matrix model.frame
#' @examples
#' X <- matrix(c(1,2,3,4,5), nrow = 5)
#' y <- c(0,0,1,1,2)
#' multi_logreg(X,y)
#' 
#' @export

multi_logreg <- function(X, y, gamma = 0.0001, iter = 35, tol = 1e-10){
  
  # using one-vs-all approach
  y_val <- unique(y)
  # initialize beta
  betas <- matrix(0, nrow = ncol(X), ncol = length(y_val))
  prob <- matrix(0, nrow = nrow(X), ncol = length(y_val))
  
  for(l in 1:length(y_val)){
    # determine whether y is this unique y or not (one vs all)
    y_iter <- ifelse(y == y_val[l], 1, 0)
    
    # initialize beta's
    beta <- rep(0, ncol(X))
    
    # performing gradient descent
    for(i in 1:iter){
      beta_old <- beta # keep old beta's for comparison
      p <- 1 / (1+exp(-X %*% beta))
      D <- diag(as.vector(p*(1-p)))
      XtDX <- t(X) %*% D %*% X
      #code adapt from book p130
      score <- t(X) %*% (y_iter - p)
      # break if the matrix is not invertible
      if(qr(XtDX)$rank != ncol(XtDX)){
        print(paste("Stop at iteration ", i," for class ",l, " next step will return to a singular matrix"))
        break
      }
      delta <- solve(XtDX, score)
      beta <- beta + delta
      # p_hat <- 1/(1+exp(-X %*% beta))
      # a = y_iter*(1-p_hat)
      # b = (1-y_iter)*p_hat
      # grad <- t(X) %*% (b-a) # compute gradient
      if(sqrt(crossprod(beta - beta_old)) < tol){
        break
      } #check
    }
    
    betas[,l] = beta
    prob[,l] = 1/(1+exp(-X%*%beta))
  }
  # look for the column containing max probability for y
  max_prob_index <- apply(prob,1,which.max)
  pred_y <- y_val[max_prob_index]
  
  ret <- list(coefficients = betas,pred_y = pred_y)
}
