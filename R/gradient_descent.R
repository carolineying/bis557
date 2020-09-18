#' @title fit a linear model
#' @description This is a function taking a formula, a dataframe 
#' and an optional list of contrast for factor variables,
#' and using gradient descent to return to coefficients.
#' with reference to https://towardsdatascience.com/linear-regression-using-gradient-descent-97a6c8700931
#' @param formula an object of class "formula": describing the model to be fitted. 
#' @param df a dataframe that containing all the variables in the model.
#' @param contrast an optional list of contrasts for factor variables.
#' @param gamma gamma_k, the learning rate
#' @param iter number of iterations
#' @examples
#' library(palmerpenguins)
#' data(penguins)
#' form <- body_mass_g~.
#' gradient_descent(formula = form,data = penguins)

#' @export
gradient_descent <- function(formula, df, contrasts = NULL, gamma = 0.0001, iter = 10^5){
  print(paste("With iteration number of ", iter))
  # create model matrix
  X <- model.matrix(formula, df, contrasts)
  
  # get dependent variable
  yname <- as.character(formula)[2]
  y <- matrix(df[,yname],ncol = 1)
  # initialize beta
  beta <- matrix(1,ncol = 1, nrow = ncol(X))
  
  # performing gradient descent
  for(i in 1:iter){
    gradient <- 2*t(X)%*%(X)%*%beta -2*t(X)%*%y
    beta = beta - gamma*gradient
  }
  
  # if there is singularity in computing gradient
  if(qr(X)$rank != ncol(X)){
    print("X matrix is not full rank, singularity existing. unable to finish gradient descent")
    return(linear_model(formula,df,contrasts))
  }
  
  # solve for beta
  ret <- (list(coefficients = beta))
  class(ret) <- "gradient_descent"
  return(ret)
}