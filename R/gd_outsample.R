#' @title gradient descent using out of sample penalty
#' @description This is a function taking a formula, a dataframe 
#' and an optional list of contrast for factor variables,
#' and using gradient descent with out of sample penalty and returning OLS coefficients.
#' Large dataset should have more accurate results for splitting samples.
#' @param formula an object of class "formula": describing the model to be fitted. 
#' @param df a dataframe that containing all the variables in the model.
#' @param contrasts an optional list of contrasts for factor variables.
#' @param gamma gamma_k, the learning rate
#' @param iter number of iterations
#' @import stats
#' @import dplyr
#' @import MASS
#' @examples
#' data(iris)
#' form <- Sepal.Length ~ Sepal.Width
#' gd_outsample(formula = form,df = iris)

#' @export

gd_outsample <- function(formula, df, contrasts = NULL, gamma = 0.0001, iter = 10^6){
  # create model matrix
  df_no_na <- model.frame(formula,df)
  set.seed(5)
  train_ind <- sample(seq_len(nrow(df_no_na)), size = floor(.9*nrow(df_no_na)), replace = F)
  df_train <- df_no_na[train_ind,] 
  df_test <- df_no_na[-train_ind,]
  X_in <- model.matrix(formula, df_train, contrasts)
  X_out <- model.matrix(formula, df_test, contrasts)
  
  # get dependent variable
  yname <- as.character(formula)[2]
  y_in <- matrix(df_train[,yname], ncol = 1)
  y_out <- matrix(df_test[,yname], ncol = 1)
  # initialize beta
  beta <- matrix(1,ncol = 1, nrow = ncol(X_in))
  #initial loss
  loss = sum((y_out - X_out%*%beta)^2)
  
  # performing gradient descent
  for(i in 1:iter){
    gradient <- 2*t(X_in)%*%(X_in)%*%beta -2*t(X_in)%*%y_in
    beta1 = beta - gamma*gradient
    # compute out of sample loss
    loss1 = sum((y_out - X_out%*%beta1)^2)
    if(loss1 < loss){
      beta = beta1
      loss = loss1
    }
    else{
      # reduce the step size
      gamma = .9*gamma
    }
  }
  
  # if there is singularity in computing gradient
  if(qr(X_in)$rank != ncol(X_in)){
    print("X matrix is not full rank, singularity existing. unable to finish gradient descent")
    return(linear_model(formula,df,contrasts))
  }
  
  # solve for beta
  ret <- (list(coefficients = beta))
  class(ret) <- "gd_outsample"
  return(ret)
}
