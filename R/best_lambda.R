#' @title find the best lambda in ridge regression function
#' 
#' @description This is a function taking a formula, a dataframe 
#' and an optional list of contrast for factor variables,
#' and taking into account colinear (or nearly colinear) regression variables and returning coefficients.
#' 
#' @param formula an object of class "formula": describing the model to be fitted. 
#' @param df a dataframe that containing all the variables in the model.
#' @param contrasts an optional list of contrasts for factor variables.
#' @param lambdas a sequence of lambdas to be selected from, default is seq(-1, 1, by = 0.01)
#' @param nfold number of folds for the cross validation
#' 
#' @import stats
#' @import foreach
#' @import doParallel
#' @import rsample
#' @import glmnet
#' 
#' @examples
#' data(iris)
#' form <- Sepal.Length ~ Sepal.Width
#' best_lambda(formula = form,df = iris)
#' 
#' @export

best_lambda <- function(formula, df, contrasts = NULL, lambdas = seq(-2,2, by = 0.01), nfold = 10){
  
  yname <- as.character(formula)[2]
  fold <- NULL
  lambda <- NULL
  #create folds
  folds <- vfold_cv(df, v = nfold)
  
  # compute MSE using cross validation
  mse <- foreach(lambda = lambdas) %dopar% {
    foreach(fold = folds$splits, .combine = c) %do% {
      #train data
      df_train <- analysis(fold)
      X_train <- model.matrix(formula, df_train, contrasts)
      y_train <- matrix(df_train[,yname],ncol = 1)
      #test data
      df_test <- assessment(fold)
      X_test <- model.matrix(formula, df_test, contrasts)
      y_test <- matrix(df_test[,yname],ncol = 1)
      # get beta from ridge regression
      fit <- ridge(formula, df_train, contrasts = contrasts, lambda = lambda)
      beta <- fit$coefficients
      mean((y_test - X_test %*% beta)^2)
    }
  }
  
  mse_table <- tibble(mean = lapply(mse,mean),
                      lambda = lambdas)
  
  lambda_min = mse_table$lambda[which.min(mse_table$mean)]
  
  ret <- list(lambda = lambda_min)
  class(ret) <- "best_lambda"
  ret
}
