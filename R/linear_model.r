#' @title fit a linear model
#' @description This is a function taking a formula, a dataframe 
#' and an optional list of contrast for factor variables,
#' and fit a linear model and return the coefficients of the model.
#' @param formula an object of class "formula": describing the model to be fitted. 
#' @param df a dataframe that containing all the variables in the model.
#' @param contrasts an optional list of contrasts for factor variables.
#' @importFrom stats model.matrix model.frame
#' @examples
#' data(iris)
#' form <- Sepal.Length ~ Sepal.Width
#' gradient_descent(formula = form,df = iris)

#' @export
linear_model <- function(formula, df, contrasts = NULL){
  # create model matrix
  df_no_na <- model.frame(formula,df)
  X <- model.matrix(formula, df_no_na, contrasts)
  
  # get dependent variable
  yname <- as.character(formula)[2]
  y <- matrix(df_no_na[,yname],ncol = 1)
  
  # solve for beta
  beta <- qr.coef(qr(X),y)
  ret <- (list(coefficients = beta))
  class(ret) <- "linear_model"
  return(ret)
}
