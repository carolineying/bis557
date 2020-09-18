#' @title fit a linear model
#' @description This is a function taking a formula, a dataframe 
#' and an optional list of contrast for factor variables,
#' and fit a linear model and return the coefficients of the model.
#' @param formula an object of class "formula": describing the model to be fitted. 
#' @param df a dataframe that containing all the variables in the model.
#' @param contrast an optional list of contrasts for factor variables.
#' @examples
#' library(palmerpenguins)
#' data(penguins)
#' form <- body_mass_g~.
#' linear_model(formula = form,data = penguins)

#' @export
linear_model <- function(formula, df, contrasts = NULL){
  # create model matrix
  X <- model.matrix(formula, df, contrasts)
  
  # get dependent variable
  yname <- as.character(formula)[2]
  y <- matrix(df[,yname],ncol = 1)
  
  # solve for beta
  beta <- qr.coef(qr(X),y)
  ret <- (list(coefficients = beta))
  class(ret) <- "linear_model"
  return(ret)
}
