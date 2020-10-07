#' @title ridge regression function
#' @description This is a function taking a formula, a dataframe 
#' and an optional list of contrast for factor variables,
#' and taking into account colinear (or nearly colinear) regression variables and returning coefficients.
#' @param formula an object of class "formula": describing the model to be fitted. 
#' @param df a dataframe that containing all the variables in the model.
#' @param contrasts an optional list of contrasts for factor variables.
#' @param lambda an optional penalty term for ridge regression, default is 0
#' @import stats
#' @examples
#' data(iris)
#' form <- Sepal.Length ~ Sepal.Width
#' ridge(formula = form,df = iris)
#' 
#' @export

ridge <- function(formula, df, contrasts = NULL, lambda = 0){
  # create model matrix
  df_no_na <- model.frame(formula,df)
  X <- model.matrix(formula, df_no_na, contrasts)
  # get dependent variable
  yname <- as.character(formula)[2]
  y <- matrix(df_no_na[,yname],ncol = 1)

  svd_x <- svd(X)
  D <- diag(svd_x$d/(svd_x$d^2 + lambda))
  beta <- svd_x$v %*% D %*% t(svd_x$u) %*% y
  ret <- list(coefficients = beta)
  class(ret) <- "my_ridge"
  ret
}
