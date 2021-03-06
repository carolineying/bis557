% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/first_order_glm.R
\name{first_order_GLM}
\alias{first_order_GLM}
\title{first-order solution for the GLM maximum likelihood problem}
\usage{
first_order_GLM(
  X,
  y,
  family,
  lr = "constant",
  gamma = 1e-04,
  iter = 1e+05,
  tol = 1e-15,
  decay = 0
)
}
\arguments{
\item{X}{numeric data matrix}

\item{y}{response vector}

\item{family}{the family of GLM}

\item{lr}{adaptive or constant learning rate, input "step" or "constant", default is constant}

\item{gamma}{gamma_k, the starting learning rate, default is 0.0001}

\item{iter}{max number of iterations, default is 1e5}

\item{tol}{tolerance, default is 1e-15}

\item{decay}{step decay rate if chosen step for lr, default is 0.00}
}
\description{
This is a function taking X, y and required parameters,
and using gradient descent to fit a GLM
}
\examples{
n <- 5000; p <- 3
beta <- c(-1, 0.2, 0.1)
X <- cbind(1, matrix(rnorm(n * (p- 1)), ncol = p - 1))
eta <- X \%*\% beta
lambda <- exp(eta)
y <- rpois(n, lambda = lambda)
first_order_GLM(X,y,family = poisson(link = "log"), lr = "constant")

}
