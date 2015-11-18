#' compute logit
#' @param x numeric vector
#' @return numeric vector 
#' @export
logit <- function(x) log(x/(1-x))

#' compute expit
#' @param x numeric vector
#' @return numeric vector 
#' @export
expit <- function(x) exp(x)/(1+exp(x))

#' compute the derivative of expit
#' @param x numeric vector
#' @return numeric vector 
#' @export
dx.expit <- function(x) expit(x)*(1-expit(x))


