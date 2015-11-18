#' Simulate example data for risk standardization
#' @param n total number of patients; default 
#' @param m initial number of centers (min. 10); default value 50
#' @param l  number of patient-specific covariates in outcome model; default
#' @param betaL if FALSE (default) L is distributed according to a N(0,1) distribution;
#'   if TRUE L is distributed according to a Beta(1,6) distribution
#' @return list with three components: L (model matrix), center (vector) and Y (vector; binary outcome variable) 
#' @export
#' @examples set.seed(130513)
#' simulatedData <- simulateData()
#' str(simulatedData)
simulateData <- function(n = 10000, m = 50, l = 5, betaL = FALSE){
 
  # Parameters for PS model of center choice
  # Large differences in patient mix across centers
  if (!betaL){
    a0.true <- rnorm(m, 0, 1) # variability in center sizes
    a1.true <- matrix(rnorm(m*l, 0, 10), ncol=m) # variability in L across centers
  } else {
    a0.true <- rnorm(m, 0, 0.1) 
    a1.true <- matrix(rnorm(m*l, 0, 20), ncol=m)
  }
  
  # Small differences in patient mix across centers
  a0.true <- rnorm(m, 0, 1)
  a1.true <- matrix(rnorm(m*l, 0, 0.1), ncol=m)
  
  # (L) Patient specific characteristics
  if (!betaL){
    L <- matrix(rnorm(n*l, 0, 1), nrow=n)
  } else {
    L <- matrix(rbeta(n*l, 1, 6), nrow=n)
  }
  colnames(L) <- apply(cbind("L",1:l), 1, paste, collapse="")
  rownames(L) <- 1:n
  
  # (C) Binary center indicators per individual
  C <- matrix(NA, nrow=n, ncol=m)
  colnames(C) <- apply(cbind("Center",1:m),1, paste, collapse="")
  prob.center <- expit(cbind(1,L)%*%rbind(a0.true, a1.true))
  for(i in 1:n){
    C[i,] <- rmultinom(1, 1, prob.center[i,]) #prob is internally normalized to sum 1
  }
  center <- colSums(t(C) * 1:m)
  
  # (Y) Binary outcome depends on center effect and covariate-specific effect
  psi.true <- rnorm(m, -7, 1)   # center effect
  beta.true <- rnorm(l, 0, 5)   # covariate-specific effects
  Y <- rbinom(n, 1, expit(cbind(L, C) %*% c(beta.true, psi.true)))
  
  
  returnValue <- list(L = L, center = center, Y = Y)
  return(returnValue)
}
