#' Estimate standardized mortality risks
#' @param patientCovariates data frame of patient-specific covariates
#' @param center center code (n values for n patients)
#' @param Y binary outcome
#' @param method method of standardization; one of 'indirect' (default) or 'direct' 
#' @param Firth logical apply Firth correction? default value TRUE
#' @param alpha statistical significance level; default value is 0.05
#' @param missing how to handle missing categorical data? one of 'completeCase' (default) or 'dummyCategory';
#'   when 'dummyCategory' is chosen a separate category is added to model a missing value effect 
#' @param trace logical print summary of fitted model? default value FALSE
#' @return object of class 'standardizedRisks'; data frame with columns
#' centerName, centerSize, standardizedRisk, varStandardizedRisk, lowerCI, upperCI and observedRisk 
#' @references Varewyck M., Goetghebeur E., Eriksson M. and Vansteelandt S. (2014),
#'   On shrinkage and model extrapolation in the evaluation of clinical center performance,
#'   Biostatistics, 15(4), p. 651--664
#' @importFrom brglm brglm
#' @export
#' @examples set.seed(130513)
#' simulatedData <- simulateData()
#' standardizedRisks <- with(simulatedData, standardizeRisks(patientCovariates = L, center = center, Y = Y))
#' head(standardizedRisks)
standardizeRisks <- function(patientCovariates, center, Y, method = c("indirect", "direct"), 
                             Firth = TRUE, 
                             alpha = 0.05,
                             missing = c("completeCase", "dummyCategory"),
                             trace = FALSE){
  
  method <- match.arg(method)
  missing <- match.arg(missing)
  
  if (is.data.frame(patientCovariates)){
    
    if (missing == "dummyCategory"){
      
      isFactor <- which(sapply(patientCovariates, is.factor))
      
      for (iFactor in isFactor){
        
        factorVariable <- patientCovariates[,iFactor]
        
        if (any(is.na(factorVariable))){
          iLevels <- levels(factorVariable)
          characterVariable <- as.character(factorVariable)
          characterVariable[is.na(characterVariable)] <- "-9"
          
          patientCovariates[,iFactor] <- factor(characterVariable,
                                                levels = c(iLevels, "-9"))
        }
        
      }  
      
    }
    
    modelFormula <- as.formula(paste("~", paste(names(patientCovariates), collapse =  " + ")))
    row.names(patientCovariates) <- 1:nrow(patientCovariates)
    L <- model.matrix(modelFormula, data = patientCovariates)[,-1] 
  } else { # in case one directly passes the model matrix WITHOUT intercept
    L <- patientCovariates
  }
  
  l <- ncol(L)
  n <- nrow(L)
    
  center <- as.factor(center)[as.numeric(rownames(L))]  #valid for complete case analysis
  m <- length(unique(center))
  C <- (diag(m)[center, ])
  # colnames(C) <- apply(cbind("Center",c(1:m)),1, paste, collapse="")
  colnames(C) <- apply(cbind("Center",levels(center)),1, paste, collapse="")
  
  Y <- Y[as.numeric(rownames(L))]  #valid for complete case analysis
  
  # model matrix for outcome, center 1 as reference
  X <- cbind(1, L, C[,-1])
  
  observedRisk <- tapply(Y, center, mean)
  
  if (any(observedRisk == 0)){
    stop('Some centers have zero observed events. This procedure cannot handle this.')
  }
  
  centerSize <- as.vector(table(center))
  
  # Model fitting
  if (!Firth){
    modelFit <- glm(Y ~ X[,-1], family = binomial)
  } else {
    modelFit <- brglm(Y ~ X[,-1], family = binomial(logit), method = "brglm.fit", pl=TRUE)
  }
  
  #Print summary of fitted model
  if(trace == TRUE){
    print(summary(modelFit))
  }
  
  
  b <- as.matrix(modelFit$coef)
  psi <- c(0,b[-(1:(l+1))])    # fixed center effects
  
  # linear predictor minus center effect
  linear.predict <- predict(modelFit) - psi[center]
  
  
  # From the estimating equations (for calculating variance)
  R <- X*as.vector(Y - expit(linear.predict + psi[center]))
  deriv <- dx.expit(as.vector(X%*%b))*X
  A <- 1/n*t(X) %*% deriv
  Ainv <- solve(A)  # square matrix with dim l+m
  rm(deriv, A)
  
  
  if (method =="direct"){
    
    deriv.pop <- matrix(NA, nrow=length(b), ncol=m)
    p.pop.expit <- matrix(NA, nrow=n, ncol=m)
    
    X.pop <- X
    X.pop[,l+(2:m)] <- 0
    deriv.pop[,1] <- apply(dx.expit(linear.predict)*X.pop, 2, mean)
    p.pop.expit[,1] <- expit(linear.predict)
    
    for(k in 2:m){
      X.pop[,l+(2:m)] <- 0
      X.pop[,l+k] <- 1
      deriv.pop[,k] <- apply(dx.expit(linear.predict + psi[k])*X.pop, 2, mean)
      p.pop.expit[,k] <- expit(linear.predict + psi[k])
    }
    
    
    #\hat{E}[Y(c)]
    out.interest <- apply(p.pop.expit, 2, mean)
    #Variance of logit(\hat{E}[Y(c)])
    var.interest <- 1/(n*(out.interest*(1-out.interest))^2)*
      apply(p.pop.expit + R%*%Ainv%*%deriv.pop,2,var)
    
    lowerCI <- expit(logit(out.interest) - qnorm(1-alpha/2) * sqrt(var.interest))
    upperCI <- expit(logit(out.interest) + qnorm(1-alpha/2) * sqrt(var.interest))
    
  } else { # indirect
    
    p.pop.expit <- matrix(NA, nrow=n, ncol=m)
    
    X.pop <- X
    X.pop[,l+(2:m)] <- 0
    deriv.center <- t(C)%*%(dx.expit(linear.predict)*X.pop)/centerSize
    p.pop.expit[,1] <- expit(linear.predict)
    
    for(k in 2:m){
      
      X.pop[,l+(2:m)] <- 0
      X.pop[,l+k] <- 1
      deriv.center <- deriv.center + t(C)%*%(dx.expit(linear.predict + psi[k])*X.pop)/centerSize
      p.pop.expit[,k] <- expit(linear.predict + psi[k])
    }
    
    deriv.center <- deriv.center/m
    
    
    average.center <- apply(p.pop.expit, 1, mean)   #\hat{E}_c*[Y(c*)]
    expc <- tapply(average.center, center, mean)
    out.interest <- observedRisk - expc   #Est. excess risk
    
    # Estimate variance    
    #saves Var[obs], Var[exp] en Cov[obs,exp]
    var.obs <- n/(centerSize^2)*apply(Y*C,2,var)
    tmp <- average.center*C + R%*%Ainv%*%t(centerSize/n*deriv.center)
    var.exp <- n/(centerSize^2)*apply(tmp, 2, var) 
    covar <- c()
    for(c in 1:m){
      covar[c] <- n/(centerSize[c]^2)*cov(Y*C[,c],tmp[,c])
    }  
    var.interest <- var.obs + var.exp - 2*covar
    
    lowerCI <- out.interest - qnorm(1-alpha/2)*sqrt(var.interest)
    upperCI <- out.interest + qnorm(1-alpha/2)*sqrt(var.interest)
    
  }
  
  
  returnValue <- data.frame(
    centerName = levels(center),
    centerSize = as.vector(centerSize), 
    standardizedRisk = out.interest, 
    varStandardizedRisk =var.interest, 
    lowerCI = lowerCI, 
    upperCI = upperCI, 
    observedRisk = observedRisk)
  
  class(returnValue) <- c("standardizedRisks", class(returnValue))
  attr(returnValue, "method") <- method
  attr(returnValue, "meanY") <- mean(Y)
  attr(returnValue, "n") <- n #nobs(modelFit)
  
  return(returnValue)
}
