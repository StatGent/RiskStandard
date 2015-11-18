#' Label centers, i.e. assign risk categories to all centers
#' @param standardizedRisks object of class 'standardizedRisk' as produced by the 'standardizeRisks' function
#' @param lambda vector of length two; the first element indicates the clinical tolerance 
#'   level before flagging as low mortality risk; the second element indicates
#'   clinical tolerance level before flagging as high mortality risk; if NULL default values
#'   of \code{c(low = 0.2, high = 0.2)} are used for direct standardization; for indirect
#'   standardization a default value of \code{c(low = 0.05, high = 0.05)} is used  
#' @param confidenceLevel statistical confidence level; default value 0.5
#' @return object of class 'labeledCenters', which is a data frame with columns
#'   centerName, centerLabel, lowerCI and upperCI 
#' @export
#' @examples set.seed(130513)
#' simulatedData <- simulateData()
#' standardizedRisks <- with(simulatedData, standardizeRisks(patientCovariates = L, center = center, Y = Y))
#' labeledCenters <- labelCenters(standardizedRisks = standardizedRisks)
#' head(labeledCenters)
labelCenters <- function(standardizedRisks, lambda = NULL, confidenceLevel = 0.5){
  
  method <- attr(standardizedRisks, "method")
  meanY <- attr(standardizedRisks, "meanY")
  
  if (is.null(lambda)){
    if (method == "direct"){
      lambda <- c(low = 0.2, high = 0.2)
    } else {
      lambda <- c(low = 0.05, high = 0.05)
    }
  }
  
  if (length(lambda) == 1){
    lambda <- rep(lambda, 2)
  }
  
  lambdaLow <- lambda[1]
  lambdaHigh <- lambda[2]
  
  e <- 1 - (1 - confidenceLevel)/2
  
  m <- nrow(standardizedRisks)
  classif <- rep('A', m) # accepted
  
  if (method == 'direct'){
    
    lowerCI <- expit(logit(standardizedRisks$standardizedRisk) - qnorm(e) * sqrt(standardizedRisks$varStandardizedRisk))
    upperCI <- expit(logit(standardizedRisks$standardizedRisk) + qnorm(e) * sqrt(standardizedRisks$varStandardizedRisk))
    
    classif[which(upperCI < ((1-lambdaLow) * meanY))] <- 'L'
    classif[which(lowerCI > ((1+lambdaHigh) * meanY))] <- 'H'
    
  } else { # indirect
    
    
    lowerCI <- standardizedRisks$standardizedRisk - qnorm(e)*sqrt(standardizedRisks$varStandardizedRisk)
    upperCI <- standardizedRisks$standardizedRisk + qnorm(e)*sqrt(standardizedRisks$varStandardizedRisk)
    
    classif[which(upperCI < -lambdaLow)] <- 'L'
    classif[which(lowerCI >  lambdaHigh)] <- 'H'
    
  }
  
  returnValue <- data.frame(centerName = standardizedRisks$centerName, 
      centerLabel = classif, 
      lowerCI = lowerCI, 
      upperCI = upperCI)
  
  attr(returnValue, "lambdaLow") <- lambdaLow
  attr(returnValue, "lambdaHigh") <- lambdaHigh
  attr(returnValue, "confidenceLevel") <- confidenceLevel
  
  class(returnValue) <- c("labeledCenters", class(returnValue))
  
  return(returnValue)
  
}
