#' Plot observed vs. expected risks
#' @param standardizedRisks object of class 'standardizedRisks' as produced by
#'   by the standardizeRisks function 
#' @param labeledCenters center labels as produced by the labelCenters function
#' @param labelColors vector of colors (length 3) for the L, A and H label respectively;
#'   default value is \code{c(L = "green", A = "black", H = "red")}
#' @param labelPch vector of plot characters to be used for the L, A and H label resp.;
#'   default value is \code{c(L = 17, A = 20, H = 17)}
#' @param ... further arguments passed to plot and points
#' @return no return value; a plot is written to the current device 
#' @seealso \link{standardizeRisks}
#' @export
#' @examples set.seed(130513)
#' simulatedData <- simulateData()
#' standardizedRisks <- with(simulatedData, standardizeRisks(patientCovariates = L, center = center, Y = Y))
#' labeledCenters <- labelCenters(standardizedRisks = standardizedRisks)
#' plotRisks(standardizedRisks = standardizedRisks, labeledCenters = labeledCenters)
plotRisks <- function(standardizedRisks, labeledCenters, 
    labelColors = c(L = "olivedrab1", A = "black", H = "red"),
    labelPch = c(L = 17, A = 20, H = 17),
    ...){
  
  meanY <- attr(standardizedRisks, "meanY")
  method <- attr(standardizedRisks, "method")
  centerLabel <- labeledCenters$centerLabel
  
  observedRisk <- standardizedRisks$observedRisk
  standardizedRisk <- standardizedRisks$standardizedRisk
  expectedRisk <- observedRisk - standardizedRisk 
  
  lowerCI <- labeledCenters$lowerCI
  upperCI <- labeledCenters$upperCI
  lambdaLow <- attr(labeledCenters, "lambdaLow")
  lambdaHigh <- attr(labeledCenters, "lambdaHigh")
  
  op <- par(mar=c(5.1,5.1,2.1,2.1))
  
  dotList <- list(...)
  
  if (is.null(dotList$xlim)){
    if (method == "direct"){
      dotList$xlim <- range(standardizedRisk)  
    } else {
      dotList$xlim <- range(expectedRisk)
    }
  }
  if (is.null(dotList$ylim)){
    dotList$ylim <- range(observedRisk)
  }
  if (is.null(dotList$cex.axis)){
    dotList$cex.axis <- 1.5
  }
  if (is.null(dotList$cex.lab)){
    dotList$cex.lab <- 1.5
  }
  if (is.null(dotList$xlab)){
    if (method == "direct"){
      dotList$xlab <- expression(hat(E) * "{Y(c)}")  
    } else {
      dotList$xlab <- "Expected risk at center c"
    }
  }
  if (is.null(dotList$ylab)){
    dotList$ylab <- expression(hat(E) * "(Y|C=c)")
  }
  
  commonCex <- if (is.null(dotList$cex)) 1.5 else dotList$cex
  dotList$cex <- NULL
  
  lwd <- if (is.null(dotList$lwd)) 2 else dotList$lwd
  
  observedRiskLow <- as.vector(observedRisk)[centerLabel == "L"]
  observedRiskAccepted <- as.vector(observedRisk)[centerLabel == "A"]
  observedRiskHigh <- as.vector(observedRisk)[centerLabel == "H"]
  
  if (method == "direct"){
    
    argList <- c(list(
            x = standardizedRisk[centerLabel=="A"],
            y = observedRiskAccepted,
            col=labelColors[2],
            pch = labelPch[2],
            cex = commonCex
        ), 
        dotList)
    
    do.call("plot",
        argList)
    
    points(standardizedRisk[centerLabel=="L"], observedRiskLow, 
        pch=labelPch[1], col= labelColors[1],
        cex = commonCex)  
    points(standardizedRisk[centerLabel=="H"], observedRiskHigh, 
        pch=labelPch[3], col=labelColors[3],
        cex=commonCex)
    
    lines(loess.smooth(standardizedRisk, observedRisk), lwd=lwd)
    
    points(meanY, meanY, pch="M", col="darkgray", cex=2*commonCex)
    abline(a=0, b=1, lwd=lwd, lty="dashed")
    
  } else { # indirect
    
    argList <- c(list(
            x = expectedRisk[centerLabel=="A"],
            y = observedRiskAccepted,
            col=labelColors[2],
            pch = labelPch[2],
            cex = commonCex
        ), 
        dotList)
    
    do.call("plot",
        argList)
    
    points(expectedRisk[centerLabel=="L"], observedRiskLow, 
        pch=labelPch[1],
        col=labelColors[1], cex = commonCex)  
    points(expectedRisk[centerLabel=="H"], observedRiskHigh, 
        pch=labelPch[3],
        col=labelColors[3], cex = commonCex)
    lines(loess.smooth(expectedRisk, observedRisk),lwd=lwd)
    points(meanY, meanY, pch = "M", col="darkgray", cex=2*commonCex)
    abline(a=0, b=1, lwd=lwd, lty="dashed")
    
  }
  
  par(op)
  
}
