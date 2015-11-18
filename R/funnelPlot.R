#' Funnel plot comparing institutional performance
#' @param standardizedRisks standardized risk as computed by the standardizeRisks function
#' @param xValue value to be plotted on the x axis, one of 'size' or 'precision'
#' @param confidenceLineColors vector of length two; colors for the 95% and 99% confidence bounds; default value
#'   is \code{c("black", "red")}
#' @param confidenceLineLty vector of length two; line types for the 95% and 99% confidence bounds; default value
#'   is \code{c("solid", "solid")}
#' @param ... further arguments passed to plot
#' @return no return value; a plot is drawn on the current device
#' @references Spiegelhalter, David J. (2005). Funnel plots for comparing institutional performance,
#'   Statistics in Medicine, 24(8), p. 1185--1202.
#' @seealso \link{standardizeRisks}
#' @examples set.seed(130513)
#' simulatedData <- simulateData()
#' standardizedRisks <- with(simulatedData, standardizeRisks(patientCovariates = L, center = center, Y = Y))
#' funnelPlot(standardizedRisks = standardizedRisks)
#' @export
funnelPlot <- function(standardizedRisks, xValue = c("size", "precision"),
    confidenceLineColors = c("black", "red"),
    confidenceLineLty = c("dashed", "dotted"),
    ...){
  
  xValue <- match.arg(xValue)
  
  dotList <- list(...)
  
  if (is.null(dotList$xlab)){
    if (xValue == "size"){
      dotList$xlab <- "Center size"  
    } else {
      dotList$xlab <- "1/sd"
    }
  }
  
  if (is.null(dotList$ylab)){
    dotList$ylab <- "Standardized risk"
  }
  
  if (is.null(dotList$pch)){
    dotList$pch <- 19
  }
  
  if (is.null(dotList$cex)){
    dotList$cex <- 1.5
  }
  if (is.null(dotList$cex.axis)){
    dotList$cex.axis <- 1.5
  }
  
  if (is.null(dotList$cex.lab)){
    dotList$cex.lab <- 1.5
  }
  
  
  lwd <- if (is.null(dotList$lwd)) 2 else dotList$lwd 
  
  standardizedRisk <- standardizedRisks$standardizedRisk
  centerSize <- standardizedRisks$centerSize
  varStandardizedRisk <- standardizedRisks$varStandardizedRisk
  method <- attr(standardizedRisks, "method")
  
  if (method == 'direct'){
    
    reference <- attr(standardizedRisks, "meanY")
    lower95 <- expit(logit(reference) - qnorm(0.975)*sqrt(varStandardizedRisk))
    upper95 <- expit(logit(reference) + qnorm(0.975)*sqrt(varStandardizedRisk))
    lower99 <- expit(logit(reference) - qnorm(0.995)*sqrt(varStandardizedRisk))
    upper99 <- expit(logit(reference) + qnorm(0.995)*sqrt(varStandardizedRisk))
    
  } else {
    
    reference <- mean(standardizedRisk)
    lower95 <- mean(standardizedRisk) - qnorm(0.975)*sqrt(varStandardizedRisk)
    upper95 <- mean(standardizedRisk) + qnorm(0.975)*sqrt(varStandardizedRisk)
    lower99 <- mean(standardizedRisk) - qnorm(0.995)*sqrt(varStandardizedRisk)
    upper99 <- mean(standardizedRisk) + qnorm(0.995)*sqrt(varStandardizedRisk)
    
  }
  
  if (xValue == 'size'){
    
    plotData <- cbind.data.frame(x = as.vector(centerSize), y = standardizedRisk)
  } else {
    plotData <- cbind.data.frame(x = as.vector(1/sqrt(varStandardizedRisk)), y = standardizedRisk)
  }
  
  argList <- c(list(
          x = plotData$x,
          y = plotData$y,
          type = "n"
      ), 
      dotList)
  
  do.call("plot", argList)
  
  abline(h = reference)
    
  lines(loess.smooth(x = plotData$x, y = upper95), lwd=lwd, col = confidenceLineColors[1],
      lty = confidenceLineLty[1])
  lines(loess.smooth(x = plotData$x, y = lower95), lwd=lwd, col = confidenceLineColors[1],
      lty = confidenceLineLty[1])
  
  lines(loess.smooth(x = plotData$x, y = upper99), lwd=lwd, col = confidenceLineColors[2],
      lty = confidenceLineLty[2])
  lines(loess.smooth(x = plotData$x, y = lower99), lwd=lwd, col = confidenceLineColors[2],
      lty = confidenceLineLty[2])
  
  
  argList <- c(list(
          x = plotData$x,
          y = plotData$y
      ), 
      dotList)
  
  do.call("points", argList)
  
  legend("topright",
      legend = c("95% CL", "99% CL"),
      lty = confidenceLineLty,
      col = confidenceLineColors,
      lwd = lwd,
      cex = dotList$cex,
      bty = "n")
  legend(max(centerSize)*0.8, mean(min(upper95), reference), legend = round(reference, 3),
         bty = "o", cex = dotList$cex, bg = "white")
  
}
