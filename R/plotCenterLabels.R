#' Plot the standardized risk with confidence limits per center 
#' @param standardizedRisks standardized risks as produced by the standardize function 
#' @param labeledCenters labeled centers as produced by the labelCenters function
#' @param yValue value to report on the y axis; one of 'size' (default; center size) or
#'   'name' (center name)
#' @param labelColors vector of colors (length 3) for the L, A and H label respectively;
#'   default value is \code{c(L = "green", A = "black", H = "red")}
#' @param labelPch vector of plot characters to be used for the L, A and H label resp.;
#'   default value is \code{c(L = 17, A = 20, H = 17)}
#' @param ... further arguments passed to plot
#' @return no return value; plot is written to the current device
#' @export
#' @examples set.seed(130513)
#' simulatedData <- simulateData()
#' standardizedRisks <- with(simulatedData, standardizeRisks(patientCovariates = L, center = center, Y = Y))
#' labeledCenters <- labelCenters(standardizedRisks = standardizedRisks)
#' plotCenterLabels(standardizedRisks = standardizedRisks, labeledCenters = labeledCenters)
plotCenterLabels <- function(standardizedRisks, labeledCenters,
    yValue = c("size", "name"),
    labelColors = c(L = "olivedrab1", A = "black", H = "red"),
    labelPch = c(L = 17, A = 20, H = 17),
    ...){
  
  confidenceLevel <- attr(labeledCenters, "confidenceLevel")
  yValue <- match.arg(yValue)  
  
  centerSize <- standardizedRisks$centerSize
  centerName <- standardizedRisks$centerName
  m <- length(centerSize)
  
  standardizedRisk <- standardizedRisks$standardizedRisk
  meanY <- attr(standardizedRisks, "meanY")
  method <- attr(standardizedRisks, "method")
  
  centerLabel <- labeledCenters$centerLabel
  lowerCI <- labeledCenters$lowerCI
  upperCI <- labeledCenters$upperCI
  lambdaLow <- attr(labeledCenters, "lambdaLow")
  lambdaHigh <- attr(labeledCenters, "lambdaHigh")
  
  op <- par(mar = c(5.1, 5.1, 2.1, 2.1))
  
  if (yValue == "size"){
    centerLow <- centerSize[centerLabel == "L"]
    centerAccepted <- centerSize[centerLabel == "A"]
    centerHigh <- centerSize[centerLabel == "H"]  
  } else {
    centerLow <- centerName[centerLabel == "L"]
    centerAccepted <- centerName[centerLabel == "A"]
    centerHigh <- centerName[centerLabel == "H"]
  }
  
  confidenceLevelPercentage <- 100 * confidenceLevel
  
  xLabel <- if(method == "direct"){
        substitute(expression(hat(E) * "{Y(c)}" * " and " * x * "% CI"),
            list(x = confidenceLevelPercentage))
      } else {
        paste("Est. excess risk and ", confidenceLevelPercentage, "% CI", sep = "")
      } 
  
  dotList <- list(...)
  
  if (is.null(dotList$ylim)){
    if (yValue == "size"){
      dotList$ylim <- range(centerSize)
    } else {
      dotList$ylim <- c(1, m)
    }
  }
  if (is.null(dotList$xlim)){
    dotList$xlim <- c(min(lowerCI), max(upperCI))
  }
  if (is.null(dotList$cex.axis)){
    dotList$cex.axis <- 1.5
  }
  if (is.null(dotList$cex.lab)){
    dotList$cex.lab <- 1.5
  }
  if (is.null(dotList$log)){
    if (yValue == "size"){
      dotList$log <- "y"  
    }
  }
  
  if (is.null(dotList$ylab)){
    if (yValue == "size"){
      dotList$ylab <- "Center size"  
    } else {
      dotList$ylab <- "Center name"
    }
    
  }
  if (is.null(dotList$xlab)){
    dotList$xlab <- xLabel
  }
  
  commonCex <- if (is.null(dotList$cex)) 1.5 else dotList$cex
  dotList$cex <- NULL
  
  lwd <- if (is.null(dotList$lwd)) 2 else dotList$lwd
  
  argList <- c(list(
          x = standardizedRisk[centerLabel == "A"],
          y = centerAccepted,
          col=labelColors[2],
          pch = labelPch[2],
          cex = commonCex
      ), 
      dotList)
  
  do.call("plot",
      argList)
  
  points(standardizedRisk[centerLabel=="L"], centerLow, 
      pch = labelPch[1], cex = commonCex, col=labelColors[1])
  
  points(standardizedRisk[centerLabel=="H"], centerHigh, 
      pch = labelPch[3], cex = commonCex, col=labelColors[3])
  
  yIndices <- if (yValue == "size") centerSize else centerName
  
  for (i in 1:m){
    yIndex <- yIndices[i]
    lines(c(lowerCI[i], upperCI[i]), c(yIndex, yIndex), lwd = lwd) # TODO check arrows
  }
  
  if (method == "direct"){
    abline(v = (1 - lambdaLow) * meanY, col="darkgray", lty="dotted", lwd=2)
    abline(v = (1 + lambdaHigh) * meanY, col="darkgray", lty="dotted", lwd=2)
  } else {
    abline(v = - lambdaLow, col = "darkgray", lty="dotted", lwd=2)  
    abline(v = lambdaHigh, col = "darkgray", lty="dotted", lwd=2)
  }
  
  par(op)
  
}
