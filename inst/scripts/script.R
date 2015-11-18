library(RiskStandard)

largeCaseMix <- read.table("largeCaseMix.txt", header=TRUE)

L <- model.matrix(~ largeCaseMix$age + largeCaseMix$sex + factor(largeCaseMix$cons))[,-1]
center <- largeCaseMix$center
Y <- largeCaseMix$outcome 

# hand in patientCovariates as data frame
standardizedRisks <- standardizeRisks(patientCovariates = largeCaseMix[, 1:3], 
    center = center, Y = Y)

# hand in patientCovariates as a model matrix
standardizedRisks <- standardizeRisks(patientCovariates = L, center = center, Y = Y)

centerLabels <- labelCenters(standardizedRisks = standardizedRisks)

plotCenterLabels(standardizedRisks = standardizedRisks, centerLabels = centerLabels)
plotRisks(standardizedRisks = standardizedRisks, centerLabels = centerLabels)
funnelPlot(standardizedRisks = standardizedRisks, centerLabels = centerLabels)

  
### small case mix
smallCaseMix <- read.table("smallCaseMix.txt", header=TRUE)

L <- model.matrix(~ smallCaseMix$age + smallCaseMix$sex + factor(smallCaseMix$cons))[,-1]
center <- smallCaseMix$center
Y <- smallCaseMix$outcome 


testDf <- smallCaseMix[, 1:3]
testDf[,2] <- factor(testDf[,2])
testDf[,3] <- factor(testDf[,3])

standardizeRisks(patientCovariates = testDfMissing, center = center, Y = Y)

standardizedRisks <- standardizeRisks(L = L, center = center, Y = Y)

centerLabels <- labelCenters(standardizedRisks = standardizedRisks)

plotCenterLabels(standardizedRisks = standardizedRisks, centerLabels = centerLabels)
plotRisks(standardizedRisks = standardizedRisks, centerLabels = centerLabels)
funnelPlot(standardizedRisk = standardizedRisks, centerLabels = centerLabels)


### simulated option 1

# cf. simulated.R

testCenter <- center

# order factor levels if they are numbers
if (!any(is.na(as.numeric(as.character(testCenter))))){
  result <- factor(testCenter, 
      levels = sort(unique(as.numeric(as.character(testCenter)))))
}


standardizedRisks <- standardizeRisks(L = L, center = center, Y = Y)

centerLabels <- labelCenters(standardizedRisks = standardizedRisks)

plotCenterLabels(standardizedRisks = standardizedRisks, centerLabels = centerLabels)

plotCenterLabels(standardizedRisks = standardizedRisks, 
    centerLabels = centerLabels, labelColors = c("pink", "blue", "orange"))

plotCenterLabels(standardizedRisks = standardizedRisks, 
    centerLabels = centerLabels, labelPch = c(3, 7, 22), cex = 2)

centerLabels <- labelCenters(standardizedRisks = standardizedRisks,
    cl = 0.25)

# pdf(file = "test.pdf")
plotCenterLabels(standardizedRisks = standardizedRisks, 
    centerLabels = centerLabels,
    yValue = "name")
# dev.off()

plotRisks(standardizedRisks = standardizedRisks, centerLabels = centerLabels)


plotRisks(standardizedRisks = standardizedRisks, centerLabels = centerLabels,
  labelPch = c(3, 5, 9),
  labelColors = c("blue", "pink", "orange"),
  cex = 5)

funnelPlot(standardizedRisks = standardizedRisks)

funnelPlot(standardizedRisks = standardizedRisks, lwd = 5, 
    confidenceLineColors = c("olivedrab1", "blue"),
    confidenceLineLty = c(1, 2))


funnelPlot(standardizedRisks = standardizedRisks, lwd = 5, 
    confidenceLineColors = c("olivedrab1", "blue"),
    confidenceLineLty = c(1, 2))
