### R code from vignette source 'Vignette-StandardRisk.Rnw'

###################################################
### code chunk number 1: installPackage (eval = FALSE)
###################################################
## install.packages("./RiskStandard_0.0.5.tar.gz", 
##                  repos = NULL, type = "source")
## 


###################################################
### code chunk number 2: loadPackage
###################################################
library(RiskStandard)


###################################################
### code chunk number 3: Vignette-StandardRisk.Rnw:60-65
###################################################
str(largeCaseMix)

m <- length(unique(largeCaseMix$center))
n <- dim(largeCaseMix)[1]
centerSize <- as.vector(table(largeCaseMix$center))


###################################################
### code chunk number 4: descr
###################################################
layout(matrix(1:4))

# Age
with(largeCaseMix, 
     plot(1:m, tapply(age, center, mean), pch = 19,
          cex = centerSize/n*m, cex.lab = 1.2, ylim = c(60,80), 
          xlab = "Center",  ylab = "", main = "Mean age per center"))
with(largeCaseMix, 
     boxplot(age ~ center, xlab = "Center", ylab = "Age distribution", 
     cex.lab = 1.2))

# Sex
with(largeCaseMix,
     plot(1:m, tapply(sex, center, mean), pch = 19, cex=centerSize/n*m,
          cex.lab = 1.2, ylim = c(0,1), xlab = 'Center', ylab = "",
          main= "Percentage women per center"))

# Consciousness
with(largeCaseMix,
     plot(1:m, tapply((cons==1), center, mean), pch = 21,
          cex = centerSize/n*m, cex.lab = 1.2, ylim = c(0,1.1), xlab='Center', 
          ylab = "", main="Distribution of consciousness level per center"))
with(largeCaseMix, 
     points(c(1:m), tapply((cons %in% c(1,2)), center, mean), pch = 19,
            cex = centerSize/n*m))
legend("bottomleft", pch = c(21,19), bty='n',
       legend = c("Alert", "Alert or drowsy"), cex=1.2)


###################################################
### code chunk number 5: descriptives
###################################################
layout(matrix(1:4))

# Age
with(largeCaseMix, 
     plot(1:m, tapply(age, center, mean), pch = 19,
          cex = centerSize/n*m, cex.lab = 1.2, ylim = c(60,80), 
          xlab = "Center",  ylab = "", main = "Mean age per center"))
with(largeCaseMix, 
     boxplot(age ~ center, xlab = "Center", ylab = "Age distribution", 
     cex.lab = 1.2))

# Sex
with(largeCaseMix,
     plot(1:m, tapply(sex, center, mean), pch = 19, cex=centerSize/n*m,
          cex.lab = 1.2, ylim = c(0,1), xlab = 'Center', ylab = "",
          main= "Percentage women per center"))

# Consciousness
with(largeCaseMix,
     plot(1:m, tapply((cons==1), center, mean), pch = 21,
          cex = centerSize/n*m, cex.lab = 1.2, ylim = c(0,1.1), xlab='Center', 
          ylab = "", main="Distribution of consciousness level per center"))
with(largeCaseMix, 
     points(c(1:m), tapply((cons %in% c(1,2)), center, mean), pch = 19,
            cex = centerSize/n*m))
legend("bottomleft", pch = c(21,19), bty='n',
       legend = c("Alert", "Alert or drowsy"), cex=1.2)


###################################################
### code chunk number 6: Vignette-StandardRisk.Rnw:120-125
###################################################
indirectRisks <- standardizeRisks(
  patientCovariates = largeCaseMix[,c('age','sex','cons')],
  center = largeCaseMix[,'center'], 
  Y = largeCaseMix[,'outcome'])
head(indirectRisks)


###################################################
### code chunk number 7: Vignette-StandardRisk.Rnw:128-133 (eval = FALSE)
###################################################
## directRisks <- standardizeRisks(
##   patientCovariates = largeCaseMix[,c('age','sex','cons')],
##   center = largeCaseMix[,'center'], 
##   Y = largeCaseMix[,'outcome'],
##   method='direct')


###################################################
### code chunk number 8: Vignette-StandardRisk.Rnw:138-144
###################################################
indirectRisks2 <- 
  standardizeRisks(patientCovariates = largeCaseMix_missing[,c('age','sex','cons')],
                   center = largeCaseMix_missing[,'center'], 
                   Y = largeCaseMix_missing[,'outcome'],
                   method='indirect', missing='completeCase')
attr(indirectRisks2, "n")


###################################################
### code chunk number 9: Vignette-StandardRisk.Rnw:152-154
###################################################
labeledCenters <- labelCenters(standardizedRisks = indirectRisks)
head(labeledCenters)


###################################################
### code chunk number 10: Vignette-StandardRisk.Rnw:163-166
###################################################
labeledCenters2 <- labelCenters(standardizedRisks = indirectRisks, 
                                lambda=c(low = 0.06, high = 0.02))
head(labeledCenters2)


###################################################
### code chunk number 11: plotRisks
###################################################
plotRisks(standardizedRisks = indirectRisks, 
          labeledCenters = labeledCenters)


###################################################
### code chunk number 12: Vignette-StandardRisk.Rnw:179-180
###################################################
plotRisks(standardizedRisks = indirectRisks, 
          labeledCenters = labeledCenters)


###################################################
### code chunk number 13: plotCenterLabels
###################################################
plotCenterLabels(standardizedRisks = indirectRisks,
                 labeledCenters = labeledCenters)


###################################################
### code chunk number 14: Vignette-StandardRisk.Rnw:202-203
###################################################
plotCenterLabels(standardizedRisks = indirectRisks,
                 labeledCenters = labeledCenters)


###################################################
### code chunk number 15: funnelPlot
###################################################
funnelPlot(standardizedRisks = indirectRisks)


###################################################
### code chunk number 16: Vignette-StandardRisk.Rnw:222-223
###################################################
funnelPlot(standardizedRisks = indirectRisks)


