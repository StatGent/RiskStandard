# RiskStandard

This `R`-package implements statistical methods for benchmarking clinical care centers based on a binary quality indicator such as 30-day mortality. For each center we provide directly or indirectly standardized risks based on fixed center effects outcome models that incorporate patient-specific baseline covariates to adjust for differential case-mix. The user can choose to apply the Firth correction in the outcome model to maintain convergence in the presence of very small centers.

Input data must contain for each patient:

1. patient-specific covariates to adjust for in the analysis, e.g., age and baseline severity, 
2. a hospital code where the patient was treated and 
3. a binary quality outcome, e.g., 30-day mortality. 

Three example datasets are included in the package. 

<!--- Full documentation of the package can be found in the vignette. --->

***

## Contents

This repository contains both the extracted R-package that can be installed by using the R-package `devtools` as well as the compressed RiskStandard_0.0.6.tar.gz that can be installed as follows:

```{r}
install.packages("./RiskStandard_0.0.6.tar.gz", 
                 repos = NULL, type = "source")
```

Included in this package are the following functions:

1. **`standardizeRisks()`**: estimates the (directly or indirectly) standardized risks. When some patients have missing values for a categorical patient covariate, two ways to handle the missingness are offered: `missing='completeCase'` performs a complete case analysis, `missing='dummyCategory'` adds a separate category to the fitted outcome model, allowing for a missing value effect. When some patients have missing values for a continuous patient covariate, the function will perform a complete case analysis.
2. **`labelCenters()`**: uses the output from the `standardizeRisks()` function to classify the centers as having 'low', 'accepted' or 'high' mortality risk. The clinically relevant cut-off boundaries can be adapted by the user.
3. **`plotRisks()`**: for indirectly standardized outcomes, this function generates a descriptive scatterplot of the observed against the expected risk under the average care level for patients of that center. For directly standardized outcomes, this function generates a descriptive scatterplot of the observed against the estimated directly standardized risk for each center. 
4. **`plotCenterLabels()`**: center performance classification can be visualised using the estimated standardized risk and variance per center from the output of `standardizeRisks()` and classification labels from `labelCenters()`. 
5. **`funnelPlot()`**: the funnelplot will compare institutional performance. The estimated standardized risks are plotted against a measure of precision. Care centers with an estimated standardized risk lying outside the 95% control limits are flagged as outlying centers.

as well as three illustrative datasets on which the functions can be run:

* `smallCaseMix` with small differences in patient mix across centers, 
* `largeCaseMix` with large differences in patient mix across centers, and 
* `largeCaseMix_missing` which is based on `largeCaseMix` but where the consciousness level is missing for some patients. 

*** 

## Documentation

A detailed description of all functions and datasets can be found in the vignette. 

*** 

## Reference to the package

Please use the following reference when referencing to the RiskStandard package:
> Varewyck, M. and Van Messem, A. (2015). RiskStandard. `https://github.ugent.be/StatGent/RiskStandard`

***

## Authors & Licence

The package was developed by [Stat-Gent CRESCENDO](http://www.statgent.org/), [Ghent University](http://www.ugent.be/). The code made available in this repository is licenced under the GPL-3 licence (see LICENCE for details).


