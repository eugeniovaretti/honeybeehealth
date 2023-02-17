# Honey Bee Health 
Evaluation of Factors and Strategies to Mitigate Colony Loss in US

Project for the course of NonParametric Statistics, 2022, Politecnico di Milano

## Description
Bees are one of the most ecologically and commercially important insects in the world, in our project:
1. we analyze the risk factors that can be associated with the disappearance of colonies,
2. estimate the economic impact associated with lost honey, (although losses from missed pollination could be even more significant),
3. show how these losses are not bearable for beekeepers.
Finally, this information was gathered to provide suggestions for public authorities to better develop resilience plans.

## Install the required packages from CRAN

```
packages_list <- c(“aplpack”, “car”, “DepthProc”, “dplyr”, “factoextra”, “fda”, “fdacluster”, “fdaPDE”, “fdatest”, “forecast”, “ggplot2”, “gridExtra”, “hexbin”, “ISLR2”, “ISLR2”, “latex2exp”, “lattice”, “maps”, “MASS”, “mgcv”, “np”,“pbapply”, “plotly”, “raster”, “RColorBrewer”, “readr”, “rgl”, “roahd”, “robustbase”, “sf”, “shiny”, “splines”, “stats”, “survival”, “survminer”, “svglite”, “tidyverse”, “tseries”, “usmap”, “viridis”, “visdat”, “weathermetrics”)
install.packages(packages_list)
```

## Data
In our analyis, we use different public data set:
- factors which stress directly bee colonies as Varroa Mite or pesticides (source: USDA)
- other possible influential factors, in particular Temperature, Drought, Precipitation (source: National Centers for Environmental Information)
- annual production and price of the honey for each state (USDA)

The required data for each Notebook is present in the folder where the code is.

To facilitate furher researches, a cleaned version of the dataset is provided in the folder [FinalDataSet](https://github.com/eugeniovaretti/honeybeehealth/tree/main/FinalDataSet)

## Preliminary analysis and Data consistency
The folder [Map_Visualization](https://github.com/eugeniovaretti/honeybeehealth/tree/main/code/Map_visualization) contains a Shiny tool that allows you to produce some plots as average losses per state. 

The folder [DataConsistency](https://github.com/eugeniovaretti/honeybeehealth/tree/main/code/DataConsistency) contains Data_Consistency.ipynb which is used to explain some apparent inconsistency in data.

## Analysis of potential exploratory factors [AnalysisOfFactors](https://github.com/eugeniovaretti/honeybeehealth/tree/main/code/AnalysisOfFactors)
- `Exploration_Plots.ipynb`: first exploratory analysis for colony losses distribution over states and seasons, effects of the stressors and creation of survival metrics.
- `Functional_Depth_Measure.R`: Outliers detections in the different features available using functional boxplots and outliergrams
- `Functional_Permutation_Tests.R`: Functional permutation tests to check difference in distributions of featues among different seasonalities, quarters...
- `paired univariate permutation test.Rmd` : two paired-population permutation test to check if the loss in summers is significantly different from the loss during winters
- `anova_for_varroa.Rmd-` : anova permutational test between 3 clusters based on minimum temperature to check if the influence of varroa is significantly different among the groups
- `Multivariate_Depth_Measure.R`: Outliers detection in multivariate case
- `data_outputs/` contains output (and input) dataset coming from the bayesmix analysis useful for `bnp_clus_and_func.Rmd`


## A Model of loss-stressors relation [LossModel](https://github.com/eugeniovaretti/honeybeehealth/tree/main/code/LossModel)
- `Gam_final.Rmd`: code for the final GAM
- `bnp_clus_and_func.Rmd`: attemptive BNP clustering using ([bayesmix](https://github.com/eugeniovaretti/honeybeehealth)) to cluster time series

## Quantification of the impact of losses on Beekeepers and economical magnitude [ImpactModel](https://github.com/eugeniovaretti/honeybeehealth/tree/main/code/ImpactModel)
- `Survival.Rmd` : Survival Analysis to quantify the impact of the colony loss on beekeepers
- `SpatialRegression_ColonyLossPct_Price_Plots.R`: Penalized Spatial Spline Regression Models with targets the money loss or the colony loss percentage
- 'TimeSpatialRegr_Price.R': Penalized Semiparametric Regression Model for Spatial Functional data, using as target the money loss in k$ for each 100 colonies present at a given state in a given quarter, + Eigen Sign-flip score test on beta coefficients of parametric part of the model
- `TimeSpatialRegr_AbsColonyLoss.R`: Penalized Semiparametric Regression Model for Spatial Functional data with the absolute values of colony loss as target
- `TimeSpatialRegr_ColonyLossPct.R`: Penalized Semiparametric Regression Model for Spatial Functional data with colony loss percentage as target
- `TimeSpatialRegr_ColonyLossPct_Inference.R`: One-at-a-time and simultaneous Eigen Sign-flip score tests on beta coefficients of parametric part of the Spatial functional regression model

## Utilities [utils](https://github.com/eugeniovaretti/honeybeehealth/tree/main/code/utils)
- `utils/`: contains utilities functions that are useful to manage data and plots
