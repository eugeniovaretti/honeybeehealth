# Honey Bee Health 
Evaluation of Factors and Strategies to Mitigate Colony Loss in US

Project for the course of NonParametric Statistics

## Description
Bees are one of the most ecologically and commercially important insects in the world, in our project:
1. we analyze the risk factors that can be associated with the disappearance of colonies,
2. estimate the economic impact associated with lost honey, (although losses from missed pollination could be even more significant),
3. show how these losses are not bearable for beekeepers.
Finally, this information was gathered to provide suggestions for public authorities to better develop resilience plans.

The repository [data](https://github.com/eugeniovaretti/honeybeehealth/tree/main/data) contains all the datasets used for the sub-analyses carried out

The folder [code](https://github.com/eugeniovaretti/honeybeehealth/tree/main/code) contains alle the code useful for the analysis, divided in subfolders (to maintain a certain order)

## Preliminary analysis
The folder [Map_Visualization](https://github.com/eugeniovaretti/honeybeehealth/tree/main/code/Map_visualization) contains a Shiny tool that allows you to produce some plots as average losses per state. 

## Data consistency
The folder [DataConsistency](https://github.com/eugeniovaretti/honeybeehealth/tree/main/code/Map_visualization)  contains Data_Consistency.ipynb

In the remaining folders  can find, divided by topic, all the following scripts:

- `Exploration_Plots.ipynb`: first exploratory analysis for colony losses distribution over states and seasons, effects of the stressors and creation of survival metrics.

- `Functional_Depth_Measure.R`: Outliers detections in the different features available using functional boxplots and outliergrams
- `Functional_Permutation_Tests.R`: Functional permutation tests to check difference in distributions of featues among different seasonalities, quarters...
- `paired univariate permutation test.Rmd`
- `anova_for_varroa.Rmd-`
- `Multivariate_Depth_Measure.R`: Outliers detection in multivariate case

- `Survival.Rmd`
- `SpatialRegression_ColonyLossPct_Price_Plots.R`: Penalized Spatial Spline Regression Models with targets the money loss or the colony loss percentage
- 'TimeSpatialRegr_Price.R': Penalized Semiparametric Regression Model for Spatial Functional data, using as target the money loss in k$ for each 100 colonies present at a given state in a given quarter, + Eigen Sign-flip score test on beta coefficients of parametric part of the model
- `TimeSpatialRegr_AbsColonyLoss.R`: Penalized Semiparametric Regression Model for Spatial Functional data with the absolute values of colony loss as target
- `TimeSpatialRegr_ColonyLossPct.R`: Penalized Semiparametric Regression Model for Spatial Functional data with colony loss percentage as target
- `TimeSpatialRegr_ColonyLossPct_Inference.R`: One-at-a-time and simultaneous Eigen Sign-flip score tests on beta coefficients of parametric part of the Spatial functional regression model
- `Gam_final.Rmd`: code for the final GAM
- `bnp_clus_and_func.Rmd`: attemptive BNP clustering using ([bayesmix](https://github.com/eugeniovaretti/honeybeehealth)) to cluster time series

- `utils/`: contains utilities functions that are useful to manage data and plots
- `data_outputs/`: contains output (and input) dataset coming from the bayesmix analysis useful for `bnp_clus_and_func.Rmd`


