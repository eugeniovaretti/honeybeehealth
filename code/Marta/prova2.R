library(ggplot2) # Plotting
library(knitr)  # kable
library(GGally) # ggpairs plot
library(ISLR) # Source of Data
library(caret) # Showing Confusion Matrix Data
library(purrr) # Organizing
library(tidyr) # Organize/tidy data
library(reshape) # Melt data for plotting
library(splines) # Splines
library (gam)    # GAM
library(boot)    # cv.glm

data <- read.csv('cleaned_data.csv')
data<-data[,-1]
d <- read.csv("cleaned_data_withpct.csv")
stressor <- read.csv('stressor.csv')
stressor[is.na(stressor)] <- 0
US <-read.csv("US_data.csv")
US[is.na(US)] <- 0

data %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value,fill=key)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram(bins=sqrt(nrow(OJ))) +
  theme(legend.position="none")

