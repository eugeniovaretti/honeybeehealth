library(dplyr)
par(mfrow = c(1,4))
boxplot(final_data_bystate%>% filter(months=="Q4") %>% select(Varroa.mites))
boxplot(final_data_bystate%>% filter(months=="Q1") %>% select(Varroa.mites))
boxplot(final_data_bystate%>% filter(months=="Q2") %>% select(Varroa.mites))
boxplot(final_data_bystate%>% filter(months=="Q3") %>% select(Varroa.mites))
boxplot(final_data_bystate%>% filter(months=="Q4") %>% select(Varroa.mites))

library(tidyverse)
library(hrbrthemes)
library(viridis)
library(ggplot2)
final_data_bystate %>%
  ggplot( aes(x=months, y=Varroa.mites, fill=months)) +
  geom_boxplot() +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Basic boxplot") +
  xlab("")