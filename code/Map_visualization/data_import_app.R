library(dplyr)

colony_plot <- read_csv("data/colony.csv")
d <- read_csv("data/data_merged.csv")
d[is.na(d)] <- 0

states.name <- factor(d$state)
i1<-which(states.name==c("United States"))
d<-d[-i1,]

i2<-which(d$months=="April-June"&d$year=="2019")
d<-d[-i2,]

colony_plot$state <- tolower(colony_plot$state)

colony_plot <- colony_plot %>% mutate(
  period = case_when(
    months=='January-March' ~ paste('winter',year,sep='-'),
    months=='October-December' ~ paste('winter',year+1,sep='-'),
    months=='April-June' ~ paste('summer',year,sep='-'),
    months=='July-September' ~ paste('summer',year,sep='-')
  ))

X = d %>%
  group_by(state,year) %>%
  #select(year, colony_lost_pct, Varroa.mites) %>%
  summarise(avg_lost = mean(colony_lost_pct) )

X = X %>% tidyr::pivot_wider(
  names_from = year, 
  values_from = avg_lost,
  values_fill = 0
) 

states = X[1]
X = X[2:9]

# import colony
# import stressor
data <- inner_join(colony, stressor, by = c("year","months","state"))
