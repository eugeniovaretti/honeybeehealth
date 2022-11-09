library(tidyverse)
library(plotly)

data <- read_csv('data_merged.csv')
stressor <- read_csv('stressor.csv')
data[is.na(data)] <- 0
stressor[is.na(stressor)] <-0

states.name <- factor(data$state)
i1<-which(states.name==c("Other States","United States"))
new_data<-data[-i1,]

i2<-which(months.name=="April-June"&year.name=="2019")
new_data<-new_data[-i1,]

#plot1
data.year <- new_data %>% 
  group_by(year) %>% 
  mutate(
    max.year = mean(colony_max),
    lost_pct.year = mean(colony_lost_pct),
    add.year = mean(colony_added),
    varroa.year = mean(`Varroa mites`),
    pests.year = mean(`Other pests/parasites`),
    Disesases.year = mean(Disesases),
    pesticides.year=mean(Pesticides),
    other.year=mean(Other))

data.year <- data.year %>% select(contains("year"))

data.year <- data.year %>% gather(key = "type", value = "value", -year)

label <- c(
  "max.year" = "avg colony in the US",
  "lost_pct.year" = "Avg pct_lost",
  "add.year" = "avg colony added",
  "varroa.year" = "avg stressor varroa",
  "pests.year" = "avg stressor pests",
  "Disesases.year" = "avg stressor Disesases",
  "pesticides.year" = "avg stressor pesticides",
  "other.year" = "avg other stressor"
)

data.year %>% 
  ggplot(aes(x = year, y = value, group = type, color = type)) + 
  geom_line(show.legend = F) + 
  facet_wrap(~type, scales = "free", labeller = as_labeller(label)) + 
  geom_vline(xintercept = 2015, color = "red", 
             linetype = "dotted", size = 1.3) + 
  labs(y = "")

state.loss <- new_data %>% 
  ggplot(aes(x = year, y = colony_lost_pct, color = state)) + 
  geom_line(show.legend = F) + 
  labs(title = "colony loss from 2015 to 2022 by each state")
state.loss %>% ggplotly()

#divide by region
west<-c("California", "Oregon", "Washington", "Idaho", "Utah", "Arizona", "Montana", "Alaska", "Hawaii")
Central<-c("Wyoming", "Colorado", "New Mexico", "North Dakota", "South Dakota", "Nebraska", "Kansas", "Oklahoma", "Texas", "Minnesota", "Iowa", "Missouri", "Arkansas", "Louisiana", "Wisconsin", "Illinois", "Mississippi")
East<-c("Michigan", "Indiana", "Kentucky", "Tennessee", "Alabama", "Ohio", "Georgia", "Florida", "South Carolina", "North Carolina", "Virginia", "West Virginia", "Maryland", "New Jersey", "Pennsylvania", "New York", "Connecticut", "Massachusetts", "Vermont", "Maine")
East %in% new_data$state

iw<-which(new_data$state %in% East)

new_data$region[new_data[iw,]]<-"west"

data.region <- new_data %>% 
  mutate(region=case_when(
    new_data$state == west~ "west",
    new_data$state==Central~ "centre",
    new_data$state == East~ "East"
  ))


new_data$region[new_data[$state==west]<-"west"
                new_data$region[new_data$state==Central]<-"centre"
                new_data$region[new_data$state==East]<-"east"

                
                

#marginal distribution of number of honey bee colonies in the US from 2015 to 2022
numcol_eda <- ggplot(new_data, aes(x = new_data$colony_max)) + 
geom_histogram(binwidth = 25000) +
  labs(title = "Distribution of Number of Honey Bee Colonies",
       x = "Number of Colonies",
       y = "Count") +
  theme(plot.title = element_text(size = 10, hjust = 0.5),
        axis.text.y = element_text(size = 8),
        axis.title = element_text(size = 8),
        plot.caption = element_text(size = 8),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 7),
        axis.text.x = element_text(size = 8))

all_neonic_eda <- ggplot(stressor, aes(x = stress_pct, 
                                       fill = stressor)) + 
  geom_histogram(binwidth = 10) +
  labs(title = "Distribution of the stressors by Type",
       x = "pct",
       y = "Count",
       fill = "stressor") +
  theme(plot.title = element_text(size = 10, hjust = 0.5)) + 
  scale_fill_manual(values = c("#C9C9C9", "#F43910", "#2765AC", 
                               "#80ced6", "#30240A","#c0c0c0"),
                    labels = c("Varroa mites", "pests", "Disesases", 
                               "Pesticides", "Other","Unknown"))+
  theme(legend.position = "bottom",
        axis.text.y = element_text(size = 8),
        axis.title = element_text(size = 8),
        plot.caption = element_text(size = 8),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 7),
        legend.box.margin = margin(-10, -10, -10, -10),
        axis.text.x = element_text(size = 8))

region_count <- ggplot(new_data, aes(x = region)) + 
  geom_bar() +
  my_theme +
  labs(title = "Number of Observations by Region",
       x = "Region",
       y = "Count") +
  theme(plot.title = element_text(size = 10, hjust = 0.5),
        axis.text.y = element_text(size = 8),
        axis.text.x = element_text(size = 8),
        axis.title = element_text(size = 8),
        plot.caption = element_text(size = 8),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 7))

year_level <- stressor %>%
  group_by(year, stressor) %>% 
  summarise(usage_year = sum(stress_pct, na.rm = T))

ggplot(year_level, aes(x = year, y = usage_year, color = stressor, group = stressor)) +
  geom_point(size = 1.4) +
  ggplot2::geom_line() +
  labs(title = "Change in the level of stressor in the US from 2015 to 2022\n",
       x = "Year",
       y = "stressor",
       color = "stressor Type") +
  scale_x_continuous(breaks = seq(2015, 2022, by = 1)) +
  geom_vline(aes(xintercept = 2016), linetype = "dotted") + 
  geom_vline(aes(xintercept = 2021), linetype = "dotted") + 
  scale_color_manual(values = c("#C9C9C9", "#F43910", "#2765AC", 
                                "#80ced6", "#30240A","#c0c0c0"),
                    labels = c("Varroa mites", "pests", "Disesases", 
                               "Pesticides", "Other","Unknown")) + 
                       
                       theme(plot.title = element_text(hjust = 0.5, size = 13),
                             axis.title.x = element_text(size = 9.5))


year_region_level <- new_data %>%
  group_by(year, region) %>%
  summarise(num_col_year = sum(colony_lost_pct, na.rm = T))

ggplot(year_region_level, aes(x = year, y = num_col_year , color = region, group = region)) +
  geom_point(size = 1.4) +
  geom_line() + 
  labs(title = "Total Number of Honey Bee Colonies lost in the US from 2015 to 2022\n",
       x = "Year",
       y = "pct",
       color = "Region") +
  scale_x_continuous(breaks = seq(2015, 2022, by = 1)) +
  geom_vline(aes(xintercept = 2015), linetype = "dotted") + 
  geom_vline(aes(xintercept = 2021), linetype = "dotted") + 
  scale_color_manual(values = c("#92a8d1", "#034f84", "#f7cac9")) +
  theme(plot.title = element_text(hjust = 0.5, size = 13),
        axis.title.x = element_text(size = 9.5))



us_data <- map_data("state")

state_region <- 
  all_neonic_honeybee[][c("state","region")] %>%
  left_join(., us_data, by = c("state" = "region" )) %>%
  unique(.)

state_level <- all_neonic_honeybee %>%
  group_by(timepoint, state) %>%
  summarise(all_neonic = mean(all_neonic, na.rm = T) / 1000) %>%
  left_join(state_region, by = c("state" = "state")) 

direct_label_state <- state_region %>%
  filter(state == "iowa" | state == "mississippi" | state == "utah" |
           state == "new york")
ggplot(state_level) + 
  geom_polygon(aes(x = long, y = lat, group = group, fill = all_neonic)) + 
  facet_wrap(~timepoint, ncol = 2,
             labeller = as_labeller(c("prior_2003" = "Prior to 2003", 
                                      "post_2003" = "After 2003"))) +
  coord_map("polyconic") + 
  scale_fill_gradient2(limits = c(1, 160), 
                       breaks = seq(from = 0, to = 160, by = 40),
                       midpoint = -20,
                       low = "#DBDEF0", high = "#2B49DE", na.value = "grey90") + 
  geom_dl(data = direct_label_state, inherit.aes = FALSE, 
          aes(x = long, y = lat, label = region),
          method = list("top.points", vjust = 1.4, cex = 0.7, 
                        fontfamily = "Arial")) +
  theme(axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.65),
        panel.grid.major = element_blank(),
        legend.key.height = unit(0.9, "line")) +
  labs(title = "Average Neonicotinoids Used in the US Before and After 2003\n",
       caption = "Data Source: NASS, USGS",
       fill = "Neonicotinoids\nUsage (tons)\n")