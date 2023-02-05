library(tidyverse)
library(plotly)

data <- read.csv('cleaned_data.csv')
d <- read.csv("cleaned_data_withpct.csv")
stressor <- read.csv('stressor.csv')
stressor[is.na(stressor)] <- 0
US <-read.csv("US_data.csv")
US[is.na(US)] <- 0
#plot1
library(tidyverse)

year_level <- stressor %>%
  group_by(year,stressor) %>% 
  summarise(usage_year = mean(stress_pct, na.rm = T),
            num_col_year = mean(d$colony_max, na.rm = T))

ggplot(year_level, aes(x = year, y = usage_year, color = stressor, group = stressor)) +
  geom_point(size = 1.4) +
  ggplot2::geom_line() +
  labs(title = "annual average stressor impact in the US from 2015 to 2022",
       x = "Year",
       y = "stressor",
       color = "stressor Type",
       caption = "Data Source: NASS, USGS") +
  scale_x_continuous(breaks = seq(2015, 2022, by = 1)) +
  geom_vline(aes(xintercept = 2018), linetype = "dotted") + 
  geom_vline(aes(xintercept = 2022), linetype = "dotted") + 
  scale_color_manual(values = c("#C9C9C9", "#F43910", "#2765AC", 
                                "#80ced6", "#30240A", "#00FF00"),
                     labels = c("Disesases",
                                "Other",
                                "Other pests/parasites",
                                "Pesticides",
                                "Unknown",
                                "Varroa mites")) + 
  theme(plot.title = element_text(hjust = 0.5, size = 13),
        axis.title.x = element_text(size = 9.5),
        plot.caption = element_text(hjust = 1))

#plot2
year_region_level <- d %>%
  group_by(year, state) %>%
  summarise(num_col_year = mean(colony_lost_pct, na.rm = T))

ggplot(year_region_level, aes(x = year, y = num_col_year, color = state, group = state)) +
  geom_point(size = 1.4) +
  geom_line() + 
  labs(title = "mean avg of colony_lost_pct in the US from 2015 to 2022",
       x = "Year",
       y = "avg pct of colony_lost",
       caption = "Data Source: NASS, USGS",
       color = "Region") +
  scale_x_continuous(breaks = seq(2015, 2022, by = 2)) +
  geom_vline(aes(xintercept = 2018), linetype = "dotted") + 
  geom_vline(aes(xintercept = 2022), linetype = "dotted") + 
  scale_color_manual(values= rainbow(47)) +
    theme(plot.title = element_text(hjust = 0.5, size = 13),
        axis.title.x = element_text(size = 9.5))

#plot3
data.year <- US %>% 
  group_by(year) %>% 
  summarise(a.numcol.year = mean(colony_n), 
            b.yeildpercol.year = mean(colony_lost),
            c.totalprod.year = mean(colony_lost_pct), 
            d.stocks.year = mean(Varroa.mites), 
            e.priceperlb.year = mean(Other.pests.parasites),
            f.prodvalue.year = mean(Disesases),
            g.prodvalue.year = mean(Pesticides),
            h.prodvalue.year = mean(Other),
            i.prodvalue.year = mean(Unknown)) %>% 
  select(year, contains("year"))
data.year <- data.year %>% 
  gather(key = "type", value = "value", -year)

label.data <- c(
  "a.numcol.year" = "avg numb of colony",
  "b.yeildpercol.year" = "Avg numb of colony lost",
  "c.totalprod.year" = "avg pct of colony lost",
  "d.stocks.year" = "avg varroa",
  "e.priceperlb.year" = "avg pests",
  "f.prodvalue.year" = "avg disease",
  "g.prodvalue.year" = "avg pesticides",
  "h.prodvalue.year" = "avg other",
  "i.prodvalue.year" = "avg unknown"
  )

library(scales)
ggplot(data.year, aes(x = year, y = value)) + 
  geom_line(color = "blue", size = 2) + 
  geom_point(color = "black", size = 2) +
  facet_wrap(~type, scales = "free", labeller = as_labeller(label.data)) +
  scale_y_continuous(labels = comma)


#plot4
n.str1.corr <- d %>% 
  group_by(state,year) %>% 
  select(year, colony_lost_pct, Varroa.mites) %>% 
  summarise(`Average colony_lost_pct` = mean(colony_lost_pct), `Average Varroa.mites` = mean(Varroa.mites))

cor(n.str1.corr$`Average colony_lost_pct`, n.str1.corr$`Average Varroa.mites`)
#-0.42 pearson correlation between variables

yield.price.corrplot <- n.str1.corr %>%
  ggplot(aes(`Average Varroa.mites`, `Average colony_lost_pct`)) + 
  geom_point(shape = 1, size = 2) +
  geom_point(aes(col=state))+
  scale_color_hue(h.start = 90) +
  geom_smooth(method='lm')

#boxplot
n.str1.corr <- d %>% 
  group_by(
    year,state) %>% 
  select(year, state,colony_lost_pct, Varroa.mites) %>% 
  summarise(`Average colony_lost_pct` = mean(colony_lost_pct), `Average Varroa.mites` = mean(Varroa.mites))

boxplot(n.str1.corr$`Average colony_lost_pct`, main="lost_pct", 
        sub=paste("Outlier Rows: ", boxplot.stats(n.str1.corr$`Average colony_lost_pct`)$out))
boxplot(n.str1.corr$`Average Varroa.mites`, main="str1", 
        sub=paste("Outlier Rows: ", boxplot.stats(n.str1.corr$`Average Varroa.mites`)$out))
#density plots
plot(density(n.str1.corr$`Average colony_lost_pct`), main="Density Plot: colony_lost_pct", ylab="Frequency", 
     sub=paste("Skewness:", round(e1071::skewness(n.str1.corr$`Average colony_lost_pct`), 2)))
polygon(density(n.str1.corr$`Average colony_lost_pct`), col="gold") 

plot(density(n.str1.corr$`Average Varroa.mites`), main="Density Plot: colony_lost_pct", ylab="Frequency", 
     sub=paste("Skewness:", round(e1071::skewness(n.str1.corr$`Average Varroa.mites`), 2)))
polygon(density(n.str1.corr$`Average Varroa.mites`), col="gold") 


#plot5
d %>% select(state, colony_lost_pct) %>%
  group_by(state) %>%
  summarize(total = mean(colony_lost_pct)) %>%
  arrange(desc(total)) %>% head(20) %>%
  ggplot(aes(reorder(state, -total), total, fill = state))+
  geom_bar(stat = 'identity')+
  scale_y_continuous(breaks = seq(from = 0, to = 250000, by = 25000))+
  labs(y = 'TONS', x = '')+
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13))



data.year <- data %>% 
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

#plot6
state.loss <- d %>% 
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
numcol_eda <- ggplot(d, aes(x = d$colony_max)) + 
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
                               "#80ced6", "#30240A","#00FF00"),
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

###
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


##region
year_region_level <- d %>%
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

