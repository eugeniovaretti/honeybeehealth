#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#import colony

library(shiny)
library(plotly)
library(usmap)
library(readr)
library(dplyr)
library(stats)
library(factoextra)

# 1. Import data
# colony_plot <- read_csv("data/colony.csv")
# d <- read_csv("data/data_merged.csv")
# d[is.na(d)] <- 0
# 
# states.name <- factor(d$state)
# i1<-which(states.name==c("United States"))
# d<-d[-i1,]
# 
# i2<-which(d$months=="April-June"&d$year=="2019")
# d<-d[-i2,]

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

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$mapPlot <- renderPlot({

      us_data <- map_data("state")
      cv <- colony_plot[colony_plot$year == input$year,c(3,7)] #select state and colony lost percentage
      cv$state <- factor(cv$state)
      cv[is.na(cv$colony_lost_pct),2]<-0 
      agg = aggregate(cv,
                      by = list(cv$state),
                      FUN = mean,
      )
      
      agg <- agg[,-2]
      
      
      
      df <- data.frame(
        state = agg[,1],
        values = agg[,2]
      )
      
      title = paste("Lost colonies in", input$year, "(%) *")
      plot_usmap(data = df, labels=T) + labs(title = title) + scale_fill_gradientn(colors = c("#1b98e0", "#f6f805", "#353436"),limits = c(0, 50))

        

    })
    
    output$Differences <- renderPlot({
      us_data <- map_data("state")
      if (input$months=="Entire Year"){
      cv1 <- colony_plot[colony_plot$year == input$year1,c(3,7)] #select state and colony lost percentage
      
      cv1$state <- factor(cv1$state)
      cv1[is.na(cv1$colony_lost_pct),2]<-0 
      agg1 = aggregate(cv1,
                      by = list(cv1$state),
                      FUN = mean,
      )
      
      agg1 <- agg1[,-2]
      
      cv2 <- colony_plot[colony_plot$year == input$year2,c(3,7)] #select state and colony lost percentage
      
      cv2$state <- factor(cv2$state)
      cv2[is.na(cv2$colony_lost_pct),2]<-0 
      agg2 = aggregate(cv2,
                       by = list(cv2$state),
                       FUN = mean,
      )
      
      agg2 <- agg2[,-2]
      
      title = paste("Difference between colonies in", input$year1, "and",input$year2, "(%) *")
      
      } else{
        cv1 <- filter(colony_plot, year == input$year1 & months == input$months)
        cv1 <- cv1[,c(3,7)] #select state and colony lost percentage
        
        cv1$state <- factor(cv1$state)
        cv1[is.na(cv1$colony_lost_pct),2]<-0 
        
        cv2 <- filter(colony_plot, year == input$year2 & months == input$months)
        cv2 <- cv2[,c(3,7)] #select state and colony lost percentage
        
        cv2$state <- factor(cv2$state)
        cv2[is.na(cv2$colony_lost_pct),2]<-0 
        
        
        title = paste("Difference between colonies in", input$months,input$year1, "and",input$months, input$year2, "(%) *")
      }
      
      df <- data.frame(
        state = cv1$state,
        values = cv2$colony_lost_pct-cv1$colony_lost_pct
      )
      
      plot_usmap(data = df, labels=T) + labs(title = title)+ scale_fill_gradientn(colors = c("#1b98e0", "#f6f805", "#353436"))#,limits = c(0, 50))
      
      
    })
    
    output$Map1 <- renderPlot({
      us_data <- map_data("state")
      if (input$months=="Entire Year"){
        cv <- colony_plot[colony_plot$year == input$year1,c(3,7)] #select state and colony lost percentage
        title = paste("Lost colonies in", input$year1, "(%) *")
        
        cv$state <- factor(cv$state)
        cv[is.na(cv$colony_lost_pct),2]<-0 
        agg = aggregate(cv,
                        by = list(cv$state),
                        FUN = mean,
        )
        
        agg <- agg[,-2]
        
        df <- data.frame(
          state = agg[,1],
          values = agg[,2]
        )
        
      }
        
        else{   
          cv <- filter(colony_plot, year == input$year1 & months == input$months)
          cv <- cv[,c(3,7)]
          title = paste("Lost colonies in", input$year1,input$months, "(%) *")
          
          df <- data.frame(
            state = cv$state,
            values = cv$colony_lost_pct)
          
        }
          
      plot_usmap(data = df, labels=T) + labs(title = title) + scale_fill_gradientn(colors = c("#1b98e0", "#f6f805", "#353436"),limits = c(0, 50))
      
    })
    
    output$Map2 <- renderPlot({
      us_data <- map_data("state")
      if (input$months=="Entire Year"){
        cv <- colony_plot[colony_plot$year == input$year2,c(3,7)] #select state and colony lost percentage
        title = paste("Lost colonies in", input$year2, "(%) *")
        
        cv$state <- factor(cv$state)
        cv[is.na(cv$colony_lost_pct),2]<-0 
        agg = aggregate(cv,
                        by = list(cv$state),
                        FUN = mean,
        )
        
        agg <- agg[,-2]

        df <- data.frame(
          state = agg[,1],
          values = agg[,2]
        )
 
      }
      
      else{   
        cv <- filter(colony_plot, year == input$year2 & months == input$months)
        cv <- cv[,c(3,7)]
        title = paste("Lost colonies in", input$year2,input$months, "(%) *")
        
        df <- data.frame(
          state = cv$state,
          values = cv$colony_lost_pct)
        
      }
      
      plot_usmap(data = df, labels=T) + labs(title = title) + scale_fill_gradientn(colors = c("#1b98e0", "#f6f805", "#353436"),limits = c(0, 50))
      
    })

    # period1 <- reactive({
    #   paste(input$period,input$year1P, sep='-')
    # })
    
    # period2 <- reactive({
    #   paste(input$period,input$year2P, sep='-')
    # })
    
    output$DifferencesP <- renderPlot({
      us_data <- map_data("state")
      if (input$period=="Entire Year"){
        cv1 <- colony_plot[colony_plot$year == input$year1P,c(3,7)] #select state and colony lost percentage
        
        cv1$state <- factor(cv1$state)
        cv1[is.na(cv1$colony_lost_pct),2]<-0 
        agg1 = aggregate(cv1,
                         by = list(cv1$state),
                         FUN = mean,
        )
        
        agg1 <- agg1[,-2]
        
        cv2 <- colony_plot[colony_plot$year == input$year2P,c(3,7)] #select state and colony lost percentage
        
        cv2$state <- factor(cv2$state)
        cv2[is.na(cv2$colony_lost_pct),2]<-0 
        agg2 = aggregate(cv2,
                         by = list(cv2$state),
                         FUN = mean,
        )
        
        agg2 <- agg2[,-2]
        
        title = paste("Difference between colonies in", input$year1, "and",input$year2, "(%) *")
        
      } else{
        period1 <- paste(input$period,input$year1P, sep='-')
        cv1 <- filter(colony_plot, period == period1)
        cv1 <- cv1[,c(3,7)] #select state and colony lost percentage
        
        cv1$state <- factor(cv1$state)
        cv1[is.na(cv1$colony_lost_pct),2]<-0 
        
        period2 <- paste(input$period,input$year2P, sep='-')
        cv2 <- filter(colony_plot, period == period2)
        cv2 <- cv2[,c(3,7)] #select state and colony lost percentage
        
        cv2$state <- factor(cv2$state)
        cv2[is.na(cv2$colony_lost_pct),2]<-0 
        
        }
      
      df <- data.frame(
        state = cv1$state,
        values = cv2$colony_lost_pct-cv1$colony_lost_pct
      )
      
      title = paste("Difference between colonies in", input$months,input$year1P, "and",input$months, input$year2P, "(%) *", "(mean=",mean(df$values),")")
      
      
      plot_usmap(data = df, labels=T) + labs(title = title)+ scale_fill_gradientn(colors = c("#1b98e0", "#f6f805", "#353436"))#,limits = c(0, 50))
      
      
    })
    
    
    output$MapP1 <- renderPlot({
      period1 <- paste(input$period,input$year1P, sep='-')
      us_data <- map_data("state")
      if (input$period=="Entire Year"){
        cv <- colony_plot[colony_plot$year == input$year1P,c(3,7)] #select state and colony lost percentage
        title = paste("Lost colonies in", input$year1P, "(%) *")
        
        cv$state <- factor(cv$state)
        cv[is.na(cv$colony_lost_pct),2]<-0 
        agg = aggregate(cv,
                        by = list(cv$state),
                        FUN = mean,
        )
        
        agg <- agg[,-2]
        
        df <- data.frame(
          state = agg[,1],
          values = agg[,2]
        )

      }
      
      else{   
        cv <- filter(colony_plot, period == period1)
        cv <- cv[,c(3,7)]
        title = paste("Lost colonies in", input$year1P,input$period, "(%) *")
        
        df <- data.frame(
          state = cv$state,
          values = cv$colony_lost_pct)
        
      }
      
      plot_usmap(data = df, labels=T) + labs(title = title) + scale_fill_gradientn(colors = c("#1b98e0", "#f6f805", "#353436"),limits = c(0, 50))
      
    })
    
    output$MapP2 <- renderPlot({
      period2 <- paste(input$period,input$year2P, sep='-')
      us_data <- map_data("state")
      if (input$period=="Entire Year"){
        cv <- colony_plot[colony_plot$year == input$year2P,c(3,7)] #select state and colony lost percentage
        title = paste("Lost colonies in", input$year2P, "(%) *")
        
        cv$state <- factor(cv$state)
        cv[is.na(cv$colony_lost_pct),2]<-0 
        agg = aggregate(cv,
                        by = list(cv$state),
                        FUN = mean,
        )
        
        agg <- agg[,-2]
        
        df <- data.frame(
          state = agg[,1],
          values = agg[,2]
        )
        
      }
      
      else{   
        cv <- filter(colony_plot, period == period2)
        cv <- cv[,c(3,7)]
        title = paste("Lost colonies in", input$year2P,input$period, "(%) *")
        
        df <- data.frame(
          state = cv$state,
          values = cv$colony_lost_pct)
        
      }
      
      plot_usmap(data = df, labels=T) + labs(title = title) + scale_fill_gradientn(colors = c("#1b98e0", "#f6f805", "#353436"),limits = c(0, 50))
      
    })
    
    dist_mat <- reactive({
      dist(X, method=input$dist)
    })
    data_c <- reactive({
      hclust(dist_mat(), method=input$linkage)
    })
    
    cluster_group <- reactive({
      return(as.factor(cutree(data_c(), k=input$k)))
    }) 
    
    title <- reactive({
      paste("Dendogram", input$dist,input$linkage, sep = " - ")
    })
    
    cophern <- reactive({
      cor(dist_mat(), cophenetic(data_c()))
    })
    
    output$cophern_value <- renderPrint({ cophern()})
    
    output$plot_dendogram <- renderPlot({  
      plot(data_c(), main=title(), hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
      rect.hclust(data_c(), k=input$k, border = 2:5)
    })
    
    table_group <- reactive({ table(cluster_group()) })
    
    output$plot_table<- renderPrint({ table_group()})
    
    output$plot_cluster_map <- renderPlot({
      #name = input$dist
      # generate 3d plot based on the name of clusters
      us_data <- map_data("state")
      
      df_cl <- data.frame(
        state = tolower(states$state),
        values = as.numeric(cluster_group())
      )
      
      plot_usmap(data = df_cl) + labs(title = "Clusters") + scale_fill_gradientn(colors = c("#1b98e0", "#f6f805", "#353436"))
      
    })

})
