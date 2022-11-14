#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)

# Define UI for application that draws a histogram
shinyUI(fluidPage(theme = shinytheme("flatly"),

    # Application title
    titlePanel("Average Lost of bee colonies in US"),
               

    # Sidebar with a slider input for number of bins
    navbarPage("Sezioni",
        tabPanel(icon("home"),
        p("This app has been developed to analyze data on bee colony loss (from USDA) "),         
        sidebarPanel(
            sliderInput("year",
                        "Year:",
                        min = 2015,
                        max = 2022,
                        value = 2020,
                        sep = "")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("mapPlot")
        ),
        
        p("* Each percentage has to be considered as average of quarter's proportions of lost colonies. Each quarter percentage is calculated as number of lost colonies over the maximum number of colonies in that period."),
        
        p("A total of 3,347 beekeepers from the United States that collectively managed 192,384 colonies on 1 October 2020 provided validated survey responses. 
        This represented 7% of the estimated 2.71 million managed honey-producing colonies in the country in 2020 (USDA NASS, 2021)."),
        p("Colony loss rates were calculated as the ratio of the number of colonies lost to the number of colonies managed over a defined period (Kulhanek et al., 2017). 
          Loss rates should not be interpreted as a change in population size because beekeepers are able to replace lost colonies throughout the year. 
          Therefore, colony loss rates are best interpreted as a turn-over rate, as high levels of losses do not necessarily result in a decrease in the total number of colonies managed in the United States.")
        
        ),
    tabPanel("Annual differences",
             p("This tool allows you to visualize the (difference between) lost colonies in two years"),
             fluidRow(
               column(4,
                      selectInput("months", label = h4("Select period"), 
                                  choices = c("Entire Year",unique(colony_plot$months)),#list("January-March" = "January-March", "April-June" = "April-June", "July-September" = "July-September"), 
                                  selected = "Entire Year")),
               column(4,
                      
                      
                        sliderInput("year1",
                           "First year:",
                           min = 2015,
                           max = 2022,
                           value = 2016,
                           sep = "")
             ),
             column(4,
             
               sliderInput("year2",
                           "Second Year:",
                           min = 2015,
                           max = 2022,
                           value = 2017,
                           sep = "")
             
               )),
             fluidRow(
             plotOutput("Differences")),
             fluidRow(  
               column(6,
                      plotOutput("Map1")),
               column(6,
                      plotOutput("Map2"),
               )
               
               
               ),
             
             
             
             
    ),
    
    tabPanel("Seasonal trends",
             p("This tool allows you to visualize how the percentage of lost colonies changes along the years for the same season "),
             fluidRow(
               column(4,
                      selectInput("period", label = h4("Select period"), 
                                  choices = list("Entire Year" = "Entire Year", "Spring-Summer" = "summer", "Fall-Winter" = "winter"), 
                                 selected = "Entire Year")),
               column(4,
                      
                      
                      sliderInput("year1P",
                                  "First year:",
                                  min = 2015,
                                  max = 2022,
                                  value = 2016,
                                  sep = "")
               ),
               column(4,
                      
                      sliderInput("year2P",
                                  "Second Year:",
                                  min = 2015,
                                  max = 2022,
                                  value = 2017,
                                  sep = "")
                      
               )),
             
             fluidRow(column(3, verbatimTextOutput("period1"))),
             
             fluidRow(
               plotOutput("DifferencesP")),
             fluidRow(  
               column(6,
                      plotOutput("MapP1")),
               column(6,
                      plotOutput("MapP2"))
            ),
    ),
    
    tabPanel("Hierarchical Clusters",
             fluidRow(column(3,
                             selectInput('dist', 'Choose a distance', choices = list(
                               "Eucledian" = 'euclidean', "Manhattan" = 'manhattan', "Canberra" = 'canberra'),
                               selected = 'euclidean')
             ),
             column(3,
                    selectInput('linkage', 'Choose a Linkage', choices = list(
                      "Single" = 'single', "Average" = 'average', "Complete" = 'complete', "Ward"='ward.D2'),
                      selected = 'average')
             ),
             
             column(3,
                    sliderInput("k", 'Choose number of clusters', min = 2,  max = 10, value = 3)
             ),
             
             column(3, 
                    p("Cophenetic Correlation Coefficient:"),
                    verbatimTextOutput("cophern_value"))
             
             
             ),
             
             
             fluidRow(
               column(5,
                      plotOutput("plot_dendogram")
             ),
             column(5,
                    verbatimTextOutput("plot_table"))),
             
             fluidRow(
               #column(5,
             #plotlyOutput("plot_cluster_map")
             #)
               
             ),
             
             
             
    )
            
)
))






