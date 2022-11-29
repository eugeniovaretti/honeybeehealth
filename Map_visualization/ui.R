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
    titlePanel("Analysis of bee colonies Loss in US"),
               

    # Sidebar with a slider input for number of bins
    navbarPage("Sezioni",
        tabPanel(icon("home"),
                 p("Data available (from USDA): colony loss and stressor associated"),         
                 selectInput("select", "Select 1 or more columns to display", colnames(colony_plot), multiple = TRUE),
                 fluidRow(column(DT::dataTableOutput("lost_table"),
                                 width = 12)),  
                 selectInput("select_lost", "Select 1 or more columns to display", colnames(stressor), multiple = TRUE),
                 fluidRow(column(DT::dataTableOutput("stressor_table"),
                                 width = 12)),    
        ),
        tabPanel("Colony loss",
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
    tabPanel("Annual differences 1",
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
    
    tabPanel("2",
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
    tabPanel("Seasonal trends",
             fluidRow(
               column(8,
                      sliderInput("year_box",
                                  "First year:",
                                  min = 2015,
                                  max = 2021,
                                  value = 2016,
                                  sep = "")
                      )),
             fluidRow(  
               column(2),
               column(8,
             plotOutput("box_plots")),
             )),
             
    
    tabPanel("Hier. Clusters",
             fluidRow(column(3,
                             selectInput('dist', 'Choose a distance', choices = list(
                               "Eucledian" = 'euclidean', "Manhattan" = 'manhattan', "Canberra" = 'canberra'),
                               selected = 'canberra')
             ),
             column(3,
                    selectInput('linkage', 'Choose a Linkage', choices = list(
                      "Single" = 'single', "Average" = 'average', "Complete" = 'complete', "Ward"='ward.D2'),
                      selected = 'complete')
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
                    plotOutput("plot_cluster_map")
                    )),
             
             fluidRow(
               #column(5,
             #plotlyOutput("plot_cluster_map")
               verbatimTextOutput("plot_table")
             #)
               
             ),
             
            
  
             
             
             
    ),
    
    tabPanel("Stressors",
             p("This tool allows you to visualize the (difference between) lost colonies in two years"),
    
             
             fluidRow(
               column(4,
                      selectInput("months_s", label = h4("Select period"), 
                                  choices = c("Entire Year",unique(colony_plot$months)),#list("January-March" = "January-March", "April-June" = "April-June", "July-September" = "July-September"), 
                                  selected = "Entire Year")),
               column(4,
                      
                      
                      sliderInput("year_s",
                                  "First year:",
                                  min = 2015,
                                  max = 2022,
                                  value = 2016,
                                  sep = "")
               ),
               column(4,
                      
                      selectInput("stressor", label = h4("Select stressor"), 
                                  choices = unique(stressor$stressor),#list("January-March" = "January-March", "April-June" = "April-June", "July-September" = "July-September"), 
                                  selected = "Varroa mites")
                      
               )),
             fluidRow(
               plotOutput("Map_s")),
             ),
    
    tabPanel("Loss vs Stressors",
             fluidRow(
               column(6,
             sliderInput("year_l",
                         "Year:",
                         min = 2015,
                         max = 2022,
                         value = 2020,
                         sep = ""),),
             column(6,
                    selectInput("stressor_l", label = h4("Select stressor"), 
                                choices = unique(stressor$stressor),#list("January-March" = "January-March", "April-June" = "April-June", "July-September" = "July-September"), 
                                selected = "Varroa mites"))
             ),
             
             fluidRow(
               column(6, plotOutput("mapPlot2")),
               column(6, plotOutput("mapPlot2_s"))),
             
             selectInput("selected_state", label = h4("Select state"), 
                         choices = unique(stressor$state),#list("January-March" = "January-March", "April-June" = "April-June", "July-September" = "July-September"), 
                         selected = "California"),
             
             fluidRow(plotOutput("plot_stressor_lost")),
               
             
             
             
             
    ),
    
    tabPanel("Trend of n during year",
             plotOutput("trend_years")         
             
    )
    
            
)
))






