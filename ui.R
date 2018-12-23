library(networkD3)
library(leaflet)
library(shiny)
library(tidyverse)

##### Data import #####

df2 <- read.csv(file="database_cleaned_v3.csv", 
                header = T, sep = ",", check.names = FALSE)
df <- as.tibble(read.csv("database.csv",
                         header = T, stringsAsFactors = F))
df$X <- NULL
df2[,1] <- NULL
countries <- sort(unique(df$Country.of.Operator.Owner))

##### Create navigation page #####
navbarPage("Satellite Data Visualizations",
           inverse = TRUE,
           selected = "Network",
           
           ##### First tab panel Network d3 #####
           tabPanel(
             "Network",
             headerPanel(title=h2("Purpose Relationships by Country")),
             headerPanel(title=h4("Abel, Olson and Siantar")),
             sidebarLayout(
               sidebarPanel(
                 
                 # Country selection
                 selectInput('country', 'Select Country', choices = countries, selected = "USA"),
                 
                 # Node color selection
                 radioButtons("node", label = h3("Node Color"),
                              choices = list("Red" = "red", "Orange" = "orange", "Green" = "green",
                                             "Blue" = "blue", "Purple" = "purple",
                                             "Black" = "black", "Gray" = "gray"), 
                              selected = "red"),
                 
                 # Link color selection
                 radioButtons("link", label = h3("Link Color"),
                              choices = list("Red" = "red", "Orange" = "orange", "Green" = "green",
                                             "Blue" = "blue", "Purple" = "purple",
                                             "Black" = "black", "Gray" = "gray"), 
                              selected = "black"),
                 hr()
          
               ),
               mainPanel( simpleNetworkOutput('plot1'),
                          verbatimTextOutput("text"))
             )
           ),
           
           ##### Second tab panel plots and stats#####
           tabPanel("Stats",
                    headerPanel(title=h2("Statistics & Plots")),
                    headerPanel(title=h4("Abel, Olson and Siantar")),
                    sidebarLayout(
                      sidebarPanel(
                        
                        # X variable input
                        selectInput('xCol', 'Scatter Plot X & Histogram', choices = names(df),
                                    selected = "Perigee..Kilometers."),
                        
                        # Y variable input
                        selectInput('yCol', 'Scatter Plot Y', choices = names(df), 
                                    selected = "Apogee..Kilometers."),
                        
                        # Regression line color selection
                        radioButtons("lmline", label = h3("Regression Line Color"),
                                     choices = list("Red" = "red", "Orange" = "orange", "Green" = "green",
                                                    "Blue" = "blue", "Purple" = "purple",
                                                    "Black" = "black", "Gray" = "gray"), 
                                     selected = "red"),

                        # Regression line width selection
                        selectInput('lmw', 'Regression Line Width',
                                    choices = c(2, 2.25, 2.50, 2.75, 3, 3.25, 3.50, 3.75, 4),
                                    selected = 2),
                        
                        # Histogram Color selection
                        radioButtons("histcol", label = h3("Histogram Color"),
                                     choices = list("Red" = "red", "Orange" = "orange", "Green" = "darkgreen",
                                                    "Blue" = "blue", "Purple" = "purple",
                                                    "Black" = "black", "Gray" = "gray"), 
                                     selected = "black")
                      ),
                      mainPanel( verbatimTextOutput("Note"),
                                 plotOutput('plot'),
                                 plotOutput('otherplot'),
                                 h3(textOutput("t_test")),
                                 verbatimTextOutput("statistics"))
                      )
                    ),
           ##### Third tab panel Map #####
           tabPanel("Launch Map",
                    headerPanel(title=h2("Launch Location, by Date")),
                    headerPanel(title=h4("Abel, Olson and Siantar")),
                    sidebarLayout(
                      sidebarPanel(
                        
                        # Date input
                        dateRangeInput("dates", label = h3("Date Range"), 
                                       start = "2011-12-07"),
                        hr()
                      ),
                      mainPanel(
                        verbatimTextOutput("mapNote"),
                        leafletOutput("map"),
                        verbatimTextOutput("value")
                      )
                    )
           )
)
