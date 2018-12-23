##### Data import #####
df2 <- read.csv(file="database_cleaned_v3.csv",
                header = T, sep = ",", check.names = FALSE)
df <- as.tibble(read.csv("database.csv",
                         header = T, stringsAsFactors = F))
df$X <- NULL
df2[,1] <- NULL

##### Removing Factor levels from numeric featrues ######

droplevels(df2)
levels(df2$Period..Minutes.)
levels(df2$Launch.Mass..Kilograms.)
levels(df2$Dry.Mass..Kilograms.)
levels(df2$Power..Watts.)
levels(df2$Expected.Lifetime..Years.)

df2$Period..Minutes. <- as.numeric(df2$Period..Minutes.)
df2$Launch.Mass..Kilograms. <- as.numeric(df2$Launch.Mass..Kilograms.)
df2$Dry.Mass..Kilograms. <- as.numeric(df2$Dry.Mass..Kilograms.)
df2$Power..Watts. <- as.numeric(df2$Power..Watts.)
df2$Expected.Lifetime..Years. <- as.numeric(df2$Expected.Lifetime..Years.)

##### Main server function #####

function(input, output, session) {
  
  ##### Node Network ####
  output$plot1 <- renderSimpleNetwork({ 
   
  # Filter down data frame to Country.of.Operator.Owner selected, Detailed.Purpose, Purpose
  newdf <- filter(df, Country.of.Operator.Owner == input$"country") %>%
    select(Country.of.Operator.Owner, Detailed.Purpose, Purpose)
  
  # Changing Empty observations to None
  for (i in 1:length(newdf$Purpose)) {
    if (newdf$Detailed.Purpose[i] == "") {
      newdf$Detailed.Purpose[i] <- "None"
    }
    
    if (newdf$Purpose[i] == "") {
      newdf$Purpose[i] <- "None"
    }
  }
  
  # Creating source and target vectors and making a new data frame from them
  src <- newdf$Detailed.Purpose
  target <- newdf$Purpose
  networkData <- data.frame(src, target)
  
  # Importing a note for the user
  output$text <- renderPrint(cat(
    paste("If there is a node with the name None, then there was no specific purpose",
          "as to why the satellite is/was used, other than its general purpose.", 
          sep="\n")))
  
  # Creating the network
  simpleNetwork(networkData, nodeColour = input$node, linkColour = input$link, zoom = TRUE)
  }) 
  
  
  ##### Stats and Plots #####
  
  #Condense data frame to the x and y variable by user input
  lit <- reactive({df2[, c(input$xCol, input$yCol)]})
  superlit <- reactive({df2[, input$xCol]})
 
  # Create the plot
  output$plot <- renderPlot({plot(lit(), ch = 20, cex = 3, pch = 20,
                                  col = "blue", main = "Bivariate Plot of Two Categories")
    # Add trend line
    mod <- lm(lit()[,c(2, 1)])
    abline(mod, col = input$lmline, lwd = input$lmw)
    })
  
  # T-test subtitle
  output$t_test <- renderText({"T-test"})
  
  # Print regression line summary
  output$statistics <- renderPrint({
    xy <- lit()[,c(2, 1)]
    mod <- lm(xy)
    summary(mod)
  })
  
  # Print Note
  output$Note <- renderPrint({
    case1 <- "Note: Two quantitative variables provide a scatter plot,
      histogram and T-test.
    "
    case2 <- "      A quantitative X variable and a qualitative Y varible
      provides a scatter plot and a histogram.
    "
    
    case3 <- "      A qualitative X variable and a quantitative Y varible
      provides a scatter plot and a T-test.
    "
    
    case4 <- "      Two qualitative varibles provide only a bivariate plot."
    
    cat(paste(case1, case2, case3, case4, sep = "\n"))
  })
  
  # Create histogram
  output$otherplot <- renderPlot({
    
    # Render a barplot
    hist(superlit(),
         xlab=input$xCol,
         main="Univariate Historgam of Specified Category", col = input$histcol)})
  
 
  
  #### LEAFTLET Launch Map ####

  # Map note
  output$mapNote <- renderPrint({
    note <- "    Click on a location marker to see an example of a satellite
    name launched at that location within that date range.

    (Note: Start date should be on the left and the end
     date on the right to return results.)"
    cat(paste(note))
  })
  
  # Subset data based on year input
  sliderData <- reactive({
    dates <- df2$Date.of.Launch
  
    years <- c()
    months <- c()
    days <- c()

    # Put each year month and day into appropriate vector
    for (i in 1:length(dates)) {
      year <- as.integer(str_sub(dates[i], 1, 4))
      month <- as.integer(str_sub(dates[i], 6, 7))
      day <- as.integer(str_sub(dates[i], 9, 10))
      
      years <- c(years, year)
      months <- c(months, month)
      days <- c(days, day)
    }
    
    # Add the vectors to the data frame
    df3 <- df2
    df3$years <- years
    df3$months <- months
    df3$days <- days
    
    df3 <- df3[-c(1)]
    
    #Grab input dates
    start <- input$dates[1]
    end <- input$dates[2]
    newdf <- df3
    
    # Fileter data frame based on the input dates
    newdf <- filter(newdf, as.numeric(years) | as.numeric(months) | as.numeric(days)) %>%
      filter((as.integer(str_sub(start, 1, 4)) <= years) &
               (as.integer(str_sub(end, 1, 4)) >= years)) %>%
      filter(!((as.integer(str_sub(start, 6, 7)) > months) &
                 (as.integer(str_sub(start, 1, 4)) == years)) & 
               !((as.integer(str_sub(end, 6, 7)) < months) &
                   (as.integer(str_sub(end, 1, 4)) == years))) %>%
      filter(!((as.integer(str_sub(start, 9, 10)) > days) &
                 (as.integer(str_sub(start, 6, 7)) == months) &
                 (as.integer(str_sub(start, 1, 4)) == years)) &
               !((as.integer(str_sub(end, 9, 10)) < days) &
                   (as.integer(str_sub(end, 6, 7)) == months) &
                   (as.integer(str_sub(end, 1, 4)) == years)))
    combo <- list(df = newdf, names = as.character(newdf$Official.Name.of.Satellite))
    combo
  })
  
  # Create Map
  output$map <- renderLeaflet({
    combo <- sliderData()
    df <- combo$df
    names <- combo$names
    leaflet(df) %>% addTiles() %>%
      addMarkers(~launch.lng, ~launch.lat, popup = names)})
  
  # Display the input dates under the map
  output$value <- renderPrint({ cat(paste(paste("Start Date:", input$dates[1]),
                                      paste("End Date:  ", input$dates[2]),
                                      sep = "\n")) })
}
