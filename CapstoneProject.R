library(shinyjs)
library(scales)
library(shiny)
library(shinyWidgets)
library(shinyTime)
library(shinythemes)
library(leaflet)
library(leaflet.extras)
library(ggplot2)
library(dplyr)
library(tools)
library(lubridate)
library(shinycssloaders)
library(RColorBrewer)
library(httr)
library(jsonlite)

# UI
ui <- fluidPage(
  
  useShinyjs(),
  theme = shinytheme("darkly"),
  
  # Create a navigation bar with three tabs
  navbarPage(
    title ="Map of natural hazard events in Armenia",
    tags$head(
      tags$style(
        HTML("
        .navbar-brand {
          font-size: 24px;
          font-weight: bold;
          color: #333333;
             cursor: default !important;
          pointer-events: none !important;
        }
      ")
      )
    ),    
    # First Tab: display a map and data of hazardous events (1)
    tabPanel("Hazardous Events Map",
             sidebarLayout(
               sidebarPanel(
                 # Allow user to upload a file with data to display on the map
                 fileInput("file", "Upload File"),
                 div(class = "slider",
                     sliderInput("year_range", "Select Year Range", min = 2018, max = 2023, value = c(2018, 2023), step = 1, 
                                 width = "100%", ticks = TRUE, animate = TRUE, sep = "", dragRange = TRUE, round = TRUE),
                 ),
                 selectInput("type_filter", "Select the event type", choices = c("Earthquakes", "Fires", "Storms", "Hailstorms", "Other", "All"),selected = "All"),
                 
                 hr(),
                 
                 div(class = "icon-container"),
                 div(class = "label-item",
                     p("Earthquakes"),
                     tags$img(src = "https://cdn-icons-png.flaticon.com/512/1684/1684394.png", width = "30px", height = "30px"),
                 ),
                 div(class = "label-item",
                     p("Fires"),
                     tags$img(src = "https://img.icons8.com/emoji/512/fire.png", width = "30px", height = "30px"),
                 ),
                 div(class = "label-item",
                     p("Storms"),
                     tags$img(src = "https://cdn-icons-png.flaticon.com/512/1146/1146799.png", width = "30px", height = "30px"),
                 ),
                 div(class = "label-item",
                     p("Hailstorms"),
                     tags$img(src = "https://cdn-icons-png.flaticon.com/512/1999/1999846.png", width = "30px", height = "30px"),
                 ),
                 div(class = "label-item",
                     p("Other"),
                     tags$img(src = "https://cdn-icons-png.flaticon.com/512/4643/4643191.png", width = "30px", height = "30px"),
                 ),
                 
                 hr(),
                 
                 actionButton("download_data", "Download Data", class = 'green-button'),
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Map", withSpinner(leafletOutput("map", width = "100%", height = "800px"), color = "#00bc8c")),
                   tabPanel("Data", withSpinner(dataTableOutput("table"), color = "#00bc8c"))
                 )
               ),
             ),
    ),
    
    # Second Tab: Event insertion and mapping (2)
    tabPanel("Insert event", icon = icon("add"),
             sidebarLayout(
               sidebarPanel(
                 tabPanel("Input Values",
                          selectInput("type", "Enter type of natural hazard:", choices = c("Earthquake", "Storm", "Fire", "Hailstorm", "Other"), selected = "Other"),
                          uiOutput("magnitude"),
                          uiOutput("satellite"),
                          numericInput("longitude", "Enter Longitude: ", value = 40),
                          numericInput("latitude", "Enter Latitude: ", value = 40),
                          textInput("place", "Enter Place: ", value = ""),
                          dateInput("updated", "Select a date:", format = "yyyy-mm-dd", value = "2023-01-01"),
                          
                          hr(),
                          
                          actionButton("save", "Save Input Values", class = "green-button"),
                          actionButton("download", "Download Saved Values", class = 'simple-button'),
                          actionButton("downloadMerged", "Download Merged Values", class = 'simple-button')
                 )
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Map Inserted Events", leafletOutput("addedMap", width = "100%", height = "800px"), dataTableOutput("inserted")),
                   tabPanel("Map Merged Events", withSpinner(leafletOutput("mergedMap", width = "100%", height = "600px"), color = "#00bc8c"), dataTableOutput("merged")),
                 )
               ),
             ),
    ),
    
    # Third Tab: Statistical analysis (3)
    tabPanel("Statistical Analysis",
      tabsetPanel(
        tabPanel("Plots",
                 sidebarPanel(
                   h2("Visualize Data"),
                   selectInput("distType", "Select the type of graph for distribution:", 
                               choices = c("Histogram", "Scatterplot"), selected = ""),
                   dateRangeInput("date_range", label = "Select the date range",
                                  start = "2018-01-01", end = "2023-03-31"),
                   uiOutput("eventType"),
                   uiOutput("eventType2"),
                   
                   uiOutput("xVariable"),
                   uiOutput("yVariable"),
                   
                   actionButton("graphPlot", "Plot the graph", class = "save-button"),
                 ),
                 mainPanel(
                   plotOutput("output_plot"),
                   hr(),
                   h3("Plot Summary"),
                   verbatimTextOutput('summaryPlot'),
                   hr(),
                   conditionalPanel(
                     condition = "input.distType == 'Scatterplot'",
                     plotOutput("output_plot2"),
                     hr(),
                     h3("Plot Summary"),
                     verbatimTextOutput('summaryPlot2')
                   ),
                   
                 )
                
        ),
        tabPanel("Time Series",
                 plotOutput("output_plot_ts"),
                 hr(),
                 h3("Time Series Summary"),
                 verbatimTextOutput('summaryTimeSeries')
        ),
        tabPanel("Heatmap",
                 leafletOutput("heatMapEarthquake", width = "70%", height = "500px"),
                 hr(),
                 h3("Summary for the Heatmap"),
                 verbatimTextOutput('summaryHeatMap')
        ),
      ),
    ),
    
    # Make the navbar fixed at the top
    position = "fixed-top",
    
    #Additional styles for the software
    tags$style("
     .well {
        padding: 50px 20px;
      }
      
      .icon-container {
        align-items: center;
        display: flex;
        justify-content: center; 
        margin-top: 21px;
      }
      
      .tab-content {
        margin: 100px 0;
      }
      
      .leaflet,
      .dataTables_wrapper {
        margin: -80px 0 70px 0;
      }
      
      .label-item {
        display: flex;
        justify-content: center;
        padding: 3px 0;
      }
      
      .label-item img {
        margin-left: 5px;
      }
      
      .insertedTable {
        margin-top: 50px;
      }
      
      .simple-button {
        margin-top: 5px;
      }
      
      .green-button {
        background-color: #00bc8c;
        border-color: #01a77c;
        margin-right: 200px;
      }
      
      .irs-bar {
        background: #00bc8c !important;
      }
      
      .message-panel {
        background-color: orange;
        border: 2px solid red;
        padding: 30px;
      }
      
      .display-year-range {
        background-color: white;
        border-radius: 5px;
        padding: 5px;
      }
      
      .input-group-addon {
        background: #00bc8c;
        color: white;
      }
      
      .heatmap-title {
        color: red;
        font: 16px bold;
        margin: 10px;
      }
    ")
  )
)

# SERVER
server <- function(input, output, session) {
  # Set up icons
  defaultIcon <- makeIcon(
    iconUrl = "https://cdn-icons-png.flaticon.com/512/4643/4643191.png",
    iconWidth = 25, 
    iconHeight = 25
  )
  earthquakeIcon <- makeIcon(
    iconUrl = "https://cdn-icons-png.flaticon.com/512/1684/1684394.png",
    iconWidth = 25, 
    iconHeight = 25
  )
  stormIcon <- makeIcon(
    iconUrl = "https://cdn-icons-png.flaticon.com/512/1146/1146799.png",
    iconWidth = 25, 
    iconHeight = 25
  )
  fireIcon <- makeIcon(
    iconUrl = "https://img.icons8.com/emoji/512/fire.png",
    iconWidth = 25, 
    iconHeight = 25
  )
  hailstormIcon <- makeIcon(
    iconUrl = "https://cdn-icons-png.flaticon.com/512/1999/1999846.png",
    iconWidth = 25, 
    iconHeight = 25
  )
  
  # Render a numeric input for earthquake magnitude if the event type is an earthquake
  output$magnitude <- renderUI({
    if (tolower(input$type) == "earthquake") {
      numericInput("mag", "Enter the earthquake magnitude:",
                   min = 0, max = 10, value = 5)
    }
  })
  
  output$eventType2 <- renderUI({
    if (input$distType == "Scatterplot") {
      selectInput("eventType2", "Select the second event type:",
                  choices = c(unique(data()$type), "all")
      )
    }
  })
  
  # Render a text input for satellite if the event type is fire
  output$satellite <- renderUI({
    if (tolower(input$type) == "fire") {
      textInput("satellite", "Enter the Satellite:")
    }
  })
  
  # Render a select input for event type with unique options
  output$eventType <- renderUI({
    selectInput("eventType", "Select the event type:",
                choices = c(unique(data()$type), "all")
    )
  })
  
  # Render a select input for time series for the event types with unique options
  output$eventTypeTimeSeries <- renderUI({
    selectInput("eventTypeTimeSeries", "Select the event type:",
                choices = c(unique(data()$type), "all")
    )
  })
  
  # Render a select input for x variable based on the selected event type
  output$xVariable <- renderUI({
    selectInput("xVar", "Select the x variable :",
                choices = getColumnNames(data(), input$eventType),
                selected = ""
    )
  })
  
  # Render a select input for y variable if the distribution type is scatterplot or heatmap
  output$yVariable <- renderUI({
    if (input$distType == "Scatterplot" || input$distType == "Heatmap") {
      selectInput("yVar", "Select the y variable :", choices = getColumnNames(data(), input$eventType), selected = "")
    } else {
      return(NULL)
    }
  })
  
  # Initialize data
  rv <- reactiveValues()
  
  # Function to get column names of a dataframe for a given event type
  getColumnNames <- function(df, event_type) {
    if(tolower(event_type) != 'all') {
      filtered <- df %>%
        filter(type == event_type) %>%
        select_if(function(col) !all(col == "null"))
      
    } else {
      filtered <- df %>%
        select_if(function(col) !all(col == "null"))
    }
    
    return(colnames(filtered))
  }
  
  # Function to group events of certain type
  groupEventsByType <- function(df, eventType) {
    filtered_df <- df %>%
      filter(type == eventType)

    new_df <- data.frame(time = filtered_df$time, latitude = round(filtered_df$latitude, 1), longitude = round(filtered_df$longitude, 1))
    duplicate_rows <- new_df[duplicated(new_df) | duplicated(new_df, fromLast = TRUE), ]
    
    grouped_df <- duplicate_rows %>%
      group_by(time, latitude, longitude) %>%
      summarise(mag = "null", place = "null", type = "storm", satellite = "null")
    
    return(rbind(df %>%
                   filter(type != eventType), grouped_df))
  }
  
  # Function to filter events by their type
  filterEventsByType <- function(df, eventType) {
    eventType <- tolower(eventType)
    
    if (eventType != 'other' && eventType != 'all') {
      filtered_df <- df %>%
        filter(type == eventType)
    } else if (eventType == 'other'){
      filtered_df <- df %>%
        filter(tolower(type) != 'fire' && tolower(type) != 'storm' && tolower(type) != 'hailstrom' && type != 'earthquake')
    } else if (eventType == 'all') {
      filtered_df <- df
    }
    
    return(filtered_df)
  }
  
  # Function to get the place name using latitude and longitude
  getPlaceName <- function(latitude, longitude) {
    # Construct the URL for the geocoding API request
    url <- paste0("https://api.opencagedata.com/geocode/v1/json?key=628e9d70692e4ac8974047e3640547c7&q=", latitude, ",", longitude)
    
    response <- GET(url)
    json <- content(response, as = "text")
    data <- fromJSON(json, flatten = TRUE)
    
    if (data$total_results > 0) {
      place_name <- paste0(data$result$components.country,", ", data$results$components.city,", ",data$results$components.road)
      return(place_name)
    } else {
      return("The place is not defined")
    }
  }
  
  # Function to map the data
  addMapMarkers <- function(df, zoom) {
    latitudes <- df$latitude
    longitudes <- df$longitude
    hazard_types <- df$type
    places <- df$place
    event_dates <- df$time
    magnitudes <- df$mag
    satellites <- df$satellite
    
    map <- leaflet() %>% addTiles()
    
    for (i in seq_along(latitudes)) {
      icon <- switch(tolower(hazard_types[i]),
                     "earthquake" = earthquakeIcon,
                     "storm" = stormIcon,
                     "fire" = fireIcon,
                     "hailstorm" = hailstormIcon,
                     defaultIcon
      )
      
      popup_text <- paste(
        "Event date: ", event_dates[i], "<br>",
        "Hazard type: ", toTitleCase(hazard_types[i]), "<br>",
        "Place: ",  ifelse(places[i] == "null", "Place is not defined" , places[i]), "<br>",
        "Longitude: ", longitudes[i], "<br>",
        "Latitude: ", latitudes[i])
      
      map <- map %>% 
        addMarkers(longitudes[i], latitudes[i], icon = icon, popup = popup_text) %>% 
        setView(lng = 44.5, lat = 40.2, zoom = zoom)
    }
    
    if (nrow(df) == 0) {
      map <- map %>% addControl(
        div(class = "message-panel", "No data for this period."), position = "topleft")
    }
    
    return(map)
  }
  
  # Function to format the data frame
  formatData <- function(df) {
    year <- substr(df$time, 1, 4)
    df <- df %>% filter(year >= input$year_range[1] & year <= input$year_range[2])
    
    if(input$type_filter != 'All') {
      filtered_df <- filterEventsByType(df, substr(input$type_filter, 1, nchar(input$type_filter) - 1))
      
      return(filtered_df)
    }
    
    return(df)
  }
  
  # Function to load and process data
  data <- reactive({
    if(is.null(input$file$datapath)) {
      df <- read.csv("default-data.csv")
    } else {
      df <- read.csv(input$file$datapath)
    }
    
    df <- groupEventsByType(df, "lightening")
    # Call to the function "getPlaceName" to get place names for each location (commented out for now as API requests are too many to handle)
    #df$place <- mapply(getPlaceName, df$latitude, df$longitude)
    
    return(df)
  })
  
  
  # Server for the First Tab (1)
  # Render the map
  output$map <- renderLeaflet({
    data <- formatData(data())
    
    addMapMarkers(data, 8) %>%
      addControl(
        tags$div(class='display-year-range',
                 paste("Year Range: from", input$year_range[1], " to ", input$year_range[2])
        ),
        position = "bottomright"
      )
  })
  
  # Render the data table
  output$table <- renderDataTable({
    formatData(data())
  })
  
  selected_date_range <- reactive({
    input$date_range
  })
  
  # Define an observer to download the data as a CSV file when the "Download Data" button is clicked
  observeEvent(input$download_data, {
    write.csv(data(), "data.csv", row.names = FALSE)
  })
  
  # Server for the Second Tab(2)
  all_data <- data.frame()
  
  # Add new event to all_data
  observeEvent(input$save, {
    new_row <- data.frame(
      time = format(input$updated, "%Y-%m-%d"),
      latitude = input$latitude,
      longitude = input$longitude,
      place = input$place,
      type = tolower(input$type),
      stringsAsFactors = FALSE
    )
    if (input$type == "Fire") {
      new_row$satellite = input$satellite
    } else if (input$type == "Earthquake") {
      new_row$mag = input$mag
    }
    
    # Add missing columns with NA values
    missing_columns <- setdiff(colnames(data()), colnames(new_row))
    for (column in missing_columns) {
      new_row[[column]] <- NA
    }
    
    all_data <<- rbind(all_data, new_row)
    
    # Render added events map
    output$addedMap <- renderLeaflet({
      addMapMarkers(all_data, 8)
    })
    
    # Render merged events map
    output$mergedMap <- renderLeaflet({
      if (ncol(all_data) != ncol(data())) {
        showNotification(
          tags$div(class='message-panel', "The number of columns in data frames do not match rendering only added events"),
          type = "warning", duration = 5)
        addMapMarkers(all_data, 7)
      } else {
        addMapMarkers(rbind(all_data, data()), 7)
      }
    })
    
    # Render added events table
    output$inserted <- renderDataTable({
      all_data
    })
    
    # Render merged events table
    output$merged <- renderDataTable({
      if (ncol(all_data) != ncol(data())) {
        rbind(all_data)
      } else {
        rbind(all_data, data())
      }
    })
  })
  
  # Download added events to a separate CSV file
  observeEvent(input$download, {
    write.csv(all_data, "myInsertedEvents.csv", row.names = FALSE)
  })
  
  # Merge created events with the default data and download them
  observeEvent(input$downloadMerged, {
    write.csv(rbind(all_data, data()), "eventsMerged.csv", row.names = FALSE)
  })
  
  
  # Server for Tab 3 (3)
  # Plot for the first tab
  observeEvent(input$graphPlot, {
    varNameX <- input$xVar
    varNameY <- ifelse(input$distType != "Histogram", input$yVar, "null")
    datafr <- subset(data(), data()$time >= selected_date_range()[1] & data()$time <= selected_date_range()[2])
    datafr$time <- as.numeric(substr(datafr$time, 1, 4))
    datafr$mag <- as.numeric(datafr$mag)
    
    output$output_plot <- renderPlot(
      if (input$distType == "Histogram") {
        if (input$eventType != "all") {
          filtered_data <- datafr[datafr$type == input$eventType, varNameX]
        } else {
          filtered_data <- datafr[, varNameX]
        }
        
        if (is.numeric(filtered_data)) {
          ggplot(data = data.frame(x = filtered_data), aes(x = x)) +
            geom_histogram(bins = 50, color = "#F5F5F5", fill = "#4169E1", alpha = 0.8) +
            labs(x = varNameX, y = "Frequency", title = paste("Histogram of", varNameX)) +
            scale_y_continuous(labels = comma_format(big.mark = ",", accuracy = 0.01)) +
            theme(plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
                  axis.title = element_text(size = 16, face = "bold"),
                  axis.text = element_text(size = 14))
        } else {
          barplot(table(filtered_data),
                  main = paste("Barplot of", varNameX),
                  xlab = varNameX,
                  ylab = "Frequency",
                  col = "#00bc8c",
          )
        }
      } else if (input$distType == "Scatterplot") {
        if (!is.na(varNameY)) {
          if (input$eventType != "all") {
            filtered_data <- datafr[datafr$type == input$eventType, c(varNameX, varNameY)]
          } else {
            filtered_data <- datafr[, c(varNameX, varNameY)]
          }
          if (is.numeric(filtered_data[[varNameX]]) && is.numeric(filtered_data[[varNameY]])) {
            plot(filtered_data, xlab = varNameX, ylab = varNameY, main = paste("Scatterplot of ", varNameX, " and ", varNameY, " for ", input$eventType, " data"),
                 cex.lab = 1.5, col.lab = "black", col = "#00bc8c", cex = 1.5)
          } else {
            filtered_data[,1] <- factor(filtered_data[,1])
            filtered_data[,2] <- factor(filtered_data[,2])
            plot(filtered_data, xlab = varNameX, ylab = varNameY, main = paste("Scatterplot of ", varNameX, " and ", varNameY, " for ", input$eventType, " data" ),
                 cex.lab = 1.5, col.lab = "black", col = "#00bc8c", cex = 1.5)
          }
        }
      } 
    )
    
    output$output_plot2 <- renderPlot(
        if (!is.na(varNameY)) {
          if (input$eventType2 != "all") {
            filtered_data <- datafr[datafr$type == input$eventType2, c(varNameX, varNameY)]
          } else {
            filtered_data <- datafr[, c(varNameX, varNameY)]
          }
          if (is.numeric(filtered_data[[varNameX]]) && is.numeric(filtered_data[[varNameY]])) {
            plot(filtered_data, xlab = varNameX, ylab = varNameY, main = paste("Scatterplot of ", varNameX, " and ", varNameY, " for ", input$eventType2, " data" ),
                 cex.lab = 1.5, col.lab = "black", col = "#00bc8c", cex = 1.5)
          } else {
            filtered_data[,1] <- factor(filtered_data[,1])
            filtered_data[,2] <- factor(filtered_data[,2])
            plot(filtered_data, xlab = varNameX, ylab = varNameY, main = paste("Scatterplot of ", varNameX, " and ", varNameY , " for ", input$eventType2, " data"),
                 cex.lab = 1.5, col.lab = "black", col = "#00bc8c", cex = 1.5)
          }
        }
      
    )
    
    #Summary for the plots
    output$summaryPlot <- renderText({
      df <- filterEventsByType(datafr, input$eventType)
      minX <- min(df[[varNameX]])
      maxX <- max(df[[varNameX]])
      minX_location <- paste("(", df$latitude[df[[varNameX]] == minX], ",", df$longitude[df[[varNameX]] == minX], ")")
      maxX_location <- paste("(", df$latitude[df[[varNameX]] == maxX], ",", df$longitude[df[[varNameX]] == maxX], ")")
      firstParagraph <- paste("This ", input$distType, " represents ", input$eventType ," that occurred between ", min(df$time), " and ", max(df$time), " for ", input$eventType , " data.", "\n\n")
      
      if (is.numeric(df[[varNameX]]) && varNameX != 'time') {
        secondParagraph <- paste("The ", varNameX, " range represented in the plot is ", minX, " to ", maxX, "\n",
                                  "The highest ", varNameX, " recorded is ", maxX, "\n",
                                  "The lowest ", varNameX, " recorded is ", minX, "\n",
                                  "The average ", varNameX," is ", mean(df[[varNameX]]), "\n")
      } else {
        secondParagraph <- paste("The plot represents the frequency of ", varNameX, " during mentioned period", "\n\n")
      }
     
      if (varNameY != "null") {
        minY <- min(df[[varNameY]])
        maxY <- max(df[[varNameY]])
        minY_location <- paste("(", df$latitude[df[[varNameY]] == minY], ",", df$longitude[df[[varNameY]] == minY], ")")
        maxY_location <- paste("(", df$latitude[df[[varNameY]] == maxY], ",", df$longitude[df[[varNameY]] == maxY], ")")

        if (is.numeric(df[[varNameY]]) && varNameY != 'time') {
          thirdParagraph <- paste("The ", varNameY, " range represented in the plot is ", minY, " to ", maxY, "\n",
                                   "The highest ", varNameY, " recorded is ", maxY, "\n",
                                   "The lowest ", varNameY, " recorded is ", minY, "\n",
                                   "The average ", varNameY," is ", mean(df[[varNameY]]))
        } else {
          thirdParagraph <- paste("The plot represents the frequency of ", varNameY, " during mentioned time period")
        }
      } else {
        thirdParagraph <- ""
      }
      
      paste(firstParagraph, secondParagraph, thirdParagraph)
    })
    
    output$summaryPlot2 <- renderText({
      df <- filterEventsByType(datafr, input$eventType2)
      minX <- min(df[[varNameX]])
      maxX <- max(df[[varNameX]])
      minX_location <- paste("(", df$latitude[df[[varNameX]] == minX], ",", df$longitude[df[[varNameX]] == minX], ")")
      maxX_location <- paste("(", df$latitude[df[[varNameX]] == maxX], ",", df$longitude[df[[varNameX]] == maxX], ")")
      
      firstParagraph <- paste("This ", input$distType, " represents ", input$eventType2 ,"that occurred between ", min(df$time), " and ", max(df$time), " for ", input$eventType , " data.", "\n\n")
      
      if (is.numeric(df[[varNameX]]) && varNameX != 'time') {
        secondParagraph <- paste("The ", varNameX, " range represented in the plot is ", minX, " to ", maxX, "\n",
                                 "The highest ", varNameX, " recorded is ", maxX, "\n",
                                 "The lowest ", varNameX, " recorded is ", minX, "\n",
                                 "The average ", varNameX," is ", mean(df[[varNameX]]), "\n")
      } else {
        secondParagraph <- paste("The plot represents the frequency of ", varNameX, " during mentioned period", "\n\n")
      }
      
      if (varNameY != "null") {
        minY <- min(df[[varNameY]])
        maxY <- max(df[[varNameY]])
        minY_location <- paste("(", df$latitude[df[[varNameY]] == minY], ",", df$longitude[df[[varNameY]] == minY], ")")
        maxY_location <- paste("(", df$latitude[df[[varNameY]] == maxY], ",", df$longitude[df[[varNameY]] == maxY], ")")
        
        if (is.numeric(df[[varNameY]]) && varNameY != 'time') {
          thirdParagraph <- paste("The ", varNameY, " range represented in the plot is ", minY, " to ", maxY, "\n",
                                  "The highest ", varNameY, " recorded is ", maxY, "\n",
                                  "The lowest ", varNameY, " recorded is ", minY, "\n",
                                  "The average ", varNameY," is ", mean(df[[varNameY]]))
        } else {
          thirdParagraph <- paste("The plot represents the frequency of ", varNameY, " during mentioned time period")
        }
      } else {
        thirdParagraph <- ""
      }
      
      paste(firstParagraph, secondParagraph, thirdParagraph)
    })
  })
  
  # Time Series plot
  output$output_plot_ts <- renderPlot({
    df <- data()
    df$year <- as.numeric(substr(df$time, 1, 4))
    df$month <- as.numeric(substr(df$time, 6, 7))
    df <- df[df$month <= 12, ]
    grouped_data <- df %>% group_by(type, month, year)%>% count()
   
     ggplot(grouped_data, aes(x = as.Date(paste(year, month, "01", sep = "-")), y = n, group = type, color = type)) +
      geom_line() +
      scale_x_date(date_labels = "%b %Y", date_breaks = "6 month")+
      labs(x = "Month", y = "Count", color = "Type") +
      theme_bw()   
  })
  
  # Summary for  Time Series
  output$summaryTimeSeries <- renderText({
    summary <- paste("The time series shows the event distribution based on its time(year, month) and type")
    isolate(summary)
  })
  
  # Heatmap for earthquakes by magnitude
  output$heatMapEarthquake <- renderLeaflet({
    leaflet(filterEventsByType(data(), 'earthquake')) %>%
      addControl(
        tags$div(class = 'heatmap-title', paste("Earthquake magnitude intensity")), 
        position = "topright"
      ) %>%
      addProviderTiles(providers$CartoDB.DarkMatter) %>%
      setView(mean(data()$longitude), lat = mean(data()$latitude), zoom = 7) %>%
      addHeatmap(lng = ~longitude, lat = ~latitude, intensity = ~mag, blur = 10, max = 0.1, radius = 15) %>%
      addLegend(
        position = "bottomright",
        title = "Magnitude Intensity",
        colors = c("blue", "#008000", "#FFFF00", "#FFA500", "#FF0000"),
        labels = c("Low", "Medium Low", "Medium", "High", "Very High"),
        opacity = 1
      )
  })
  
  # Summary for Earthquakes' Heatmap
  output$summaryHeatMap <- renderText({
    df <- filterEventsByType(data(), 'earthquake')
    min <- min(df$mag)
    max <- max(df$mag)
    
    min_location <- paste("(", df$latitude[df$mag == min], ",", df$longitude[df$mag == min], ")")
    max_location <- paste("(", df$latitude[df$mag == max], ",", df$longitude[df$mag == max], ")")

    summary <- paste("The magnitude range represented in the heatmap is ", min, " to ", max, "\n",
                    "The highest magnitude recorded was ", max, " on ", df$time[df$mag == max], " at location ", max_location, "\n",
                    "The average magnitude was ", mean(as.numeric(df$mag)))
    isolate(summary)
  })
}

#run the app
shinyApp(ui = ui, server = server)
