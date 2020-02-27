# Load the packages
library(jsonlite)
library(shiny)
library(leaflet)
library(RColorBrewer)

# Create a function to read the json file based on the Timeframe
grabData <- function(times){
  
  # Load the json file
  url.LastHr <- "https://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/all_hour.geojson"
  url.LastDy <- "https://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/4.5_day.geojson"
  url.LastWk <- "https://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/4.5_week.geojson"
  url.LastMn <- "https://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/4.5_month.geojson"
  
  # Determine which json file the program reads
  if(times=="Past Hour"){
    url <- url.LastHr
  } else if(times=="Past Day"){
    url <- url.LastDy
  } else if(times=="Past Week"){
    url <- url.LastWk
  } else {
    url <- url.LastMn
  }
  
  # Read the JSON file
  quakeData <- jsonlite::fromJSON(url, flatten = TRUE)
  
  size <- dim(quakeData$features)[1]
  
  # Create the quakeRecord dataframe
  quakeRecord = data.frame()
  
  # Create a loop to read lattitude, longitude, depth and magnitude of earthquakes
  for (i in rep(1:size)){
    tempcoords <- quakeData$features$geometry.coordinates[[i]]
    record <- data.frame(tempcoords[2], tempcoords[1], tempcoords[3], quakeData$features$properties.mag[i])
    quakeRecord <- rbind(quakeRecord, record)
    #print(record)
  }
  
  colnames(quakeRecord) <- c("lat", "long", "depth", "mag")
  print(size)
  
  # Assign all values to quakes
  quakes <- quakeRecord
  quakes
}

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                sliderInput("range", "Magnitudes", 0, 10, value=c(0.0,10.0), step = 0.1),
                # create a drop-down menu called Timeframe that has 4 selections
                selectInput("time", "Timeframe", 
                            choices=c("Past Hour", "Past Day", "Past Week", "Past 30 Days"), 
                            selected = "Past 30 Days"), # set the default to Past 30 Days
                selectInput("colors", "Color Scheme",
                            rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
                ),
                checkboxInput("legend", "Show legend", TRUE)
  )
)

server <- function(input, output, session) {
  
  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    quakes<-grabData(input$time)    # call the function to read values based on time
    quakes[quakes$mag >= input$range[1] & quakes$mag <= input$range[2],]
  })
  
  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
  colorpal <- reactive({
    colorNumeric(input$colors, quakes$mag)
  })
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(filteredData()) %>% addTiles() %>%
      fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat))
  })
  
  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  observe({
    pal <- colorpal()
    leafletProxy("map", data = filteredData()) %>%
      clearShapes() %>%
      addCircles(radius = ~10^mag/10, weight = 1, color = "#777777",
                 fillColor = ~pal(mag), fillOpacity = 0.7, popup = ~paste(mag)
      )
  })
  
  # Use a separate observer to recreate the legend as needed.
  observe({
    proxy <- leafletProxy("map", data = filteredData())
    
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
    if (input$legend) {
      pal <- colorpal()
      proxy %>% addLegend(position = "bottomright",
                          pal = pal, values = ~mag
      )
    }
  })
}

shinyApp(ui, server)










