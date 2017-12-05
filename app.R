library(shiny)
library(leaflet)
library(RColorBrewer)
library(ggplot2)
library(sp)
require(reshape)
library(jsonlite)
source('global.R', local = TRUE)



#
# zie https://rstudio.github.io/leaflet/shiny.html
# klikken op een bol en dan plot: http://www.r-graph-gallery.com/2017/03/14/4-tricks-for-working-with-r-leaflet-and-shiny/
#

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(
    top = 10,
    right = 10,
    sliderInput(
      "tijdstip",
      "tijdstip",
      min(as.Date(waarnemingen$eventDate)),
      max(as.Date(waarnemingen$eventDate)),
      value = range(as.Date(waarnemingen$eventDate))
    ),
    checkboxInput("natuurgebieden", "Toon kaartlaag met natuurgebieden", TRUE),
    plotOutput("plot2"),
    selectInput("soort", "Kies de soort", choices = sort(unique(
      waarnemingen$vernacularName
    ))),
    selectInput("waarnemer", "kies de waarnemer", choices =
                  waarnemers),
    checkboxInput("legend", "Show legend", TRUE),
    
    textOutput("message", container = h3)
  )
)

server <- function(input, output, session) {
  source('functions.R', local = TRUE)
 
  waarnemingen_in <- reactiveValues(clickedGeojson = NULL)
  data_of_click <- reactiveValues(clickedMarker = NULL)
  
 
  
  
  # Reactive expression for the data subsetted to what the user selected
  
  filteredwaarnemingen <- reactive({
    if (input$waarnemer == "Alle waarnemers") {
      waarnemingen[as.Date(waarnemingen$eventDate) >= as.Date(input$tijdstip[1]) &
                     as.Date(waarnemingen$eventDate) <= as.Date(input$tijdstip[2]) &
                     waarnemingen$vernacularName == input$soort,]
    } else {
      waarnemingen[as.Date(waarnemingen$eventDate) >= as.Date(input$tijdstip[1]) &
                     as.Date(waarnemingen$eventDate) <= as.Date(input$tijdstip[2]) &
                     waarnemingen$vernacularName == input$soort &
                     grepl(input$waarnemer, waarnemingen$identifiedBy),]
    }
  })
  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
  colorpal <- reactive({
    colorNumeric("YlGnBu", waarnemingen[waarnemingen$vernacularName == input$soort,]$individualCount)
  })
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    
    leaflet() %>%
      addTiles(attribution = 'Data <a href="http://dataset.inbo.be/watervogels-occurrences">http://dataset.inbo.be/watervogels-occurrences</a>') %>%
      #addGeoJSON(topoData,  weight = 1,   color = "#444444", fill = TRUE) %>%
      setView(lng = 4.5809,
              lat = 51.3535,
              zoom = 12)
  })
  
  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  observe({
    pal <- colorpal()
    leafletProxy("map", data = filteredwaarnemingen()) %>%
      clearShapes() %>%
      clearMarkerClusters() %>%
      clearMarkers() %>%
      addCircleMarkers( radius = ~ log(individualCount + 1) * 10 ,  weight = 1,    layerId =  ~ id,    lng = ~ decimalLongitude,  lat = ~ decimalLatitude,  color = "#777777",  fillColor = ~ pal(individualCount),    fillOpacity = 0.7,
        popup = ~ paste(verbatimLocality,municipality,stateProvince,"<br/>","aantal",vernacularName,individualCount,"<br/>","datum",eventDate,"<br/>","waarnemer(s)",  identifiedBy , "<br/>",samplingProtocol, "<br/>",samplingEffort),
        clusterOptions = markerClusterOptions()
      )
    #addWebGLHeatmap(lng = ~decimalLongitude, lat = ~decimalLatitude, intensity = ~log(individualCount+1)/100,size = 10000 )
    
  })
  
  

  
  
  plot_waarnemingen <- function(wnm,locatie){
    
    #http://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html
    # By default, geom_bar() has the stat set to count.
    # That means, when you provide just a continuous X variable (and no Y variable), it tries to make a histogram out of the data.
    # In order to make a bar chart create bars instead of histogram, you need to do two things.
    #     1. Set stat=identity
    #     2. Provide both x and y inside aes() where, x is either character or factor and y is numeric.
    p <- ggplot(data = wnm, aes(x = jaar, y = individualCount)) + theme(axis.text.x = element_text(angle = 60, hjust = 1))
    
    # add geom
    p <- p + geom_bar(stat = 'identity', width = .5)
    
    # update data layer with new mapping
    p <- p %+% aes(fill = maand)
    
    # titel en subtitel
    p <-  p %+% labs(subtitle = locatie,  title = input$soort)
    return(p)
  }
  
  ################## Plot on click
  # store the click
  observeEvent(input$map_marker_click, {
    data_of_click$clickedMarker <- input$map_marker_click
    #output$message <- renderText(paste(waarnemingen_in$clickedGeojson$decimalLatitude[1],data_of_click$clickedMarker$id))
    output$plot2 <- renderPlot({
      my_waarnemingen <-  subset(waarnemingen, waarnemingen$id == data_of_click$clickedMarker$id)
      #my_waarnemingen <-  subset(waarnemingen, waarnemingen$id == data_of_click$clickedMarker$id)
      my_lat <- head(my_waarnemingen$decimalLatitude, 1)
      my_lon <- head(my_waarnemingen$decimalLongitude, 1)
      my_filteredwaarnemingen <-
        waarnemingen[as.Date(waarnemingen$eventDate) >= as.Date(input$tijdstip[1]) &
                       as.Date(waarnemingen$eventDate) <= as.Date(input$tijdstip[2]) &
                       waarnemingen$decimalLongitude == my_lon &
                       waarnemingen$decimalLatitude == my_lat &
                       waarnemingen$vernacularName == input$soort,]
      
      my_location <-paste(
        my_filteredwaarnemingen$verbatimLocality[1],
        my_filteredwaarnemingen$municipality[1]
      )
      p <- plot_waarnemingen(my_filteredwaarnemingen,my_location) 
      p
      
    })
  })
  observeEvent(input$map_geojson_click, {
    topodata_selected_object <- subset(topoData.df, topoData.df$features.properties$code == input$map_geojson_click$properties$code)
    polygon_matrix <- matrix(unlist(topodata_selected_object$features.geometry$coordinates),ncol = 2)
    waarnemingen_in$clickedGeojson  <- subset(waarnemingen, sp::point.in.polygon(c(waarnemingen$decimalLatitude),c(waarnemingen$decimalLongitude), polygon_matrix[,2], polygon_matrix[,1]) == 1 )
    output$plot2 <- renderPlot({
      my_waarnemingen <- waarnemingen_in$clickedGeojson
      #my_waarnemingen <-  subset(waarnemingen, waarnemingen$id == data_of_click$clickedMarker$id)
      my_lat <- head(my_waarnemingen$decimalLatitude, 1)
      my_lon <- head(my_waarnemingen$decimalLongitude, 1)
      my_filteredwaarnemingen <-
        waarnemingen[as.Date(waarnemingen$eventDate) >= as.Date(input$tijdstip[1]) &
                       as.Date(waarnemingen$eventDate) <= as.Date(input$tijdstip[2]) &
                       waarnemingen$decimalLongitude == my_lon &
                       waarnemingen$decimalLatitude == my_lat &
                       waarnemingen$vernacularName == input$soort,]
      
      my_location <-paste(
        my_filteredwaarnemingen$verbatimLocality[1],
        my_filteredwaarnemingen$municipality[1]
      )
      
      
      p <- plot_waarnemingen(my_filteredwaarnemingen,my_location) 
      p
      
    })
    
  })
 

  # Use a separate observer to add geojson objects.
  observe({
    proxy <- leafletProxy("map", data = waarnemingen)
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearGeoJSON()
    if (input$natuurgebieden) {
      proxy %>% 
        addGeoJSON(topoData,  weight = 1,   color = "#444444", fill = TRUE) 
    }
  })
  
 

  # Use a separate observer to recreate the legend as needed.
  observe({
    proxy <- leafletProxy("map", data = waarnemingen)
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
    if (input$legend) {
      pal <- colorpal()
      proxy %>% addLegend(
        position = "bottomright",
        pal = pal,
        values = waarnemingen[waarnemingen$vernacularName == input$soort,]$individualCount
      )
    }
  })
}

shinyApp(ui, server)