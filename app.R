library(shiny)
library(leaflet)
library(RColorBrewer)
library(ggplot2)
library(plotly)



# zie https://rstudio.github.io/leaflet/shiny.html
# klikken op een bol en dan plot: http://www.r-graph-gallery.com/2017/03/14/4-tricks-for-working-with-r-leaflet-and-shiny/

#waarnemingen <- readRDS("data/occurrence.rds")
waarnemingen <- readRDS("data/V3/occurrence.rds")
# we sorteren hier Descending, zodat de kleinste bollen bovenaan liggen
waarnemingen <-waarnemingen[waarnemingen$municipality == "Kalmthout" | waarnemingen$municipality == "Brecht" | waarnemingen$municipality == "Schoten",]
#waarnemingen <-waarnemingen[waarnemingen$verbatimLocality == "De Moerkens KALMTHOUT" | waarnemingen$verbatimLocality == "Stappersven KALMTHOUT" | waarnemingen$verbatimLocality == "Drielingenven KALMTHOUT"  | waarnemingen$verbatimLocality == "Biezenkuilen KALMTHOUT" ,]
waarnemingen <-waarnemingen[order(-waarnemingen$individualCount),]

# waarnemers <- waarnemingen$identifiedBy
# saveRDS(file = "data/waarnemers.rds", waarnemers)
# write.csv(file = "data/waarnemers.csv", waarnemers)
waarnemers <- read.csv(file = "data/waarnemers_kalmthout.csv", header = TRUE)

ui <- bootstrapPage(

  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
       
               sliderInput("tijdstip", "tijdstip", min(as.Date(waarnemingen$eventDate)), max(as.Date(waarnemingen$eventDate)), value = range(as.Date(waarnemingen$eventDate))
                ),

               
              # plotOutput("plot", height="300px"),
               plotlyOutput("plot"),
              selectInput("soort", "Kies de soort", choices=sort(unique(waarnemingen$vernacularName) )),
              #selectInput("waarnemer", "kies de waarnemer", choices=sort(unique(waarnemingen$identifiedBy) )),
              selectInput("waarnemer", "kies de waarnemer", choices=waarnemers ),
              #selectInput("colors", "Kleurenschema",  rownames(subset(brewer.pal.info, category %in% c("seq", "div")))  ),
                checkboxInput("legend", "Show legend", TRUE),
              textOutput("message", container = h3)
               
  )
)

server <- function(input, output, session) {
  
  
  v <- reactiveValues(msg = "")
  
  data_of_click <- reactiveValues(clickedMarker=NULL)
  
  # Reactive expression for the data subsetted to what the user selected

  filteredwaarnemingen <- reactive({
    if (input$waarnemer == "Alle waarnemers") {
      waarnemingen[as.Date(waarnemingen$eventDate) >= as.Date(input$tijdstip[1]) & as.Date(waarnemingen$eventDate) <= as.Date(input$tijdstip[2]) & waarnemingen$vernacularName== input$soort,]
    } else {
      waarnemingen[as.Date(waarnemingen$eventDate) >= as.Date(input$tijdstip[1]) & as.Date(waarnemingen$eventDate) <= as.Date(input$tijdstip[2]) & waarnemingen$vernacularName== input$soort & grepl(input$waarnemer,waarnemingen$identifiedBy),]
    }

   
    #waarnemingen[waarnemingen$vernacularName== input$soort,]
  })
  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
  colorpal <- reactive({
   # colorNumeric(input$colors, adreslocaties$Aantal.artsen)
    #colorNumeric(input$colors, waarnemingen[waarnemingen$vernacularName== input$soort,]$individualCount)
    colorNumeric("YlGnBu", waarnemingen[waarnemingen$vernacularName== input$soort,]$individualCount)
  })
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    topoData <- readLines("data/V3/localities.geojson") 

    leaflet() %>% 
      addTiles(attribution = 'Data <a href="http://dataset.inbo.be/watervogels-occurrences">http://dataset.inbo.be/watervogels-occurrences</a>'  ) %>% 
      #addProviderTiles(providers$Stamen.TonerLite ) %>%
      
      addGeoJSON(topoData, weight = 1, color = "#444444", fill = TRUE) %>%
      #addTiles(        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'      ) %>%
     setView(lng = 4.5809, lat = 51.3535, zoom = 12)
    #setView(lng = 4.433277, lat = 51.408484, zoom = 14)
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
        
        #addCircles(radius = ~Aantal.artsen * 200, weight = 1, color = "#777777", fillColor = ~pal(Aantal.artsen),                    fillOpacity = 0.7, popup = ~paste(Praktijk,"<br/>",Straat,Nummer,"<br/>",Postcode,Plaatsnaam))%>%
        addCircleMarkers( radius = ~log(individualCount+1)*10 , weight = 1, layerId=~id, lng = ~decimalLongitude, lat = ~decimalLatitude, color = "#777777", fillColor = ~pal(individualCount),  fillOpacity = 0.7, popup = ~paste(verbatimLocality, municipality, stateProvince,"<br/>", "aantal", vernacularName, individualCount,"<br/>", "datum", eventDate, "<br/>", "waarnemer(s)",  identifiedBy ,"<br/>",samplingProtocol,"<br/>",samplingEffort), clusterOptions = markerClusterOptions(freezeAtZoom = 16) ) 
       #addCircleMarkers(radius = ~individualCount * 2, weight = 1, layerId=~id, lng = ~decimalLongitude, lat = ~decimalLatitude, color = "#777777", fillColor = ~pal(individualCount),   fillOpacity = 0.7, popup = ~paste(verbatimLocality, municipality, stateProvince,"<br/>", "aantal", vernacularName, individualCount,"<br/>", "datum", eventDate, "<br/>", "waarnemer(s)",  identifiedBy ,"<br/>",samplingProtocol,"<br/>",samplingEffort))
     # addCircles(lng = ~decimalLongitude, lat = ~decimalLatitude,  layerId=~id, radius = ~individualCount * 3 ,  color = ~pal(individualCount), fillColor = ~pal(individualCount),   fillOpacity = 0.7, popup = ~paste(verbatimLocality, municipality, stateProvince,"<br/>", "aantal", vernacularName, individualCount,"<br/>", "datum", eventDate, "<br/>", "waarnemer(s)",  identifiedBy ,"<br/>",samplingProtocol,"<br/>",samplingEffort))
      
      })
  
  ################## Plot on click
  # store the click
  observeEvent(input$map_marker_click,{
    data_of_click$clickedMarker <- input$map_marker_click
  })
  
  observeEvent(input$map_geojson_click, {
    v$msg <- paste("Clicked on", input$map_geojson_click)
  })

  output$message <- renderText(v$msg)
  
  
  # Make a barplot or scatterplot depending of the selected point
  output$plot=renderPlotly({
  #output$plot=renderPlot({
    my_waarnemingen <- subset(waarnemingen, waarnemingen$id == data_of_click$clickedMarker$id )
    my_lat <- head(my_waarnemingen$decimalLatitude,1)
    my_lon <- head(my_waarnemingen$decimalLongitude,1)

   
    
    my_filteredwaarnemingen <- waarnemingen[as.Date(waarnemingen$eventDate) >= as.Date(input$tijdstip[1]) & as.Date(waarnemingen$eventDate) <= as.Date(input$tijdstip[2]) & waarnemingen$decimalLongitude==my_lon & waarnemingen$decimalLatitude==my_lat & waarnemingen$vernacularName== input$soort,]
    my_location <- my_filteredwaarnemingen$verbatimLocality[1]
    # aantal_vogels <- my_filteredwaarnemingen$individualCount
    # datums <- strptime(as.character(as.Date(my_filteredwaarnemingen$eventDate)),"%y/%m/%d")
    # tellingen <- data.frame(date=datums,aantal_vogels)
    # daterange=c(as.POSIXlt(min(tellingen$date)),as.POSIXlt(max(tellingen$date)))
    

    #ggplot(data = my_filteredwaarnemingen, aes(x = eventDate, y = individualCount)) +      geom_bar(stat="identity") + geom_smooth(method = "lm")
 
    # plot(tellingen,aantal_vogels,xaxt="n",ylab="KWH/day")          #don't plot the x axis
    # axis.POSIXct(1, at=seq(daterange[1], daterange[2], by="month"), format="%b") #label the x axis by months
   plot_ly(my_filteredwaarnemingen, x = ~eventDate) %>%    
     #axis.POSIXct(1, at=seq(daterange[1], daterange[2], by="month"), format="%b") %>%  
     add_markers(y = ~individualCount)  %>%

     layout(title = paste(my_location, "\n" , input$tijdstip[1], "-", input$tijdstip[2]),
             paper_bgcolor='rgb(255,255,255)', plot_bgcolor='rgb(229,229,229)',
             xaxis = list(title = "Datum",
                          gridcolor = 'rgb(255,255,255)',
                          showgrid = TRUE,
                          showline = FALSE,
                          showticklabels = TRUE,
                          tickcolor = 'rgb(127,127,127)',
                          ticks = 'outside',
                          zeroline = FALSE),
             yaxis = list(title = paste("Aantal ",input$soort),
                          gridcolor = 'rgb(255,255,255)',
                          showgrid = TRUE,
                          showline = FALSE,
                          showticklabels = TRUE,
                          tickcolor = 'rgb(127,127,127)',
                          ticks = 'outside',
                          zeroline = FALSE))
   
    #plot_ly(my_filteredwaarnemingen, x = ~eventDate, y = ~individualCount, mode = "markers")# %>%     add_trace(data = my_filteredwaarnemingen, x = ~eventDate, y = fitted(fit), mode = "lines")
 


 
    #ggplot(data=waarnemingen, aes(x=eventDate, y=individualCount, group=vernacularName, fill=vernacularName, color=vernacularName)) + stat_summary(fun.y = sum, na.rm=TRUE, geom='line')   
  })

  
  

  
  # Use a separate observer to recreate the legend as needed.
  observe({

    proxy <- leafletProxy("map", data = waarnemingen)
    
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
    if (input$legend) {
      pal <- colorpal()
      proxy %>% addLegend(position = "bottomright",
                          pal = pal, values = waarnemingen[waarnemingen$vernacularName== input$soort,]$individualCount
                         # pal = pal, values = ~individualCount
      ) 
    }
  })
}

shinyApp(ui, server)