library(shiny)
library(leaflet)
library(RColorBrewer)
library(ggplot2)
library(shinyjs)



# zie https://rstudio.github.io/leaflet/shiny.html
# klikken op een bol en dan plot: http://www.r-graph-gallery.com/2017/03/14/4-tricks-for-working-with-r-leaflet-and-shiny/

waarnemingen <- readRDS("data/occurrence.rds")
# we sorteren hier Descending, zodat de kleinste bollen bovenaan liggen
waarnemingen <-waarnemingen[order(-waarnemingen$individualCount),]

# ==== fonction allowing geolocalisation
jsCode <- '
shinyjs.geoloc = function() {
    navigator.geolocation.getCurrentPosition(onSuccess, onError);
    function onError (err) {
        Shiny.onInputChange("geolocation", false);
    }
    function onSuccess (position) {
        setTimeout(function () {
            var coords = position.coords;
            console.log(coords.latitude + ", " + coords.longitude);
            Shiny.onInputChange("geolocation", true);
            Shiny.onInputChange("lat", coords.latitude);
            Shiny.onInputChange("long", coords.longitude);
        }, 5)
    }
};'

ui <- bootstrapPage(
  # Tell shiny we will use some Javascript
  useShinyjs(),
  extendShinyjs(text = jsCode, functions = c("pageCol")),
  
  # One button and one map
 # br(),
  #actionButton("geoloc", "Localize me", class="btn btn-primary", onClick="shinyjs.geoloc()"),

  
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
       
               sliderInput("tijdstip", "tijdstip", min(as.Date(waarnemingen$eventDate)), max(as.Date(waarnemingen$eventDate)), value = range(as.Date(waarnemingen$eventDate))
                ),

                selectInput("soort", "Kies de soort", choices=sort(unique(waarnemingen$vernacularName) )),
                selectInput("colors", "Kleurenschema",
                            rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
                ),
               plotOutput("plot", height="300px"),
                checkboxInput("legend", "Show legend", TRUE)
               
  )
)

server <- function(input, output, session) {
  
  data_of_click <- reactiveValues(clickedMarker=NULL)
  
  # Reactive expression for the data subsetted to what the user selected

  filteredwaarnemingen <- reactive({
    waarnemingen[as.Date(waarnemingen$eventDate) >= as.Date(input$tijdstip[1]) & as.Date(waarnemingen$eventDate) <= as.Date(input$tijdstip[2]) & waarnemingen$vernacularName== input$soort,]
    #waarnemingen[waarnemingen$vernacularName== input$soort,]
  })
  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
  colorpal <- reactive({
   # colorNumeric(input$colors, adreslocaties$Aantal.artsen)
    colorNumeric(input$colors, waarnemingen$individualCount)
  })
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet() %>% 
      #addTiles() %>% 
      #addProviderTiles(providers$Stamen.TonerLite ) %>%
      addTiles(        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'      ) %>%
      setView(lng = 4.468, lat = 51.052, zoom = 9)
  })
  
  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  observe({
    pal <- colorpal()
      leafletProxy("map", data = filteredwaarnemingen()) %>%
      clearShapes() %>%
        #addCircles(radius = ~Aantal.artsen * 200, weight = 1, color = "#777777", fillColor = ~pal(Aantal.artsen),                    fillOpacity = 0.7, popup = ~paste(Praktijk,"<br/>",Straat,Nummer,"<br/>",Postcode,Plaatsnaam))%>%
         addCircles(radius = ~individualCount * 2, weight = 1, lng = ~decimalLongitude, lat = ~decimalLatitude, color = "#777777", fillColor = ~pal(individualCount), 
                    fillOpacity = 0.7, popup = ~paste(verbatimLocality, municipality, stateProvince,"<br/>", "aantal", vernacularName, individualCount,"<br/>", "datum", eventDate, "<br/>", "waarnemer(s)", 
                                                      identifiedBy ,"<br/>",samplingProtocol,"<br/>",samplingEffort, onClick="shinyjs.geoloc()")

    )
  })
  
  ################## Plot on click
  # store the click
  observeEvent(input$map_marker_click,{
    data_of_click$clickedMarker <- input$map_marker_click
  })
  
  # Find geolocalisation coordinates when user clicks
  observeEvent(input$geoloc, {
    js$geoloc()
  })
  # Make a barplot or scatterplot depending of the selected point
  output$plot=renderPlot({
    my_place=data_of_click$clickedMarker$id
    if(is.null(my_place)){my_place="place1"}
    if(my_place=="place1"){
      plot(rnorm(1000), col=rgb(0.9,0.4,0.1,0.3), cex=3, pch=20)
    }else{
      barplot(rnorm(10), col=rgb(0.1,0.4,0.9,0.3))
    }    
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
                          pal = pal, values = ~individualCount
      ) 
    }
  })
}

shinyApp(ui, server)