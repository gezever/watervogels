library(sp)
library(geojsonR)


#Dit is een stuk code om de functie sp::point.in.polygon te testen

setwd("/home/gehau/git/watervogels")
topoData <- readLines("data/V3/localities.geojson")
topoData.df <- as.data.frame(fromJSON(topoData))
waarnemingen <- readRDS("data/V3/occurrence.rds")
waarnemingen <-  waarnemingen[waarnemingen$municipality == "Kalmthout" |
                                waarnemingen$municipality == "Brecht" |
                                waarnemingen$municipality == "Schoten",]

topodata_selected_object <- subset(topoData.df, topoData.df$features.properties$code == 3150101)
#identiek
#topodata_selected_object <- topoData.df[topoData.df$features.properties$code == 4122002, ]
m <- unlist(topodata_selected_object$features.geometry$coordinates)

matrix <- matrix(topodata_selected_object$features.geometry$coordinates,ncol = 2)
matrix <- matrix(topodata_selected_object$features.geometry$coordinates[[1]][[1]][[1]],ncol = 2)

sp::point.in.polygon(c(51.407368),c(4.4442576), matrix[,2], matrix[,1]) == 1 

test6 <- subset(waarnemingen, sp::point.in.polygon(c(waarnemingen$decimalLatitude),c(waarnemingen$decimalLongitude), matrix[,2], matrix[,1]) == 1 )

test6 <- waarnemingen[sp::point.in.polygon(c(waarnemingen$decimalLatitude),c(waarnemingen$decimalLongitude), matrix[,2], matrix[,1]) == 1 ,]

test7 <- waarnemingen[sp::point.in.polygon(c(51.407368),c(4.4442576), matrix[,2], matrix[,1]) == 1 ,]


file_js <- FROM_GeoJson(url_file_string = "data/V3/localities.geojson")

a <- file_js[file_js$features$properties$code == 3150101,]
       