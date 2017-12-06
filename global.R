
# topoData <- readLines("data/V3/localities.geojson")
# topoData.df <- as.data.frame(fromJSON(topoData))
topoData.df <- as.data.frame(fromJSON("data/V3/localities.geojson"))
waarnemingen <- readRDS("data/V3/occurrence.rds")
waarnemingen <-  waarnemingen[waarnemingen$municipality == "Kalmthout" |
                                waarnemingen$municipality == "Brecht" |
                                waarnemingen$municipality == "Schoten",]
# waarnemingen <-  waarnemingen[waarnemingen$verbatimLocality == "De Moerkens KALMTHOUT" |
#                  waarnemingen$verbatimLocality == "Stappersven KALMTHOUT" |
#                  waarnemingen$verbatimLocality == "Drielingenven KALMTHOUT"  |
#                  waarnemingen$verbatimLocality == "Biezenkuilen KALMTHOUT" , ]

date_split <-
  colsplit(waarnemingen$eventDate,
           split = "-",
           names = c('jaar', 'maand', 'tmp'))
day_split <-
  colsplit(date_split$tmp,
           split = "T",
           names = c('dag', 'tijd'))
jaar <- as.character(date_split$jaar)
maand <- as.character(date_split$maand)
dag <- as.character(day_split$dag)
tijd <- as.character(day_split$tijd)
waarnemingen <- cbind(waarnemingen, jaar, maand, dag, tijd)



# we sorteren hier Descending, zodat de kleinste bollen bovenaan liggen
waarnemingen <- waarnemingen[order(-waarnemingen$individualCount),]

waarnemers <-  read.csv(file = "data/alle-waarnemers.csv", header = TRUE)


