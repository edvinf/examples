library(rworldxtra)
library(mapplots)

#' Mercator projected map using mappplots
plot_map <- function(minlong=0, maxlong=60, minlat=70, maxlat=83, projection="mercator"){
  data(countriesHigh)
  map <- countriesHigh

  basemap(
    xlim = c(minlong, maxlong),
    ylim = c(minlat, maxlat),
    bg = "lightblue"
  )
  plot(map, add=T, col="grey")
    
}
plot_map()