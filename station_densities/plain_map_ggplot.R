library(ggplot2)
library(maps)

#' custom projected map using ggplot
plot_map <- function(minlong=0, maxlong=60, minlat=68, maxlat=85, projection="azequalarea"){

  selection_map <- map_data("world2")
  map <- ggplot(selection_map) +
    geom_polygon( aes(x = long, y = lat, group = group), fill="black", colour = "white") + 
    coord_map(projection, xlim=c(minlong,maxlong), ylim=c(minlat, maxlat))+
    guides(fill=FALSE)
  plot(map)
}
plot_map()