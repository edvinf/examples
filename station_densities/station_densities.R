library(plyr)
library(dplyr)
library(ggplot2)
library(readr)

#example densities from biotic, obtained by example cruise_series_extracion
winter <- readRDS("../cruise_series_extraction/output/Barents Sea NOR-RUS demersal fish cruise in winter.rda")
stations2018 <- winter$raw_data$stationdata[winter$raw_data$stationdata$startyear==2018,]
catches2018 <- winter$raw_data$catchdata[winter$raw_data$catchdata$serialnumber %in% stations2018$serialnumber,]
totalcatches <- aggregate(list(totalcatch=catches2018$catchweight), list(taxa=catches2018$catchcategory, serialnumber=catches2018$serialnumber), FUN=sum, drop=F)
totalcatches <- merge(totalcatches, winter$reference_data$taxa[,c("tsn", "aphiaid", "English", "Norwegian", "Scientific")], by.x="taxa", by.y="tsn", all.x=T)
totalcatches$totalcatch[is.na(totalcatches$totalcatch)]<-0
stationscatches <- merge(totalcatches, stations2018[,c("serialnumber", "longitudestart", "latitudestart", "distance")])
stationscatches$catchrate <- stationscatches$totalcatch/stationscatches$distance

haddockcatches <- stationscatches[stationscatches$taxa=="164744",]
sebastesmantellacatches <-stationscatches[stationscatches$taxa=="166756",]

### Get bathymetry data
bath1<-read_csv("~/bathymetry/bath_files_from_Harald_G/GeoData/ETOPO1_nm2.csv")

### Map area etc

d.contours<-c(-100, -300,-500)   #Depth contours for plot with bathymetry
Bubble.size<-20        #Max size of bubbles in bubble plots
### Slim down the bathimetry xyz file. Makes the graph part run faster
Xlim <- c(10,50)
Ylim <- c(70,80)
bath<-subset(bath1, bath1$y>(Ylim[1]-2) & (bath1$y<Ylim[2]+2) & bath1$ x>(Xlim[1]-2)& bath1$x<(Xlim[2]+2) & bath1$z<=(d.contours[1]+1500) & bath1$z>=(d.contours[length(d.contours)]-1500))

#' Adapted from Harald Gjøsæter
#' Plots a map with spheres proportional to densities at planarized lon-lat locations
#' @param densities vector of densities
#' @param longitudes vector of longitudes corresponding to the densities
#' @param latitidues vector of latitudes corresponding to the densities
#' @param bathymetry data frame with batymetry longitudes in column x, latitudes in column y and negative depth in column z.
#' @param contours vector with contour line specification, given in negative depths.
#' @param density_label explanatory text for densities used in legend
#' @param title title for plot
#' @param bubblesize maximal size for bubbles (max_size in scale_size{ggplot2})
#' @param xlim vector with limits for x-axis, if NULL range of x values in bathymetry will be used, ignoring NAs
#' @param ylim vector with limits for y-axis, if NULL tange of y values in bathymetry will be used, ingnoring NAs
#' @param path logical if T a path is drawn on the map between points in the order they appear in the vectors longitudes an latitudes
#' @param projection projection of map, passed to coord_map, if NULL naive planarization is used.
plot_station_bubblemap <- function(densities, longitudes, latitudes, bathymetry=bath, contours=d.contours, density_label="density", title="", bubblesize=Bubble.size, xlim=NULL, ylim=NULL, path=F, projection=NULL){
  if (is.null(ylim)){
    ylim=c(min(bathymetry$y, na.rm=T), max(bathymetry$y, na.rm=T))
  }
  if (is.null(xlim)){
    xlim=c(min(bathymetry$x, na.rm=T), max(bathymetry$x, na.rm=T))
  }
  bubbles <- ggplot(bathymetry, aes(x=x, y=y))+
    geom_contour(aes(z=z), breaks=contours,colour="lightblue", size=0.5,show.legend = TRUE)+ 
    geom_contour(aes(z=z), breaks=c(0,1),color="darkgrey", size=.6) +
    geom_point(data=data.frame(longitudes=longitudes, latitudes=latitudes, densities=densities), aes(longitudes,latitudes,size = densities),shape=21, alpha = 0.3, colour = "black",fill="orange",stroke = .2)
  if (path){
    bubbles <- bubbles + geom_path(data=data.frame(longitudes=longitudes, latitudes=latitudes, densities=densities),aes(longitudes,latitudes), linejoin="round",lineend="square",alpha=0.4,colour="black",size=0.4)
  }
  bubbles <- bubbles +  
    scale_size_area(max_size=Bubble.size)+
    coord_cartesian(xlim = xlim,ylim = ylim)+
    labs(x = NULL, y = NULL, size = density_label, title=title)+
    theme_bw()
  bubbles <- bubbles +
    geom_point(data=data.frame(longitudes=longitudes, latitudes=latitudes, densities=densities), aes(longitudes,latitudes, size=.1), shape=1, alpha = 1)
  if (!is.null(projection)){
    bubbles <- bubbles + coord_map(projection)
  }
  plot(bubbles)
}

plot_station_bubblemap(haddockcatches$catchrate, haddockcatches$longitudestart, haddockcatches$latitudestart, projection = "azequalarea")


#
# playing with bath vis
#
bath_plot <- function(){
  warning("consider scales::col_numeric for mapping land and sea on different scales")
  ggplot()+ 
    geom_raster(aes(fill=bath$z[bath$z<0 & bath$z>-500], x=bath$x[bath$z<0 & bath$z>-500], y=bath$y[bath$z<0 & bath$z>-500])) + 
    labs(x = NULL, y = NULL, fill="- depth")+
    geom_contour(aes(x=bath$x, y=bath$y, z=bath$z), data=bath, breaks=seq(0,-500, -100), colour="grey", size=0.1) + 
    theme_bw()  
}
bath_plot()
  
  