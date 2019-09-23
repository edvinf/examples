library(rgdal)
library(Rstox)

#plygons from from "//ces.imr.no/nmdstorage/GIS/eksterneData/Fiskeridirektoratet/FDIR_Statistikk_lokasjoner"
lokasjonsdata <- rgdal::readOGR("~/shapefiles/fdir/NMD_annotated/FDIR_Statistikk_lokasjoner","FDIR_Statistikk_lokasjoner", stringsAsFactors = F)
#check projection +proj=longlat +datum=WGS84 matches data
print(proj4string(lokasjonsdata))

#example stationdata
y2018 <- Rstox::readXMLfiles(files = list(biotic=c("~/bioticsets/v3/biotic_year_2018.xml")))


#
# Example for annotating area codes based on polygon files
#
catches_w_cod <- y2018$ReadBioticXML_BioticData_catchsample.txt[y2018$ReadBioticXML_BioticData_catchsample.txt$catchcategory=="164712",]
stations_w_cod <- y2018$ReadBioticXML_BioticData_fishstation.txt[y2018$ReadBioticXML_BioticData_fishstation.txt$serialnumber %in% catches_w_cod$serialnumber,]

# area and location coding is inconsistently recorded in our data, code "00" to "09" are sometimes recored as e.g. "9", and sometimes as "09"
# convert to integer for consistent handling
stations_w_cod$location <- as.integer(stations_w_cod$location)

#remove recordings in other area coding systems
stations_w_cod[!is.na(stations_w_cod$system) & stations_w_cod$system!=2,"area"]<-NA
stations_w_cod[!is.na(stations_w_cod$system) & stations_w_cod$system!=2,"location"]<-NA
stations_w_cod[!is.na(stations_w_cod$system) & stations_w_cod$system!=2,"system"]<-NA

#use startpos, if missing use endpos
stations_w_cod$LAT <- stations_w_cod$latitudestart
stations_w_cod$LON <- stations_w_cod$longitudestart
missingpos <- is.na(stations_w_cod$LAT) & is.na(stations_w_cod$LON)
stations_w_cod[missingpos, "LAT"] <- stations_w_cod[missingpos, "latitudeend"]
stations_w_cod[missingpos, "LON"] <- stations_w_cod[missingpos, "longitudeend"]

#stations w area and location, these are fine already, even if positions are missing
stations_w_cod_w_lok <- stations_w_cod[!is.na(stations_w_cod$system) & stations_w_cod$system=="2" & !is.na(stations_w_cod$location) & !is.na(stations_w_cod$area),]
#stations missing location, that has coordinates for station, these we can fix
stations_w_cod_wo_lok <- stations_w_cod[(is.na(stations_w_cod$system) | stations_w_cod$system!="2" | is.na(stations_w_cod$area) | is.na(stations_w_cod$location)) & !is.na(stations_w_cod$LAT) & !is.na(stations_w_cod$LON),]
#stations missing location, that does not have coordinates for station, these will be excluded from final stations
stations_w_cod_wo_lok_wo_pos <- stations_w_cod[(is.na(stations_w_cod$system) | stations_w_cod$system!="2" | is.na(stations_w_cod$area) | is.na(stations_w_cod$location)) & (is.na(stations_w_cod$LAT) | is.na(stations_w_cod$LON)),]

if (nrow(stations_w_cod_w_lok) + nrow(stations_w_cod_wo_lok) + nrow(stations_w_cod_wo_lok_wo_pos) != nrow(stations_w_cod)){stop(paste("Some stations not taken care of:", nrow(stations_w_cod_w_lok) + nrow(stations_w_cod_wo_lok) + nrow(stations_w_cod_wo_lok_wo_pos), nrow(stations_w_cod)))}

#' Adds area and location from polygon file
#' @param data must have column LON and LAT
add_area_location <- function(data){
  #add location data from polygon file
  pos <- data[,c("LON", "LAT")]
  coordinates(pos) <- ~ LON + LAT
  proj4string(pos) <- proj4string(lokasjonsdata)
  location_codes <- over(pos, lokasjonsdata)
  data$system<-2
  data$area <- as.integer(location_codes$HAVOMR)
  data$location <- as.integer(location_codes$Lokasjon)
  return(data)
}

stations_w_cod_location_added <- add_area_location(stations_w_cod_wo_lok)
#put back with already annotated
final_stations <- rbind(stations_w_cod_location_added, stations_w_cod_w_lok)
#remove introduced column LON and LAT
final_stations$LON <- NULL
final_stations$LAT <- NULL

#
# Checks
#

#inspect missing values
plot(lokasjonsdata)
points(stations_w_cod_location_added[is.na(stations_w_cod_location_added$location),c("LON","LAT")], col="red")


#add location data to existing location data
existing <- stations_w_cod_w_lok[!is.na(stations_w_cod_w_lok$LAT) & !is.na(stations_w_cod_w_lok$LON),]
existing$oldarea <- existing$area
existing$oldlocation <- existing$location
existing <- add_area_location(existing)

print("Codes where data differ from polygon file based on posisjon. A few differences is expected")
print(paste("Areas:", sum(existing$area != existing$oldarea)))
print(existing[existing$area != existing$oldarea,c("area", "oldarea")])
print(paste("Location (for same area):", sum(existing$area == existing$oldarea & existing$location != existing$oldlocation)))
print(existing[existing$area == existing$oldarea & existing$location != existing$oldlocation, c("location", "oldlocation")])
