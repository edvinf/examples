#' How to get raw xml files from biotic API for commercial fisheries sampling
#' Database is currently transitioning ot new format (biotic 3.0). 2019 data will probably not be available in biotic 1.4
#' 
#' Documentation for biotic 1.4: http://www.imr.no/formats/nmdbiotic/v1.4/nmdbioticv1_4.xsd
#' Documentation for biotic 3.0 (soon to be released): http://www.imr.no/formats/nmdbiotic/v3/nmdbioticv3_no.html and http://www.imr.no/formats/nmdbiotic/v3/nmdbioticv3_en.html
#' 
#' 

#' Fetches biotic 1.4 data based on serialnubmers. Function will be deprecated in future versions, when fetch on missiontype will be supported.
#' Need to be within imr firewall for access via the API used here.
#' @param year
#' @param start fist serialnumber to include
#' @param send last serialnumber to include
#' @param species optional filtering on species (codes as in NMD-taxa)
#' @param target_location location for storing fetched files. overwrites existing.
#' @param timeout timeout for API call. Set generously.
fetch <- function(year, start, send, species=NULL, target_location="./output", timeout=60*190){
  if (is.null(species)){
    url <- paste("http://tomcat7.imr.no:8080/apis/nmdapi/biotic/v2/", year, start, send, "serial?version=1.4", sep="/")  
  }
  else{
    url <- paste("http://tomcat7.imr.no:8080/apis/nmdapi/biotic/v2/", year, start, send, species, "serial?version=1.4", sep="/")  
  }
  
  print(url)
  destfile <- paste(year, "_", start, "_", send, ".xml", sep="")
  destfile <- paste(target_location, destfile, sep="/")
  print(destfile)
  ret <- F
  ret <- download.file(url, destfile=destfile, mode="wb", method="wget", timeout=timeout)
  return(destfile)
}

#' 
#' Extract data for coastal reference fleet.
#' As far as I can tell the Coastal reference fleet has been using a consistent serialnumber ranger since it started.
#' 
for (y in 2018:2006){fetch(y, 27000, 36999)}

