library(Rstox)
# 
# Example fetching time series data using Rstox.
# Contains adaptations for making column names consistent with biotic definitions, as future StoX/Rstox releases are expected to standardize naming
#


#' Download data and organize it in stox projects
#' @return list of stoc project locations
fetch_survey_timeseries <- function(survey, model=modelBio){
  # specify some stox processes to run on all surveys (must be supported by compile_output)
  # specifying read biotic which reads the data and LengthDist, which combines length distribution taking account of when the catch of a species is sorted in categories before indivdiual parameters are sampled (delprøve)
  modelBio <- list("ReadBioticXML", StationLengthDist=list(LengthDistType="LengthDist", BioticData="ReadBioticXML"))
  projects <- getNMDdata(cruise=survey, group="year", subset=NULL, model=modelBio, abbrev=TRUE, subdir=TRUE, ow=TRUE)  
}


#' Get some parameters currently left out in stox parsing, rename some that are not currently named consistently with biotic 3
fix_parameter_names_station <- function(stations){
  if (any(is.na(stations$startdata))){
    stop("Could not intriduce startyear based on startdates. Missing startdates.")
  }
  # not consistent with 1.4 or 3
  stations$startyear <- substr(stations$startdate, 7, 10)
  stations$stationtype <- stations$fishstationtype
  stations$fishstationtype <- NULL
  
  #not consistent with 3
  stations$serialnumber <- stations$serialno
  stations$serialno <- NULL
  stations$stationstartdate <- stations$startdate
  stations$startdate <- NULL
  stations$stationstarttime <- stations$starttime
  stations$starttime <- NULL
  stations$stationstopdate <- stations$stopdate
  stations$stopdate <- NULL
  stations$stationstoptime <- stations$stoptime
  stations$stoptime <- NULL
  stations$stationcomment <- stations$comment
  stations$comment <- NULL
  stations$samplequality <- stations$trawlquality
  stations$trawlquality <- NULL
  stations$verticaltrawlopening <- stations$trawlopening
  stations$trawlopening <- NULL
  stations$verticaltrawlopeningsd <- stations$trawlopeningsd
  stations$trawlopeningsd <- NULL
  stations$catchplatform <- stations$platform
  stations$platform <- NULL
  stations$gearflow <- stations$gearspeed
  stations$gearspeed <- NULL
  stations$bottomdepthmean <- stations$meanbottomdepth
  stations$meanbottomdepth <- NULL
  stations$fishingdepthtemperature <- stations$temperaturefishingdepth
  stations$temperaturefishingdepth <- NULL
  stations$trawldoorspread <- stations$doorspread 
  stations$doorspread <- NULL
  stations$trawldoorspreadsd <- stations$doorspreadsd
  stations$doorspreadsd <- NULL
  stations$vesselcount <- stations$countofvessels
  stations$countofvessels <- NULL
  stations$logstart <- stations$startlog
  stations$startlog <- NULL
  stations$logstop <- stations$stoplog
  stations$stoplog <- NULL
  
  stations$flowconst <- NULL
  stations$flowcount <- NULL
  return(stations)
}

fix_parameter_names_catches <- function(catches){
  catches$commonname <- catches$noname
  catches$noname <- NULL
  catches$catchcategory <- catches$species
  catches$species <- NULL
  catches$catchproducttype <- catches$producttype
  catches$producttype <- NULL
  catches$catchweight <- catches$weight
  catches$weight <- NULL
  catches$catchvolume <- catches$volume
  catches$volume <- NULL
  catches$catchcount <- catches$count
  catches$count <- NULL
  catches$catchcomment <- catches$comment
  catches$comment <- NULL
  catches$catchpartnumber <- catches$samplenumber
  catches$samplenumber <- NULL
  return(catches)
}

fix_parameter_names_individuals <- function(individuals){
  individuals$lengthresolution <- individuals$lengthunit
  individuals$lengthunit <- NULL
  individuals$maturationstage <- individuals$stage
  individuals$stage <- NULL
  individuals$tissuesample <- individuals$genetics
  individuals$genetics <- NULL
  individuals$tissuesamplenumber <- individuals$geneticsnumber
  individuals$geneticsnumber <- NULL
  individuals$individualproducttype <- individuals$producttype
  individuals$producttype <- NULL
  individuals$individualweight <- individuals$weight
  individuals$weight <- NULL
  individuals$individualvolume <- individuals$volume
  individuals$volume <- NULL
  individuals$individualcomment <- individuals$comment
  individuals$comment <- NULL
  individuals$vertebraecount <- individuals$vertebrae
  individuals$vertebrae <- NULL
  individuals$specimenid <- individuals$specimenno
  individuals$specimenno <- NULL
  
  individuals$developmentalstage <- NULL
  return(individuals)
}

#' Runs all given stox projects and stacks tabular output files
#' @return list with data compiled for all projects: stations, catches, individuals and lengthdistributions for each station
compile_output <- function(stoxprojects){
  individualdata <- NULL
  catchdata <- NULL
  stationdata <- NULL
  lengthdistdata <- NULL
  
  for (p in stoxprojects){
    g <- getBaseline(p)
    
    ind <- g$outputData$ReadBioticXML$ReadBioticXML_BioticData_Individual.txt
    if (is.null(individualdata)){
      individualdata <- ind
    }
    else{
      individualdata <- rbind(individualdata, ind)
    }

    catch <- g$outputData$ReadBioticXML$ReadBioticXML_BioticData_CatchSample.txt
    if (is.null(catchdata)){
      catchdata <- catch
    }
    else{
      catchdata <- rbind(catchdata, catch)
    }
    
    stations <- g$outputData$ReadBioticXML$ReadBioticXML_BioticData_FishStation.txt
    if (is.null(stationdata)){
      stationdata <- stations
    }
    else{
      stationdata <- rbind(stationdata, stations)
    }
     
    ldist <- g$outputData$StationLengthDist
    if (is.null(lengthdistdata)){
      lengthdistdata <- ldist
    }
    else{
      lengthdistdata <- rbind(lengthdistdata, ldist)
    }   
    closeProject(p)
  }
  
  raw_data <- list()
  stationdata <- fix_parameter_names_station(stationdata)
  raw_data$stationdata <- stationdata
  catchdata <- fix_parameter_names_catches(catchdata)
  raw_data$catchdata <- catchdata
  individualdata <- fix_parameter_names_individuals(individualdata)
  raw_data$individualdata <- individualdata
  
  computed_data <- list()
  computed_data$lengthdistdata <- lengthdistdata
  
  reference_data <- list()
  reference_data$taxa <- getNMDinfo("taxa")
  
  out <- list()
  
  out$raw_data <- raw_data
  out$computed_data <- computed_data
  out$reference_data <- reference_data
  
  return(out)
}

#' saves data
save_data <- function(data, survey, outpath){
  saveRDS(data, file.path(outpath, paste(survey, "rda", sep=".")))
}

get_surveyseries <- function(survey){
  outpath <- "./output"
  projects <- fetch_survey_timeseries(survey)
  output <- compile_output(projects)
  save_data(output, survey, outpath)
}

#
# Commands for browsing db content (geting cruise and survey identifiers)
#

#list surveys
getNMDinfo("cs", recursive=FALSE)
#get all cruises
eco_cruises <- getNMDinfo(c("cs", "Barents Sea NOR-RUS ecosystem cruise in autumn"))
zero_g_cruises <- getNMDinfo(c("cs", "Barents Sea NOR-RUS 0-group cruise in autumn"))

#
#extracts time_series for ecosystem survey and the preceeding 0-group survey
#
get_surveyseries("Barents Sea NOR-RUS ecosystem cruise in autumn")
get_surveyseries("Barents Sea NOR-RUS 0-group cruise in autumn")

#
#extracts time_series for winter survey
#
get_surveyseries("Barents Sea NOR-RUS demersal fish cruise in winter")

#
# read data back in with readRDS(path)
#