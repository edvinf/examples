library(Rstox)

#' Download data and organize it in stox projects
#' @return list of stoc project locations
fetch_survey_timeseries <- function(survey, model=modelBio){
  # specify some stox processes to run on all surveys (must be supported by compile_output)
  # specifying read biotic which reads the data and LengthDist, which combines length distribution taking account of when the catch of a species is sorted in categories before indivdiual parameters are sampled (delprøve)
  modelBio <- list("ReadBioticXML", StationLengthDist=list(LengthDistType="LengthDist", BioticData="ReadBioticXML"))
  projects <- getNMDdata(cruise=survey, group="year", subset=c(2005,2006), model=modelBio, abbrev=TRUE, subdir=TRUE, ow=TRUE)  
}


#' Get some parameters currently left out in stox parsing, rename some that are not currently named consistently with biotic 3
fix_parameter_names_station <- function(stations){
  if (any(is.na(stations$startdata))){
    stop("Could not intriduce startyear based on startdates. Missing startdates.")
  }
  stations$startyear <- substr(stations$startdate, 7, 10)
  return(stations)
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
  }
  
  out <- list()
  stationdata <- fix_parameter_names_station(stationdata)
  out$stationdata <- stationdata
  out$catchdata <- catchdata
  out$individualdata <- individualdata
  out$lengthdistdata <- lengthdistdata
  
  return(out)
}

#' saves data
save_data <- function(data, survey, outpath){
  saveRDS(data, file.path(outpath, paste(survey, "rda", sep=".")))
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
survey <- "Barents Sea NOR-RUS ecosystem cruise in autumn"
outpath <- "./output"
projects <- fetch_survey_timeseries(survey)
output <- compile_output(projects)
save_data(output, survey, outpath)

#
# read data back in with readRDS(path)
#