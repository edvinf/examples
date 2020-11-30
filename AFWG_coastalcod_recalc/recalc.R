library(xlsx)
library(RstoxData)

#' Variant
readXls <- function(path, sheetnr){
  colclasses <- c("integer", "integer", "character", "character", "character", "character", "character", "character", "numeric")
  names(colclasses) <- c("Fangstår", "Landingsmnd", "Kyst..Hav", "Hovedområde", "Lokalitet", "Reiskap", "Beskrivelse", "Namn", "Kvantum.i.kg")
  ss <- xlsx::read.xlsx(path, sheetnr, colClasses = colclasses, stringsAsFactors=F)
  extr <- ss[,names(ss)[names(ss) %in% names(colclasses)]]
  names(extr) <- c("year", "month", "coast", "mainarea","location", "gear", "geardescr", "species", "liveWeightKg")
  agg <- data.table::as.data.table(aggregate(list(liveWeightKg=extr$liveWeightKg), by=list(year=extr$year, month=extr$month, coast=extr$coast, mainarea=extr$mainarea, gear=extr$gear, species=extr$species), FUN=function(x){sum(x,na.rm=T)}))
  return(agg)
}

#' Variant 2002
readXlsV3 <- function(path, sheetnr){
  colclasses <- c("integer", "integer", "character", "character", "character", "character", "character", "character", "numeric")
  names(colclasses) <- c("Fangstår", "Landingsmnd", "Kyst..hav", "Hovedområde", "Lokalitet", "Reiskap", "Beskrivelse", "Namn", "Kvantum.i.kg")
  ss <- xlsx::read.xlsx(path, sheetnr, colClasses = colclasses, stringsAsFactors=F)
  extr <- ss[,names(ss)[names(ss) %in% names(colclasses)]]
  names(extr) <- c("year", "month", "coast", "mainarea","location", "gear", "geardescr", "species", "liveWeightKg")
  agg <- data.table::as.data.table(aggregate(list(liveWeightKg=extr$liveWeightKg), by=list(year=extr$year, month=extr$month, coast=extr$coast, mainarea=extr$mainarea, gear=extr$gear, species=extr$species), FUN=function(x){sum(x,na.rm=T)}))
  return(agg)
}

#' Variant 2001
readXlsV2 <- function(path, sheetnr){
  colclasses <- c("integer", "integer", "character", "character", "character", "character", "character", "character", "numeric")
  names(colclasses) <- c("Fangstår", "Landingsmnd.", "Hav..kyst", "Hovedområde", "Lokalitet", "Redskap", "Beskrivelse", "Namn", "Kvantum.i.kg")
  ss <- xlsx::read.xlsx(path, sheetnr, colClasses = colclasses, stringsAsFactors=F)
  extr <- ss[,names(ss)[names(ss) %in% names(colclasses)]]
  names(extr) <- c("year", "month", "coast", "mainarea","location", "gear", "geardescr", "species", "liveWeightKg")
  agg <- data.table::as.data.table(aggregate(list(liveWeightKg=extr$liveWeightKg), by=list(year=extr$year, month=extr$month, coast=extr$coast, mainarea=extr$mainarea, gear=extr$gear, species=extr$species), FUN=function(x){sum(x,na.rm=T)}))
  return(agg)
}

readLss <- function(path){
  ss<-RstoxData::readLssFile(path)
  ss$month <- as.integer(substr(ss$`Siste fangstdato`,6,7))
  extr <- ss[,c("Fangstår", "month", "Kyst/hav (kode)", "Hovedområde (kode)", "Lokasjon (kode)", "Redskap (kode)", "Redskap", "Art - FDIR", "Rundvekt")]
  names(extr) <- c("year", "month", "coast", "mainarea","location", "gear", "geardescr", "species", "liveWeightKg")
  agg <- data.table::as.data.table(aggregate(list(liveWeightKg=extr$liveWeightKg), by=list(year=extr$year, month=extr$month, coast=extr$coast, mainarea=extr$mainarea, gear=extr$gear, species=extr$species), FUN=function(x){sum(x,na.rm=T)}))
  return(agg)
}

compileYears <- function(indexfile="tables/landingsloc.txt", output="tables/allYears.rds"){
  files <- read.table(indexfile, na.strings = "NA", colClasses = c("character","character", "integer","character"), header = T)
  dat <- NULL
  for (i in 1:nrow(files)){
    
    cat("Loading", files[i, "path"],"\n")
    
    if (files[i,"format"] == "xls"){
      dat <- rbind(dat, readXls(files[i, "path"], files[i, "sheet"]))
    }
    else if (files[i,"format"] == "xlsv2"){
      dat <- rbind(dat, readXlsV2(files[i, "path"], files[i, "sheet"]))
    }
    else if (files[i,"format"] == "xlsv3"){
      dat <- rbind(dat, readXlsV3(files[i, "path"], files[i, "sheet"]))
    }
    else if (files[i,"format"] == "LSS"){
      dat <- rbind(dat, readLss(files[i, "path"]))
    }
    else{
      stop()
    }
  }
  
  dat$mainarea[dat$mainarea=="0"] <- "00"
  dat$mainarea[dat$mainarea=="1"] <- "01"
  dat$mainarea[dat$mainarea=="2"] <- "02"
  dat$mainarea[dat$mainarea=="3"] <- "03"
  dat$mainarea[dat$mainarea=="4"] <- "04"
  dat$mainarea[dat$mainarea=="5"] <- "05"
  dat$mainarea[dat$mainarea=="6"] <- "06"
  dat$mainarea[dat$mainarea=="7"] <- "07"
  dat$mainarea[dat$mainarea=="8"] <- "08"
  dat$mainarea[dat$mainarea=="9"] <- "09"
  
  
  saveRDS(dat, output)
}

#' retains the landings of species in retaion that is not landed from aquaculture
filterSpecies <- function(landings, retain){
  output <- list()
  output$removed <- sort(unique(landings$species[!(landings$species %in% retain)]))
  output$data <- landings[landings$species %in% retain & landings$gear!=90,]  
  return(output)
}

#' loads prepared landings data and filters to retain cod.
loadCod <- function(landingsfile="tables/allYears.rds"){
  data <- readRDS(landingsfile)
  cod <- filterSpecies(data, c("Torsk", "Skrei", "Nordøstarktisk torsk", "Annen torsk", "Annen Torsk", "Vårtorsk"))
  cod$removedcod <- c(cod$removed[grep("*torsk*", cod$removed)], cod$removed[grep("*skrei*", cod$removed)])
  return(cod)
}

loadTab2.1a <- function(oldTable){
  tab <- xlsx::read.xlsx(oldTable, 1, header = T, colClasses = c("numeric", rep("numeric",10)))
  names(tab) <- c("year", paste("age", 2:9), "age 10+", "landedCoastalTotal")
  tab <- data.table::as.data.table(tab)
  return(tab)
}

#' filters coastal area
filterCoastalAreas <- function(landings, mainareas=c("00","03","04","05","06","07")){
  landings <- landings[landings$mainarea %in% mainareas,]
  return(landings)
}

table2.1b <- function(landings){
  lc2018 <- filterCoastalAreas(landings[landings$year==2018,])
  lc2018$mainarea[lc2018$mainarea %in% c("06", "07")] <- "06/07"
  lc2018$gearGroup <- NA
  lc2018$gearGroup[lc2018$gear %in% c("50","51","52","58","59")] <- "Trawl"
  lc2018$gearGroup[lc2018$gear %in% c("30","31","32","33","34","35")] <- "L.Line/Jig"
  lc2018$gearGroup[lc2018$gear %in% c("61")] <- "D.Seine"
  lc2018$gearGroup[lc2018$gear %in% c("20","22")] <- "Gillnet"
  lc2018$gearGroup[is.na(lc2018$gearGroup)] <- "Others"
  return(aggregate(list(ton=lc2018$liveWeightKg), by=list(mainarea=lc2018$mainarea, gear=lc2018$gearGroup), FUN=function(x){sum(x)/1000}))
}

table2.1bTotals <- function(landings){
  lc2018 <- filterCoastalAreas(landings[landings$year==2018,])
  lc2018$mainarea[lc2018$mainarea %in% c("06", "07")] <- "06/07"
  return(aggregate(list(ton=lc2018$liveWeightKg), by=list(mainarea=lc2018$mainarea), FUN=function(x){sum(x)/1000}))
}

makeKey <- function(landings, areaDef=list(fN67=c("00","05","04","03"), h06=c("06"), h07=c("07"))){
  landings$mgmtArea <- NA
  for (n in names(areaDef)){
    landings$mgmtArea[landings$mainarea %in% areaDef[[n]]] <- n      
  }
  
  key <- aggregate(list(landedAllCodMgtArea=landings$liveWeightKg), by=list(managementArea=landings$mgmtArea, year=landings$year), FUN=function(x){sum(x)/1000})
  yearTotal <- aggregate(list(landedAllCodTotal=landings$liveWeightKg), by=list(year=landings$year), FUN=function(x){sum(x)/1000})
  return(merge(key, yearTotal))
}

redistrCaa <- function(oldTable="tables/tab2.1a.xlsx", landings="tables/allYears.rds"){
  landings <- filterCoastalAreas(loadCod()$data)
  key <- makeKey(landings)
  coastalCaa <- loadTab2.1a(oldTable)
  
  # merge inn redistribution parameters
  stopifnot(all(coastalCaa$managementArea %in% c("h06", "h07", "fN67")))
  coastalCaa <- merge(coastalCaa, key, by="year")
  coastalCaa[,2:10] <- coastalCaa[,2:10]
  
  #set revised weights to total from sales notes
  
  revisedCAA <- coastalCaa
  revisedCAA$landedCoastalMgtArea <- round(revisedCAA$landedCoastalTotal*revisedCAA$landedAllCodMgtArea / revisedCAA$landedAllCodTotal)
  revisedCAA[,2:10] <- round(revisedCAA[,2:10] * revisedCAA$landedAllCodMgtArea / revisedCAA$landedAllCodTotal)
  revisedCAA$unitAgeGroups <- "(’000)"
  revisedCAA$unitWeights <- "Tonnes"
  return(revisedCAA)
}

saveRedestibuted <- function(redistr, output="output"){
  
  for (fv in unique(redistr$managementArea)){
    tab <- redistr[redistr$managementArea == fv,]
    xlsx::write.xlsx2(tab, paste("output/caa_coastalcod_", fv, ".xlsx",sep=""), row.names = F)
  }
  
}

# andre tabeller ?
# skriv forbehold

run <- function(){
  warning("2019 uses revision of data from nov 2020. Other years are considered at final revision.")
  saveRedestibuted(redistrCaa())
}

run()
