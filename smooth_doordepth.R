

#' smooths depth data by taking median over consecutive measurements to obtain the desired resolution
#' @param timestamps timestampms in seconds
#' @param measurement measurment to be smoothed
#' @param resolution the desired resoltion in seconds
smooth <- function(timestamps, measurement, resolution){
  existingres <- (max(timestamps) - min(timestamps))/length(timestamps)
  if (length(timestamps)!=length(measurement)){
    stop(paste("Length of timestamps (", length(timestamps),") and length og measurements (", length(measurement),") must match"))
  }
  if (existingres > resolution){
    stop(paste("Must provide resolution (s) larger than exisitng resolution (", existingres, "s)"))
  }
  
  newt <- cut(as.numeric(timestamps), breaks=seq(as.numeric(min(timestamps)), as.numeric(max(timestamps))+resolution, resolution), right=F)
  
  timesl <- aggregate(list(time=timestamps), list(timeslot=newt), FUN=median, ordered_result=T)
  meas <- aggregate(list(measurement=measurement), list(timeslot=newt), FUN=median, ordered_result=T) 
  
  tab <- merge(timesl, meas)
  tab <- tab[order(tab$time),]
  return(tab)
}

#' Calculates spatial resolution of a given time inverval for a given speed
#' @speed speed over ground in knots
#' @timeint time interval in seconds
spatial_resolution <- function(speed, timeint){
  return(timeint*(1852*speed/60/60))
}


plot_and_smooth_doordepth <- function(sm2telegramsfile, sog=3.0){
  telegrams <- read.csv(sm2telegramsfile, stringsAsFactors = F, strip.white = T, colClasses=c("character", NA, NA, NA, NA, NA, NA, NA))
  doordepths_master <- telegrams[telegrams$sensortype=="DVTLAM" & telegrams$measurementid=="D",]
  doordepths_master$time <- strptime(doordepths_master$timestamp.hhmmss.ss, format="%H%M%S")-strptime(doordepths_master$timestamp.hhmmss.ss, format="%H%M%S")[[1]]
  doordepths_master$measurementvalue <- doordepths_master$measurementvalue*(-1)
  
  
  raw <- doordepths_master[doordepths_master$status.A.V=="A",]
  
  
  ores <- as.numeric((max(raw$time)-min(raw$time))/length(raw$time))
  ospatres <- as.numeric(spatial_resolution(sog, (max(raw$time)-min(raw$time))/length(raw$time)))
  print(paste("Original resolution:", ores, "s /", ospatres, paste("m @ ", sog, " kt. SOG", sep="")))
  plot(raw$time*ospatres/ores, raw$measurementvalue, col="grey", xlab="distance (m)", ylab="-depth (m)", main=paste("raw vs smoothed (10s, 20s, 30s)\n", sm2telegramsfile, sep=""), type="l", asp=15)
  dd<-smooth(raw$time, raw$measurementvalue, resolution=10)
  lines(dd$time*spatial_resolution(sog, 10)/10, dd$measurement, col="blue")
  dd<-smooth(raw$time, raw$measurementvalue, resolution=20)
  lines(dd$time*spatial_resolution(sog, 20)/20, dd$measurement, col="red")
  dd<-smooth(raw$time, raw$measurementvalue, resolution=30)
  lines(dd$time*spatial_resolution(sog, 30)/30, dd$measurement, col="black")

  l <- list()
  l$fdmin <- -round(max(dd$measurement))
  l$fdmax <- -round(min(dd$measurement))
  l$filename <- sm2telegramsfile
  l$res.s <- 30
  l$ores.s <- ores
  return(l)
}
telegramfiles <- c("data/BT158_SM2.csv","data/BT159_SM2.csv","data/BT161_SM2.csv","data/BT162_SM2.csv","data/BT163_SM2.csv","data/BT164_SM2.csv","data/BT165_SM2.csv","data/BT166_SM2.csv","data/BT167_SM2.csv","data/BT168_SM2.csv","data/BT169_SM2.csv")

d <- NULL
for (t in telegramfiles){
  if (is.null(d)){
    d <- as.data.frame(plot_and_smooth_doordepth(t))
  }
  else{
    d <- rbind(d, as.data.frame(plot_and_smooth_doordepth(t)))
  }
}
