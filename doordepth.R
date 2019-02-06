
sm2telegramsfile <- "data/BT12_SM2.csv"
telegrams <- read.csv(sm2telegramsfile, stringsAsFactors = F, strip.white = T, colClasses=c("character", NA, NA, NA, NA, NA, NA, NA))
doordepths_master <- telegrams[telegrams$sensortype=="DVTLAM" & telegrams$measurementid=="D",]
doordepths_master$time <- strptime(doordepths_master$timestamp.hhmmss.ss, format="%H%M%S")-strptime(doordepths_master$timestamp.hhmmss.ss, format="%H%M%S")[[1]]
doordepths_master$measurementvalue <- doordepths_master$measurementvalue*(-1)

doordepths_slave <- telegrams[telegrams$sensortype=="DVTLAS" & telegrams$measurementid=="D",]
doordepths_slave$time <- strptime(doordepths_slave$timestamp.hhmmss.ss, format="%H%M%S")-strptime(doordepths_slave$timestamp.hhmmss.ss, format="%H%M%S")[[1]]
doordepths_slave$measurementvalue <- doordepths_slave$measurementvalue*(-1)


raw <- doordepths_master[doordepths_master$status.A.V=="A",]
extr <- doordepths_master[doordepths_master$status.A.V=="V",]
hq <- raw[raw$qualityfactor==15,]

raw_slave <- doordepths_slave[doordepths_slave$status.A.V=="A",]

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

plot(raw$time, raw$measurementvalue, col="grey", xlab="time (s)", ylab="-depth (m)", main="raw vs extrapolated", type="l")
lines(extr$time, extr$measurementvalue, col="green")

plot(raw$time, raw$measurementvalue, col="grey", xlab="time (s)", ylab="-depth (m)", main="raw vs quality=15", type="l")
lines(hq$time, hq$measurementvalue, col="green")

print(paste("Original resolution:", (max(raw$time)-min(raw$time))/length(raw$time), "s /", spatial_resolution(3.5, (max(raw$time)-min(raw$time))/length(raw$time)), "m @ 3.5 kt."))

plot(raw$time, raw$measurementvalue, col="grey", xlab="time (s)", ylab="-depth (m)", main="raw vs smoothed (10s, 20s, 30s)\n(18m, 36m, 54m @ 3.5 kt)", type="l")
dd<-smooth(raw$time, raw$measurementvalue, resolution=10)
lines(dd$time, dd$measurement, col="blue")
dd<-smooth(raw$time, raw$measurementvalue, resolution=20)
lines(dd$time, dd$measurement, col="red")
dd<-smooth(raw$time, raw$measurementvalue, resolution=30)
lines(dd$time, dd$measurement, col="black")



# difference between doors might define sensible depth resolution target

plot(raw$time, raw$measurementvalue, col="grey", xlab="time (s)", ylab="-depth (m)", main="raw master vs slave", type="l")
lines(raw_slave$time, raw_slave$measurementvalue, col="black")

res=30
dd<-smooth(raw$time, raw$measurementvalue, resolution=res)
dd_slave<-smooth(raw_slave$time, raw_slave$measurementvalue, resolution=res)

plot(c(as.numeric(dd$time), as.numeric(dd_slave$time)), c(dd$measurement, dd_slave$measurement), xlab="time (s)", ylab="-depth (m)", main=paste("smothed (",res," s) master vs slave", sep=""), type="n")
lines(dd$time, dd$measurement, col="grey")
lines(dd_slave$time, dd_slave$measurement, col="black")
