source("gerritsenPlot.R")

d <- read.csv("data/Catch-rates-countries.csv", sep=";")

stackedPanels(d, "Quarter", "Country", "Year", "Catch_rate")

#specify y-axis label
stackedPanels(d, "Quarter", "Country", "Year", "Catch_rate", ylab = "Catch rate")

# switch columns and rows
stackedPanels(d, "Country", "Quarter", "Year", "Catch_rate", ylab = "Catch rate")

#fake some error bars for testing plot
d$upper <- d$Catch_rate + runif(nrow(d))*d$Catch_rate
d$lower <- d$Catch_rate - runif(nrow(d))*d$Catch_rate

stackedPanels(d, "Quarter", "Country", "Year", "Catch_rate", "lower", "upper", ylab = "Catch rate")

#try with variable colors
linecols <- list()
linecols[["Q1"]] <- "green"
linecols[["Q3"]] <- "pink"
stackedPanels(d, "Quarter", "Country", "Year", "Catch_rate", "lower", "upper", ylab = "Catch rate", linecol = linecols)

# with fixed y axis
stackedPanels(d, "Quarter", "Country", "Year", "Catch_rate", "lower", "upper", ylab = "Catch rate", ymax = 40)

# with set tickmarks
stackedPanels(d, "Quarter", "Country", "Year", "Catch_rate", "lower", "upper", ylab = "Catch rate", tickmarks = c(1,10,20,25,40))

# changing base theme
stackedPanels(d, "Quarter", "Country", "Year", "Catch_rate", "lower", "upper", ylab = "Catch rate", ymax = 40, basetheme = theme_light)

# notice ugly behaviour when y-axis labels are horisontal and y-axis is variable
stackedPanels(d, "Quarter", "Country", "Year", "Catch_rate", "lower", "upper", ylab = "Catch rate", basetheme = theme_light)
