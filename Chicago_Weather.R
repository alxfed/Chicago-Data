## Chicago Weather from weather stations
#
#  URLs for data https://data.cityofchicago.org/ 
#  and https://www.cityofchicago.org/city/en/narr/foia/sample_code0.html
#  
#  as a first step you 'export' the dataset from:
#  https://data.cityofchicago.org/Parks-Recreation/Beach-Weather-Stations-Automated-Sensors-2016-Humi/7edu-s3u7
#  in CSV format
#  
#  you open it in Excel and re-save it as CSV - there is no way to deal
#  properly with AM/PM dates in the timestamps. If you re-save it
#  through Excel as csv the format of Timestamps becomes m/d/Y H:M

main <- function (d) {
  
## the file name is: "Beach_Weather_Stations_-_Automated_Sensors_-_2016_-_Humidity.csv"
  
  W <- read.csv("Beach_Weather_Stations_-_Automated_Sensors_-_2016_-_Humidity.csv")

# there are three weather stations, represented like rows ( :( )
# the data frame should be sliced into three dataframes and 
# processed separately (unles we will be averaging), but first we need
# to deal with date-time that is represented by a factor after
# loading. Let's append four columns to the data frame, then
# position them after the Measurement.Timestamp. 
  
  W$Hour <-0; W$Day <- 0; W$Month <- 0
  W <- W[,c("Station.Name", 
                        "Measurement.Timestamp", 
                        "Hour", "Day", "Month", 
                        "Air.Temperature", 
                        "Wet.Bulb.Temperature", "Humidity", 
                        "Rain.Intensity", "Interval.Rain", 
                        "Total.Rain", "Precipitation.Type", 
                        "Wind.Direction", "Wind.Speed", 
                        "Maximum.Wind.Speed", "Barometric.Pressure", 
                        "Solar.Radiation", "Heading", 
                        "Battery.Life", 
                        "Measurement.Timestamp.Label", 
                        "Measurement.ID")]

#Then we convert the Measurement.Timestamp to character
#then - to POSIXlt
  
  W$Measurement.Timestamp <- as.character.POSIXt(W$Measurement.Timestamp)
  W$Measurement.Timestamp <- as.POSIXlt(W$Measurement.Timestamp, 
                                         format = "%m/%d/%Y %H:%M", tz="")

## about dates format: http://www.stat.berkeley.edu/classes/s133/dates.html
# fill in the columns Hour Day (day of the year mday for anthr) Month
  
  W$Hour <- W$Measurement.Timestamp$hour
  W$Day <- W$Measurement.Timestamp$yday
  W$Month <- W$Measurement.Timestamp$mon
  
## Now let's split the data frame into three; for each station 
#  separately  
  
  FO <- subset.data.frame(W, Station.Name == 
               "Foster Weather Station", select = 
               Station.Name:Measurement.Timestamp.Label)
  
  OS <- subset.data.frame(W, Station.Name == 
               "Oak Street Weather Station", select = 
               Station.Name:Measurement.Timestamp.Label)
  
  ST <- subset.data.frame(W, Station.Name == 
               "63rd Street Weather Station", select = 
               Station.Name:Measurement.Timestamp.Label)
  
  
  save(FO, file = "Foster_Weather.rda")
  save(OS, file = "Oak_Street_Weather.rda")
  save(ST, file = "63rd_Weather.rda")
  
  TDW <- FO[,c("Day", "Month", 
                "Air.Temperature", 
                "Wet.Bulb.Temperature", "Humidity", 
                "Rain.Intensity", "Interval.Rain", 
                "Total.Rain", "Precipitation.Type", 
                "Wind.Direction", "Wind.Speed", 
                "Maximum.Wind.Speed", "Barometric.Pressure", 
                "Solar.Radiation", "Heading")]
  
  Daily_Foster_Weather <- aggregate.data.frame(TDW, 
                                               by = list(TDW$Day),
                                               FUN = mean )
  Monthly_Foster_Weather <- aggregate.data.frame(TDW, 
                                                 by = list(TDW$Month),
                                                 FUN = mean )
  
  save(Daily_Foster_Weather, file = "Daily_Foster_Weather.rda")
  save(Monthly_Foster_Weather, file = "Monthly_Foster_Weather.rda")
  
# Plotting Monthly and Daily

# plot(Monthly_Foster_Weather$Month, Monthly_Foster_Weather$Humidity, "l")
# plot(Daily_Foster_Weather$Day, Daily_Foster_Weather$Humidity, "l")

  humidity <- Daily_Foster_Weather$Humidity
  
# hist(humidity, right=FALSE)
# > colors = c("red", "yellow", "green", "violet", "orange", 
#  +   "blue", "pink", "cyan") 
# hist(duration,    # apply the hist function 
#       +   right=FALSE,    # intervals closed on the left 
#       +   col=colors,     # set the color palette 
#       +   main="Old Faithful Eruptions", # the main title 
#       +   xlab="Duration minutes")       # x-axis label
  
#  breaks <- seq(33, 93, by = 5)
#  humidity.cut <- cut(humidity, breaks, right=FALSE)
#  humidity.freq = table(humidity.cut)
#  humidity.cumfreq = cumsum(humidity.freq)
#  cumfreq0 = c(0, cumsum(humidity.freq)) 
#  plot(breaks, cumfreq0, main="Humidity at Foster",  
#        xlab="Humidity", ylab="Cumulative number of days")   
# y axis label 
#  lines(breaks, cumfreq0)           # join the points
  
#  cbind(humidity.freq)
# maps http://www.kevjohnson.org/making-maps-in-r/
# and http://eglenn.scripts.mit.edu/citystate/tag/r/
  
}