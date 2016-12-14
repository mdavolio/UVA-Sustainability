library(dplyr)
library(lubridate)
weather1 <- read.csv("weather_1.csv")
weather2 <- read.csv("weather_2.csv")
weather3 <- read.csv("weather_3.csv")

weather<-rbind(weather1,weather2)
weather<-rbind(weather,weather3)


#Convert date
weather$Month<-month(as.Date(weather$EDT))
weather$Year<-year(as.Date(weather$EDT))
weather$PrecipitationIn<-as.numeric(as.character(weather$PrecipitationIn))


#find aveage humidity/temperature
Avg_Weather<- weather %>%
  group_by(Year,Month) %>%
  summarise(AvgTemp = mean(Mean.TemperatureF),
            AvgHum = mean(Mean.Humidity),
            sumPrep = sum(PrecipitationIn, na.rm = TRUE))

#order by year, month
Avg_Weather<-Avg_Weather[order(Avg_Weather$Year,Avg_Weather$Month),]

write.csv(Avg_Weather,"Avg_Weather.csv")
