#####################################
########## sustain_read.R ###########
#####################################

# Data Mining SYS 6018 Final Project
# Code to read in and clean/wrangle the data into a useable format
suppressPackageStartupMessages({
library(readxl)
library(dplyr)
library(purrr)
library(data.table)
library(lubridate)
})

# No scientific notation
options(scipen=999)

#### Round Timestamps ####
ts_round <- function(df){
  df$Timestamp <- as.POSIXct(round(as.numeric(strptime(df$Timestamp, 
                                                       '%Y-%m-%d %H:%M:%S'))/900) * 900, origin='1970-01-01')
  df$DayOfWeek<-as.POSIXlt(df$Timestamp)$wday + 1
  return(df)
}

#### Sum Energy Consumption ####
## first grab the hour from the timestamp
energy <- function(df) {
  
  df$Timestamp <- as.POSIXct(df$Timestamp)
  try(
    df <- mutate(df, Hour = hour(df$Timestamp)) %>% 
          mutate(Date = format(df$Timestamp,"%Y-%m-%d")) %>% 
          group_by(buildingID,Date,Hour) %>% 
          summarise(electricity = sum(Interpolative_e), hot_water=sum(Interpolative_hw), cold_water=sum(Interpolative_cw)) %>% 
          mutate(total_energy = electricity+hot_water+cold_water), silent = T
  )
  try(
    df <- mutate(df, Hour = hour(df$Timestamp)) %>% 
          mutate(Date = format(df$Timestamp,"%Y-%m-%d")) %>% 
          group_by(buildingID,Date,Hour) %>% 
          summarise(electricity = sum(Interpolative_e), steam = sum(Interpolative_s)) %>% 
          mutate(total_energy = electricity+steam), silent = T
  )
  try(
    df <- mutate(df, Hour = hour(df$Timestamp)) %>% 
      mutate(Date = format(df$Timestamp,"%Y-%m-%d")) %>% 
      group_by(buildingID,Date,Hour) %>% 
      summarise(electricity = sum(Interpolative_e), steam = sum(Interpolative_s), cold_water=sum(Interpolative_cw)) %>% 
      mutate(total_energy = electricity+steam+cold_water), silent = T
  )
  try(
    df <- mutate(df, Hour = hour(df$Timestamp)) %>% 
      mutate(Date = format(df$Timestamp,"%Y-%m-%d")) %>% 
      group_by(buildingID,Date,Hour) %>% 
      summarise(electricity = sum(Interpolative_e), steam = sum(Interpolative_s), hot_water = sum(Interpolative_hw), cold_water=sum(Interpolative_cw)) %>% 
      mutate(total_energy = electricity+steam+cold_water+hot_water), silent = T
  )
  return(df)
}

#### Read Building Info Data ####
buildingInfo <- function(path){
  # Read in file
  df <- read_excel(path) %>% 
    select(c(1,2,6,7,8,9))
  names(df) <- c('buildingID','buildingName','square_foot','YearBuilt','ConstructionType','Category')
  return(df) 
}

buildings <- buildingInfo('Basic Building Info.xlsx')

##### convert start and end dates of semester to numeric
session <- function(df){
  start_F13 <- as.numeric(as.POSIXct("08/27/2013  12:00:00 AM", format="%m/%d/%Y  %H:%M:%S %p"))
  end_F13 <- as.numeric(as.POSIXct("12/18/2013  12:00:00 AM", format="%m/%d/%Y  %H:%M:%S %p"))
  start_S14 <- as.numeric(as.POSIXct("01/13/2014  12:00:00 AM", format="%m/%d/%Y  %H:%M:%S %p"))
  end_S14 <- as.numeric(as.POSIXct("05/10/2014  12:00:00 AM", format="%m/%d/%Y  %H:%M:%S %p"))
  start_F14 <- as.numeric(as.POSIXct("08/26/2014  12:00:00 AM", format="%m/%d/%Y  %H:%M:%S %p"))
  end_F14 <- as.numeric(as.POSIXct("12/17/2014  12:00:00 AM", format="%m/%d/%Y  %H:%M:%S %p"))
  start_S15 <- as.numeric(as.POSIXct("01/12/2015  12:00:00 AM", format="%m/%d/%Y  %H:%M:%S %p"))
  end_S15 <- as.numeric(as.POSIXct("05/09/2015  12:00:00 AM", format="%m/%d/%Y  %H:%M:%S %p"))
  start_F15 <- as.numeric(as.POSIXct("08/25/2015  12:00:00 AM", format="%m/%d/%Y  %H:%M:%S %p"))
  end_F15 <- as.numeric(as.POSIXct("12/19/2015  12:00:00 AM", format="%m/%d/%Y  %H:%M:%S %p"))
  start_S16 <- as.numeric(as.POSIXct("01/20/2016  12:00:00 AM", format="%m/%d/%Y  %H:%M:%S %p"))
  end_S16 <- as.numeric(as.POSIXct("05/14/2016  12:00:00 AM", format="%m/%d/%Y  %H:%M:%S %p"))
  start_F16 <- as.numeric(as.POSIXct("08/23/2016  12:00:00 AM", format="%m/%d/%Y  %H:%M:%S %p"))
  end_F16 <- as.numeric(as.POSIXct("12/17/2016  12:00:00 AM", format="%m/%d/%Y  %H:%M:%S %p"))
  start_S17 <- as.numeric(as.POSIXct("01/18/2017  12:00:00 AM", format="%m/%d/%Y  %H:%M:%S %p"))
  end_S17 <- as.numeric(as.POSIXct("05/13/2017  12:00:00 AM", format="%m/%d/%Y  %H:%M:%S %p"))
  
  x <- as.numeric(as.POSIXct(df$Date, '%Y-%m-%d'))
  
  df$semester <- ifelse((x < start_F13), 0, 
                        ifelse((x < start_S14 & x > end_F13), 0, 
                               ifelse((x < start_F14 & x > end_S14), 0,
                                      ifelse((x < start_S15 & x > end_F14), 0,
                                             ifelse((x < start_F15 & x > end_S15), 0,
                                                    ifelse((x < start_S16 & x > end_F15), 0,
                                                           ifelse((x < start_F16 & x > end_S16), 0,
                                                                  ifelse((x < start_S17 & x > end_F16), 0, 1))))))))
  
  return(df)
}

#### Read Builiding Energy Data  #####
# First Format
building.1 <- function(path){
  
  if(length(excel_sheets(path)) == 3){
    # Read in Files
    e <- read_excel(path, sheet = 1) # Electricity
    hw <- read_excel(path, sheet = 2) # Hot Water
    cw <- read_excel(path, sheet = 3) # Chilled Water
    
    # Remove incomplete observations
    e <- e[complete.cases(e),]
    hw <- hw[complete.cases(hw),]
    cw <- cw[complete.cases(cw),]
    
    # keep necessary columns
    keep <- c(1,2,4,5,7,8,10)
    
    e <- e[, keep]
    hw <- hw[, keep]
    cw <- cw[, keep]
    
    # rename columns for ease
    oldnames_e <- names(e)
    newnames_e <- c('Timestamp','Min_e','Min_Time_e','Max_e','Max_Time_e','Avg_e','Interpolative_e')
    setnames(e, oldnames_e, newnames_e)
    
    oldnames_hw <- names(hw)
    newnames_hw <- c('Timestamp','Min_hw','Min_Time_hw','Max_hw','Max_Time_hw','Avg_hw','Interpolative_hw')
    setnames(hw, oldnames_hw, newnames_hw)
    
    oldnames_cw <- names(cw)
    newnames_cw <- c('Timestamp','Min_cw','Min_Time_cw','Max_cw','Max_Time_cw','Avg_cw','Interpolative_cw')
    setnames(cw, oldnames_cw, newnames_cw)
    
    #merge
    temp <- merge(e, hw, by = 'Timestamp')
    final <- merge(temp, cw, by = 'Timestamp')
  }
  else{
    # Read in Files
    e <- read_excel(path, sheet = 1) # Electricity
    s <- read_excel(path, sheet = 2) # steam
    
    # Remove incomplete observations
    e <- e[complete.cases(e),]
    s <- s[complete.cases(s),]
    
    # keep necessary columns
    keep <- c(1,2,4,5,7,8,10)
    
    e <- e[, keep]
    s <- s[, keep]
    
    # rename columns for ease
    oldnames_e <- names(e)
    newnames_e <- c('Timestamp','Min_e','Min_Time_e','Max_e','Max_Time_e','Avg_e','Interpolative_e')
    setnames(e, oldnames_e, newnames_e)
    
    oldnames_s <- names(s)
    newnames_s <- c('Timestamp','Min_s','Min_Time_s','Max_s','Max_Time_s','Avg_s','Interpolative_s')
    setnames(s, oldnames_s, newnames_s)
    
    # merge
    final <- merge(e, s, by = 'Timestamp')
  }
  
  return(final)
}

# Second Format
building.2 <- function(path){
  # Read in Files
  e <- read_excel(path, sheet = 1) # Electricity
  s <- read_excel(path, sheet = 2) # Steam
  cw <- read_excel(path, sheet = 3) # Chilled Water
  
  # Remove incomplete observations
  e <- e[complete.cases(e),]
  s <- s[complete.cases(s),]
  cw <- cw[complete.cases(cw),]
  
  # keep necessary columns
  keep <- c(1,2,4,5,7,8,10)
  
  e <- e[, keep]
  s <- s[, keep]
  cw <- cw[, keep]
  
  # rename columns for ease
  oldnames_e <- names(e)
  newnames_e <- c('Timestamp','Min_e','Min_Time_e','Max_e','Max_Time_e','Avg_e','Interpolative_e')
  setnames(e, oldnames_e, newnames_e)
  
  oldnames_s <- names(s)
  newnames_s <- c('Timestamp','Min_s','Min_Time_s','Max_s','Max_Time_s','Avg_s','Interpolative_s')
  setnames(s, oldnames_s, newnames_s)
  
  oldnames_cw <- names(cw)
  newnames_cw <- c('Timestamp','Min_cw','Min_Time_cw','Max_cw','Max_Time_cw','Avg_cw','Interpolative_cw')
  setnames(cw, oldnames_cw, newnames_cw)
  
  #merge
  temp <- merge(e, s, by = 'Timestamp')
  return(merge(temp, cw, by = 'Timestamp'))
}

# Third Format
building.3 <- function(path){
  # Read in Files
  e <- read_excel(path, sheet = 1) # Electricity
  s <- read_excel(path, sheet = 2) # Steam
  hw <- read_excel(path, sheet = 3) # Chilled Water
  cw <- read_excel(path, sheet = 4) # Chilled Water
  
  # Remove incomplete observations
  e <- e[complete.cases(e),]
  s <- s[complete.cases(s),]
  hw <- hw[complete.cases(hw),]
  cw <- cw[complete.cases(cw),]
  
  # keep necessary columns
  keep <- c(1,2,4,5,7,8,10)
  
  e <- e[, keep]
  s <- s[, keep]
  hw <- hw[, keep]
  cw <- cw[, keep]
  
  
  # rename columns for ease
  oldnames_e <- names(e)
  newnames_e <- c('Timestamp','Min_e','Min_Time_e','Max_e','Max_Time_e','Avg_e','Interpolative_e')
  setnames(e, oldnames_e, newnames_e)
  
  oldnames_s <- names(s)
  newnames_s <- c('Timestamp','Min_s','Min_Time_s','Max_s','Max_Time_s','Avg_s','Interpolative_s')
  setnames(s, oldnames_s, newnames_s)
  
  oldnames_cw <- names(cw)
  newnames_cw <- c('Timestamp','Min_cw','Min_Time_cw','Max_cw','Max_Time_cw','Avg_cw','Interpolative_cw')
  setnames(cw, oldnames_cw, newnames_cw)
  
  oldnames_hw <- names(hw)
  newnames_hw <- c('Timestamp','Min_hw','Min_Time_hw','Max_hw','Max_Time_hw','Avg_hw','Interpolative_hw')
  setnames(hw, oldnames_hw, newnames_hw)
  
  #merge
  temp <- merge(e, s, by = 'Timestamp') %>% 
    merge(hw, by = 'Timestamp')
  return(merge(temp, cw, by = 'Timestamp'))
}

#### Function for all functions
read_build.1 <- function(path, bID){
  df <- building.1(path) %>%
    mutate(buildingID = bID) %>%
    ts_round() %>%
    energy() %>%
    merge(buildings, by = "buildingID", all.x = TRUE) %>% 
    session()
}

read_build.2 <- function(path, bID){
  df <- building.2(path) %>%
    mutate(buildingID = bID) %>%
    ts_round() %>%
    energy() %>%
    merge(buildings, by = "buildingID", all.x = TRUE) %>% 
    session()
}

read_build.3 <- function(path, bID){
  df <- building.3(path) %>%
    mutate(buildingID = bID) %>%
    ts_round() %>%
    energy() %>%
    merge(buildings, by = "buildingID", all.x = TRUE) %>% 
    session()
}

# Normal format
physics <- read_build.1('Physics 0221.xlsx', '0221')
rice <- read_build.1('Rice Hall 0214.xlsx','0214')
echols <- read_build.1('Echols 2213.xlsx','2213')
humphreys <- read_build.1('Humphreys 2214.xlsx','2214')
afc <- read_build.1('AFC 5271.xlsx', '5271')

# Other format - Electricity, Steam, Chilled Water
kellogg <- read_build.2('Kellogg 2368.xlsx', '2214')
oHill_Din <- read_build.2('Ohill Dining 0201.xlsx', '0201')
matSci <- read_build.2('Materials Science 0270.xlsx', '0270')
gilmer <- read_build.2('Gilmer 0210.xlsx', '0210')

# Other Format - Electricity, Steam, Hot Water, Chilled Water
mechEng <- read_build.3('Mech Eng 0259.xlsx', '0259')

#### Combine into one data frame ####
final_buildings <- bind_rows(list(echols,humphreys,kellogg,oHill_Din,physics,rice,afc,gilmer,mechEng,matSci)) 

#### Read Weather Data  ######

grabWeather <- function(path){
  
  # Read in files
  temp <- read_excel(path, sheet = 1) %>% 
    select(c(2,3,4,5,6,7))
  
  humid <- read_excel(path, sheet = 2) %>% 
    select(c(2,3,4,5,6,7))
  
  names(temp) <- c('Timestamp', 'Min_T', 'Max_T', 'Avg_T', 'INTERPOLATIVE_T', 'AvgSPH_T')
  names(humid) <- c('Timestamp', 'Min_H', 'Max_H', 'Avg_H', 'INTERPOLATIVE_H', 'AvgSPH_H') 
  merged <- merge(temp,humid, by = "Timestamp", all = TRUE)
  
  merged$Timestamp <- as.POSIXct(round(as.numeric(strptime(merged$Timestamp, 
                                                       '%Y-%m-%d %H:%M:%S'))/900) * 900, origin='1970-01-01')

  merged <- mutate(merged, Hour = hour(merged$Timestamp)) %>% 
            mutate(Date = format(merged$Timestamp,"%Y-%m-%d"))
  
  return(merged)
}

grabWeather2 <- function(path){
  
  df <- read.csv(path, header = TRUE, sep = ",") %>% 
    select(c(1,5:7,11:23)) %>% 
    mutate(Date = as.character(as.POSIXct(EST))) %>% 
    select(-c(1)) %>% 
    select(c(17,1:16)) %>% 
    mutate(Rain = ifelse(Events == "Fog-Rain" | Events == "Fog-Rain-Thunderstorm" | Events == "Rain" | Events == "Rain-Thunderstorm" | 
                           Events == "Thunderstorm" | Events == "Fog-Rain-Snow" | Events == "Rain-Snow", 1, 0)) %>% 
    mutate(Snow = ifelse(Events == "Fog-Rain-Snow" | Events == "Fog-Snow" | Events == "Rain-Snow" | Events == "Snow", 1, 0)) %>% 
    select(-c(16))
    
  return(df)
}

weather_1 <- grabWeather('OA Data.xlsx')
weather_2 <- grabWeather2('CvilleWeather.txt')

####### MERGE WEATHER AND BUILDINGS & RANDOM CLEANING#########
final <- merge(final_buildings, weather_1, all.x = T) %>% 
  merge(weather_2, by = 'Date', all.x = T)

final$age = (as.numeric(format(as.Date(final$Date, '%Y-%m-%d'),'%Y')) - as.numeric(final$YearBuilt))
final$Date <- as.Date(final$Date)
final$ConstructionType <- as.factor(final$ConstructionType)
final$Category <- as.factor(final$Category)
final <- as.data.frame(final, order(final$Timestamp))
remove <- c('Timestamp','buildingName','YearBuilt')
final <- final[ , !(names(final) %in% remove)]

#### Remove Unnecessary Things from Environment AND Save ####
rm(list=setdiff(ls(), c("final")))


#### Conversion to MMBtu and separate source
final$`electricity(mmbtu)` <- final$electricity * 0.00340951
final$`steam(mmbtu)` <- final$steam * 1.04

final[c('steam(mmbtu)', 'hot_water', 'cold_water')][is.na(final[c('steam(mmbtu)', 'hot_water', 'cold_water')])] <- 0

final <- mutate(final, total_energy = `electricity(mmbtu)`+`steam(mmbtu)`+hot_water+cold_water)

# Still concerned that this is taking the boolean result of the first row... and applying it
if(final[,'Date'] <= '2013-12-31'){
  final$coal = final$total_energy * .308
  final$nat_gas = final$total_energy * .691
  final$oil = final$total_energy * .001
} else if(final[,'Date'] <= '2014-12-31'){
  final$coal = final$total_energy * .424
  final$nat_gas = final$total_energy * .552
  final$oil = final$total_energy * .025
} else if(final[,'Date'] <= '2015-12-31'){
  final$coal = final$total_energy * .262
  final$nat_gas = final$total_energy * .717
  final$oil = final$total_energy * .022
} else {
  final$coal = final$total_energy * .217
  final$nat_gas = final$total_energy * .779
  final$oil = final$total_energy * .005
}

# convert mmbtu values to co2 output
# co2 counts are in pounds of co2
# oil was labeld as no.2 type
# asumed coal was bituminous
final$coal_co2 <- final$coal * 205.691
final$nat_gas_co2 <- final$nat_gas * 116.999
final$oil_co2 <- final$oil * 161.290

# sum total co2 output
final <- mutate(final, total_co2 = coal_co2 + nat_gas_co2 + oil_co2)

# Remove unnecessary columns
remove <- c("electricity","steam","Min_T","Max_T","Avg_T","AvgSPH_T","Min_H",
            "Max_H","Avg_H","AvgSPH_H","Max.Dew.PointF","Min.DewpointF",
            "Max.Sea.Level.PressureIn","Min.Sea.Level.PressureIn","Max.VisibilityMiles",
            "Min.VisibilityMiles","Max.Wind.SpeedMPH","Max.Gust.SpeedMPH")
final <- final[ , !(names(final) %in% remove)]

final$co2_per_sqft <- final$total_co2 / final$square_foot

#### Training and Testing Split
final.train <- final[final$Date < "2016-01-01",]
final.test <- final[final$Date >= "2016-01-01",]

#### Remove Unnecessary Things from Environment AND Save ####
rm(list=setdiff(ls(), c("final", "final.train", "final.test")))
save.image("sustain_read.RData")