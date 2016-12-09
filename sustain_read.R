#####################################
########## sustain_read.R ###########
#####################################

# Data Mining SYS 6018 Final Project
# Code to read in and clean/wrangle the data into a useable format

library(readxl)
library(dplyr)
library(purrr)
library(data.table)

#### Round Timestamps ####

# Function to rounds timestamps to nearest quarter hour

ts_round <- function(df){
  df$Timestamp <- as.POSIXct(round(as.numeric(strptime(df$Timestamp, 
                                                       '%Y-%m-%d %H:%M:%S'))/900) * 900, origin='1970-01-01')
  df$DayOfWeek<-as.POSIXlt(df$Timestamp)$wday + 1
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
  
  x <- as.numeric(as.POSIXct(df$Timestamp, '%Y-%m-%d %H:%M:%S'))
  
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

##### Read Builiding Energy Data  #####


building <- function(path){
  
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


rice <- building('Rice Hall 0214.xlsx') %>%
  mutate(buildingID = '0214') %>% 
  merge(buildings, by = "buildingID", all.x = TRUE) %>% 
  ts_round() %>%
  session()
echols <- building('Echols 2213.xlsx') %>% 
  mutate(buildingID = '2213') %>% 
  merge(buildings, by = "buildingID", all.x = TRUE) %>% 
  ts_round() %>%
  session()
humphreys <- building('Humphreys 2214.xlsx') %>% 
  mutate(buildingID = '2214') %>% 
  merge(buildings, by = "buildingID", all.x = TRUE) %>% 
  ts_round() %>%
  session()
kellogg <- building('Kellogg 2368.xlsx') %>% 
  mutate(buildingID = '2368') %>% 
  merge(buildings, by = "buildingID", all.x = TRUE) %>% 
  ts_round() %>%
  session()
oHill_Din <- building('Ohill Dining 0201.xlsx') %>% 
  mutate(buildingID = '0201') %>% 
  merge(buildings, by = "buildingID", all.x = TRUE) %>% 
  ts_round()%>%
  session()
physics <- building('Physics 0221.xlsx') %>% 
  mutate(buildingID = '0221') %>% 
  merge(buildings, by = "buildingID", all.x = TRUE) %>% 
  ts_round() %>%
  session()
afc <- building('AFC 5271.xlsx') %>% 
  mutate(buildingID = '5271') %>% 
  merge(buildings, by = "buildingID", all.x = TRUE) %>% 
  ts_round() %>%
  session()
gilmer <- building('Gilmer 0210.xlsx') %>% 
  mutate(buildingID = '0210') %>% 
  merge(buildings, by = "buildingID", all.x = TRUE) %>% 
  ts_round() %>%
  session()
gooch <- building('Gooch 382 2382.xlsx') %>% 
  mutate(buildingID = '2382') %>% 
  merge(buildings, by = "buildingID", all.x = TRUE) %>% 
  ts_round() %>%
  session()
mechEng <- building('Mech Eng 0259.xlsx') %>% 
  mutate(buildingID = '0259') %>% 
  merge(buildings, by = "buildingID", all.x = TRUE) %>% 
  ts_round() %>%
  session()
matSci <- building('Materials Science 0270.xlsx') %>% 
  mutate(buildingID = '0270') %>% 
  merge(buildings, by = "buildingID", all.x = TRUE) %>% 
  ts_round() %>%
  session()
pav <- building('Pav VII 0022.xlsx') %>% 
  mutate(buildingID = '0022') %>% 
  merge(buildings, by = "buildingID", all.x = TRUE) %>% 
  ts_round() %>%
  session()
  
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
  
  return(merged) 
}

weather <- grabWeather('OA Data.xlsx')
