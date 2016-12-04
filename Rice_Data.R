# Rice Data

library(readxl)

# Read in Files
rice_e <- read_excel('Rice Hall 0214.xlsx', sheet = 1) # Rice Hall Electricity
rice_hw <- read_excel('Rice Hall 0214.xlsx', sheet = 2) # Rice Hall Hot Water
rice_cw <- read_excel('Rice Hall 0214.xlsx', sheet = 3) # Rice Hall Chilled Water

# Remove incomplete observations
rice_e <- rice_e[complete.cases(rice_e),]
rice_hw <- rice_hw[complete.cases(rice_hw),]
rice_cw <- rice_cw[complete.cases(rice_cw),]

# keep necessary columns
keep <- c('Timestamp','Min_Value','Min_Timestamp','Max_Value','Max_Timestamp',
          'Avg_Value',"INTERPOLATIVE_Value (Use this value - iit's a better average)",
          'Total_Value')

rice_e <- rice_e[, names(rice_e) %in% keep]
rice_hw <- rice_hw[, names(rice_hw) %in% keep]
rice_cw <- rice_cw[, names(rice_cw) %in% keep]

# rename columns for ease
oldnames_e <- names(rice_e)
newnames_e <- c('Timestamp','Min_e','Min_Time_e','Max_e','Max_Time_e','Avg_e','Interpolative_e')
setnames(rice_e, oldnames_e, newnames_e)

oldnames_hw <- names(rice_hw)
newnames_hw <- c('Timestamp','Min_hw','Min_Time_hw','Max_hw','Max_Time_hw','Avg_hw','Interpolative_hw')
setnames(rice_hw, oldnames_hw, newnames_hw)

oldnames_cw <- names(rice_cw)
newnames_cw <- c('Timestamp','Min_cw','Min_Time_cw','Max_cw','Max_Time_cw','Avg_cw','Interpolative_cw')
setnames(rice_cw, oldnames_cw, newnames_cw)

"""
read_build <- function(building, path){
  library(readxl)
  
  electric <- read_excel(path, sheet = 1)
  assign(paste(building, 'e', sep='_'), electric)
  return(paste(building, 'e', sep='_'))
  
  hot <- read_excel(path, sheet = 2)
  assign(paste(building, 'hw', sep='_'), hot)
  return(paste(building, 'hw', sep='_'))
  
  chilled <- read_excel(path, sheet = 1)
  assign(paste(building, 'cw', sep='_'), chilled)
  return(paste(building, 'cw', sep='_'))
}

read_build('Rice', 'Rice Hall 0214.xlsx')
"""