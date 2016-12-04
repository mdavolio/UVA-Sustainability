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