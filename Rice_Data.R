# Read Builiding Data

building <- function(path){
  library(readxl)
  
  # Read in Files
  e <- read_excel(path, sheet = 1) # Rice Hall Electricity
  hw <- read_excel(path, sheet = 2) # Rice Hall Hot Water
  cw <- read_excel(path, sheet = 3) # Rice Hall Chilled Water
  
  # Remove incomplete observations
  e <- e[complete.cases(e),]
  hw <- hw[complete.cases(hw),]
  cw <- cw[complete.cases(cw),]
  
  # keep necessary columns
  keep <- c('Timestamp','Min_Value','Min_Timestamp','Max_Value','Max_Timestamp',
            'Avg_Value',"INTERPOLATIVE_Value (Use this value - iit's a better average)",
            'Total_Value')
  
  e <- e[, names(e) %in% keep]
  hw <- hw[, names(hw) %in% keep]
  cw <- cw[, names(cw) %in% keep]
  
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
  
  temp <- merge(e, hw, by = 'Timestamp')
  final <- merge(temp, cw, by = 'Timestamp')
  
  return(final)
}

rice <- building('Rice Hall 0214.xlsx')
