# Rice Data

library(readxl)

rice_e <- read_excel('Rice Hall 0214.xlsx', sheet = 1) # Rice Hall Electricity

rice_hw <- read_excel('Rice Hall 0214.xlsx', sheet = 2) # Rice Hall Hot Water

rice_cw <- read_excel('Rice Hall 0214.xlsx', sheet = 3) # Rice Hall Chilled Water

