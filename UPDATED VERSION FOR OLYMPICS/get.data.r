## get.data.r
## Read in cleandata from gym2024data repo with github links 
library(readxl)

# read in data_2017_2021.csv 
data_1721 <- read.csv('https://raw.githubusercontent.com/ucsas/gym2024data/main/cleandata/data_2017_2021.csv')

# read in data_2022_2023.csv
data_2223 <- read_excel('GYM data 2022-2024.xlsx')

#combine dfs
#data_2223 <- rbind(data_2223,data_1721) #(uncomment in the future if we deem it necessary)
