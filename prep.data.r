## prep.data.r

## This script prepares the data from the csv files for modeling

## First, deduplicate names, clean dates, etc

## Then, create some predictors

## etc (add more subheadings for other main tasks)

# Separate data frames for men and women DONE
# Separate by apparatus DONE
# Remove duplicate rows
# Clean up names to match up athletes
# Clean up apparatus names (VT1 VT_1) DONE
# Separate qualifying & final (or superscore, etc)
# Do basic analysis based on the team
library(dplyr)

data_2223

# Rename columns so that the same event is grouped together
data_2223$Apparatus <- gsub("hb", "HB", data_2223$Apparatus)
data_2223$Apparatus <- gsub("VT_1", "VT1", data_2223$Apparatus)
data_2223$Apparatus <- gsub("VT_2", "VT2", data_2223$Apparatus)
  
# Split data by gender
men <- data_2223[data_2223$Gender == "m",]
women <- data_2223[data_2223$Gender == "w",]

# Further split data by event (so split is now by event by gender)
apparatus_list_men = sort(unique(men$Apparatus))
apparatus_group_men = men %>% group_by(Apparatus)
events_m = group_split(apparatus_group_men)
for(event in events_m){
  assign(paste(event$Apparatus[1],"m",sep="_"), event)
}

apparatus_list_women = sort(unique(women$Apparatus))
apparatus_group_women = women %>% group_by(Apparatus)
events_w = group_split(apparatus_group_women)
for(event in events_w){
  assign(paste(event$Apparatus[1],"w",sep="_"), event)
}

# the names of the events for the different genders, we access the event data
# by gender by indicating the event and gender e.g. 'VT_w'
apparatus_list_men
apparatus_list_women


