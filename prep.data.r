## prep.data.r

## This script prepares the data from the csv files for modeling

## First, deduplicate names, clean dates, etc

## Then, create some predictors

## etc (add more subheadings for other main tasks)

# Separate data frames for men and women
# Separate by apparatus
# Remove duplicate rows
# Clean up names to match up athletes
# Clean up apparatus names (VT1 VT_1)
# Separate qualifying & final (or superscore, etc)
# Do basic analysis based on the team

data_2223
data_2223$Apparatus <- gsub("hb", "HB", data_2223$Apparatus)
data_2223$Apparatus <- gsub("VT_1", "VT1", data_2223$Apparatus)
data_2223$Apparatus <- gsub("VT_2", "VT2", data_2223$Apparatus)
  
men <- data_2223[data_2223$Gender == "m",]
women <- data_2223[data_2223$Gender == "w",]

hb <- men[men$Apparatus == "HB",]
ph <- men[men$Apparatus == "PH",]
fx_m <- men[men$Apparatus == "FX",]
pb <- men[men$Apparatus == "PB",]
sr <- men[men$Apparatus == "SR",]
vt1_m <- men[men$Apparatus == "VT1",]
vt2_m <- men[men$Apparatus == "VT2",]
vt_m <- men[men$Apparatus == "VT",]

bb <- women[women$Apparatus == "BB",]
fx_w <- women[women$Apparatus == "FX",]
ub <- women[women$Apparatus == "UB",]
vt1_w <- women[women$Apparatus == "VT1",]
vt2_w <- women[women$Apparatus == "VT2",]
vt_w <- women[women$Apparatus == "VT",]

