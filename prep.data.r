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
library(tidyr)
library(stringi)
library(stringr)
library(lubridate)

data_2223


# Split when there are spaces in between first names we are only checking when
# first name and last name and country matches for a person
data_2223 = separate(data_2223, FirstName, into = c("FirstName", "OtherName"), 
                     sep = "^\\S*\\K\\s+")

#Removing all accents
apply(data_2223,2,function(x) stringi::stri_trans_general(x, "Latin-ASCII") )

# Convert all names to upper case
data_2223$FirstName = toupper(data_2223$FirstName)
data_2223$LastName = toupper(data_2223$LastName)

# Rename columns so that the same event is grouped together
data_2223$Apparatus <- gsub("hb", "HB", data_2223$Apparatus)
data_2223$Apparatus <- gsub("VT_1", "VT1", data_2223$Apparatus)
data_2223$Apparatus <- gsub("VT_2", "VT2", data_2223$Apparatus)
data_2223$Apparatus <- gsub("VT1", "VT", data_2223$Apparatus)
data_2223$Apparatus <- gsub("VT2", "VT", data_2223$Apparatus)

data_2223$Country <- gsub("CCS", "GUA", data_2223$Country)
data_2223$Country <- gsub("ENG", "GBR", data_2223$Country)
data_2223$Country <- gsub("IOM", "GBR", data_2223$Country)
data_2223$Country <- gsub("JEY", "GBR", data_2223$Country)
data_2223$Country <- gsub("SCO", "GBR", data_2223$Country)
data_2223$Country <- gsub("WAL", "GBR", data_2223$Country)
data_2223$Country <- gsub("GE1", "GER", data_2223$Country)
data_2223$Country <- gsub("GE2", "GER", data_2223$Country)
data_2223$Country <- gsub("NIR", "IRL", data_2223$Country) # come back to this if there are new athletes cause they might not all be IRL

data_2223$outlier <- (data_2223$LastName == "BARBOSA") + (data_2223$FirstName == "JADE")
data_2223[data_2223$outlier == 2,]$Country <- "BRA"
data_2223$outlier <- (data_2223$LastName == "DJORDJEVIC") + (data_2223$FirstName == "DUSAN")
data_2223[data_2223$outlier == 2,]$Country <- "SRB"
data_2223$outlier <- (data_2223$LastName == "GRÜNBERG") + (data_2223$FirstName == "JERMAIN")
data_2223[data_2223$outlier == 2,]$Country <- "NED"
data_2223$outlier <- (data_2223$LastName == "JONASSON") + (data_2223$FirstName == "ARNTHOR")
data_2223[data_2223$outlier == 2,]$Country <- "ISL"
data_2223$outlier <- (data_2223$LastName == "LIMA") + (data_2223$FirstName == "ANA")
data_2223[data_2223$outlier == 2,]$Country <- "BRA"
data_2223$outlier <- (data_2223$LastName == "MEULEMAN") + (data_2223$FirstName == "CASEY-JANE")
data_2223[data_2223$outlier == 2,]$Country <- "NED"
data_2223$outlier <- (data_2223$LastName == "POGHOSYAN") + (data_2223$FirstName == "MANE")
data_2223[data_2223$outlier == 2,]$Country <- "ARM"
data_2223$outlier <- (data_2223$LastName == "RAPOSO") + (data_2223$FirstName == "CLARA")
data_2223[data_2223$outlier == 2,]$Country <- "CAN"
data_2223$outlier <- (data_2223$LastName == "TOVMASYAN") + (data_2223$FirstName == "ARTUR")
data_2223[data_2223$outlier == 2,]$Country <- "ARM"
data_2223$outlier <- (data_2223$LastName == "ZANETTI") + (data_2223$FirstName == "ARTHUR")
data_2223[data_2223$outlier == 2,]$Country <- "BRA"
data_2223 <- subset(data_2223, select = -outlier)


#Remove all duplicated rows
data_2223 = distinct(data_2223)

# Extract start dates and end dates
convertDate <- function(daterange){
  daterange = gsub(",","",daterange)
  dates = str_split(daterange,"-",simplify = T)
  endDate = dmy(trimws(dates[2]))
  start = str_split(trimws(dates[1])," ")[[1]]
  if(length(start) ==1){
    #print(paste(dates[1],format(as.Date(endDate, format="%d-%m-%Y"),"%m-%Y"),sep="-"))
    startDate = dmy(paste(dates[1],format(as.Date(endDate, format="%d-%m-%Y"),"%m-%Y"),sep="-"))
  }else if(length(start)==2){
    startDate = dmy(paste(dates[1],format(as.Date(endDate, format="%d-%m-%Y"),"%Y"),sep="-"))
  }else{
    startDate = dmy(dates[1])
  }
  return(c(paste(startDate),paste(endDate)))
}
# startDates = unlist(lapply(data_2223$Date,function(x) convertDate(x)[1]),use.names = F)
# endDates = unlist(lapply(data_2223$Date,function(x) convertDate(x)[2]),use.names = F)
# data_2223$StartDate = startDates
# data_2223$EndDate = endDates


# Split data by gender
men <- data_2223[data_2223$Gender == "m",]
women <- data_2223[data_2223$Gender == "w",]

# Further split data by event (so split is now by event by gender)
apparatus_men = sort(unique(men$Apparatus))
apparatus_group_men = men %>% group_by(Apparatus)
events_m = group_split(apparatus_group_men)
for(event in events_m){
  assign(paste(event$Apparatus[1],"m",sep="_"), event)
}

apparatus_women = sort(unique(women$Apparatus))
apparatus_group_women = women %>% group_by(Apparatus)
events_w = group_split(apparatus_group_women)
for(event in events_w){
  assign(paste(event$Apparatus[1],"w",sep="_"), event)
}

# the names of the events for the different genders, we access the event data
# by gender by indicating the event and gender e.g. 'VT_w'
apparatus_men
apparatus_women


# Find athletes with missing countries to fill later by matching with existing athletes
data_2223 = data_2223 %>% group_by(FirstName,LastName) %>% 
  mutate(Country = sort(Country,decreasing=T)[1])

data_2223 |> filter(Country == '') |> select(LastName, FirstName, Country) |>
  unique()
