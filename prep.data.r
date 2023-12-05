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

# Remove spaces from names
data_2223$FirstName <- gsub(" ", "", data_2223$FirstName)
data_2223$LastName <- gsub(" ", "", data_2223$LastName)


#Removing all accents
data_2223$LastName <- stri_trans_general(data_2223$LastName, "Latin-ASCII")
data_2223$FirstName <- stri_trans_general(data_2223$FirstName, "Latin-ASCII")

# Remove non A-Z characters from names
data_2223$FirstName <- gsub("[[:punct:]]", "", data_2223$FirstName)
data_2223$LastName <- gsub("[[:punct:]]", "", data_2223$LastName)

# Split when there are spaces in between first names we are only checking when
# first name and last name and country matches for a person
data_2223 = separate(data_2223, FirstName, into = c("FirstName", "OtherName"), 
                     sep = "^\\S*\\K\\s+")

# Convert all names to upper case
data_2223$FirstName = toupper(data_2223$FirstName)
data_2223$LastName = toupper(data_2223$LastName)

# Rename columns so that the same event is grouped together
data_2223$Apparatus <- gsub("hb", "HB", data_2223$Apparatus)
data_2223$Apparatus <- gsub("VT_1", "VT1", data_2223$Apparatus)
data_2223$Apparatus <- gsub("VT_2", "VT2", data_2223$Apparatus)
data_2223$Apparatus <- gsub("VT1", "VT", data_2223$Apparatus)
data_2223$Apparatus <- gsub("VT2", "VT", data_2223$Apparatus)

# Special cases for country
data_2223$Country <- gsub("CCS", "GUA", data_2223$Country)
data_2223$Country <- gsub("ENG", "GBR", data_2223$Country)
data_2223$Country <- gsub("IOM", "GBR", data_2223$Country)
data_2223$Country <- gsub("JEY", "GBR", data_2223$Country)
data_2223$Country <- gsub("SCO", "GBR", data_2223$Country)
data_2223$Country <- gsub("WAL", "GBR", data_2223$Country)
data_2223$Country <- gsub("GE1", "GER", data_2223$Country)
data_2223$Country <- gsub("GE2", "GER", data_2223$Country)
data_2223$Country <- gsub("NIR", "IRL", data_2223$Country) # come back to this if there are new athletes cause they might not all be IRL

# Special cases for individuals
data_2223$LastName[data_2223$LastName=="ODRISCOL"]="ODRISCOLL"
data_2223 = data_2223 %>% 
  mutate(FirstName = ifelse(FirstName=="SAM"&LastName=="ZAKUTNEY","SAMUEL",FirstName))
  
data_2223$outlier <- (data_2223$LastName == "BARBOSA") + (data_2223$FirstName == "JADE")
data_2223[data_2223$outlier == 2,]$Country <- "BRA"
data_2223$outlier <- (data_2223$LastName == "DJORDJEVIC") + (data_2223$FirstName == "DUSAN")
data_2223[data_2223$outlier == 2,]$Country <- "SRB"
# data_2223$outlier <- (data_2223$LastName == "GRÜNBERG") + (data_2223$FirstName == "JERMAIN")
# data_2223[data_2223$outlier == 2,]$Country <- "NED"
data_2223$outlier <- (data_2223$LastName == "JONASSON") + (data_2223$FirstName == "ARNTHOR")
data_2223[data_2223$outlier == 2,]$Country <- "ISL"
# data_2223$outlier <- (data_2223$LastName == "LIMA") + (data_2223$FirstName == "ANA")
# data_2223[data_2223$outlier == 2,]$Country <- "BRA"
# data_2223$outlier <- (data_2223$LastName == "MEULEMAN") + (data_2223$FirstName == "CASEY-JANE")
# data_2223[data_2223$outlier == 2,]$Country <- "NED"
data_2223$outlier <- (data_2223$LastName == "POGHOSYAN") + (data_2223$FirstName == "MANE")
data_2223[data_2223$outlier == 2,]$Country <- "ARM"
data_2223$outlier <- (data_2223$LastName == "RAPOSO") + (data_2223$FirstName == "CLARA")
data_2223[data_2223$outlier == 2,]$Country <- "CAN"
data_2223$outlier <- (data_2223$LastName == "TOVMASYAN") + (data_2223$FirstName == "ARTUR")
data_2223[data_2223$outlier == 2,]$Country <- "ARM"
data_2223$outlier <- (data_2223$LastName == "ZANETTI") + (data_2223$FirstName == "ARTHUR")
data_2223[data_2223$outlier == 2,]$Country <- "BRA"
# data_2223$outlier <- (data_2223$LastName=="CALLUM") + (data_2223$FirstName=="MC")
# data_2223[data_2223$outlier == 1,]$FirstName <- "GRACE"
# data_2223[data_2223$outlier == 1,]$LastName <- "MCCALLUM"
# data_2223$outlier <- (data_2223$FirstName == "CALLUM") + (data_2223$LastName == "MC")
# data_2223[data_2223$outlier == 2,]$FirstName <- "GRACE"
# data_2223[data_2223$outlier == 2,]$LastName <- "MCCALLUM"
data_2223$outlier <- (data_2223$FirstName == "FREDERICKNATHANIEL")
data_2223[data_2223$outlier == 1,]$FirstName <- "FREDERICK"
data_2223$outlier <- (data_2223$FirstName == "FRED") + (data_2223$LastName == "RICHARD")
data_2223[data_2223$outlier == 2,]$FirstName <- "FREDERICK"

data_2223$outlier <- (data_2223$FirstName == "KHOIALEXANDER") + (data_2223$LastName == "YOUNG")
data_2223[data_2223$outlier == 2,]$FirstName <- "KHOI"

data_2223$outlier <- (data_2223$FirstName == "CURRANMICHAEL") + (data_2223$LastName == "PHILLIPS")
data_2223[data_2223$outlier == 2,]$FirstName <- "CURRAN"


data_2223$outlier <- (data_2223$FirstName == "PAULINE") + (data_2223$LastName == "SCHAFER")
data_2223[data_2223$outlier == 2,]$LastName <- "SCHAEFERBETZ"


data_2223$outlier <- (data_2223$FirstName == "ALEXA") + (data_2223$LastName == "MORENOMEDINA")
data_2223[data_2223$outlier == 2,]$LastName <- "MORENO"

data_2223$outlier <- (data_2223$FirstName == "ALEXACITLALI") + (data_2223$LastName == "MORENOMEDINA")
data_2223[data_2223$outlier == 2,]$FirstName <- "ALEXA"
data_2223[data_2223$outlier == 2,]$LastName <- "MORENO"

data_2223$outlier <- (data_2223$FirstName == "NOESAMUEL") + (data_2223$LastName == "SEIFERT")
data_2223[data_2223$outlier == 2,]$FirstName <- "NOE"

data_2223$outlier <- (data_2223$LastName == "HERONSOTO")
data_2223[data_2223$outlier == 1,]$LastName <- "HERON"

data_2223$outlier <- (data_2223$FirstName == "HILLARYALEXANDRA")
data_2223[data_2223$outlier == 1,]$FirstName<- "HILLARY"

data_2223$outlier <- (data_2223$LastName == "OSYSSEKREIMER")
data_2223[data_2223$outlier == 1,]$LastName<- "OSYSSEK"

data_2223$outlier <- (data_2223$FirstName == "AHTZIRIVIRIDIANA")
data_2223[data_2223$outlier == 1,]$FirstName<- "AHTZIRI"

data_2223$outlier <- (data_2223$FirstName == "ANDREIVASILE")
data_2223[data_2223$outlier == 1,]$FirstName<- "ANDREI"

data_2223$outlier <- (data_2223$FirstName == "CARLOS")+ (data_2223$LastName == "YULO")
data_2223[data_2223$outlier == 2,]$FirstName<- "CARLOSEDRIEL"

data_2223$outlier <- (data_2223$FirstName == "MAX")+ (data_2223$LastName == "WHITLOCKOBE")
data_2223[data_2223$outlier == 2,]$LastName<- "WHITLOCK"

data_2223$outlier <- (data_2223$FirstName == "MCRHYS")+ (data_2223$LastName == "CLENAGHAN")
data_2223[data_2223$outlier == 2,]$LastName<- "MCCLENAGHAN"
data_2223[data_2223$outlier == 2,]$FirstName<- "RHYS"

data_2223$outlier <- (data_2223$FirstName == "CARLO")+ (data_2223$LastName == "HORR")
data_2223[data_2223$outlier == 2,]$LastName<- "HOERR"

data_2223$outlier <- (data_2223$FirstName == "LORANDE")+ (data_2223$LastName == "MUNCK")
data_2223[data_2223$outlier == 2,]$FirstName<- "LORAN"
data_2223[data_2223$outlier == 2,]$LastName<- "DEMUNCK"

data_2223$outlier <- (data_2223$LastName == "MUNCK")
data_2223[data_2223$outlier == 1,]$LastName<- "DEMUNCK"

data_2223$outlier <- (data_2223$FirstName == "DOMINIC")+ (data_2223$LastName == "TAMSEL")
data_2223[data_2223$outlier == 2,]$FirstName<- "DOMINIC"


data_2223$outlier <- (data_2223$FirstName == "MEHMETAYBERK")+ (data_2223$LastName == "KOSAK")
data_2223[data_2223$outlier == 2,]$FirstName<- "MEHMET"

data_2223$outlier <- (data_2223$FirstName == "ELSABETH")+ (data_2223$LastName == "BLACK")
data_2223[data_2223$outlier == 2,]$FirstName<- "ELLIE"

data_2223$outlier <- (data_2223$FirstName == "MELANIEDEJESUSDOS")+ (data_2223$LastName == "SANTOS")
data_2223[data_2223$outlier == 2,]$FirstName<- "MELANIE"
data_2223[data_2223$outlier == 2,]$LastName<- "DEJESUSDOSSANTOS"


data_2223$outlier <- (data_2223$FirstName == "VERA")+ (data_2223$LastName == "POL")
data_2223[data_2223$outlier == 2,]$LastName<- "VANPOL"

data_2223$outlier <- (data_2223$FirstName == "EDOARDODE")+ (data_2223$LastName == "ROSA")
data_2223[data_2223$outlier == 2,]$LastName<- "DEROSA"
data_2223[data_2223$outlier == 2,]$FirstName<- "EDOARDO"

data_2223$outlier <- (data_2223$FirstName == "MARTIJN")+ (data_2223$LastName == "VEER")
data_2223[data_2223$outlier == 2,]$LastName<- "DEVEER"



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



# Find athletes with missing countries to fill later by matching with existing athletes
data_2223 = data_2223 %>% group_by(FirstName,LastName) %>% 
  mutate(Country = sort(Country,decreasing=T)[1])



