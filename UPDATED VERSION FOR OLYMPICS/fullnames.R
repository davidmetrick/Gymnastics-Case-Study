# Take names from https://en.wikipedia.org/wiki/List_of_gymnasts_at_the_2024_Summer_Olympics and turn them into CSVs, then manipulate names to match what we did in prep.data (see example below with men's team competitors - not finished cause FRED RICHARD needs to become FREDERICK RICHARD etc)

library(stringi)

men_teams_names <- read.csv("Men Teams Names.csv")
men_teams_names <- men_teams_names[,2]
men_teams_names <- stri_trans_general(men_teams_names, "Latin-ASCII")
men_teams_names <- gsub("[[:punct:]]", "", men_teams_names)
men_teams_names <- toupper(men_teams_names)
