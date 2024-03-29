# Silencing some annoying messages
options("dplyr.summarise.inform" = F)

# Choosing 12 countries for the men and women
# For the men, USA was originally between ITA and ESP; moving them to the front to allow for the simulation algorithm to select them last

countries_men <- c('USA', 'CHN', 'JPN', 'GBR', 'ITA', 'ESP',
                   'GER', 'CAN', 'TUR', 'SUI', 'NED', 'UKR')

countries_women <- c('USA', 'GBR', 'CAN', 'BRA', 'ITA', 'CHN',
                     'JPN', 'FRA', 'NED', 'ROU', 'AUS', 'KOR')

# Create combined tables with avg/sd scores for each apparatus for each athlete

men_df <- data_2223 %>% 
  select(FirstName, LastName, Gender, Country, Apparatus, Score) %>%
  drop_na() %>%
  filter(Gender == 'm') %>%
  group_by(FirstName, LastName, Country, Apparatus) %>% 
  summarize(avg_score = mean(Score,na.rm=T),
            var_score = ifelse(is.na(var(Score)),0,sqrt(var(Score))),
            Country=Country[1]) %>%
  arrange(var_score) %>%
  head(-floor(nrow(.)/15)) %>% mutate(fullname=paste(FirstName,LastName))

women_df <- data_2223 %>% 
  filter(Gender == 'w') %>%
  group_by(FirstName, LastName, Country, Apparatus) %>% 
  summarize(avg_score = mean(Score,na.rm=T),
            var_score = ifelse(is.na(var(Score)),0,sqrt(var(Score))),
            Country=Country[1]) %>%
  arrange(var_score) %>%
  head(-floor(nrow(.)/15))%>% mutate(fullname=paste(FirstName,LastName))

#######

women_others_fullnames = c(
  "KAYLIA NEMOUR", "PAULINE SCHAEFERBETZ","ALEXA MORENO",
  "FILIPA MARTINS","ALEAH FINNEGAN", "BETTINALILI CZIFRA",
  "ALBA PETISCO", "ANNA LASHCHEVSKA", "LENA BICKEL",
  "HILLARY HERON","CAITLIN ROOSKRANTZ","LIHIE RAZ","LUCIJA HRIBAR",
  "CSENGEMARIA BACSKAY","AHTZIRI SANDOVAL","ANA PEREZ", "SARAH VOSS",
  "RIFDA IRFANALUTHFI"
)

men_others_fullnames = c(
  "MILAD KARIMI", "ARTEM DOLGOPYAT","ARTUR DAVTYAN","KRISZTOFER MESZAROS",
  "JUNHO LEE","DIOGO SOARES","LUKA VANDENKEYBUS","ANDREI MUNTEAN",
  "CARLOSEDRIEL YULO","RHYS MCCLENAGHAN","ELEFTHERIOS PETROUNIAS",
  "KEVIN PENEV","NOAH KUAVITA","TIN SRBIC"
)

men_all_df = data_2223 %>% 
  select(FirstName, LastName, Gender, Country, Apparatus, Score) %>%
  drop_na() %>%
  filter(Gender == 'm') %>%
  group_by(FirstName, LastName, Country, Apparatus) %>% 
  summarize(avg_score = mean(Score,na.rm=T),
            var_score = ifelse(is.na(var(Score)),0,sqrt(var(Score))),
            Country=Country[1]) %>%
  arrange(var_score) %>%
  mutate(fullname=paste(FirstName,LastName))


men_others = men_all_df %>%
  mutate(fullname=paste(FirstName,LastName)) %>%
  filter(fullname %in% men_others_fullnames)

# men_others =rbind(men_others,
#                     men_all_df%>% group_by(FirstName,LastName,Country) %>%
#                       summarise(Apparatus = "AA",avg_score=sum(avg_score),var_score=0)%>%
#                       mutate(fullname=paste(FirstName,LastName))%>%
#                       filter(!(Country %in% countries_men) & !(fullname %in% men_others_fullnames))%>%
#                       arrange(avg_score)%>%head(36-length(men_others_fullnames))%>% select(FirstName,LastName) %>%left_join(men_all_df)%>%
#                       mutate(fullname=paste(FirstName,LastName)) %>% select(-c("avg_score","var_score"))
# )



women_all_df = data_2223 %>% 
  select(FirstName, LastName, Gender, Country, Apparatus, Score) %>%
  drop_na() %>%
  filter(Gender == 'w') %>%
  group_by(FirstName, LastName, Country, Apparatus) %>% 
  summarize(avg_score = mean(Score,na.rm=T),
            var_score = ifelse(is.na(var(Score)),0,sqrt(var(Score))),
            Country=Country[1]) %>%
  arrange(var_score) %>%
  mutate(fullname=paste(FirstName,LastName))

women_others = women_all_df %>%
  mutate(fullname=paste(FirstName,LastName)) %>%
  filter(fullname %in% women_others_fullnames)

# women_others =rbind(women_others,
#                     women_all_df%>% group_by(FirstName,LastName,Country) %>%
#                       summarise(Apparatus = "AA",avg_score=sum(avg_score),var_score=0)%>%
#                       mutate(fullname=paste(FirstName,LastName))%>%
#                       filter(!(Country %in% countries_women) & !(fullname %in% women_others_fullnames))%>%
#                       arrange(avg_score)%>%head(36-length(women_others_fullnames))%>% select(FirstName,LastName) %>%left_join(women_all_df)%>%
#                       mutate(fullname=paste(FirstName,LastName)) %>% select(-c("avg_score","var_score"))
# )


########
men_df_composite_top5 <- men_df%>% group_by(FirstName,LastName,Country) %>% 
  filter(Country %in% countries_men)%>%
  summarise(Apparatus = "AA", avg_score=sum(avg_score), var_score=0) %>%
  group_by(Country) %>%slice_max(avg_score,n=3,with_ties = F) %>%
  mutate(fullname = paste(FirstName, LastName))

women_df_composite_top5 <- women_df%>%filter(Country %in% countries_women)%>%
  group_by(FirstName,LastName,Country) %>% 
  summarise(Apparatus = "AA",avg_score=sum(avg_score),var_score=0) %>%
  group_by(Country) %>%slice_max(avg_score,n=3,with_ties = F) %>%
  mutate(fullname = paste(FirstName, LastName))

men_top5 <- men_df %>% filter(Country %in% countries_men)%>%
  group_by(Country, Apparatus) %>%
  slice_max(avg_score, n = 3, with_ties = F) %>% ungroup() %>%
  mutate(fullname = paste(FirstName, LastName))

men_top5_names <- men_top5 %>% rbind(men_df_composite_top5)%>%
  select(FirstName, LastName, Country) %>% unique()

women_top5 <- women_df %>%  filter(Country %in% countries_women)%>%
  group_by(Country, Apparatus) %>%
  slice_max(avg_score, n = 4, with_ties = F)%>% ungroup()%>%
  mutate(fullname = paste(FirstName, LastName))

women_top5_names <- women_top5 %>% rbind(women_df_composite_top5)%>%
  select(FirstName, LastName, Country) %>% unique()

table(men_top5_names$Country)
table(women_top5_names$Country)

# --------SC-------
# If a combination contains 0 top 1 athletes for any event, exclude it
# we can change this to say how many top 1 athletes we require for a valid
# comb
contains_top_1<-function(comb,country_df,type="men"){
  if(type=="men"){
    min=1
  }else{
    min=1
  }
  top_1s = (country_df %>%group_by(Apparatus)%>%slice_max(avg_score,n=1,with_ties = F)%>%
              ungroup()%>%select(fullname) %>% unique())[['fullname']]
  return(length(intersect(as.vector(unlist(comb)),top_1s))>=min)
}

##################

set.seed(123)

source('teampick.R')

weights_list <- list(c(0,0,1), c(0,1,0), c(1,0,0), c(0,1,1), c(1,0,1), c(1,1,0), c(0,1,2), c(0,2,1), c(1,0,2), c(1,2,0), c(2,0,1), c(2,1,0), c(1,1,1), c(1,1,2), c(1,2,1), c(2,1,1), c(1,2,2),c(2,1,2), c(2,2,1))

priority_list <- list(c(1,1,1), c(1,0,0), c(3,2,1))

weight_vectors <- list()
index <- 1

for (i in (1:19)) {
  for (j in (1:3)) {
    weight_vectors[[index]] <- as.vector(outer(priority_list[[j]], weights_list[[i]],"*"))
    index <- index + 1
  }
}

weight_vectors2 <- weight_vectors[35:57]

##########################

# Time to pick!

# Initialize teams randomly
random_teams <- men_top5_names %>% group_by(Country) %>% sample_n(5) %>%
  left_join(men_df) %>% arrange(FirstName, LastName)

team_roster <- rbind(random_teams, men_others)
men_others
tm = Sys.time()
index <- 1
# Loop over countries one by one and go through each combination
for(weight_vector in weight_vectors){
  print(weight_vector)
  for (country in rep(rev(countries_men), 2)){
    print(c(country,index))
    tm2 = Sys.time()
    #current <- men_top5 %>%ungroup() %>%filter(Country == country) 
    current <- men_top5_names %>% filter(Country==country) %>%
      left_join(men_df)
    
    other_teams <- team_roster %>% filter(Country != country)
    best_team <- team_pick(current, other_teams, weights = weight_vector,
                           gender='m')
    print(best_team)
    # Update team list with optimal team
    team_roster <- rbind(other_teams,
                         men_df %>% filter(fullname %in% unlist(best_team)))
    print(Sys.time()-tm2)
  }
  print(Sys.time()-tm)
  index <- index + 1
}



##########################

# Time to pick!
source('./teampick.R')
# Initialize teams randomly
random_teams <- women_top5_names %>% group_by(Country) %>% sample_n(5) %>% 
  left_join(women_df) %>% arrange(FirstName, LastName)
team_roster <- rbind(random_teams, women_others)
tm = Sys.time()
index <- 1
# Loop over countries one by one and go through each combination 
for(weight_vector in weight_vectors){
  for (country in rep(rev(countries_women),2)){
    tm2 = Sys.time()
    print(c(country,index))
    #current <- women_top5 %>% filter(Country == country)
    current <- women_top5_names %>% filter(Country==country) %>%
      left_join(women_df)
    other_teams <- team_roster %>% filter(Country != country)
    best_team <- team_pick(current, other_teams, gender='w',
                           weights = weight_vector)
    print(as.vector(best_team))
    # Update team list with optimal team
    team_roster <- rbind(other_teams, 
                         women_df%>% filter(fullname %in% unlist(best_team)))
    print(Sys.time()-tm2)
  }
  print(Sys.time()-tm)
  index <- index + 1
}
