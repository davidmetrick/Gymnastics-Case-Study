# Silencing some annoying messages
options("dplyr.summarise.inform" = F)
library(tidyr)

# Choosing 12 countries for the men and women
# For the men, USA was originally between ITA and ESP; moving them to the front to allow for the simulation algorithm to select them last

countries_men <- c('USA', 'CHN', 'JPN', 'GBR', 'ITA', 'ESP',
                   'BRA', 'KOR', 'GER', 'CAN', 'TUR', 'HUN')

countries_women <- c('USA', 'GBR', 'CAN', 'BRA', 'ITA', 'CHN',
                     'JPN', 'FRA', 'NED', 'HUN', 'ROU', 'BEL')

# Create combined tables with avg/sd scores for each apparatus for each athlete

men_df <- data_2223 %>% 
  select(FirstName, LastName, Gender, Country, Apparatus, Score) %>%
  drop_na() %>%
  filter(Gender == 'm', Country %in% countries_men) %>%
  group_by(FirstName, LastName, Country, Apparatus) %>% 
  summarize(avg_score = mean(Score,na.rm=T),
            var_score = ifelse(is.na(var(Score)),0,sqrt(var(Score))),
            Country=Country[1]) %>%
  arrange(var_score) %>%
  head(-floor(nrow(.)/15)) %>% mutate(fullname=paste(FirstName,LastName))

women_df <- data_2223 %>% 
  filter(Gender == 'w', Country %in% countries_women) %>%
  group_by(FirstName, LastName, Country, Apparatus) %>% 
  summarize(avg_score = mean(Score,na.rm=T),
            var_score = ifelse(is.na(var(Score)),0,sqrt(var(Score))),
            Country=Country[1]) %>%
  arrange(var_score) %>%
  head(-floor(nrow(.)/15))%>% mutate(fullname=paste(FirstName,LastName))

#######
men_all_df = data_2223 %>% 
  select(FirstName, LastName, Gender, Country, Apparatus, Score) %>%
  drop_na() %>%
  filter(Gender == 'm') %>%
  group_by(FirstName, LastName, Country, Apparatus) %>% 
  summarize(avg_score = mean(Score,na.rm=T),
            var_score = ifelse(is.na(var(Score)),0,sqrt(var(Score))),
            Country=Country[1]) %>%
  arrange(var_score) %>%
  head(-floor(nrow(.)/15))

men_others =men_all_df%>% group_by(FirstName,LastName,Country) %>% 
  summarise(Apparatus = "AA",avg_score=sum(avg_score),var_score=0)%>%
  filter(!(Country %in% countries_men))%>%
  arrange(avg_score)%>%head(36)%>% select(FirstName,LastName) %>%left_join(men_all_df)


women_all_df = data_2223 %>% 
  select(FirstName, LastName, Gender, Country, Apparatus, Score) %>%
  drop_na() %>%
  filter(Gender == 'w') %>%
  group_by(FirstName, LastName, Country, Apparatus) %>% 
  summarize(avg_score = mean(Score,na.rm=T),
            var_score = ifelse(is.na(var(Score)),0,sqrt(var(Score))),
            Country=Country[1]) %>%
  arrange(var_score) %>%
  head(-floor(nrow(.)/15))

women_others = women_all_df%>% group_by(FirstName,LastName,Country) %>% 
  summarise(Apparatus = "AA",avg_score=sum(avg_score),var_score=0)%>%
  filter(!(Country %in% countries_women))%>%
  arrange(avg_score)%>%head(36)%>% select(FirstName,LastName) %>%left_join(women_all_df)


########
men_df_composite_top5 <- men_df%>% group_by(FirstName,LastName,Country) %>% 
  summarise(Apparatus = "AA",avg_score=sum(avg_score),var_score=0) %>%
  group_by(Country) %>%slice_max(avg_score,n=3,with_ties = F) %>%
  mutate(fullname = paste(FirstName, LastName))

women_df_composite_top5 <- women_df%>% group_by(FirstName,LastName,Country) %>% 
  summarise(Apparatus = "AA",avg_score=sum(avg_score),var_score=0) %>%
  group_by(Country) %>%slice_max(avg_score,n=3,with_ties = F) %>%
  mutate(fullname = paste(FirstName, LastName))

men_top5 <- men_df %>% 
  group_by(Country, Apparatus) %>%
  slice_max(avg_score, n = 3, with_ties = F) %>% ungroup() %>%
  mutate(fullname = paste(FirstName, LastName))

men_top5_names <- men_top5 %>% rbind(men_df_composite_top5)%>%
  select(FirstName, LastName, Country) %>% unique()

women_top5 <- women_df %>% 
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

team_pick <- function(country_df, others_df){
  # country_df = data frame with scores and apparatus for All athletes from chosen country
  # others_df = data frame with scores and apparatus for 5 athletes from all other teams
  
  c <- country_df[1, "Country"] %>% as.character()
  # Get all combinations of 5 athletes
  comb <- country_df%>% 
    select(FirstName, LastName) %>%
    mutate(name = paste(FirstName, LastName)) %>%
    unique() %>% select(name) %>% unlist()%>% combn(5) %>% data.frame()
  
  print(ncol(comb))
  # Data frame to give expected number of medals for each combination
  # --------SC-------
  # I'm going to comment this out - keeping track of best instead of having another large df  might be better
  if (c == "USA") { # Need to get this working for next part (deliverable)
    usa_df <<- data.frame(matrix(nrow = nrow(comb), ncol = 2))
    
    colnames(usa_df) <- c('comb', 'medals')
  }
  
  # Run each combination through function to get # of medals
  bestmedals <- 0
  bestcomb<-comb[1]
  
  for (i in 1:ncol(comb)){
    cat(i)
    # The lineup for qualifying is the 5 chosen from comb[i] + the provided 11 other teams
    # renaming this qual for later
    
    qual <- rbind(country_df %>% filter(fullname %in% unlist(comb[i])), 
                    others_df)
    
    # First, simulate qualifier round a lot of times 
    # (Ideally this would be a big number but keeping it at 10 for time interest)
    n <- 10
    
    #check comb contains at least 1 top 1
    if(!contains_top_1(comb[i],country_df,"women")){
      cat("excl")
      next
    }
    
    # Make dataframe of how many medals are earned in each simulation
    ## --------SC-------
    # I'm replacing dataframe with single vector we add to add end of loop
    medals <- rep(0,9)
    names(medals) <- c('team_g', 'team_s', 'team_b',
                          'aa_g', 'aa_s', 'aa_b',
                          'event_g', 'event_s', 'event_b')
   
    # Repeat lots of times!!
    for (j in 1:n){
      # Simulate everyone's score for qualifier round
      # --------SC-------
      # We only need to add one column, we don't need to create a whole new df
      # before roster is what qual is in above
      #qual <- qual %>% mutate(sim = rnorm(n(), avg_score, var_score))
      qual$sim = rnorm(nrow(qual), qual$avg_score, qual$var_score)
      # Team round:
      # Choose best 3 for apparatus (Dumb way of picking top 3 for now)
      # --------SC-------
      # slice_max should be faster than a full sort
      team_comp <- qual %>% group_by(Country, Apparatus) %>%slice_max(sim, n = 3, with_ties = F) %>% group_by(Country) %>% 
         summarize(composite = sum(sim)) %>% ungroup()%>% slice_max(composite,n=8,with_ties = F)#arrange(desc(composite)) %>% head(8)
      # Individual All-around: 
      # 24 gymnasts with the best cumulative individual scores from qualifying 
      # No more than two gymnasts from any one country 
      # --------SC-------
      # slice_max should be faster than a full sort
      ind_aa <- qual %>% group_by(fullname, Country) %>%
        summarize(cumulative = sum(sim)) %>%
        ungroup() %>% 
        group_by(Country) %>% # no more than 2 per country
        slice_max(cumulative, n = 2, with_ties = F) %>% ungroup() %>%
        slice_max(cumulative,n=24,with_ties = F)
        # arrange(desc(cumulative)) %>%
        # head(24)
    # Event finals: best 8 for each apparatus, no more than 2 per country
    event_fin <- qual %>% group_by(Country, Apparatus) %>% 
      slice_max(sim, n = 2, with_ties = F) %>% ungroup() %>%
      group_by(Apparatus) %>%
      slice_max(sim, n = 8, with_ties = F) %>% mutate(place = row_number())
    # Save row with number of medals won in this trial
    # team_gold team_silv team_bronze
    # aa_g aa_s aa_b 
    # event_g event_s event_b (summing all apparatuses for now since it doesn't matter)
    
    # --------SC-------
    # Only need to filter by country once, don't need to do it 3 times
    country_event_fin = event_fin %>% filter(Country == c)
    country_event_fin = country_event_fin %>% filter(place<=3)
    # --------SC-------
    # just checking all medals at the same time
    medals <- medals + c(as.numeric(team_comp[1:3,'Country']==c),
                         as.numeric(ind_aa[1:3,'Country']==c),
                    nrow(country_event_fin%>% filter(place == 1)),
                    nrow(country_event_fin %>% filter(place == 2)),
                    nrow(country_event_fin %>% filter(place == 3)))
    }
    
    if (c == "USA") {
      usa_df[i,1] <- comb[i]
      usa_df[i,2] <- sum(medals/n)
    }
    # Average up medals over n trials to get expected # of medals
    if( sum(medals/n) > bestmedals){
      bestmedals <- sum(medals/n) 
      bestcomb = comb[i]
    }
  }
  # returns dataframe of ideal team
  # Right now just summing up total expected medals but could put in weights
  # (weight G/S/B differently and/or weight team/AA/event differently )
  return(bestcomb)
}

##########################

# Time to pick!

# Initialize teams randomly
random_teams <- men_top5_names %>% group_by(Country) %>% sample_n(5) %>%
 left_join(men_df) %>% arrange(FirstName, LastName)

team_roster <- random_teams
men_others
# Loop over countries one by one and go through each combination
for (country in rep(rev(countries_men), 2)){
  ptm<-proc.time()
  print(country)
  ptm<-proc.time()
  #current <- men_top5 %>%ungroup() %>%filter(Country == country) 
  current <- men_top5_names %>% filter(Country==country) %>%
    left_join(men_df)
  
  other_teams <- rbind(team_roster %>% filter(Country != country),men_others)
  print(other_teams)
  best_team <- team_pick(current, other_teams)

  # Update team list with optimal team
  team_roster <- rbind(other_teams,
                       men_df %>% filter(fullname %in% unlist(best_team)))
  print(proc.time()-ptm)
}
##########################

# Time to pick!

# Initialize teams randomly
random_teams <- women_top5_names %>% group_by(Country) %>% sample_n(5) %>% 
  left_join(women_df) %>% arrange(FirstName, LastName)

team_roster <- random_teams

# Loop over countries one by one and go through each combination 
for (country in rep(rev(countries_women), 2)) {
  ptm<-proc.time()
  print(country)
  #current <- women_top5 %>% filter(Country == country)
  current <- women_top5_names %>% filter(Country==country) %>%
    left_join(women_df)
  other_teams <- team_roster %>% filter(Country != country)
  best_team <- team_pick(current, other_teams)
  print(as.vector(best_team))
  # Update team list with optimal team
  team_roster <- rbind(other_teams, 
                       women_df%>% filter(fullname %in% unlist(best_team)))
  print(proc.time()-ptm)
}


