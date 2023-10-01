# Silencing some annoying messages
options("dplyr.summarise.inform" = F)

# Choosing 12 countries for the men and women
countries_men <- c('CHN', 'JPN', 'GBR', 'ITA', 'USA', 'ESP',
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
            var_score = ifelse(is.na(var(Score)),0,var(Score)),
            Country=Country[1]) %>%
  arrange(var_score) %>%
  head(-floor(nrow(.)/15))

women_df <- data_2223 %>% 
  filter(Gender == 'w', Country %in% countries_women) %>%
  group_by(FirstName, LastName, Country, Apparatus) %>% 
  summarize(avg_score = mean(Score,na.rm=T),
            var_score = ifelse(is.na(var(Score)),0,var(Score)),
            Country=Country[1]) %>%
  arrange(var_score) %>%
  head(-floor(nrow(.)/15))

########

men_top5 <- men_df %>% 
  group_by(Country, Apparatus) %>%
  slice_max(avg_score, n = 5, with_ties = F) %>% ungroup() %>%
  mutate(fullname = paste(FirstName, LastName))

men_top5_names <- men_top5 %>%
  select(FirstName, LastName, Country) %>% unique()

women_top5 <- women_df %>% 
  group_by(Country, Apparatus) %>%
  slice_max(avg_score, n = 5, with_ties = F) %>% ungroup() 

women_top5_names <- women_top5 %>% 
  select(FirstName, LastName, Country) %>% unique()

table(men_top5_names$Country)
table(women_top5_names$Country)


##################

set.seed(123)

team_pick <- function(country_df, others_df){
  # country_df = data frame with scores and apparatus for All athletes from chosen country
  # others_df = data frame with scores and apparatus for 5 athletes from all other teams
  
  c <- country_df[1, "Country"] %>% as.character()
  
  # Get all combinations of 5 athletes
  comb <- country_df %>% 
    select(FirstName, LastName) %>%
    mutate(name = paste(FirstName, LastName)) %>%
    unique() %>% select(name) %>% unlist() %>% combn(5) %>% data.frame()
  
  # Data frame to give expected number of medals for each combination
  medal_scores <- data.frame(matrix(nrow = nrow(comb), ncol = 9))
  colnames(medal_scores) <- c('team_g', 'team_s', 'team_b',
                        'aa_g', 'aa_s', 'aa_b',
                        'event_g', 'event_s', 'event_b')
  
  # Run each combination through function to get # of medals
  for (i in 1:ncol(comb)){
    cat(i)
    
    # The lineup is the 5 chosen from comb[i] + the provided 11 other teams
    roster <- rbind(men_top5 %>% filter(fullname %in% unlist(comb[i])), 
                    others_df)
    
    # First, simulate qualifier round a lot of times 
    # (Ideally this would be a big number but keeping it at 10 for time interest)
    n <- 10
    
    # Make dataframe of how many medals are earned in each simulation
    medals <- data.frame(matrix(nrow = n, ncol = 9))
    colnames(medals) <- c('team_g', 'team_s', 'team_b',
                          'aa_g', 'aa_s', 'aa_b',
                          'event_g', 'event_s', 'event_b')
    
    # Repeat lots of times!!
    for (j in 1:n){
      # Simulate everyone's score for qualifier round
      qual <- roster %>% ungroup() %>% mutate(sim = rnorm(n(), avg_score, sqrt(var_score))) 
      
      # Team round:
      # Choose best 3 for apparatus (Dumb way of picking top 3 for now)
      team_comp <- qual %>% 
        group_by(Country, Apparatus) %>% 
        slice_max(sim, n = 3, with_ties = F) %>% group_by(Country) %>% 
         summarize(composite = sum(sim)) %>% arrange(desc(composite)) %>% head(8)
      
      # Individual All-around: 
      # 24 gymnasts with the best cumulative individual scores from qualifying 
      # No more than two gymnasts from any one country 
      ind_aa <- qual %>% group_by(FirstName, LastName, Country) %>%
        summarize(cumulative = sum(sim)) %>%
        ungroup() %>% 
        group_by(Country) %>% # no more than 2 per country
        slice_max(cumulative, n = 2, with_ties = F) %>%
        arrange(desc(cumulative)) %>%
        head(24)
      
    # Event finals: best 8 for each apparatus, no more than 2 per country
    event_fin <- qual %>% group_by(Country, Apparatus) %>% 
      slice_max(sim, n = 2, with_ties = F) %>% ungroup() %>%
      group_by(Apparatus) %>%
      slice_max(sim, n = 8, with_ties = F) %>% mutate(place = row_number())
    
    # Save row with number of medals won in this trial
    # team_gold team_silv team_bronze
    # aa_g aa_s aa_b 
    # event_g event_s event_b (summing all apparatuses for now since it doesn't matter)
    
    medals[j,] <- c(team_comp[1, 'Country'] == c,
                    team_comp[2, 'Country'] == c,
                    team_comp[3, 'Country'] == c,
                    ind_aa[1, 'Country'] == c,
                    ind_aa[2, 'Country'] == c,
                    ind_aa[3, 'Country'] == c,
                    nrow(event_fin %>% filter(Country == c, place == 1)),
                    nrow(event_fin %>% filter(Country == c, place == 2)),
                    nrow(event_fin %>% filter(Country == c, place == 3)))
    }
    
    # Average up medals over n trials to get expected # of medals
    medal_scores[i,] <- colMeans(medals)
    
  }
  # returns dataframe of ideal team
  # Right now just summing up total expected medals but could put in weights
  # (weight G/S/B differently and/or weight team/AA/event differently )
  return(comb[which.max(rowSums(medal_scores))])
}

##########################

# Time to pick!

# Initialize teams randomly
random_teams <- men_top5_names %>% group_by(Country) %>% sample_n(5) %>% 
 left_join(men_top5) %>% arrange(FirstName, LastName)

team_roster <- random_teams

# Loop over countries one by one and go through each combination 
for (country in countries_men){
  print(country)
  current <- men_top5 %>% filter(Country == country)
  other_teams <- team_roster %>% filter(Country != country)
  best_team <- team_pick(current, other_teams)
  
  # Update team list with optimal team
  team_roster <- rbind(other_teams, 
                       men_top5 %>% filter(fullname %in% unlist(best_team)))
  
}


