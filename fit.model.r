## fit.model.r
## use cleaned dataset to create scree plot, tier lists, interactive team choices, etc.

## first step: make model that finds the average score for each person on each individual apparatus.

library(dplyr)
library(purrr)
library(tidyr)
library(kit)
options(dplyr.summarise.inform = FALSE)

# Choosing 12 countries for the men and women
countries_men <- c('CHN', 'JPN', 'GBR', 'ITA', 'USA', 'ESP',
                   'BRA', 'KOR', 'GER', 'CAN', 'TUR', 'HUN')

countries_women <- c('USA', 'GBR', 'CAN', 'BRA', 'ITA', 'CHN',
                     'JPN', 'FRA', 'NED', 'HUN', 'ROU', 'BEL')
apparatus_men = sort(unique(men$Apparatus))
apparatus_women = sort(unique(women$Apparatus))

# Create combined tables with avg/sd scores for each apparatus for each athlete

men_df_unpivot = men_df <- data_2223 %>% 
  filter(Gender == 'm', Country %in% countries_men) %>%
  group_by(FirstName, LastName, Country, Apparatus) %>% 
  summarize(avg_score = mean(Score,na.rm=T),
            var_score = ifelse(is.na(var(Score)),0,var(Score)),
            Country=Country[1]) %>%
  arrange(var_score) %>%
  head(-floor(nrow(.)/15))

women_df_unpivot = men_df <- data_2223 %>% 
  filter(Gender == 'w', Country %in% countries_women) %>%
  group_by(FirstName, LastName, Country, Apparatus) %>% 
  summarize(avg_score = mean(Score,na.rm=T),
            var_score = ifelse(is.na(var(Score)),0,var(Score)),
            Country=Country[1]) %>%
  arrange(var_score) %>%
  head(-floor(nrow(.)/15))

men_df <- data_2223 %>% 
  filter(Gender == 'm') %>%
  group_by(FirstName, LastName, Country, Apparatus) %>% 
  summarize(avg_score = mean(Score,na.rm=T),
            var_score = ifelse(is.na(var(Score)),0,var(Score)),
            Country=Country[1]) %>%
  arrange(var_score) %>%
  head(-floor(nrow(.)/15)) %>%
  pivot_wider(names_from = Apparatus, values_from = c(avg_score, var_score)) %>%
  select("FirstName", "LastName", "Country", 
         paste0(rep(c("avg_score_", "var_score_"), 8), 
                sort(rep(apparatus_men,2)))) %>% ungroup()

women_df <- data_2223 %>% 
  filter(Gender == 'w') %>%
  group_by(FirstName, LastName, Country, Apparatus) %>% 
  summarize(avg_score = mean(Score,na.rm=T),
            var_score = ifelse(is.na(var(Score)),0,var(Score)),
            Country=Country[1]) %>%
  arrange(var_score) %>%
  head(-floor(nrow(.)/15)) %>%
  pivot_wider(names_from = Apparatus, values_from = c(avg_score, var_score),
              names_sort = T) %>%
  select("FirstName", "LastName", "Country", 
         paste0(rep(c("avg_score_", "var_score_"), 4), 
                sort(rep(apparatus_women,2)))) %>% ungroup()

# Fill in NAs and sum up composite score, order by composite score 
men_df[is.na(men_df)] <- 0
men_df$composite_score <- rowSums(men_df%>% select(contains("avg_score")))
men_df <- men_df %>% arrange(desc(composite_score))

women_df[is.na(women_df)] <- 0
women_df$composite_score <- rowSums(women_df%>% select(contains("avg_score")))
women_df <- women_df %>% arrange(desc(composite_score))

teams_men <- men_df |> group_by(Country) |> group_nest() |> 
  filter(Country %in% countries_men) |>
  mutate(top5 = purrr::map(data, ~ head(.x, 5)))

teams_women <- women_df |> group_by(Country) |> group_nest() |> 
  filter(Country %in% countries_women) |>
  mutate(top5 = purrr::map(data, ~ head(.x, 5)))

teams_others_women <- women_df |> 
  filter(!Country %in% countries_women) |>
  head(n=36)

teams_others_men <- men_df |> 
  filter(!Country %in% countries_men) |>
  head(n=36)

#### SIMULATION OF QUALIFYING ####

# keep it simple force top 2 composites to participate in everything and deal
# with the rest

# first simulate the individual events

qualifying_scores_women <- data.frame(matrix(ncol = 5, nrow = 0))
colnames(qualifying_scores_women) <- c("FirstName","LastName","Country",
                                       "Event","sim_score")

team_aa_qualifying <- data.frame("Country" = teams_women$Country,
                                 "score" = 0)

#simulate countries
for(country in teams_women$Country){
  
  print(country)
  team = (teams_women%>% filter(Country == country))$top5[[1]]
  top2 = team %>% head(2)
  bot3 = team %>% tail(3)
  
  for(event in apparatus_women){
    top2_event = top2%>%select(FirstName,LastName,contains(event))
    bot3_event_ordered = bot3 %>%
      arrange(desc((eval(as.name(paste0("avg_score_",event)))))) %>% 
      head(2)
    
    bot3_event = bot3_event_ordered %>% 
      select(FirstName,LastName,contains(event))
    
    event_data = rbind(top2_event,bot3_event)
    
    ## change hard coding later
    event_data$sim_score = rnorm(4,unlist(event_data[,3]),sqrt(unlist(event_data[,4])))
    event_data$Country = country
    event_data$event = event

    # Think of how to restructure 
    qualifying_scores_women = rbind(qualifying_scores_women,
          event_data%>%select(FirstName,LastName,Country,event,sim_score))
    
    
    team_event_qual_score = sum(event_data %>% arrange(desc(sim_score)) %>%
                                  head(3) %>%select(sim_score))
    
    # add event score to team score 
    team_aa_qualifying <- team_aa_qualifying %>% 
      mutate(score = ifelse(Country == country, 
                            score + team_event_qual_score, 
                            score ))
  }
}


#simulate individuals
for (event in apparatus_women){
  
  others_women_ordered = teams_others_women %>%
    arrange(desc((eval(as.name(paste0("avg_score_",event))))))
  
  others_women_event = others_women_ordered %>% 
    select(FirstName,LastName,Country,contains(event))
  
  others_women_event$sim_score = rnorm(nrow(others_women_event),
                                       unlist(others_women_event[,4]),
                                       unlist(others_women_event[,5]))
  others_women_event$event = event
  qualifying_scores_women= rbind(qualifying_scores_women,
                                 others_women_event %>% 
                                   select(FirstName, LastName, 
                                          Country, event, sim_score))
}

# order_sim = function(data){
#   return(arrange(data,desc(sim_score)))
# }

qualifying_scores_women = qualifying_scores_women %>% group_by(event)%>%
  arrange(desc(sim_score)) %>% group_nest()

get_qualifiers_ind = function(data,n=8){
  #assumes data is ordered
  data_max_2 = data %>% group_by(Country) %>% slice(1:2)%>%
    arrange(desc(sim_score))
  return(data_max_2%>%head(n))
}

qualified = qualifying_scores_women
qualified$data = lapply(qualified$data,get_qualifiers_ind)

# find athletes who participated in everything and sort them by composite score
aa_athletes = qualifying_scores_women %>% unnest(data) %>% 
  group_by(FirstName,LastName,Country)%>%
  filter(n()==4) %>%
  summarise(sim_score = sum(sim_score)) %>%
  arrange(desc(sim_score))

aa_qualified = get_qualifiers_ind(aa_athletes,24)

# TEAM all around

team_aa_qualifying = team_aa_qualifying %>% arrange(desc(score))
team_aa_qualified = team_aa_qualifying %>% arrange(desc(score))%>%head(8)

qualified
aa_qualified
team_aa_qualified



#######

# New version for men

# Get all male competitors (teams + additional individuals)
comp_men <- rbind(teams_men %>% unnest(top5) %>% select(-data),
                  teams_others_men)

# Getting simulated scores for each apparatus per person
m_sim <- inner_join(
  comp_men %>% select(FirstName, LastName, Country, composite_score, contains('avg')) %>%
  pivot_longer(cols = contains('avg'), 
               names_to = 'apparatus', 
               names_prefix = 'avg_score_',
               values_to = 'avg'),
  comp_men %>% select(FirstName, LastName, Country, composite_score, contains('var')) %>%
    pivot_longer(cols = contains('var'), 
                 names_to = 'apparatus', 
                 names_prefix = 'var_score_',
                 values_to = 'var'),
  by = join_by(FirstName, LastName, Country, composite_score, apparatus)
  ) %>% mutate(sim = rnorm(n(), avg, sqrt(var))) 

# Only men who compete as part of the team
men_team <- m_sim %>% filter(Country %in% countries_men) 

# The top 2 best men per country are kept to compete in everything 
top2_men <- men_team %>% select(FirstName, LastName, Country, composite_score) %>%
  unique() %>% group_by(Country) %>% 
  slice_max(composite_score, n=2, with_ties = F)

# pick best 2 out of the remaining 3 men per team
m_team_qual_2 <- men_team %>% anti_join(top2_men) %>% 
  group_by(Country, apparatus) %>%
  slice_max(sim, n = 2, with_ties = F) 

# Simulate team competition by picking top 3 competitors per event per team
# and sum them up ("4 up 3 count")
team_qual_m <- rbind(men_team %>% semi_join(top2_men), m_team_qual_2) %>%
  group_by(Country, apparatus) %>% slice_max(sim, n = 3, with_ties = F) %>%
  summarise(score = sum(sim)) %>% group_by(Country) %>% 
  summarise(score = sum(score))

# All around qualifier 
# Find men who competed in everything and sort by summed score
aa_qual_m <- rbind(men_team %>% semi_join(top2_men), m_team_qual_2) %>% 
  group_by(FirstName, LastName, Country) %>%
  filter(n() == 6) %>% summarise(sum_score = sum(sim)) %>% 
  arrange(desc(sum_score)) %>% group_by(Country) %>% 
  slice_max(sum_score, n = 2, with_ties = F) %>% 
  arrange(desc(sum_score)) %>% head(24) 


event_qual_m <- m_sim %>% group_by(Country, apparatus) %>% 
  slice_max(sim, n = 2, with_ties = F) %>% group_by(apparatus) %>%
  slice_max(sim, n = 8, with_ties = F) %>%
  arrange(apparatus, desc(sim)) %>% 
  select(FirstName, LastName, Country, apparatus, sim)
  
team_final_m <- team_qual_m %>% arrange(desc(score)) %>% head(8)
team_final_m

############
men_top5 <- men_df_unpivot %>% 
  group_by(Country, Apparatus) %>%
  slice_max(avg_score, n = 5, with_ties = F) %>% ungroup() %>%
  select(FirstName, LastName, Country) %>%
  unique()

women_top5 <- women_df_unpivot %>% 
  group_by(Country, Apparatus) %>%
  slice_max(avg_score, n = 5, with_ties = F) %>% ungroup() %>%
  select(FirstName, LastName, Country) %>%
  unique()

table(men_top5$Country)
table(women_top5$Country)

get_medal_probs <- function(means, variances){
  nsims = 10000
  num_athletes = length(means)
  prob_medal = data.frame(replicate(3,rep(0,num_athletes)))
  names(prob_medal) = c("bronze","silver","gold")
  for(i in 1:nsims){
    sim_scores = rnorm(num_athletes,means,sqrt(variances))
    top_3_athletes = kit::topn(sim_scores,n=3)
    prob_medal[cbind(top_3_athletes,3:1)] = prob_medal[cbind(top_3_athletes,3:1)] + 1
  }
  return(prob_medal/nsims)
}
men_top5 <- dplyr::left_join(men_top5, men_df)
women_top5 <- dplyr::left_join(women_top5, women_df)

men_top5_country_groups <- men_top5 %>% group_by(Country)
men_top5_countries <- group_split(men_top5_country_groups)

names(men_top5_countries) <- c("BRA", "CAN", "CHN", "ESP", "GBR", "GER", "HUN", "ITA", "JPN", "KOR", "TUR", "USA")

women_top5_country_groups <- women_top5 %>% group_by(Country)
women_top5_countries <- group_split(women_top5_country_groups)

names(women_top5_countries) <- c("BEL", "BRA", "CAN", "CHN", "FRA", "GBR", "HUN", "ITA", "JPN", "NED", "ROU", "USA")
