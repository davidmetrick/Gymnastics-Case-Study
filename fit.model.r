## fit.model.r
## use cleaned dataset to create scree plot, tier lists, interactive team choices, etc.

## first step: make model that finds the average score for each person on each individual apparatus.

library(dplyr)
library(purrr)
library(tidyr)
options(dplyr.summarise.inform = FALSE)
# 
# # For ease of debugging we shall loop through the events for men and women and
# # calculate average scores
# 
# # As a basic way of penalising athletes with high variances, all athletes with a score greater than 1
# # standard deviation from the mean are excluded from consideration
# 
# apparatus_list_men = paste(apparatus_men,"m",sep="_")
# apparatus_list_women = paste(apparatus_women,"w",sep="_")
# 
# apparatus_scores_men = paste(apparatus_list_men,"scores",sep="_")
# apparatus_scores_women = paste(apparatus_list_women,"scores",sep="_")
# 
# for(i in 1:length(apparatus_list_men)){
#   assign(apparatus_scores_men[i],eval(as.name(apparatus_list_men[i])) %>% 
#     group_by(FirstName,LastName) %>%
#     summarise(avg_score = mean(Score,na.rm=T),var_score = 
#                 ifelse(is.na(var(Score)),0,var(Score)),Country=Country[1]) %>%
#     arrange(var_score)%>%
#     head(-floor(nrow(.)/15))%>%
#     arrange(desc(avg_score)))
# }
# 
# for(i in 1:length(apparatus_list_women)){
#   assign(apparatus_scores_women[i],eval(as.name(apparatus_list_women[i])) %>% 
#            group_by(FirstName,LastName) %>%
#            summarise(avg_score = mean(Score,na.rm=T),var_score = 
#                        ifelse(is.na(var(Score)),0,var(Score)),Country=Country[1]) %>%
#            arrange(var_score) %>%
#            head(-floor(nrow(.)/15))%>%
#            arrange(desc(avg_score)))
# }
# 
# # Choosing 12 countries for the men and women 
# countries_men <- c('CHN', 'JPN', 'GBR', 'ITA', 'USA', 'ESP', 
#                    'BRA', 'KOR', 'GER', 'CAN', 'TUR', 'HUN')
# 
# countries_women <- c('USA', 'GBR', 'CAN', 'BRA', 'ITA', 'CHN', 
#                      'JPN', 'FRA', 'NED', 'HUN', 'ROU', 'BEL')
# 
# #Now we display the top 5 athletes from the US for each of the events
# 
# 
# for(event in apparatus_scores_men){
#   paste(event,"USA top 5")
#   print(eval(as.name(event)) %>% 
#     filter(Country=='USA') %>%
#     head(5))
# }
# 
# for(event in apparatus_scores_women){
#   paste(event,"USA top 5")
#   print(eval(as.name(event)) %>% 
#           filter(Country=='USA') %>%
#           head(5))
# }
# 
# apparatus_scores_men
# apparatus_scores_women
# 
# 
# for(event in apparatus_scores_men){
#   paste(event,"GBR top 5")
#   print(eval(as.name(event)) %>% 
#           filter(Country=='GBR') %>%
#           head(5))
# }
# 
# for(event in apparatus_scores_women){
#   for (country in countries_women){
#   paste(event, country, " top 5")
#   
#   print(eval(as.name(event)) %>% 
#           filter(Country==country) %>%
#           head(5))
#   }
# }
# 
# ex <- BB_w_scores |> group_by(Country) |> group_nest() |> 
#   filter(Country %in% countries_women) |>
#   mutate(top5 = tidyr::map(data, ~ head(.x, 5))) 
# 
# 
# 
# for (country in countries_women){
#   paste(event, country, " top 5")
#   
#   print(eval(as.name(event)) %>% 
#           filter(Country==country) %>%
#           head(5))
# }
# 
# 

####################

# New format 


men_pivot <- data_2223 %>% 
  filter(Gender == 'm') %>%
  group_by(FirstName, LastName, Country, Apparatus) %>% 
  summarize(avg_score = mean(Score,na.rm=T),
            var_score = ifelse(is.na(var(Score)),0,var(Score)),
            Country=Country[1]) %>%
  arrange(var_score)%>%
  head(-floor(nrow(.)/15))%>%
  pivot_wider(names_from = c(Apparatus), values_from = c(avg_score, var_score)) %>%
  select("FirstName", "LastName", "Country", 
         paste0(rep(c("avg_score_", "var_score_"), 8), sort(rep(apparatus_men,2))))

women_pivot <- data_2223 %>% 
  filter(Gender == 'w') %>%
  group_by(FirstName, LastName, Country, Apparatus) %>% 
  summarize(avg_score = mean(Score,na.rm=T),
            var_score = ifelse(is.na(var(Score)),0,var(Score)),
            Country=Country[1]) %>%
  arrange(var_score)%>%
  head(-floor(nrow(.)/15))%>%
  pivot_wider(names_from = c(Apparatus), values_from = c(avg_score, var_score),
              names_sort = T) %>%
  select("FirstName", "LastName", "Country", 
         paste0(rep(c("avg_score_", "var_score_"), 4), sort(rep(apparatus_women,2))))


men_pivot_2 <- men_pivot %>% ungroup()
men_pivot_2[is.na(men_pivot_2)] <- 0
men_pivot$composite_score <- rowSums(men_pivot_2%>% select(contains("avg_score")))

women_pivot_2 <- women_pivot %>% ungroup()
women_pivot_2[is.na(women_pivot_2)] <- 0
women_pivot$composite_score <- rowSums(women_pivot_2%>% select(contains("avg_score")))


men_sorted <- men_pivot[order(-men_pivot$composite_score),]
women_sorted <- women_pivot[order(-women_pivot$composite_score),]

teams_men <- men_sorted |> group_by(Country) |> group_nest() |> 
  filter(Country %in% countries_men) |>
  mutate(top5 = purrr::map(data, ~ head(.x, 5)))
teams_women <- women_sorted |> group_by(Country) |> group_nest() |> 
  filter(Country %in% countries_women) |>
  mutate(top5 = purrr::map(data, ~ head(.x, 5)))

teams_others_women <- women_sorted |> 
  filter(!Country %in% countries_women) |>
  head(n=36)

teams_others_men <- men_sorted |> 
  filter(!Country %in% countries_men) |>
  head(n=36)

#### SIMULATION OF QUALIFYING ####

# keep it simple force top 2 composites to participate in everything and deal
# with the rest

# first simulate the individual events
qualifying_scores_women <- data.frame(matrix(ncol = 5, nrow = 0))
colnames(qualifying_scores_women) <- c("FirstName","LastName","Country","Event","sim_score")

team_aa_qualifying = data.frame(matrix(ncol = 2, nrow = 0))
colnames(team_aa_qualifying) <- c("Country","sim_score")

# qualifying_scores_women = qualifying_scores_women %>% 
#   group_by(apparatus_women) %>% group_nest() %>%
#   data.frame(matrix(ncol = 3, nrow = 0))

#simulate countries
for(country in teams_women$Country){
  team = (teams_women%>% filter(Country == country))$top5[[1]]
  top2 = team%>% head(2)
  bot3 = team %>% tail(3)
  aa_team_score = data.frame(Country = country,score=0)
  for(event in apparatus_women){
    top2_event = top2%>%select(FirstName,LastName,contains(event))
    bot3_event_ordered = bot3 %>%arrange(desc((eval(as.name(
      paste0("avg_score_",event)
    ))))) %>%head(2)
    bot3_event = bot3_event_ordered%>%select(FirstName,LastName,contains(event))
    event_data = rbind(top2_event,bot3_event)
    
    ## change hard coding later
    event_data$sim_score = rnorm(4,unlist(event_data[,3]),unlist(event_data[,4]))
    event_data$Country = country
    event_data$event = event
    
    qualifying_scores_women= rbind(qualifying_scores_women,
          event_data%>%select(FirstName,LastName,Country,event,sim_score))
    
    team_event_qual_score = sum(event_data %>% arrange(desc(sim_score)) %>% head(3) %>%select(sim_score))
    aa_team_score$score=aa_team_score$score+team_event_qual_score
  }
  team_aa_qualifying = rbind(team_aa_qualifying,aa_team_score)
}
#simulate individuals
for(event in apparatus_women){
  others_women_ordered= teams_others_women%>%
    arrange(desc((eval(as.name(
      paste0("avg_score_",event)
    )))))
  others_women_event = others_women_ordered%>%select(FirstName,LastName,Country,contains(event))
  others_women_event$sim_score = rnorm(nrow(others_women_event)
                                       ,unlist(others_women_event[,4]),unlist(others_women_event[,5]))
  others_women_event$event = event
  qualifying_scores_women= rbind(qualifying_scores_women,
                                 others_women_event%>%select(FirstName,LastName,Country,event,sim_score) )
}
order_sim = function(data){
  return(arrange(data,desc(sim_score)))
}
qualifying_scores_women=qualifying_scores_women%>%group_by(event)%>%
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
team_aa_qualified =team_aa_qualifying %>% arrange(desc(score))%>%head(8)

qualified
aa_qualified
team_aa_qualified