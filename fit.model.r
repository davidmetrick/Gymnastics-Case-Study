## fit.model.r
## use cleaned dataset to create scree plot, tier lists, interactive team choices, etc.

## first step: make model that finds the average score for each person on each individual apparatus.

library(dplyr)
library(purrr)
options(dplyr.summarise.inform = FALSE)

# For ease of debugging we shall loop through the events for men and women and
# calculate average scores

# As a basic way of penalising athletes with high variances, all athletes with a score greater than 1
# standard deviation from the mean are excluded from consideration

apparatus_list_men = paste(apparatus_men,"m",sep="_")
apparatus_list_women = paste(apparatus_women,"w",sep="_")

apparatus_scores_men = paste(apparatus_list_men,"scores",sep="_")
apparatus_scores_women = paste(apparatus_list_women,"scores",sep="_")

for(i in 1:length(apparatus_list_men)){
  assign(apparatus_scores_men[i],eval(as.name(apparatus_list_men[i])) %>% 
    group_by(FirstName,LastName) %>%
    summarise(avg_score = mean(Score,na.rm=T),var_score = 
                ifelse(is.na(var(Score)),0,var(Score)),Country=Country[1]) %>%
    arrange(var_score)%>%
    head(-floor(nrow(.)/15))%>%
    arrange(desc(avg_score)))
}

for(i in 1:length(apparatus_list_women)){
  assign(apparatus_scores_women[i],eval(as.name(apparatus_list_women[i])) %>% 
           group_by(FirstName,LastName) %>%
           summarise(avg_score = mean(Score,na.rm=T),var_score = 
                       ifelse(is.na(var(Score)),0,var(Score)),Country=Country[1]) %>%
           arrange(var_score) %>%
           head(-floor(nrow(.)/15))%>%
           arrange(desc(avg_score)))
}

# Choosing 12 countries for the men and women 
countries_men <- c('CHN', 'JPN', 'GBR', 'ITA', 'USA', 'ESP', 
                   'BRA', 'KOR', 'GER', 'CAN', 'TUR', 'HUN')

countries_women <- c('USA', 'GBR', 'CAN', 'BRA', 'ITA', 'CHN', 
                     'JPN', 'FRA', 'NED', 'HUN', 'ROU', 'BEL')

#Now we display the top 5 athletes from the US for each of the events


for(event in apparatus_scores_men){
  paste(event,"USA top 5")
  print(eval(as.name(event)) %>% 
    filter(Country=='USA') %>%
    head(5))
}

for(event in apparatus_scores_women){
  paste(event,"USA top 5")
  print(eval(as.name(event)) %>% 
          filter(Country=='USA') %>%
          head(5))
}

apparatus_scores_men
apparatus_scores_women


for(event in apparatus_scores_men){
  paste(event,"GBR top 5")
  print(eval(as.name(event)) %>% 
          filter(Country=='GBR') %>%
          head(5))
}

for(event in apparatus_scores_women){
  for (country in countries_women){
  paste(event, country, " top 5")
  
  print(eval(as.name(event)) %>% 
          filter(Country==country) %>%
          head(5))
  }
}

ex <- BB_w_scores |> group_by(Country) |> group_nest() |> 
  filter(Country %in% countries_women) |>
  mutate(top5 = map(data, ~ head(.x, 5))) 



for (country in countries_women){
  paste(event, country, " top 5")
  
  print(eval(as.name(event)) %>% 
          filter(Country==country) %>%
          head(5))
}



####################

# New format 


men_pivot <- data_2223 %>% 
  filter(Gender == 'm', Country %in% countries_men) %>%
  group_by(FirstName, LastName, Country, Apparatus) %>% 
  summarize(avg_score = mean(Score,na.rm=T),
            var_score = ifelse(is.na(var(Score)),0,var(Score)),
            Country=Country[1]) %>%
  pivot_wider(names_from = c(Apparatus), values_from = c(avg_score, var_score)) %>%
  select("FirstName", "LastName", "Country", 
         paste0(rep(c("avg_score_", "var_score_"), 8), sort(rep(apparatus_men,2))))

women_pivot <- data_2223 %>% 
  filter(Gender == 'w', Country %in% countries_women) %>%
  group_by(FirstName, LastName, Country, Apparatus) %>% 
  summarize(avg_score = mean(Score,na.rm=T),
            var_score = ifelse(is.na(var(Score)),0,var(Score)),
            Country=Country[1]) %>%
  pivot_wider(names_from = c(Apparatus), values_from = c(avg_score, var_score),
              names_sort = T) %>%
  select("FirstName", "LastName", "Country", 
         paste0(rep(c("avg_score_", "var_score_"), 4), sort(rep(apparatus_women,2))))


