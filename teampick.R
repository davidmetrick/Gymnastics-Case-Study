library("parallel")
library("foreach")
library("doParallel")

team_pick <- function(country_df, others_df, weights=rep(1,9), gender){
  n<-49 # number of simulations of athletes
  weights <- 9*weights / sum(weights) # normalize to 9 (so medal sum normalizes to 1)
  
  # Get country name
  c <- country_df[1, "Country"] %>% as.character()
  
  # Get all combinations of 5 athletes
  comb <- country_df%>% 
    select(FirstName, LastName) %>%
    mutate(name = paste(FirstName, LastName)) %>%
    unique() %>% select(name) %>% unlist()%>% combn(5) %>% data.frame()
  print(paste("Number of combinations",ncol(comb)))
  
  # get a list of the athlete names from our country
  team_names<- country_df %>% select(fullname) %>% unique() %>% unlist()
  
  # The total potential lineup for qualifying, the exact combinations will be filtered later
  qual_all <- rbind(country_df, others_df)
  
  # Setting up workers for parallel processing
  cl <- makeCluster(detectCores() -1)
  registerDoParallel(cl, cores = detectCores() -1)
  
  # looping through each of the simulations in parallel (all combinations are checked)
  # for each simulation and stores the number of medals was for each of them
  medal_scores2 = foreach(i = 1:n,.packages = c("dplyr"),.combine ="+",.inorder = FALSE)%dopar%{
    ##############
    # NOTE
    # Almost all the time of the code is not spent in this part of the loop
    # Being a little inefficient is fine here, we want as many useful things
    # for later being generated here
    ##############
    medal_scores <- matrix(0,nrow = ncol(comb), ncol = 9)
    colnames(medal_scores) <- c('team_g', 'team_s', 'team_b',
                                'aa_g', 'aa_s', 'aa_b',
                                'event_g', 'event_s', 'event_b')
    
    # simulate scores for all athletes
    qual_all$sim = rnorm(nrow(qual_all), qual_all$avg_score, qual_all$var_score)
    
    # treat the all around like an additional event and add it to the dataframe
    qual_all <- bind_rows(qual_all,qual_all%>% group_by(FirstName,LastName,Country,fullname) %>% 
                            summarise(Apparatus = "AA",sim=sum(sim,na.rm = T)))
    
    # Getting rid of unnecessary columns speeds up our code later when slicing data,etc
    qual = qual_all %>% select(-c(FirstName,LastName,avg_score,var_score))
    
    # Get simulation scores for other countries and current country of interest
    other_team_sim <- qual %>% filter(Country!=c)
    current_team_sim <- qual %>% filter(Country==c)

    # Team round:
    # We calculate the top 3 overall scores for all countries except the current country
    # of interest. Choose best 3 for apparatus (Dumb way of picking top 3 for now)
    team_comp <- other_team_sim %>% filter(Apparatus=="AA")%>%group_by(Country) %>%
      slice_max(sim, n = 3, with_ties = F) %>%group_by(Country)%>% summarize(composite = sum(sim)) %>% 
      ungroup() %>% slice_max(composite,n=3,with_ties = F)
    
    # Country of interest data frame for the all around
    country_aa = current_team_sim%>%filter(Apparatus=="AA")
    
    # We calculate the best score we could possibly have for the all-around, this lets
    # us avoid having to find AA scores for each team combination later if its less than the 
    # third place score from other countries
    country_best = country_aa%>%slice_max(sim, n = 3, with_ties = F)%>%select(sim) %>%sum()
    country_can_win = as.logical(country_best >= team_comp[3,"composite"])
    # Initialise team medals for all-around (updated when country_can_win=TRUE)
    team_medals = c(0,0,0)
    
    # Individual All Around: Calculate best 3 individual scores not from our country
    # doing what we do with the events would be a speedup here
    ind_aa <- other_team_sim %>% filter(Country!=c)%>% filter(Apparatus=="AA")%>%
      group_by(Country) %>% # no more than 2 per country
      slice_max(sim, n = 2, with_ties = F) %>% ungroup() %>%
      slice_max(sim ,n=3,with_ties = F)
    
    # Individual events: Get top 3 individual scores by event (not from country of interest)
    top3_event <- other_team_sim %>% filter(Apparatus!="AA")%>% group_by(Country, Apparatus) %>% 
      slice_max(sim, n = 2, with_ties = F) %>% # no more than 2 per country
      ungroup() %>% group_by(Apparatus) %>% slice_max(sim, n = 3, with_ties = F)
    
    # We store the third place value by event (this lets us filter out athletes from our country of interest if they don't score this high)
    third_place = (top3_event%>% group_by(Apparatus)%>%slice_min(sim,n=1,with_ties = F))$sim
    names(third_place) = (top3_event%>% group_by(Apparatus)%>%slice_min(sim,n=1,with_ties = F))$Apparatus
    
    # Includes athletes from our country,by event, that are larger than the third place we calculated above
    # we store their raw place from the standings and their place within the team, these two values together
    # can easily allow us to calculate the individual medals for a given combination quickly
    event_fin_all = current_team_sim%>% filter(Apparatus!="AA")%>% group_by(Apparatus) %>%
      filter(sim>=third_place[Apparatus])%>%arrange(desc(sim))%>%mutate(within_place=row_number()) %>%
      ungroup() %>%bind_rows(top3_event)%>%group_by(Apparatus)%>%
      arrange(desc(sim))%>%mutate(place=row_number()) %>% filter(Country==c)
    
    # Check each combination and how many medals will come from it
    for(j in 1:ncol(comb)){
      cur_team_names <- unlist(comb[j])# find which team names are included in this loop
      
      # Team AA:
      #if country can't win team medals are 0, otherwise we calculate if we get a medal
      if(country_can_win){
        team_aa_country = country_aa %>% filter(fullname %in% cur_team_names) %>% 
          slice_max(sim, n = 3, with_ties = F)%>% summarize(Country = c,composite = sum(sim))%>%rbind(team_comp)%>%
          slice_max(composite,n=3,with_ties = F)
        team_medals = as.numeric(team_aa_country[1:3,'Country']==c)
      }
      
      # Individual AA: determine if our team has any AA winners
      ind_aa_country <- country_aa %>% filter(fullname %in% cur_team_names) %>%
        slice_max(sim ,n=2,with_ties = F) %>% rbind(ind_aa) %>% 
        slice_max(sim,n=3,with_ties = F)
      
      # Individual events: determine how each athlete places in the individual events we use the 
      # raw place and within country place to find this and calculate medals from this
      event_fin_country = event_fin_all%>% filter(fullname %in% cur_team_names)%>%
        group_by(Apparatus)%>% mutate(difference = 
                                        ifelse(!is.na(within_place-lag(within_place)),within_place-lag(within_place)-1,0))%>%
        mutate(place=place-difference) %>% filter(place<=3)%>%group_by(place)%>%
        summarise(medals = n())%>%arrange(desc(place))%>%as.vector()
      ind_medals = event_fin_country$medals
      names(ind_medals) = event_fin_country$place
      ind_medals= ind_medals[c("1","2","3")]
      ind_medals[is.na(ind_medals)] <- 0
      
      medal_scores[j,] <- medal_scores[j,] + c(team_medals,
                                               as.numeric(ind_aa_country[1:3,'Country']==c),
                                               ind_medals) * weights
    }
    medal_scores
  }
  # stop cluster
  stopCluster(cl)
  # output what the number of medals for our score was
  write.csv(medal_scores2/n, paste0("scores-", paste(weights,collapse="."),"-",c,"-", gender, ".csv"), row.names=FALSE)
  write.csv(comb, paste0("names-",paste(weights,collapse="."),"-",c,"-", gender, ".csv"), row.names=FALSE)
  # DM - the lines below don't do anything because they're inside the
  # function, but they work as a way to consolidate the data frame in a
  # separate R script (replacing paste with scores-w and names-w, obviously)
  scores <- read.csv(paste0("scores-", paste(weights,collapse="."),
                            "-",c,"-",gender, ".csv"))
  scores$composite <- rowSums(scores)
  names <- read.csv(paste0("names-",paste(weights,collapse="."),"-",c,"-", gender, ".csv"))
  scores$name1 <- unlist(names[1,], use.names=FALSE)
  scores$name2 <- unlist(names[2,], use.names=FALSE)
  scores$name3 <- unlist(names[3,], use.names=FALSE)
  scores$name4 <- unlist(names[4,], use.names=FALSE)
  scores$name5 <- unlist(names[5,], use.names=FALSE)
  print(max(rowSums(medal_scores2))/n)
  return(comb[which.max(rowSums(medal_scores2))])
}

