#################################################################################
#####           Misc Functions            ||             11/01/18           #####
#################################################################################

# ALLSCRAPE Dependencies
library(RCurl); library(rjson); library(lubridate); library(doMC); library(rvest)

# Script Dependencies
library(tidyverse)

options(scipen = 999)
set.seed(123)


# Source ALLSCRAPE.R & Shiny_All_Functions.R Scripts
source("ALLSCRAPE.R")



## ------------------------ ##
##   Full Schedule Scrape   ##
## ------------------------ ##

##############################

# Scrape Schedule of previous games
fun.schedule <- function(start, end) { 
  
  sched <- ds.scrape_schedule(start,
                              end, 
                              try_tolerance = 5, 
                              agents = ds.user_agents)
  
  sched <- filter(sched, session != "PR")
  
  sched$home_team_id <- Team_ID$Team[match(sched$home_team_id, Team_ID$ID)]
  sched$away_team_id <- Team_ID$Team[match(sched$away_team_id, Team_ID$ID)]
  
  sched$test <- format(as.POSIXct(sched$game_datetime, 
                                  tz = "UTC", 
                                  format = "%Y-%m-%d %H:%M:%S"), 
                       tz = "Canada/Eastern")
  
  sched$test <- as.Date(sched$test)
  sched$test <- as.Date(ifelse(is.na(sched$test), 
                               as.Date(sched$game_datetime) - 1,
                               sched$test), 
                        origin = "1970-01-01")
  
  sched$game_date <- sched$test
  
  sched <- sched %>% 
    arrange(game_id) %>% 
    rename(home_team = home_team_id, 
           away_team = away_team_id)
  
  }
schedule_1819 <- fun.schedule("2018-10-03", "2019-04-07")

hold <- schedule_1819 %>% select(-c(game_status, test))

# SAVE
#write.csv(hold, "schedule_1819.csv", row.names = F)

rm(hold)


##############################



## ----------------------------- ##
##   Game Score Games Function   ##
## ----------------------------- ##

###################################

fun.game_score_all_sit <- function(data) { 
  
  print(paste0("season(s): ", unique(data$season)), quote = F)
  
  # Modify pbp data
  data <- data %>% 
    filter(game_period < 5) %>% 
    select(-c(face_index:shift_length)) %>% 
    mutate(scradj = home_score - away_score, 
           home_lead = ifelse(scradj >= 3, 3, 
                              ifelse(scradj <= -3, -3, scradj)),
           home_lead_state = ifelse(home_lead < 0, 1, 
                                    ifelse(home_lead == 0, 2, 
                                           ifelse(home_lead > 0, 3, home_lead))), 
           str_state = ifelse(game_strength_state == "5v5", 1, 
                              ifelse(game_strength_state == "4v4", 2, 
                                     ifelse(game_strength_state == "3v3", 3, NA))), 
           home_lead = home_lead + 4, 
           event_length = ifelse(is.na(event_length), 0, event_length)
           ) %>% 
    rename(pred_goal = pred_XGB_7)
  
  
  
  # Corsi Diff / Goal Diff
  fun.oniceCorsiH <- function(data, player_slot) {
    
    hold <- data %>% 
      summarise(TOI = sum(event_length) / 60, 
                Team = first(home_team), 
                Venue = first(home_team), 
                is_home = 1, 
                
                GF =  sum((event_type == "GOAL" & event_team == home_team) * score_adj_EV$home_goal_adj[home_lead_state]), 
                GA =  sum((event_type == "GOAL" & event_team == away_team) * score_adj_EV$away_goal_adj[home_lead_state]), 
                
                CF =  sum((event_type %in% st.corsi_events & event_team == home_team) * score_adj_EV$home_corsi_adj[home_lead]), 
                CA =  sum((event_type %in% st.corsi_events & event_team == away_team) * score_adj_EV$away_corsi_adj[home_lead])
                )
    
    return(hold)
  
    }
  fun.oniceCorsiA <- function(data, player_slot) {
    
    hold <- data %>% 
      summarise(TOI = sum(event_length) / 60, 
                Team = first(away_team), 
                Venue = first(home_team), 
                is_home = 0, 
                
                GF =  sum((event_type == "GOAL" & event_team == away_team) * score_adj_EV$away_goal_adj[home_lead_state]), 
                GA =  sum((event_type == "GOAL" & event_team == home_team) * score_adj_EV$home_goal_adj[home_lead_state]), 
                
                CF =  sum((event_type %in% st.corsi_events & event_team == away_team) * score_adj_EV$away_corsi_adj[home_lead]), 
                CA =  sum((event_type %in% st.corsi_events & event_team == home_team) * score_adj_EV$home_corsi_adj[home_lead])
                )
    
    return(hold)
  
    }
  fun.componiceCorsi <- function(data) {
    
    data <- data %>% 
      filter(game_strength_state == "5v5")
    
    print("on_ice_home", quote = F)
    h1 <- data %>% group_by(game_id, season, home_on_1, home_team) %>% fun.oniceCorsiH(., "home_on_1") %>% rename(player = home_on_1) %>% data.frame()
    h2 <- data %>% group_by(game_id, season, home_on_2, home_team) %>% fun.oniceCorsiH(., "home_on_2") %>% rename(player = home_on_2) %>% data.frame()
    h3 <- data %>% group_by(game_id, season, home_on_3, home_team) %>% fun.oniceCorsiH(., "home_on_3") %>% rename(player = home_on_3) %>% data.frame()
    h4 <- data %>% group_by(game_id, season, home_on_4, home_team) %>% fun.oniceCorsiH(., "home_on_4") %>% rename(player = home_on_4) %>% data.frame()
    h5 <- data %>% group_by(game_id, season, home_on_5, home_team) %>% fun.oniceCorsiH(., "home_on_5") %>% rename(player = home_on_5) %>% data.frame()
    h6 <- data %>% group_by(game_id, season, home_on_6, home_team) %>% fun.oniceCorsiH(., "home_on_6") %>% rename(player = home_on_6) %>% data.frame()
    
    print("on_ice_away", quote = F)
    a1 <- data %>% group_by(game_id, season, away_on_1, away_team) %>% fun.oniceCorsiA(., "away_on_1") %>% rename(player = away_on_1) %>% data.frame()
    a2 <- data %>% group_by(game_id, season, away_on_2, away_team) %>% fun.oniceCorsiA(., "away_on_2") %>% rename(player = away_on_2) %>% data.frame()
    a3 <- data %>% group_by(game_id, season, away_on_3, away_team) %>% fun.oniceCorsiA(., "away_on_3") %>% rename(player = away_on_3) %>% data.frame()
    a4 <- data %>% group_by(game_id, season, away_on_4, away_team) %>% fun.oniceCorsiA(., "away_on_4") %>% rename(player = away_on_4) %>% data.frame()
    a5 <- data %>% group_by(game_id, season, away_on_5, away_team) %>% fun.oniceCorsiA(., "away_on_5") %>% rename(player = away_on_5) %>% data.frame()
    a6 <- data %>% group_by(game_id, season, away_on_6, away_team) %>% fun.oniceCorsiA(., "away_on_6") %>% rename(player = away_on_6) %>% data.frame()
    
    
    # Join all data.frames
    merged <- Reduce(function(...) merge(..., all = TRUE), list(h1, h2, h3, h4, h5, h6, a1, a2, a3, a4, a5, a6))
    
    merge_return <- merged %>% 
      group_by(player, game_id, season) %>%  
      summarise(Team =    first(Team), 
                Venue =   first(Venue), 
                is_home = first(is_home), 
                GF =      sum(GF), 
                GA =      sum(GA), 
                CF =      sum(CF), 
                CA =      sum(CA)
                ) %>% 
      filter(!is.na(player)) %>% 
      data.frame()
    
    return(merge_return)
  
    }
  
  # Boxscore
  fun.counts <- function(data, venue) {
    
    if (venue == "home_team") {
      
      # Counts
      counts_1 <- data %>% 
        filter(event_type %in% c("GOAL", "BLOCK", "MISS", "SHOT", "HIT", "TAKE", "GIVE"), 
               event_team == home_team
               ) %>% 
        group_by(event_player_1, game_id, season) %>% 
        summarise(Team = first(home_team),
                  G = sum(event_type == "GOAL"),
                  iSF = sum(event_type %in% st.shot_events)
                  ) %>% 
        rename(player = event_player_1) %>% 
        data.frame()
      
      counts_2 <- data %>% 
        filter(event_type %in% c("GOAL"), 
               event_team == home_team
               ) %>% 
        group_by(event_player_2, game_id, season) %>% 
        summarise(Team = first(home_team), 
                  A1 = sum(event_type == "GOAL")
                  ) %>% 
        rename(player = event_player_2) %>% 
        data.frame()
      
      counts_3 <- data %>% 
        filter(event_type == "GOAL", 
               event_team == home_team
               ) %>% 
        group_by(event_player_3, game_id, season) %>%
        summarise(Team = first(home_team), 
                  A2 = sum(event_type == "GOAL")
                  ) %>% 
        rename(player = event_player_3) %>% 
        data.frame()
      
      # Join
      merged <- Reduce(function(...) merge(..., all = TRUE), list(counts_1, counts_2, counts_3))
      
      joined <- merged %>% 
        mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>% 
        select(player, game_id, season, Team, 
               G, A1, A2, iSF
               ) %>% 
        arrange(player, game_id) %>% 
        data.frame()
      
      return(joined)
    
      }
    else {
      
      # Compile
      counts_1 <- data %>% 
        filter(event_type %in% c("GOAL", "BLOCK", "MISS", "SHOT", "HIT", "TAKE", "GIVE"), 
               event_team == away_team
               ) %>% 
        group_by(event_player_1, game_id, season) %>% 
        summarise(Team = first(away_team),
                  G = sum(event_type == "GOAL"),
                  iSF = sum(event_type %in% st.shot_events)
                  ) %>% 
        rename(player = event_player_1) %>% 
        data.frame()
      
      counts_2 <- data %>% 
        filter(event_type %in% c("GOAL"), 
               event_team == away_team
               ) %>% 
        group_by(event_player_2, game_id, season) %>% 
        summarise(Team = first(away_team), 
                  A1 = sum(event_type == "GOAL")
                  ) %>% 
        rename(player = event_player_2) %>% 
        data.frame()
      
      counts_3 <- data %>% 
        filter(event_type == "GOAL", 
               event_team == away_team
               ) %>% 
        group_by(event_player_3, game_id, season) %>%
        summarise(Team = first(away_team), 
                  A2 = sum(event_type == "GOAL")
                  ) %>% 
        rename(player = event_player_3) %>% 
        data.frame()
      
      # Join
      merged <- Reduce(function(...) merge(..., all = TRUE), list(counts_1, counts_2, counts_3))
      
      joined <- merged %>% 
        mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>% 
        select(player, game_id, season, Team, 
               G, A1, A2, iSF
               ) %>% 
        arrange(player, game_id) %>% 
        data.frame()
      
      return(joined)
    
      }
  
    }
  fun.faceoff <- function(data, venue) {
    
    if (venue == "home_team") {
      
      faceoffs <- data %>% 
        filter(event_type == "FAC") %>% 
        group_by(event_player_2, game_id, season) %>% 
        mutate(FOW = 1 * (home_team == event_team), 
               FOL = 1 * (away_team == event_team)
               ) %>% 
        summarise(Team = first(home_team),
                  FOW = sum(FOW), 
                  FOL = sum(FOL)
                  ) %>% 
        mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>% 
        rename(player = event_player_2) %>% 
        ungroup() %>% 
        arrange(player, game_id) %>% 
        data.frame()
      
      return(faceoffs)
    
      }
    else {
      
      faceoffs <- data %>% 
        filter(event_type == "FAC") %>% 
        group_by(event_player_1, game_id, season) %>% 
        mutate(FOW = 1 * (away_team == event_team), 
               FOL = 1 * (home_team == event_team)
               ) %>% 
        summarise(Team = first(away_team),
                  FOW = sum(FOW), 
                  FOL = sum(FOL)
                  ) %>% 
        mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>% 
        rename(player = event_player_1) %>% 
        ungroup() %>% 
        arrange(player, game_id) %>% 
        data.frame()
      
      return(faceoffs)
    
      }
  
    }
  fun.penalty <- function(data, venue) {
    
    if (venue == "home_team") {
      
      pen_1 <- data %>% 
        filter(event_team == home_team, 
               event_type %in% c("PENL", "BLOCK")
               ) %>% 
        group_by(event_player_1, game_id, season) %>% 
        summarise(Team = first(home_team),
                  iPENT2 = sum(na.omit(1 * (event_type == "PENL") +
                                         1 * (event_type == "PENL" & grepl("double minor", tolower(event_detail)) == TRUE) -
                                         1 * (event_type == "PENL" & grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE) - 
                                         1 * (event_type == "PENL" & grepl("too many men", tolower(event_description)) == TRUE) - 
                                         1 * (event_type == "PENL" & grepl("misconduct", tolower(event_description)) == TRUE)))
                  ) %>% 
        rename(player = event_player_1) %>% 
        data.frame()
      
      
      pen_2 <- data %>% 
        filter(event_team == away_team, 
               event_type %in% c("PENL", "BLOCK")
               ) %>% 
        group_by(event_player_2, game_id, season) %>% 
        summarise(Team = first(home_team), 
                  iPEND2 = sum(na.omit(1 * (event_type == "PENL") +
                                         1 * (event_type == "PENL" & grepl("double minor", tolower(event_detail)) == TRUE) -
                                         1 * (event_type == "PENL" & grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE) - 
                                         1 * (event_type == "PENL" & grepl("too many men", tolower(event_description)) == TRUE) - 
                                         1 * (event_type == "PENL" & grepl("misconduct", tolower(event_description)) == TRUE))),
                  iBLK = sum(event_type == "BLOCK")
                  ) %>% 
        rename(player = event_player_2) %>% 
        data.frame()
      
      
      merged <- Reduce(function(...) merge(..., all = TRUE), list(pen_1, pen_2))
      
      joined <- merged %>% 
        mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>% 
        select(player, game_id, season, Team, 
               iPENT2, iPEND2, iBLK
               ) %>% 
        arrange(player, game_id) %>% 
        data.frame()
      
      return(joined)
    
      }
    else {
      
      pen_1 <- data %>% 
        filter(event_team == away_team, 
               event_type %in% c("PENL", "BLOCK")
               ) %>% 
        group_by(event_player_1, game_id, season) %>% 
        summarise(Team = first(away_team),
                  iPENT2 = sum(na.omit(1 * (event_type == "PENL") +
                                         1 * (event_type == "PENL" & grepl("double minor", tolower(event_detail)) == TRUE) -
                                         1 * (event_type == "PENL" & grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE) - 
                                         1 * (event_type == "PENL" & grepl("too many men", tolower(event_description)) == TRUE) - 
                                         1 * (event_type == "PENL" & grepl("misconduct", tolower(event_description)) == TRUE)))
                  ) %>% 
        rename(player = event_player_1) %>% 
        data.frame()
      
      pen_2 <- data %>% 
        filter(event_team == home_team, 
               event_type %in% c("PENL", "BLOCK")
               ) %>% 
        group_by(event_player_2, game_id, season) %>% 
        summarise(Team = first(away_team),
                  iPEND2 = sum(na.omit(1 * (event_type == "PENL") +
                                         1 * (event_type == "PENL" & grepl("double minor", tolower(event_detail)) == TRUE) -
                                         1 * (event_type == "PENL" & grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE) - 
                                         1 * (event_type == "PENL" & grepl("too many men", tolower(event_description)) == TRUE) - 
                                         1 * (event_type == "PENL" & grepl("misconduct", tolower(event_description)) == TRUE))),
                  iBLK = sum(event_type == "BLOCK")
                  ) %>% 
        rename(player = event_player_2) %>% 
        data.frame()
      
      
      merged <- Reduce(function(...) merge(..., all = TRUE), list(pen_1, pen_2))
      
      joined <- merged %>% 
        mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>% 
        select(player, game_id, season, Team, 
               iPENT2, iPEND2, iBLK
               ) %>% 
        arrange(player, game_id) %>% 
        data.frame()
      
      return(joined)
    
      }
  
    }
  
  
  # Run Functions
  print("corsi/goal diff", quote = F)
  corsi_G_diff <- fun.componiceCorsi(data)
  
  print("counts", quote = F)
  counts_all <- fun.counts(data, "home_team") %>% 
    rbind(., fun.counts(data, "away_team")) %>% 
    arrange(player, game_id)
  
  print("faceoffs", quote = F)
  faceoff_all <- fun.faceoff(data, "home_team") %>% 
    rbind(., fun.faceoff(data, "away_team")) %>% 
    arrange(player, game_id)
  
  print("penalties", quote = F)
  penalty_all <- fun.penalty(data, "home_team") %>% 
    rbind(., fun.penalty(data, "away_team")) %>% 
    arrange(player, game_id)
  
  # Join
  print("bind", quote = F)
  test_join <- corsi_G_diff %>% 
    full_join(., counts_all, by = c("player", "game_id", "season", "Team")) %>% 
    full_join(., faceoff_all, by = c("player", "game_id", "season", "Team")) %>% 
    full_join(., penalty_all, by = c("player", "game_id", "season", "Team")) %>% 
    mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))
  
  # Remove goalies
  fun.goalie_remove <- function(data_) {
    
    # Identifies goalies within a given pbp data.frame & returns a data.frame to join for removal
    goalie_return <- data.frame(player = sort(unique(na.omit(as.character(rbind(data_$home_goalie, data_$away_goalie))))), 
                                is_goalie = 1)
    
    goalie_return$player <- as.character(goalie_return$player)
    
    return(goalie_return)
  
    }
  goalieremove <- fun.goalie_remove(data_ = data)
  
  all <- test_join %>% 
    left_join(., goalieremove, "player") %>% 
    filter(is.na(is_goalie)) %>% 
    select(-c(is_goalie)) %>%
    arrange(player, game_id) %>% 
    filter(!is.na(player)) %>% 
    left_join(., player_position, by = "player") %>% 
    data.frame()
  
  ## ----------- ##
  ##   Goalies   ##
  ## ----------- ##
  
  print("goalie", quote = F)
  
  # Goalie Game Score
  goalies_home <- data %>% 
    group_by(home_goalie, game_id, season, home_team) %>% 
    summarise(GA = sum((event_type == "GOAL" & event_team == away_team)), 
              SA = sum((event_type %in% st.shot_events & event_team == away_team))
              ) %>% 
    rename(player = home_goalie, 
           Team = home_team
           ) %>% 
    data.frame()
  
  goalies_away <- data %>% 
    group_by(away_goalie, game_id, season, away_team) %>% 
    summarise(GA = sum((event_type == "GOAL" & event_team == home_team)), 
              SA = sum((event_type %in% st.shot_events & event_team == home_team))
              ) %>% 
    rename(player = away_goalie, 
           Team = away_team
           ) %>% 
    data.frame()
  
  game_score_goalies_all <- goalies_home %>% 
    rbind(., goalies_away) %>% 
    filter(!is.na(player)) %>% 
    group_by(player, game_id, season, Team) %>% 
    summarise_at(vars(SA, GA), funs(sum)) %>% 
    mutate(position = 3, 
           SV = SA - GA, 
           GS = (-0.75 * GA) + (0.1 * SV)
           ) %>% 
    select(player, position, game_id, season, Team, SA:GS) %>% 
    ungroup() %>% 
    rename_at(vars(SA, GA, SV), funs(paste0(., "_goalie"))) %>% 
    data.frame()
  
  
  ## --------------- ##
  ##   Combine All   ##
  ## --------------- ##
  
  game_score_all_sit_sum <- all %>% 
    group_by(player, position, game_id, season, Team) %>% 
    summarise_at(vars(GF:iBLK), funs(sum)) %>% 
    ungroup() %>% 
    mutate(GS = round((0.75 * G) + (0.7 * A1) + (0.55 * A2) + (0.075 * iSF) + (0.05 * iBLK) + (0.15 * iPEND2) - (0.15 * iPENT2) + 
                        (0.01 * FOW) - (0.01 * FOL) + (0.05 * CF) - (0.05 * CA) + (0.15 * GF) - (0.15* GA), 2)
           ) %>% 
    full_join(., game_score_goalies_all, by = c("player", "position", "game_id", "season", "Team", "GS")) %>% 
    mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .))) %>% 
    arrange(player, season) %>% 
    select(player, position, game_id, season, Team, GF:iBLK, SA_goalie:SV_goalie, GS) %>% 
    data.frame()
  
  
  return(game_score_all_sit_sum)
  
  }
game_score_games_new <- fun.game_score_all_sit(data = pbp_df)


###################################







