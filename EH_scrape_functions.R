#################################################################################
#####         Evolving-Hockey Scraper         ||          02/14/19          #####
#################################################################################

## Dependencies
#library(RCurl); library(xml2); library(rvest); library(jsonlite); library(doMC)
#library(lubridate); library(tidyverse)


## --------------- ##
##   Source URLs   ##
## --------------- ##

#####################

## HTML main source:
# events source:        http://www.nhl.com/scores/htmlreports/20182019/PL020001.HTM
# rosters source:       http://www.nhl.com/scores/htmlreports/20182019/RO020001.HTM
# shifts source (home): http://www.nhl.com/scores/htmlreports/20182019/TH020001.HTM
# shifts source (away): http://www.nhl.com/scores/htmlreports/20182019/TV020001.HTM

## HTML extras:
# game summary:  http://www.nhl.com/scores/htmlreports/20182019/GS020001.HTM
# event summary: http://www.nhl.com/scores/htmlreports/20182019/ES020001.HTM

## NHL API:
# events source: https://statsapi.web.nhl.com/api/v1/game/2018020001/feed/live?site=en_nhl
# shifts source: http://www.nhl.com/stats/rest/shiftcharts?cayenneExp=gameId=2018020001 (not used at this time)
# shifts charts: http://www.nhl.com/stats/shiftcharts?id=2018020001

## ESPN links:
# ESPN game IDs source: http://www.espn.com/nhl/scoreboard?date=20181003
# ESPN XML source:      http://www.espn.com/nhl/gamecast/data/masterFeed?lang=en&isAll=true&rand=0&gameId=401044320
# ESPN events check:    http://www.espn.com/nhl/playbyplay/_/gameId/401044320

#####################


## ------------------ ##
##   Create Objects   ##
## ------------------ ##

########################

# Create Team IDs (to use for API team triCodes)
Team_ID <- data.frame(Team = c("N.J", "NYI", "NYR", "PHI", "PIT", "BOS", "BUF", "MTL", "OTT", "TOR", "ATL", "CAR", "FLA", "T.B", 
                               "WSH", "CHI", "DET", "NSH", "STL", "CGY", "COL", "EDM", "VAN", "ANA", "DAL", "L.A", "ARI", "S.J", 
                               "CBJ", "MIN", "WPG", "ARI", "VGK"), 
                      ID =   c(seq(1:33)), 
                      stringsAsFactors = FALSE
                      ) %>% 
  mutate(ID = ifelse(ID == 31, 52, 
                     ifelse(ID == 32, 53, 
                            ifelse(ID == 33, 54, ID))))

# For identifying event_team in HTM events
Team_ID_vec <- c("ANA", "ARI", "BOS", "BUF", "CAR", "CBJ", "CGY", "CHI", "COL", "DAL", "DET", "EDM", "FLA", "L.A", "MIN", 
                 "MTL", "N.J", "NSH", "NYI", "NYR", "OTT", "PHI", "PIT", "S.J", "STL", "T.B", "TOR", "VAN", "WPG", "WSH", 
                 "PHX", "ATL", "VGK", "L.V")

# ESPN's team IDs & event type codes
ESPN_team_IDs <- data.frame(team_ID = as.numeric(c("25", "24", "1", "2", "7", "29", "3", "4", "17", "9", "5", "6", "26", "8", 
                                                   "30", "10", "11", "27", "12", "13", "14", "15", "16", "18", "19", "20", "21", 
                                                   "22", "37", "28", "23")), 
                            Team =    c("ANA", "ARI", "BOS", "BUF", "CAR", "CBJ", "CGY", "CHI", "COL", "DAL", "DET", "EDM", "FLA", 
                                        "L.A", "MIN", "MTL", "N.J", "NSH", "NYI", "NYR", "OTT", "PHI", "PIT", "S.J", "STL", "T.B", 
                                        "TOR", "VAN", "VGK", "WPG", "WSH"), 
                            stringsAsFactors = FALSE)

ESPN_codes <- data.frame(event = c("FAC", "HIT", "GvTk", "GOAL", "SHOT", "MISS", "BLOCK", "PENL","STOP", "PRDY", "PSTR", "PEND", 
                                   "PERD", "SOC", "GEND", "SOut","error", "TAKE", "GIVE", "early intermission", "nothing", "nothing"
                                   ),
                         code = as.character(c(502, 503, 504, 505, 506, 507, 508, 509, 516, 517, 518, 519, 520, 521, 522, 0, 9999, 
                                               1401, 1402, -2147483648, 1, 5)), 
                         stringsAsFactors = FALSE)

# Other objects
sc.main_events <- c("GOAL", "SHOT", "MISS", "BLOCK", "HIT", "GIVE", "TAKE", "FAC", "PENL")

st.shot_events <-     c("SHOT",  "GOAL")
st.fenwick_events <-  c("SHOT", "GOAL", "MISS")
st.corsi_events <-    c("SHOT", "GOAL", "MISS", "BLOCK" )
st.strength_states <- c("3v3", "5v5", "4v4", "5v4", "4v5", "5v3", "3v5", "4v3", "3v4", "5vE", "Ev5", "4vE", "Ev4", "3vE", "Ev3") %>% as.factor()
st.even_strength <-   c("5v5", "4v4", "3v3") %>% as.factor()
st.pp_strength <-     c("5v4", "4v5", "5v3", "3v5", "4v3", "3v4") %>% as.factor()
st.empty_net <-       c("5vE", "Ev5", "4vE", "Ev4", "3vE", "Ev3") %>% as.factor()


# Functions
na_if_null <- function(x) {
  
  return(ifelse(is.null(x), NA, x))
  
  }


########################


## --------------------- ##
##   Scraper Functions   ##
## --------------------- ##

###########################

## --------------- ##
##   Scrape Data   ##
## --------------- ##

# Scrape Schedule
sc.scrape_schedule <- function(start_date, end_date, print_sched = TRUE) { 
  
  # getURL for schedule data
  url_schedule <- NULL
  try_count <- 3
  
  while (!is.character(url_schedule) & try_count > 0) { 
    
    url_schedule <- try(
      getURL(paste0("https://statsapi.web.nhl.com/api/v1/schedule?startDate=",
                    as.character(start_date),
                    "&endDate=",
                    as.character(end_date)
                    ))
      )
    
    try_count <- try_count - 1
    
    }
  
  # Parse JSON data
  if (is.character(url_schedule)) {
    schedule_list <- jsonlite::fromJSON(url_schedule)
    
    }
  
  # Return from function if scrape returned no data
  if (length(schedule_list$dates) == 0) {
    
    warning("NHL Schedule API Returned No Data")
    
    # Return empty data.frame in same format if error occurred
    return(
      data.frame(game_id = character(), 
                 game_date = character(), 
                 season = character(), 
                 session = character(), 
                 game_status = character(), 
                 away_team = character(), 
                 home_team = character(), 
                 game_venue = character(), 
                 game_datetime = character(), 
                 EST_time_convert = character(), 
                 EST_date = character(), 
                 stringsAsFactors = FALSE)
      )
    
    }
  
  
  # Bind games from schedule list
  bind_schedule <- foreach(i = 1:length(schedule_list$dates$games), .combine = rbind) %do% {
    
    schedule_current <- data.frame(game_id =       as.character(schedule_list$dates$games[[i]]$gamePk), 
                                   game_date =     as.Date(schedule_list$dates$games[[i]]$gameDate),
                                   season =        schedule_list$dates$games[[i]]$season, 
                                   session =       schedule_list$dates$games[[i]]$gameType, 
                                   game_status =   schedule_list$dates$games[[i]]$status$detailedState, 
                                   away_team_id =  schedule_list$dates$games[[i]]$teams$away$team$id, 
                                   home_team_id =  schedule_list$dates$games[[i]]$teams$home$team$id, 
                                   game_venue =    schedule_list$dates$games[[i]]$venue$name, 
                                   game_datetime = schedule_list$dates$games[[i]]$gameDate, 
                                   
                                   stringsAsFactors = FALSE
                                   )
    
    }
  
  
  # Modify bound schedule data
  schedule_current <- bind_schedule %>% 
    arrange(game_id) %>% 
    filter(session != "PR") %>%   ## filter out preseason games
    mutate(home_team_id = Team_ID$Team[match(home_team_id, Team_ID$ID)], 
           away_team_id = Team_ID$Team[match(away_team_id, Team_ID$ID)], 
           
           EST_time_convert = format(
             as.POSIXct(gsub("T", " ", game_datetime) %>% gsub("Z", "", .), 
                        tz = "UTC", 
                        format = "%Y-%m-%d %H:%M:%S"), 
             tz = "Canada/Eastern"
             ), 
           
           EST_date = as.Date(
             ifelse(is.na(EST_time_convert), as.Date(game_datetime) - 1, EST_time_convert), 
             origin = "1970-01-01"
             ), 
           
           game_date = EST_date
           ) %>% 
    arrange(game_id) %>% 
    rename(home_team = home_team_id, 
           away_team = away_team_id
           ) %>% 
    data.frame()
  
  # Arrange if playoff games
  if (unique(schedule_current$session == "P")) { 
    schedule_current <- arrange(schedule_current, game_date, EST_time_convert)
    
    }
  
  # print schedule
  if (print_sched == TRUE) print(head(schedule_current, 20))
  
  # return schedule
  return(schedule_current)
  
  }

# Scrape Events (HTM)
sc.scrape_events_HTM <- function(game_id_fun, season_id_fun, attempts) { 
  
  url_events_HTM <- NULL
  try_count <-  attempts
  
  while (!is.character(url_events_HTM) & try_count > 0) { 
    
    url_events_HTM <- try(
      getURL(paste0("http://www.nhl.com/scores/htmlreports/",
                    as.character(season_id_fun),
                    "/PL0",
                    as.character(substr(game_id_fun, 6, 10)),
                    ".HTM"))
      )
    
    try_count <- try_count - 1
    
    }
  
  # Pull out events data
  events_body_text <- rvest::html_text(rvest::html_nodes(xml2::read_html(url_events_HTM), ".bborder"))
  
  }

# Scrape Events (API)
sc.scrape_events_API <- function(game_id_fun, attempts) { 
  
  url_events_API <- NULL
  try_count <-  attempts
  
  while (!is.character(url_events_API) & try_count > 0) { 
    
    url_events_API <- try(
      getURL(paste0("https://statsapi.web.nhl.com/api/v1/game/", 
                    game_id_fun, 
                    "/feed/live?site=en_nhl"))
      )
    
    try_count <- try_count - 1
    
    }
  
  
  ## Parse JSON // Error handling
  if (is.character(url_events_API)) {
    events_list_API <- try(jsonlite::fromJSON(url_events_API), silent = TRUE)
    } else {
      events_list_API <- list()
      }
  
  if (class(events_list_API) == "try-error") { 
    events_list_API <- jsonlite::fromJSON(
      suppressWarnings(
        readLines(paste0("https://statsapi.web.nhl.com/api/v1/game/", 
                         game_id_fun, 
                         "/feed/live?site=en_nhl"))
        )
      )
    
    }
  
  if (class(events_list_API) != "list") { 
    events_list_API <- list()
    
    }
  
  
  return(events_list_API)
  
  }

# Scrape Events (ESPN)
sc.scrape_events_ESPN <- function(game_id_fun, season_id_fun, game_info_data, attempts) { 
  
  ## Scrape ESPN to locate game IDs for the specified date
  url_ESPN_page <- NULL
  try_count <- attempts
  
  while(class(url_ESPN_page) != "character" & try_count > 0) {
    
    url_ESPN_page <- try(
      getURL(.opts = curlOptions(referer = "www.espn.com",
                                 verbose = FALSE,
                                 followLocation = TRUE), 
             # url to scrape
             paste0("http://www.espn.com/nhl/scoreboard?date=", gsub("-", "", as.character(game_info_data$game_date)))
             )
      )
    
    try_count <- try_count - 1
    
    }
  
  
  # Parse games from scraped ESPN day page
  ESPN_game_ids <- as.character(unique(unlist(str_extract_all(url_ESPN_page, "gameId=[0-9]+")))) %>% gsub("gameId=", "", .)
  ESPN_teams <-    toupper(gsub("team/_/name/|>|</div>", "", unique(unlist(str_extract_all(url_ESPN_page, "team/_/name/[a-zA-Z]+|>(Coyotes|Thrashers)</div>")))))
  
  # Check if ESPN URL returned game IDs
  if (length(ESPN_game_ids) > 0) { 
    
    ESPN_games_df <- suppressWarnings(
      cbind(
        ESPN_game_ids, 
        matrix(unique(ESPN_teams), byrow = TRUE, ncol = 2)
        )) %>% 
      data.frame(stringsAsFactors = FALSE) %>% 
      select(game_id =   1, 
             away_team = V2, 
             home_team = V3
        ) %>% 
      mutate_at(vars(home_team, away_team), 
                funs(
                  case_when(
                    . == "PHX" ~ "ARI", 
                    . == "TB" ~ "T.B", 
                    . == "NJ" ~ "N.J", 
                    . == "SJ" ~ "S.J", 
                    . == "LA" ~ "L.A", 
                    . == "VGS" ~ "VGK", 
                    . == "COYOTES" ~ "ARI", 
                    . == "THRASHERS" ~ "ATL", 
                    TRUE ~ .
                    )
                  )
                ) %>% 
      ## ensure duplicate games are not created
      group_by(game_id) %>% 
      mutate(index = row_number()) %>% 
      filter(index == 1) %>% 
      select(-index) %>% 
      data.frame()
    
    
    ## Scrape individual game
    ESPN_game_id_ <- filter(ESPN_games_df, away_team == game_info_data$away_team, home_team == game_info_data$home_team)$game_id
    
    url_ESPN_game <- NULL
    try_count <- 3
    
    while(class(url_ESPN_game) != "character" & try_count > 0) {
      
      url_ESPN_game <- try(
        getURL(.opts = curlOptions(referer = "www.espn.com",
                                   verbose = FALSE,
                                   followLocation = TRUE), 
               # url to scrape
               paste0("http://www.espn.com/nhl/gamecast/data/masterFeed?lang=en&isAll=true&rand=0&gameId=", 
                      ESPN_game_id_)
               )
        )
      
      try_count <- try_count - 1
      
      }
    
    
    ## Parse xml data
    xml_ESPN_events <- url_ESPN_game %>% 
      gsub("\023", "", .) %>%   ## prevent xml parse from erroring
      read_xml %>% 
      xml_nodes("Plays") %>% 
      xml_children() %>% 
      xml_text() %>% 
      strsplit("~") %>% 
      do.call(rbind, .) %>% 
      data.frame(stringsAsFactors = FALSE)
    
    
    return(xml_ESPN_events)
    
    } else {
    
      return(return_char <- "ESPN_NULL")
    
      }
  
  }

# Scrape Shifts
sc.scrape_shifts <- function(game_id_fun, season_id_fun, attempts) { 
  
  url_home_shifts <- NULL
  try_count <-  attempts
  
  while (!is.character(url_home_shifts) & try_count > 0) { 
    
    url_home_shifts <- try(
      getURL(paste0("http://www.nhl.com/scores/htmlreports/", 
                    season_id_fun, 
                    "/TH0", 
                    as.character(substr(game_id_fun, 6, 10)), 
                    ".HTM"))
      )
    
    try_count <- try_count - 1
    
    }
  
  url_away_shifts <- NULL
  try_count <- attempts
  
  while (!is.character(url_away_shifts) & try_count > 0) { 
    
    url_away_shifts <- try(
      getURL(paste0("http://www.nhl.com/scores/htmlreports/", 
                    season_id_fun, 
                    "/TV0", 
                    as.character(substr(game_id_fun, 6, 10)), 
                    ".HTM"))
      )
    
    try_count <- try_count - 1
    
    }
  
  # Pull out scraped shifts data
  home_shifts_titles <- rvest::html_text(rvest::html_nodes(xml2::read_html(url_home_shifts), ".border"))
  away_shifts_titles <- rvest::html_text(rvest::html_nodes(xml2::read_html(url_away_shifts), ".border"))
  
  home_shifts_text <- rvest::html_text(rvest::html_nodes(xml2::read_html(url_home_shifts), ".bborder"))
  away_shifts_text <- rvest::html_text(rvest::html_nodes(xml2::read_html(url_away_shifts), ".bborder"))
  
  # Return data as list
  return_list  <- list(home_shifts_titles = home_shifts_titles, 
                       away_shifts_titles = away_shifts_titles, 
                       home_shifts_text =   home_shifts_text, 
                       away_shifts_text =   away_shifts_text)
  
  }

# Scrape Rosters
sc.scrape_rosters <- function(game_id_fun, season_id_fun, attempts) { 
  
  url_rosters <- NULL
  try_count <- attempts
  
  while (is.null(url_rosters) & try_count > 0) { 
    
    url_rosters <- try(
      getURL(paste0("http://www.nhl.com/scores/htmlreports/",
                    as.character(season_id_fun),
                    "/RO0",
                    as.character(substr(game_id_fun, 6, 10)),
                    ".HTM"))
      )
    
    try_count <- try_count - 1
    
    }
  
  # Pull out roster data
  rosters_text <- rvest::html_text(rvest::html_nodes(xml2::read_html(url_rosters), "td"))
  
  }

# Scrape Event Summary
sc.scrape_event_summary <- function(game_id_fun, season_id_fun, attempts) { 
  
  url_event_summary <- NULL
  try_count <- attempts
  
  while (is.null(url_event_summary) & try_count > 0) { 
    
    url_event_summary <- try(
      getURL(paste0("http://www.nhl.com/scores/htmlreports/",
                    as.character(season_id_fun),
                    "/ES0",
                    as.character(substr(game_id_fun, 6, 10)),
                    ".HTM"))
      )
    
    try_count <- try_count - 1
    
    }
  
  # Pull out roster data
  event_summary_text <- rvest::html_text(rvest::html_nodes(xml2::read_html(url_event_summary), "td"))
  
  }


## ---------------------------- ##
##   Create Basic Data Frames   ##
## ---------------------------- ##

# Create game information data frame
sc.game_info <- function(game_id_fun, season_id_fun, events_data, roster_data) { 
  
  # Find coaches
  coach_index <- which(roster_data %in% c("Head Coaches", "Entraîneurs chef / Head Coaches")) + 1
  
  
  # Find referees (french format / standard format)
  if (roster_data[grep("^Referee|^Arbitre/Referee", roster_data)] %in% c("Referee", "Arbitre/Referee") & 
      roster_data[grep("^Referee|^Arbitre/Referee", roster_data) + 1] %in% c("Linesman", "JL/Linesman")) { 
    referee_index <- grep("^Linesman|^JL/Linesman|^Referee:\\s*", roster_data) + 2
    referee_1 <-     na_if_null(toupper(gsub("#[0-9]*\\s", "", roster_data[referee_index    ]) %>% gsub("\\s", ".", .)))
    referee_2 <-     na_if_null(toupper(gsub("#[0-9]*\\s", "", roster_data[referee_index + 1]) %>% gsub("\\s", ".", .)))
    linesman_1 <-    na_if_null(toupper(gsub("#[0-9]*\\s", "", roster_data[referee_index + 3]) %>% gsub("\\s", ".", .)))
    linesman_2 <-    na_if_null(toupper(gsub("#[0-9]*\\s", "", roster_data[referee_index + 4]) %>% gsub("\\s", ".", .)))
    
    } else {
      referee_index <- grep("^Referee:", roster_data)
      referee_1 <-     na_if_null(toupper(gsub("#[0-9]*\\s", "", roster_data[referee_index + 2]) %>% gsub("\\s", ".", .)))
      referee_2 <-     na_if_null(toupper(gsub("#[0-9]*\\s", "", roster_data[referee_index + 3]) %>% gsub("\\s", ".", .)))
      linesman_1 <-    na_if_null(toupper(gsub("#[0-9]*\\s", "", roster_data[referee_index + 6]) %>% gsub("\\s", ".", .)))
      linesman_2 <-    na_if_null(toupper(gsub("#[0-9]*\\s", "", roster_data[referee_index + 7]) %>% gsub("\\s", ".", .)))
    
      }
  
  
  # Find venue & attendance (french format / standard format)
  if (sum(grep("Les Formations", roster_data)) == 0) { 
    venue_vec <-      first(roster_data[grep("^Attendance\\s*", roster_data)]) %>% gsub(".*at\\s*", "", .)
    attendance_vec <- first(roster_data[grep("^Attendance*", roster_data)]) %>% str_extract(., "[0-9]+,[0-9]+") %>% gsub(",", "", .) %>% as.numeric()
    
    } else {
      venue_vec <-      first(roster_data[grep("^Ass./Att.\\s*", roster_data)]) %>% gsub(".*@\\s*", "", .)
      attendance_vec <- first(roster_data[grep("^Ass./Att.\\s*", roster_data)]) %>% str_extract(., "[0-9]+,[0-9]+") %>% gsub(",", "", .) %>% as.numeric()
    
      }
  
  
  # Create game info data frame
  game_info_df <- data.frame(game_id =         game_id_fun, 
                             season =          season_id_fun, 
                             game_date =       first(roster_data[grep("^[a-zA-Z]*, ", roster_data)]) %>% gsub("^[a-zA-Z]*, ", "", .) %>% as.Date(., format = "%B %d, %Y") %>% as.character(), 
                             game_time_start = first(na.omit(str_extract(roster_data, "[sS]tart\\s*[0-9]+:[0-9]+\\s*[A-Z]+"))) %>% gsub("[sS]tart\\s*", "", .), 
                             game_time_end =   first(na.omit(str_extract(roster_data, "[eE]nd\\s*[0-9]+:[0-9]+\\s*[A-Z]+"))) %>% gsub("[eE]nd\\s*", "", .), 
                             venue =           venue_vec, 
                             attendance =      attendance_vec, 
                             session =         ifelse(as.character(substr(game_id_fun, 6, 10)) > 30000, "P", "R"), 
                             home_team =       events_data[8] %>% gsub(" On Ice", "", .), 
                             away_team =       events_data[7] %>% gsub(" On Ice", "", .), 
                             home_coach =      na_if_null(toupper(gsub("\\\r|\\\n", "", roster_data[coach_index + 2]) %>% gsub("\\s", ".", .))), 
                             away_coach =      na_if_null(toupper(gsub("\\\r|\\\n", "", roster_data[coach_index    ]) %>% gsub("\\s", ".", .))), 
                             referee_1 =       ifelse(as.character(substr(game_id_fun, 6, 10)) < 30000, referee_1, NA),   ##  not adding referees for playoff games ...
                             referee_2 =       ifelse(as.character(substr(game_id_fun, 6, 10)) < 30000, referee_2, NA),   ##  additional refs present
                             linesman_1 =      ifelse(as.character(substr(game_id_fun, 6, 10)) < 30000, linesman_1, NA), 
                             linesman_2 =      ifelse(as.character(substr(game_id_fun, 6, 10)) < 30000, linesman_2, NA), 
                             
                             stringsAsFactors = FALSE 
                             ) %>% 
    mutate(home_team = ifelse(home_team == "PHX", "ARI", home_team), 
           away_team = ifelse(away_team == "PHX", "ARI", away_team), 
           
           ## Fix wrong game date in source
           game_date = ifelse(game_id == "2007020003", "2007-10-03", game_date)
           ) 
  
  }

# Create rosters data frame
sc.roster_info <- function(game_id_fun, season_id_fun, roster_data, game_info_data, shifts_list) { 
  
  # Get counts of roster & scratched players
  roster_player_counts <- lapply(str_extract_all(roster_data[grep("^\r\n#\r\nPos\r\nName|^\r\n#\r\nPos\r\nNom/Name", roster_data)], "[0-9]+"), length)
  
  if (length(roster_player_counts) == 0) {  ## extract string for alternate formatting
    roster_player_counts <- lapply(str_extract_all(roster_data[grep("^\n#\nPos\nName", roster_data)], "[0-9]+"), length)
    
    }
  
  # Find players in rosters html text
  roster_index <- which(roster_data  %in% c("Name", "Nom/Name")) + 1
  
  roster_players <- bind_rows(
    bind_cols(
      player =   roster_data[seq(roster_index[1] + 2, roster_index[1] + 59, by = 3)][1:as.numeric(roster_player_counts[[1]])], 
      number =   roster_data[seq(roster_index[1]    , roster_index[1] + 57, by = 3)][1:as.numeric(roster_player_counts[[1]])], 
      position = roster_data[seq(roster_index[1] + 1, roster_index[1] + 58, by = 3)][1:as.numeric(roster_player_counts[[1]])]
      ) %>% 
      mutate(Team = game_info_data$away_team), 
    bind_cols(
      player =   roster_data[seq(roster_index[2] + 2, roster_index[2] + 59, by = 3)][1:as.numeric(roster_player_counts[[2]])], 
      number =   roster_data[seq(roster_index[2]    , roster_index[2] + 57, by = 3)][1:as.numeric(roster_player_counts[[2]])], 
      position = roster_data[seq(roster_index[2] + 1, roster_index[2] + 58, by = 3)][1:as.numeric(roster_player_counts[[2]])]
      ) %>% 
      mutate(Team = game_info_data$home_team)
    ) %>% 
    data.frame(stringsAsFactors = FALSE) %>% 
    mutate(player = gsub("\\s*\\([A-Z]\\)", "", player), 
           player_team_num = paste0(Team, number))
  
  
  # Find scratches
  away_scratch_count <- try(roster_player_counts[[3]], silent = TRUE)
  home_scratch_count <- try(roster_player_counts[[4]], silent = TRUE)
  
  if (class(away_scratch_count) != "try-error" & class(home_scratch_count) != "try-error" & game_info_data$session == "R") {  ## not including scratches for playoff games
    
    roster_scratches <- bind_rows(
      bind_cols(
        player =   roster_data[seq(roster_index[3] + 2, roster_index[3] + 8, by = 3)][1:away_scratch_count], 
        number =   roster_data[seq(roster_index[3]    , roster_index[3] + 6, by = 3)][1:away_scratch_count], 
        position = roster_data[seq(roster_index[3] + 1, roster_index[3] + 7, by = 3)][1:away_scratch_count]
        ) %>% 
        mutate(Team = game_info_data$away_team, 
               venue = "Away"), 
      bind_cols(
        player =   roster_data[seq(roster_index[4] + 2, roster_index[4] + 8, by = 3)][1:home_scratch_count], 
        number =   roster_data[seq(roster_index[4]    , roster_index[4] + 6, by = 3)][1:home_scratch_count], 
        position = roster_data[seq(roster_index[4] + 1, roster_index[4] + 7, by = 3)][1:home_scratch_count]
        ) %>% 
        mutate(Team = game_info_data$home_team, 
               venue = "Home")
      ) %>% 
      data.frame(stringsAsFactors = FALSE) %>% 
      mutate(player =          gsub("\\s*\\([A-Z]\\)", "", player), 
             player =          gsub("\\s", ".", player), 
             player_team_num = paste0(Team, number), 
             position_type =   ifelse(position == "G", "G",
                                      ifelse(position == "D", "D", 
                                             "F")), 
             game_id =         game_info_data$game_id, 
             game_date =       game_info_data$game_date, 
             season =          game_info_data$season, 
             session =         game_info_data$session, 
             Opponent =        ifelse(Team == game_info_data$home_team, game_info_data$away_team, game_info_data$home_team), 
             is_home =         1 * (Team == game_info_data$home_team)
             ) %>% 
      select(player, position_type, position, game_id, game_date, season, session, Team, Opponent, is_home, 
             player_num = number, 
             player_team_num
             ) %>% 
      arrange(is_home, player)
    
    } else {
      
      roster_scratches <- data.frame(player =          character(), 
                                     position_type =   character(), 
                                     position =        character(), 
                                     game_id =         character(), 
                                     game_date =       character(), 
                                     season =          character(), 
                                     session =         character(), 
                                     Team =            character(), 
                                     Opponent =        character(), 
                                     is_home =         character(), 
                                     player_num =      character(), 
                                     player_team_num = character(), 
                                     stringsAsFactors = FALSE)
      
      }
  
  
  # Construct roster data frame
  roster_df <- bind_rows(
    data.frame(team_name =      shifts_list$home_shifts_titles[1],
               Team =           game_info_data$home_team,
               venue =          "Home",
               num_last_first = shifts_list$home_shifts_titles[-1], 
               stringsAsFactors = FALSE
               ),
    data.frame(team_name =      shifts_list$away_shifts_titles[1],
               Team =           game_info_data$away_team,
               venue =          "Away",
               num_last_first = shifts_list$away_shifts_titles[-1], 
               stringsAsFactors = FALSE
               )
    ) %>%
    data.frame() %>% 
    filter(grepl("[A-Z0-9]", num_last_first)) %>%
    mutate(game_date =       game_info_data$game_date,
           game_id =         as.character(game_id_fun),
           season =          as.character(season_id_fun),
           session =         game_info_data$session,
           player_num =      parse_number(num_last_first),
           player_team_num = paste0(Team, player_num)
           ) %>% 
    group_by(player_team_num) %>% 
    mutate(firstName =     strsplit(gsub("^[0-9]+ ", "", num_last_first), ", ")[[1]][2], 
           lastName =      strsplit(gsub("^[0-9]+ ", "", num_last_first), ", ")[[1]][1], 
           player =        paste0(firstName, ".", lastName)
           ) %>% 
    ungroup() %>% 
    left_join(., roster_players %>% 
                select(player_team_num, position), 
              by = "player_team_num"
              ) %>% 
    mutate(position_type = ifelse(position == "G", "G", 
                                  ifelse(position == "D", "D", 
                                         "F"))
           ) %>%
    select(player, player_num, Team, game_id, game_date, season, session, num_last_first, 
           player_team_num, firstName, lastName, venue, position_type, position
           ) %>% 
    arrange(venue, player) %>% 
    data.frame()
  
  
  # Roster data to be returned from final compile function
  roster_df_return <- roster_df %>% 
    mutate(Opponent = ifelse(Team == game_info_data$home_team, game_info_data$away_team, game_info_data$home_team), 
           is_home =  1 * (Team == game_info_data$home_team)
           ) %>% 
    select(player, position, position_type, game_id, game_date, season, session, Team, Opponent, is_home, 
           num_last_first, player_num, player_team_num, firstName, lastName
           ) %>% 
    data.frame()
  
  
  # Return data as list
  return_list <- list(roster_df =       roster_df, 
                      roster_df_final = roster_df_return, 
                      scratches_df =    roster_scratches)
  
  }

# Create event summary data frame
sc.event_summary <- function(game_id_fun, season_id_fun, event_summary_data, roster_data, game_info_data) { 
  
  # Find players in scraped html
  index <-     grep("^([A-Z]+|[A-Z]+.+[A-Z+]),\\s*[A-Z]+", event_summary_data) - 2
  index_sep <- grep("^TEAM TOTALS", event_summary_data)
  
  away_index <- index[index > 0 & index < index_sep[1]]
  home_index <- index[index > index_sep[1] & index < index_sep[2]]
  
  
  # Away players
  event_summary_away <- foreach(i = 1:length(away_index), .combine = bind_rows) %do% {
    
    event_summary_data[away_index[i]:(away_index[i] + 24)] %>%
      matrix(ncol = 25, byrow = TRUE) %>%
      data.frame(stringsAsFactors = FALSE)
    
    } %>% 
    mutate_all(funs(gsub("\\b\\s\\b", 0, .))) %>% 
    mutate(Team =     game_info_data$away_team, 
           Opponent = game_info_data$home_team, 
           is_home =  0)
  
  # Home players
  event_summary_home <- foreach(i = 1:length(home_index), .combine = bind_rows) %do% {
    
    event_summary_data[home_index[i]:(home_index[i] + 24)] %>%
      matrix(ncol = 25, byrow = TRUE) %>%
      data.frame(stringsAsFactors = FALSE)
    
    } %>% 
    mutate_all(funs(gsub("\\b\\s\\b", 0, .))) %>% 
    mutate(Team =     game_info_data$home_team, 
           Opponent = game_info_data$away_team, 
           is_home =  1)
  
  
  event_summary_all <- bind_rows(event_summary_away, event_summary_home)
  
  colnames(event_summary_all) <- c("player_num", "position", "player", "G", "A", "P", "P_M", "PEN", "PIM", 
                                   "TOI_all", "SHF", "AVG", "TOI_PP", "TOI_SH", "TOI_EV", 
                                   "S", "A_B", "MS", "HT", "GV", "TK", "BS", "FW", "FL", "FO_perc", 
                                   "Team", "Opponent", "is_home")
  
  
  event_summary_all <- event_summary_all %>% 
    mutate_at(vars(G:PIM, SHF, S:FO_perc), funs(suppressWarnings(as.numeric(.)))) %>%   ## Warning: In evalq(as.numeric(G), <environment>) : NAs introduced by coercion
    mutate(player_team_num = paste0(Team, player_num), 
           player =          roster_data$player[match(player_team_num, roster_data$player_team_num)], 
           position_type =   ifelse(position == "D", "D", 
                                    ifelse(position == "G", "G", 
                                           "F")), 
           game_id =         game_id_fun, 
           game_date =       game_info_data$game_date, 
           season =          season_id_fun, 
           session =         game_info_data$session
           ) %>% 
    filter(!is.na(player)) %>% 
    mutate_at(vars(TOI_all, AVG, TOI_PP, TOI_SH, TOI_EV), 
              funs(suppressWarnings(round(period_to_seconds(ms(.)) / 60, 2)))
              ) %>% 
    mutate_all(funs(ifelse(is.na(.), 0, .))) %>% 
    select(player, position, position_type, game_id, game_date, season, session, Team, Opponent, is_home, 
           TOI_all, TOI_EV, TOI_PP, TOI_SH, SHF, AVG, 
           G, A, P, P_M, PEN, PIM, S, A_B, MS, HT, GV, TK, BS, FW, FL, FO_perc
           ) %>% 
    data.frame()
  
  }


## ------------------------------- ##
##   Prepare Events & Shift Data   ##
## ------------------------------- ##

# Prepare Events Data (HTM)
sc.prepare_events_HTM <- function(game_id_fun, season_id_fun, events_data, game_info_data) { 
  
  # Convert raw html to data frame
  pbp_HTM_events <- events_data %>% 
    matrix(byrow = TRUE, 
           ncol = 8
           ) %>% 
    data.frame(stringsAsFactors = FALSE) %>% 
    select(eventIdx =          X1,  
           game_period =       X2, 
           strength =          X3, 
           times =             X4, 
           event_type =        X5, 
           event_description = X6, 
           away_skaters =      X7, 
           home_skaters =      X8
           ) %>% 
    filter(game_period != "Per") %>% 
    mutate(eventIdx =     as.numeric(eventIdx),  
           game_id =      as.character(game_id_fun), 
           game_period =  as.numeric(game_period), 
           event_detail = 
             case_when(
               event_type %in% c("SHOT", "MISS", "BLOCK", "GOAL") ~ str_extract(event_description, ",\\s*[a-zA-Z|-]+,") %>% gsub(",|\\s|-", "", .), 
               event_type %in% c("PSTR", "PEND", "SOC", "GEND") ~   str_extract(event_description, "[0-9]+:[0-9]+\\s*[A-Z]+"), 
               event_type == "PENL" ~ str_extract(event_description, "[(][0-9]+\\s[a-z]*[)]") %>% gsub("[(]|[)]|\\s*", "", .), 
               event_type == "CHL" ~  str_extract(event_description, "-[a-zA-Z\\s]+-") %>% gsub("\\s*-|-\\s*", "", .)
               ), 
           time_elapsed = str_extract(times, "[0-9]+:[0-9]{2}"), 
           game_seconds = suppressWarnings(period_to_seconds(ms(time_elapsed))), 
           game_seconds = 
             case_when(
               game_period == 2 ~ game_seconds + 1200, 
               game_period == 3 ~ game_seconds + 2400, 
               game_period == 4 ~ game_seconds + 3600, 
               game_period == 5 & game_info_data$session == "R" ~ game_seconds + 3900, 
               game_period == 5 & game_info_data$session == "P" ~ game_seconds + 4800, 
               game_period == 6 & game_info_data$session == "P" ~ game_seconds + 6000, 
               game_period == 7 & game_info_data$session == "P" ~ game_seconds + 7200, 
               TRUE ~ game_seconds
               ), 
           home_team =    game_info_data$home_team, 
           away_team =    game_info_data$away_team
           ) %>% 
    group_by(eventIdx) %>% 
    mutate(event_description = gsub("\\bPHX\\b", "ARI", event_description),   ## change PHX to ARI in event description
           
           event_team = ifelse(event_type %in% c(sc.main_events, "CHL"), str_extract(event_description, "^[A-Z]\\.[A-Z]|^[A-Z]+"), NA), 
           event_team = ifelse(!event_team %in% Team_ID_vec, NA, event_team),  ## ensure event_team extacted is an actual team
           
           event_player_1 = case_when(
             event_type %in% c("GOAL", "HIT", "MISS", "BLOCK", "FAC") ~ 
               str_extract(event_description, "([A-Z]+\\.[A-Z]+|[A-Z]+)\\s*#[0-9]+"), 
             
             event_type %in% c("SHOT", "GIVE", "TAKE") ~ 
               gsub("ONGOAL\\s*-\\s*|GIVEAWAY\\s*-\\s*|TAKEAWAY\\s*-\\s*", "", event_description) %>% str_extract(., "([A-Z]+\\.[A-Z]+|[A-Z]+)\\s*(#[0-9]+|[0-9]+)"),
             
             event_type == "PENL" & !grepl("TEAM", event_description) ~                                       ## normal penalites
               str_extract(event_description, "([A-Z]+\\.[A-Z]+|[A-Z]+)\\s*#[0-9]+"), 
             
             event_type == "PENL" & grepl("TEAM", event_description) & grepl("#[0-9]+", event_description) ~  ## bench minors (delay of game, faceoff violation, etc.)
               paste(event_team, str_extract(event_description, "#[0-9]+"))
             ), 
           
           event_player_2 = case_when(
             event_type %in% c("BLOCK", "FAC", "HIT", "PENL") & length(str_extract_all(event_description, "#[0-9]+")[[1]]) > 1 ~  ## ensure a player is present
               str_extract_all(event_description, "([A-Z]+\\.[A-Z]+|[A-Z]+)\\s*#[0-9]+")[[1]][2], 
             
             event_type == "GOAL" & length(str_extract_all(event_description, "#[0-9]+")[[1]]) > 1 ~  ## ensure a player is present
               paste(event_team, str_extract_all(event_description, "#[0-9]+")[[1]][2])
             ), 
           
           event_player_3 = case_when(
             event_type == "GOAL" & length(str_extract_all(event_description, "#[0-9]+")[[1]]) > 2 ~  ## ensure a player is present
               paste(event_team, str_extract_all(event_description, "#[0-9]+")[[1]][3])
             ), 
           
           event_zone = str_extract(event_description, "[a-zA-Z]{3}\\.\\s*[zZ]one") %>% gsub("\\.\\s*[zZ]one", "", .)
           ) %>% 
    ungroup() %>% 
    mutate_at(vars(event_player_1:event_player_3), 
              funs(gsub("#|\\s*", "", .))
              ) %>% 
    mutate_at(vars(event_player_1:event_player_3),  ## remove event players for true team/bench penalties
              funs(ifelse(grepl("[tT]oo\\s*many\\s*men|[A-Z]+\\s*[A-Z]+\\s*[bB]ench[(]", event_description), NA, .))
              ) %>% 
    select(-c(home_skaters, away_skaters)) %>% 
    data.frame()
  
  }

# Prepare Events Data (API)
sc.prepare_events_API <- function(game_id_fun, events_data_API, game_info_data) { 
  
  # Parse API events data
  events_join_API_raw <- 
    bind_cols(
      events_data_API$liveData$plays$allPlays$about %>% select(-goals), 
      events_data_API$liveData$plays$allPlays$about$goals %>% rename(away.goals = away, home.goals = home) 
      ) %>% 
    mutate(eventIdx = eventIdx + 1) %>% 
    left_join(
      bind_cols(
        events_data_API$liveData$plays$allPlays$result %>% select(-strength), 
        events_data_API$liveData$plays$allPlays$result$strength %>% rename(strength.code = code, strength.name = name)) %>% 
        mutate(eventIdx = row_number()), 
      by = "eventIdx"
      ) %>% 
    left_join( 
      events_data_API$liveData$plays$allPlays$coordinates %>% mutate(eventIdx = row_number()), 
      by = "eventIdx"
      ) %>% 
    left_join(
      events_data_API$liveData$plays$allPlays$team %>% 
        mutate(triCode =  Team_ID$Team[match(id, Team_ID$ID)], ## use HTM source team triCodes
               eventIdx = row_number()
          ), 
      by = "eventIdx"
      ) %>% 
    select(eventIdx, 
           game_period = period, 
           time_elapsed = periodTime, 
           event_type = eventTypeId, 
           event_team = triCode, 
           event_description = description, 
           coords_x = x, 
           coords_y = y
           ) %>% 
    mutate(game_seconds = suppressWarnings(period_to_seconds(ms(time_elapsed))), 
           game_seconds = 
             case_when(
               game_period == 2 ~ game_seconds + 1200, 
               game_period == 3 ~ game_seconds + 2400, 
               game_period == 4 ~ game_seconds + 3600, 
               game_period == 5 & game_info_data$session == "R" ~ game_seconds + 3900, 
               game_period == 5 & game_info_data$session == "P" ~ game_seconds + 4800, 
               game_period == 6 & game_info_data$session == "P" ~ game_seconds + 6000, 
               game_period == 7 & game_info_data$session == "P" ~ game_seconds + 7200, 
               TRUE ~ game_seconds
               ), 
           event_type = 
             case_when(
               event_type == "BLOCKED_SHOT" ~ "BLOCK", 
               event_type == "FACEOFF" ~ "FAC", 
               event_type == "PENALTY" ~ "PENL", 
               event_type == "GIVEAWAY" ~ "GIVE", 
               event_type == "TAKEAWAY" ~ "TAKE", 
               event_type == "MISSED_SHOT" ~ "MISS", 
               TRUE ~ event_type
               )
           ) %>% 
    filter(event_type %in% c("TAKE", "GIVE", "MISS", "HIT", "SHOT", "BLOCK", "GOAL", "PENL", "FAC")) %>% 
    data.frame()
  
  
  # Parse Roster Data
  roster_data_API <- bind_rows(
    # Home players
    foreach(i = 1:length(events_data_API$liveData$boxscore$teams$home$players), .combine = bind_rows) %do% {
      
      data.frame(player_ID = na_if_null(events_data_API$liveData$boxscore$teams$home$players[[i]]$person$id), 
                 fullName =  na_if_null(events_data_API$liveData$boxscore$teams$home$players[[i]]$person$fullName), 
                 Team =      game_info_data$home_team, 
                 number =    na_if_null(events_data_API$liveData$boxscore$teams$home$players[[i]]$jerseyNumber), 
                 stringsAsFactors = FALSE
        )
      
      } %>% 
      data.frame(row.names = NULL), 
    # Away players
    foreach(i = 1:length(events_data_API$liveData$boxscore$teams$away$players), .combine = bind_rows) %do% {
      
      data.frame(player_ID = na_if_null(events_data_API$liveData$boxscore$teams$away$players[[i]]$person$id), 
                 fullName =  na_if_null(events_data_API$liveData$boxscore$teams$away$players[[i]]$person$fullName), 
                 Team =      game_info_data$away_team, 
                 number =    na_if_null(events_data_API$liveData$boxscore$teams$away$players[[i]]$jerseyNumber), 
                 stringsAsFactors = FALSE
        )
      
      } %>% 
      data.frame(row.names = NULL)
    ) %>% 
    mutate(player_team_num = paste0(Team, number))
  
  
  # Parse event players data
  events_players_list_API <- events_data_API$liveData$plays$allPlays$players
  
  events_players_raw_API <- foreach(i = 1:length(events_players_list_API), .combine = rbind) %do% { 
    
    if (length(events_players_list_API[[i]]$player$fullName) > 0) { 
      
      data.frame(eventIdx =       i,  
                 event_player_1 = as.character(events_players_list_API[[i]]$player$id[1]), 
                 event_player_2 = as.character(events_players_list_API[[i]]$player$id[2]), 
                 stringsAsFactors = FALSE)
      
      } else {
      
        data.frame(eventIdx =       i,  
                   event_player_1 = NA, 
                   event_player_2 = NA, 
                   stringsAsFactors = FALSE)
      
        }
    
    } %>% 
    mutate(event_player_1 = roster_data_API$player_team_num[match(event_player_1, roster_data_API$player_ID)], 
           event_player_2 = roster_data_API$player_team_num[match(event_player_2, roster_data_API$player_ID)]
           ) %>% 
    data.frame()
  
  
  # Final Join
  events_join_API <- events_join_API_raw %>% 
    left_join(., events_players_raw_API, by = "eventIdx") %>% 
    ## Correct event players and event team for blocked shots
    mutate(hold_player_1 =  ifelse(event_type == "BLOCK", event_player_2, NA), 
           hold_player_2 =  ifelse(event_type == "BLOCK", event_player_1, NA), 
           event_player_1 = ifelse(event_type == "BLOCK", hold_player_1, event_player_1), 
           event_player_2 = ifelse(event_type == "BLOCK", hold_player_2, event_player_2), 
           event_team =     ifelse(event_type == "BLOCK", 
                                   ifelse(event_team == game_info_data$home_team, game_info_data$away_team, game_info_data$home_team), 
                                   event_team)
           ) %>% 
    select(game_period, game_seconds, event_type, event_team, event_description, coords_x, coords_y, event_player_1) %>% 
    data.frame()
  
  }

# Prepare Shifts Data
sc.prepare_shifts <- function(game_id_fun, season_id_fun, shifts_list, roster_data, game_info_data) { 
  
  ## --------------------- ##
  ##   Main Shifts Parse   ##
  ## --------------------- ##
  
  # Create player data frames utilized in loops
  player_home_df <- filter(roster_data, venue == "Home")
  player_away_df <- filter(roster_data, venue == "Away")
  
  
  # Combine home and away shifts
  shifts_parse_full <- bind_rows(
    # Home Shifts 
    foreach(i = 1:nrow(player_home_df), .combine = bind_rows) %do% {
      
      index <- which(shifts_list$home_shifts_titles[-1] == player_home_df$num_last_first[i])
      
      shifts_list$home_shifts_text[which(shifts_list$home_shifts_text %in% c("Shift #", "Présence #Shift #"))[index]:(which(shifts_list$home_shifts_text %in% c("SHF", "PR/SHF"))[index] - 3)] %>%
        matrix(ncol = 6,
               byrow = TRUE
               ) %>%
        data.frame(stringsAsFactors = FALSE) %>%
        mutate(player =         player_home_df$player[i],  
               num_last_first = player_home_df$num_last_first[i],
               event_team =     game_info_data$home_team
               ) %>%
        filter(X2 != "Per") %>%
        data.frame()
      
      } %>% 
      mutate_all(funs(ifelse(. == "", NA, .))) %>% 
      select(7:9, 1:5), 
    
    # Away Shifts
    foreach(i = 1:nrow(player_away_df), .combine = bind_rows) %do% {
      
      index <- which(shifts_list$away_shifts_titles[-1] == player_away_df$num_last_first[i])
      
      shifts_list$away_shifts_text[which(shifts_list$away_shifts_text %in% c("Shift #", "Présence #Shift #"))[index]:(which(shifts_list$away_shifts_text %in% c("SHF", "PR/SHF"))[index] - 3)] %>%
        matrix(ncol = 6,
               byrow = TRUE
               ) %>%
        data.frame(stringsAsFactors = FALSE) %>%
        mutate(player =         player_away_df$player[i],  
               num_last_first = player_away_df$num_last_first[i],
               event_team =     game_info_data$away_team
               ) %>%
        filter(X2 != "Per") %>%
        data.frame()
      
      } %>% 
      mutate_all(funs(ifelse(. == "", NA, .))) %>% 
      select(7:9, 1:5)
    ) %>% 
    # Fix rare shift_end issue (flip shift_end string if duration is over 30 minutes)
    group_by(player, num_last_first, event_team, X1, X2) %>% 
    mutate(X4 = ifelse(as.numeric(str_extract(X5, "^[0-9]+")) > 30, 
                       paste0(str_extract_all(X4, "[0-9]+:[0-9]+")[[1]][2], " / ", str_extract_all(X4, "[0-9]+:[0-9]+")[[1]][1]), 
                       X4)
           ) %>% 
    # Attempt to remove incorrect shifts
    ungroup() %>% 
    mutate(filtered = ifelse(X3 == "20:00 / 0:00" & lag(X4) == "20:00 / 0:00", 1, 0)) %>% 
    filter(filtered == 0) %>% 
    data.frame()
  
  colnames(shifts_parse_full)[4:8] <- c("shift_num", "game_period", "shift_start", "shift_end", "duration")
  
  
  ## --------------------- ##
  ##   Period Sums Parse   ##
  ## --------------------- ##
  
  # Duplicate shifts html text for parsing
  home_shifts_text_dup <- c(shifts_list$home_shifts_text, shifts_list$home_shifts_text)
  away_shifts_text_dup <- c(shifts_list$away_shifts_text, shifts_list$away_shifts_text)
  
  
  # Combine home and away period sums
  period_sum_full <- bind_rows(
    # Home Period Sums
    foreach(i = 1:nrow(player_home_df), .combine = bind_rows) %do% {
      
      index <- which(shifts_list$home_shifts_titles[-1] == player_home_df$num_last_first[i])
      
      home_shifts_text_dup[(which(home_shifts_text_dup %in% c("SHF", "PR/SHF"))[index] + 6):(which(home_shifts_text_dup %in% c("Shift #", "Présence #Shift #"))[index + 1] - 8)] %>% 
        matrix(ncol = 7,
               byrow = TRUE
               ) %>% 
        data.frame(stringsAsFactors = FALSE) %>%
        mutate(player =         player_home_df$player[i],  
               num_last_first = player_home_df$num_last_first[i],
               event_team =     game_info_data$home_team
               ) %>% 
        data.frame()
      
      } %>% 
      mutate_at(vars(X1:X7), funs(gsub("\\s", NA, .))),  ## turn blanks into NAs
    
    # Away Period Sums
    foreach(i = 1:nrow(player_away_df), .combine = bind_rows) %do% {
      
      index <- which(shifts_list$away_shifts_titles[-1] == player_away_df$num_last_first[i])
      
      away_shifts_text_dup[(which(away_shifts_text_dup %in% c("SHF", "PR/SHF"))[index] + 6):(which(away_shifts_text_dup %in% c("Shift #", "Présence #Shift #"))[index + 1] - 8)] %>% 
        matrix(ncol = 7,
               byrow = TRUE
               ) %>% 
        data.frame(stringsAsFactors = FALSE) %>%
        mutate(player =         player_away_df$player[i],  
               num_last_first = player_away_df$num_last_first[i],
               event_team =     game_info_data$away_team
               ) %>% 
        data.frame()
      
      } %>% 
      mutate_at(vars(X1:X7), funs(gsub("\\s", NA, .)))  ## turn blanks into NAs
    )
  
  colnames(period_sum_full)[1:7] <- c("Per", "SHF", "AVG", "TOI", "EV_TOT", "PP_TOT", "SH_TOT")
  
  # Finalize period sum data
  period_sum_full <- period_sum_full %>% 
    mutate(game_id =   as.character(game_id_fun), 
           game_date = game_info_data$game_date, 
           Per =       as.numeric(ifelse(Per == "OT", 4, Per)), 
           season =    game_info_data$season, 
           session =   game_info_data$session,
           Team =      event_team, 
           Opponent =  ifelse(Team == game_info_data$home_team, game_info_data$home_team, game_info_data$away_team), 
           is_home =   1 * (Team == game_info_data$home_team)
           ) %>% 
    left_join(., roster_data %>% select(num_last_first, player_team_num, position_type, position), 
              by = "num_last_first"
              ) %>% 
    mutate_at(vars(Per, SHF), 
              funs(suppressWarnings(as.numeric(.)))
              ) %>% 
    mutate_at(vars(AVG, TOI, EV_TOT, PP_TOT, SH_TOT), 
              funs(suppressWarnings(period_to_seconds(ms(.))))  ## convert "00:00" format to seconds
              ) %>% 
    select(player, num_last_first, player_team_num, position, position_type, 
           game_id, game_date, season, session, Team, Opponent, is_home, 
           game_period = Per, 
           SHF, AVG, 
           TOI_all = TOI, 
           TOI_EV =  EV_TOT, 
           TOI_PP = PP_TOT, 
           TOI_SH = SH_TOT
           ) %>% 
    data.frame()
  
  
  ## ---------------------- ##
  ##   Create Shifts Data   ##
  ## ---------------------- ##
  
  shifts_raw <- shifts_parse_full %>% 
    mutate(game_id =       as.character(game_id_fun), 
           game_date =     game_info_data$game_date, 
           season =        game_info_data$season, 
           game_period =   as.numeric(ifelse(game_period == "OT", 4, game_period)), 
           home_team =     game_info_data$home_team, 
           away_team =     game_info_data$away_team, 
           shift_start =   gsub(" / .*", "", shift_start), 
           seconds_start = suppressWarnings(period_to_seconds(ms(shift_start))), 
           seconds_start = 
             case_when(
               game_period == 2 ~ seconds_start + 1200, 
               game_period == 3 ~ seconds_start + 2400, 
               game_period == 4 ~ seconds_start + 3600, 
               game_period == 5 & game_info_data$session == "R" ~ seconds_start + 3900, 
               game_period == 5 & game_info_data$session == "P" ~ seconds_start + 4800, 
               game_period == 6 & game_info_data$session == "P" ~ seconds_start + 6000, 
               game_period == 7 & game_info_data$session == "P" ~ seconds_start + 7200, 
               TRUE ~ seconds_start
               ),  
           shift_end =     gsub(" / .*", "", shift_end), 
           seconds_end =   suppressWarnings(period_to_seconds(ms(shift_end))), 
           seconds_end = 
             case_when(
               game_period == 2 ~ seconds_end + 1200, 
               game_period == 3 ~ seconds_end + 2400, 
               game_period == 4 ~ seconds_end + 3600, 
               game_period == 5 & game_info_data$session == "R" ~ seconds_end + 3900, 
               game_period == 5 & game_info_data$session == "P" ~ seconds_end + 4800, 
               game_period == 6 & game_info_data$session == "P" ~ seconds_end + 6000, 
               game_period == 7 & game_info_data$session == "P" ~ seconds_end + 7200, 
               TRUE ~ seconds_end
               ), 
           seconds_duration = seconds_end - seconds_start
           ) %>% 
    left_join(roster_data %>% select(num_last_first, player_team_num, position_type, position), 
              by = "num_last_first"
              ) %>% 
    select(player, position_type, position, num_last_first, player_team_num, game_id, game_date, season, event_team, game_period, shift_num, 
           seconds_start, seconds_end, seconds_duration, shift_start, shift_end, duration, 
           everything()
           ) %>% 
    arrange(seconds_start, event_team) %>% 
    data.frame()
  
  
  
  ## ------------------------------- ##
  ##   Missing Goalie Shifts Check   ##
  ## ------------------------------- ##
  
  # Get goalie shifts that started a period
  goalie_shifts_check <- shifts_raw %>% 
    filter(position == "G", 
           seconds_start %in% seq(0, 7200, by = 1200)
           ) %>% 
    mutate(max_period =  max(game_period)) %>% 
    arrange(game_period)
  
  # Create goalie period start table to verify a goalie started each period
  goalie_start_df <- data.frame(seconds_start = unique(goalie_shifts_check$seconds_start), 
                                game_period =   unique(goalie_shifts_check$game_period)
                                ) %>% 
    left_join(goalie_shifts_check %>% 
                filter(event_team == game_info_data$home_team) %>% 
                select(seconds_start, game_period, home_goalie = player), 
              by = c("seconds_start", "game_period")
              ) %>% 
    left_join(goalie_shifts_check %>% 
                filter(event_team == game_info_data$away_team) %>% 
                select(seconds_start, game_period, away_goalie = player), 
              by = c("seconds_start", "game_period")
              ) %>% 
    mutate(home_g_fix =  1 * is.na(home_goalie), 
           away_g_fix =  1 * is.na(away_goalie)
           ) %>% 
    data.frame()
  
  
  # Create new goalie shifts if a goalie did not start a period for a team
  if (sum(goalie_start_df$home_g_fix + goalie_start_df$away_g_fix) > 0) { 
    
    # Pull out specific problem line from period_sum_full object
    goalie_period_check <- period_sum_full %>% 
      filter(position == "G", 
             is.na(AVG))
    
    # Create new home goalie shifts if able to identify which goalie's shift is missing in period_sum_full object
    if (nrow(goalie_period_check) > 0) { 
      
      # Create new home goalie shift
      if (sum(goalie_start_df$home_g_fix) > 0 & goalie_period_check$Team == game_info_data$home_team) { 
        new_goalie_shift_home <- goalie_period_check %>% 
          filter(Team == game_info_data$home_team) %>% 
          mutate(event_team =       Team, 
                 shift_num =        NA, 
                 seconds_start =    ifelse(TOI_all == 1200, game_period * 1200 - 1200, NA), 
                 seconds_end =      ifelse(TOI_all == 1200, game_period * 1200, NA), 
                 seconds_duration = ifelse(TOI_all == 1200, 1200, NA), 
                 shift_start =      NA, 
                 shift_end =        NA, 
                 duration =         NA, 
                 home_team =        game_info_data$home_team, 
                 away_team =        game_info_data$away_team
                 ) %>% 
          select(colnames(shifts_raw)) %>% 
          data.frame()
        
        } else {
          new_goalie_shift_home <- data.frame(matrix(ncol = ncol(shifts_raw), nrow = 0))
          colnames(new_goalie_shift_home) <- colnames(shifts_raw)
        
          }
      
      # Create new away goalie shift
      if (sum(goalie_start_df$away_g_fix) > 0 & goalie_period_check$Team == game_info_data$away_team) {
        new_goalie_shift_away <- goalie_period_check %>% 
          filter(Team == game_info_data$away_team) %>% 
          mutate(event_team =       Team, 
                 shift_num =        NA, 
                 seconds_start =    ifelse(TOI_all == 1200, game_period * 1200 - 1200, NA), 
                 seconds_end =      ifelse(TOI_all == 1200, game_period * 1200, NA), 
                 seconds_duration = ifelse(TOI_all == 1200, 1200, NA), 
                 shift_start =      NA, 
                 shift_end =        NA, 
                 duration =         NA, 
                 home_team =        game_info_data$home_team, 
                 away_team =        game_info_data$away_team
                 ) %>% 
          select(colnames(shifts_raw)) %>% 
          data.frame()
        
        } else {
          new_goalie_shift_away <- data.frame(matrix(ncol = ncol(shifts_raw), nrow = 0))
          colnames(new_goalie_shift_away) <- colnames(shifts_raw)  
        
          }
      
      # Combine
      shifts_raw_hold <- bind_rows(
        shifts_raw, 
        new_goalie_shift_home, 
        new_goalie_shift_away
        ) %>% 
        arrange(seconds_start, event_team) %>% 
        data.frame()
      
      
      ## ---------- ##
      ##   Verify   ##
      ## ---------- ##
      
      # Get goalie shifts that started a period
      goalie_shifts_check_again <- shifts_raw_hold %>% 
        filter(position == "G", 
               seconds_start %in% seq(0, 7200, by = 1200)
               ) %>% 
        mutate(max_period =  max(game_period)) %>% 
        arrange(game_period)
      
      # Create goalie period start table to verify a goalie started each period
      goalie_start_df_again <- data.frame(seconds_start = unique(goalie_shifts_check_again$seconds_start), 
                                          game_period =   unique(goalie_shifts_check_again$game_period)
                                          ) %>% 
        left_join(goalie_shifts_check_again %>% 
                    filter(event_team == game_info_data$home_team) %>% 
                    select(seconds_start, game_period, home_goalie = player), 
                  by = c("seconds_start", "game_period")
                  ) %>% 
        left_join(goalie_shifts_check_again %>% 
                    filter(event_team == game_info_data$away_team) %>% 
                    select(seconds_start, game_period, away_goalie = player), 
                  by = c("seconds_start", "game_period")
                  ) %>% 
        mutate(home_g_fix =  1 * is.na(home_goalie), 
               away_g_fix =  1 * is.na(away_goalie)
               ) %>% 
        data.frame()
      
      
      # Replace shifts_raw object if conditions met
      if (sum(goalie_start_df_again$home_g_fix) + sum(goalie_start_df_again$away_g_fix) == 0 & nrow(goalie_start_df_again) ==  max(goalie_start_df_again$game_period)) { 
        shifts_raw <- shifts_raw_hold
        
        }
      
      }
    
    }
  
  
  
  ## -------------------------------- ##
  ##   Determine ON/OFF Event Types   ##
  ## -------------------------------- ##
  
  # Determine Changes - ON Events
  shifts_parse_ON <- shifts_raw %>% 
    select(player_team_num, event_team, game_id, game_period, seconds_start, home_team, away_team) %>% 
    group_by(event_team, home_team, away_team, game_id, game_period, seconds_start) %>% 
    mutate(changes = n(), 
           players_substituted = paste(unique(player_team_num), collapse = ", ")
           ) %>% 
    summarise_at(vars(changes, players_substituted), funs(first(.))) %>% 
    mutate(event_type = "ON") %>% 
    rename(game_seconds = seconds_start) %>% 
    data.frame()
  
  
  # Determine Changes - OFF Events
  shifts_parse_OFF <- shifts_raw %>% 
    select(player_team_num, event_team, game_id, game_period, seconds_end, home_team, away_team) %>% 
    group_by(event_team, home_team, away_team, game_id, game_period, seconds_end) %>% 
    mutate(changes = n(), 
           players_substituted = paste(unique(player_team_num), collapse = ", ")
           ) %>% 
    summarise_at(vars(changes, players_substituted), funs(first(.))) %>% 
    mutate(event_type = "OFF") %>% 
    rename(game_seconds = seconds_end) %>% 
    data.frame()
  
  
  # Combine ON & OFF Events   
  shifts_parse_ALL <- bind_rows(
    shifts_parse_ON, 
    shifts_parse_OFF
    ) %>% 
    mutate(index = 1 * (event_team == home_team)) %>%  ## ensure away team changes first
    arrange(game_seconds, event_type, index) %>% 
    select(game_id, game_period, game_seconds, event_team, event_type, changes, players_substituted) %>% 
    data.frame()
  
  
  # Modify shifts_raw to return in desired format
  player_shifts_final <- shifts_raw %>% 
    mutate(session =  game_info_data$session, 
           Team =     ifelse(event_team == home_team, home_team, away_team), 
           Opponent = ifelse(event_team == home_team, away_team, home_team), 
           is_home =  1 * (event_team == home_team)
           ) %>% 
    select(player, num_last_first, player_team_num, position, position_type, game_id, game_date, season, session, Team, Opponent, is_home,
           game_period, shift_num, seconds_start, seconds_end, seconds_duration, shift_start, shift_end, duration
           ) %>% 
    data.frame()
  
  
  # Return data as list
  return_list <- list(player_shifts_final = player_shifts_final, 
                      player_period_sums =  period_sum_full %>% mutate_at(vars(AVG, TOI_all:TOI_SH), funs(round(. / 60, 2))), 
                      on_off_events =       shifts_parse_ALL
                      )
  
  }


## -------------------- ##
##   Join Coordinates   ##
## -------------------- ##

# Join coordinates (NHL API, main)
sc.join_coordinates_API <- function(events_data_API, events_data_HTM) { 
  
  # Base JSON events
  pbp_JSON_events <- events_data_API %>% 
    group_by(game_period, game_seconds, event_type, event_team) %>% 
    mutate(group_count = n()) %>% 
    data.frame()
  
  
  ## ----------------------------------- ##
  ##   Add Coords to HTM Events - Base   ##
  ## ----------------------------------- ##
  
  combine_events_okay_API <- events_data_HTM %>% 
    select(eventIdx, game_period, game_seconds, event_type, event_team) %>% 
    filter(event_type %in% unique(na.omit(pbp_JSON_events$event_type))) %>% 
    left_join(., pbp_JSON_events %>% 
                select(-c(event_player_1)) %>% 
                filter(group_count == 1),  ## filter out concurrent events
              by = c("game_period", "game_seconds", "event_type", "event_team")
              ) %>% 
    filter(!is.na(group_count)) %>% 
    select(-c(group_count)) %>% 
    data.frame()
  
  
  
  ## ---------------------------------------- ##
  ##   Add Coords to HTM Events - Reconcile   ##
  ## ---------------------------------------- ##
  
  pbp_JSON_events_prob <- NULL
  
  # Reconcile coordinates for concurrent events (same type, same second)
  if (nrow(filter(pbp_JSON_events, group_count > 1, event_type != "PENL")) > 0) { 
    
    # Prepare concurrent events data (JSON)
    pbp_JSON_events_prob <- pbp_JSON_events %>% 
      filter(group_count > 1,           ## only act on problem rows
             event_type != "PENL"       ## not adding coordinates for concurrent penalties
             ) %>%  
      mutate(index = row_number()) %>% 
      group_by(game_period, game_seconds, event_type, event_player_1) %>% 
      mutate(same_check = n()) %>%      ## determine if/which event_player_1s are the same
      data.frame()
    
    
    # Fix non-penalty events using pbp_JSON_events_prob object
    if (nrow(pbp_JSON_events_prob) > 0) { 
      
      # Initial join with HTM events
      hold_df_API <- events_data_HTM %>% 
        select(eventIdx, game_period, game_seconds, event_type, event_team, event_player_1) %>% 
        filter(event_type %in% unique(na.omit(pbp_JSON_events$event_type))) %>% 
        left_join(., pbp_JSON_events_prob, 
                  by = c("game_period", "game_seconds", "event_type", "event_team", "event_player_1")
                  ) %>% 
        filter(!is.na(group_count))
      
      # Each event_player_1 is separate
      done_df_API <- hold_df_API %>% 
        filter(same_check == 1) %>% 
        select(eventIdx, game_period, game_seconds, event_type, event_team, event_description, coords_x, coords_y)
      
      # 2 concurrent events - same event_player_1
      fix_df_API_2 <- hold_df_API %>% 
        filter(same_check == 2) %>% 
        # Determine rows to keep
        group_by(game_period, game_seconds, event_type) %>% 
        mutate(row =      row_number(),  
               filtered = ifelse(row_number() == max(row) | row_number() == min(row), 1, 0)
               ) %>% 
        filter(filtered == 1) %>% 
        # Ensure new rows created from join are filtered out if something went wrong
        group_by(eventIdx) %>% 
        mutate(remove = row_number() - 1) %>%  ##  (any "created" row marked with a 1, 2, 3...)
        filter(remove == 0) %>%                ##  ("created" rows filtered out)
        ungroup() %>% 
        select(eventIdx, game_period, game_seconds, event_type, event_team, event_description, coords_x, coords_y) %>% 
        data.frame()
      
      # 3 concurrent events - same event_player_1
      fix_df_API_3 <- hold_df_API %>% 
        filter(same_check == 3) %>% 
        # Determine rows to keep
        group_by(game_period, game_seconds, event_type) %>% 
        mutate(row =      row_number(),  
               filtered = ifelse(row_number() == max(row) | row_number() == min(row) | row_number() == min(row) + 4, 1, 0)
               ) %>% 
        filter(filtered == 1) %>% 
        # Ensure new rows created from join are filtered out if something went wrong
        group_by(eventIdx) %>% 
        mutate(remove = row_number() - 1) %>%  ##  (any "created" row marked with a 1, 2, 3...)
        filter(remove == 0) %>%                ##  ("created" rows filtered out)
        ungroup() %>% 
        select(eventIdx, game_period, game_seconds, event_type, event_team, event_description, coords_x, coords_y) %>% 
        data.frame()
      
      # 4 concurrent events - same event_player_1
      fix_df_API_4 <- hold_df_API %>% 
        filter(same_check == 4) %>% 
        # Determine rows to keep
        group_by(game_period, game_seconds, event_type) %>% 
        mutate(row =      row_number(),  
               filtered = ifelse(row_number() == max(row) | row_number() == min(row) | row_number() == min(row) + 5 | row_number() == min(row) + 10, 1, 0)
               ) %>% 
        filter(filtered == 1) %>% 
        # Ensure new rows created from join are filtered out if something went wrong
        group_by(eventIdx) %>% 
        mutate(remove = row_number() - 1) %>%  ##  (any "created" row marked with a 1, 2, 3...)
        filter(remove == 0) %>%                ##  ("created" rows filtered out)
        ungroup() %>% 
        select(eventIdx, game_period, game_seconds, event_type, event_team, event_description, coords_x, coords_y) %>% 
        data.frame()
      
      # 5 concurrent events - *** NO EXAMPLES TO CORRECT
      
      
      # Combine to final object for joining
      combine_events_prob_API <- bind_rows(
        done_df_API, 
        fix_df_API_2, 
        fix_df_API_3, 
        fix_df_API_4
        ) %>% 
        arrange(eventIdx)
      
      } 
    
    
    # Join to data not effected by concurrent event issues
    if (exists("combine_events_prob_API")) { 
      combine_events_reconcile_API <- bind_rows(
        combine_events_okay_API, 
        combine_events_prob_API
        ) %>% 
        arrange(eventIdx) %>% 
        data.frame()
      
      } else {
        combine_events_reconcile_API <- combine_events_okay_API
      
        }
    
    }
  
  
  
  ## ---------------------------------- ##
  ##   Join JSON Coords w/ HTM Events   ##
  ## ---------------------------------- ##
  
  # Determine object to join
  if (nrow(filter(pbp_JSON_events, group_count > 1, event_type != "PENL")) == 0) { 
    combine_events_final_API <- combine_events_okay_API
    
    } else {
      combine_events_final_API <- combine_events_reconcile_API
    
      }
  
  
  # Final Join
  pbp_events_full <- events_data_HTM %>% 
    left_join(., combine_events_final_API %>% 
                select(eventIdx, game_period, game_seconds, event_type, coords_x, coords_y, event_team, 
                       event_description_alt = event_description
                       ), 
              by = c("eventIdx", "game_period", "game_seconds", "event_type", "event_team")
              ) %>% 
    data.frame()
  
  
  # Override reconciliation process if duplicate rows created
  if (nrow(events_data_HTM) != nrow(pbp_events_full)) {
    warning("additional events created in coordinate reconciliation, NAs created in lieu")
    
    pbp_events_full <- events_data_HTM %>% 
      left_join(., combine_events_okay %>% 
                  select(eventIdx, game_period, game_seconds, event_type, coords_x, coords_y, event_team, 
                         event_description_alt = event_description
                         ), 
                by = c("eventIdx", "game_period", "game_seconds", "event_type", "event_team")
                ) %>% 
      data.frame()
    
    }
  
  
  return(pbp_events_full)
  
  }

# Join coordinates (ESPN, backup)
sc.join_coordinates_ESPN <- function(season_id_fun, events_data_ESPN, events_data_HTM, roster_data, game_info_data) { 
  
  # Change WPG to ATL if applicable (same ESPN team ID)
  if (as.numeric(season_id_fun) <= 20102011) { 
    ESPN_team_IDs_update <- ESPN_team_IDs %>% 
      mutate(Team = ifelse(Team == "WPG", "ATL", Team))
    
    } else {
      ESPN_team_IDs_update <- ESPN_team_IDs
    
      }
  
  
  ## --------------------------- ##
  ##   Prepare All ESPN Events   ##
  ## --------------------------- ##
  
  pbp_ESPN_events <- events_data_ESPN %>%
    select(ESPN_type =         X3, 
           coords_x =          X1, 
           coords_y =          X2, 
           time =              X4, 
           game_period =       X5, 
           team_ID_ESPN =      X15, 
           event_description = X9
           ) %>% 
    mutate_at(vars(coords_x, coords_y, game_period), 
              funs(as.numeric(.))
              ) %>% 
    mutate(game_seconds = period_to_seconds(ms(time)), 
           game_seconds = ifelse(game_period == 2, game_seconds + 1200, 
                                 ifelse(game_period == 3, game_seconds + 2400, 
                                        ifelse(game_period == 4, game_seconds + 3600, 
                                               ifelse(game_period == 5, game_seconds + 3900, 
                                                      game_seconds)))
                                 ), 
           # Join in event types
           event_type = ESPN_codes$event[match(ESPN_type, ESPN_codes$code)], 
           event_type = 
             case_when(
               event_type == "GvTk" & grepl("Giveaway", event_description) ~ "GIVE", 
               event_type == "GvTk" & grepl("Takeaway", event_description) ~ "TAKE", 
               event_type == "SOut" & grepl("SAVE", event_description) ~ "SHOT", 
               event_type == "SOut" & grepl("MISS", event_description) ~ "MISS", 
               event_type == "SOut" & grepl("GOAL", event_description) ~ "GOAL", 
               TRUE ~ event_type
               ), 
           event_team = ESPN_team_IDs_update$Team[match(team_ID_ESPN, ESPN_team_IDs_update$team_ID)], 
           # Account for ESPN's strange method of recording coordinates 
           coords_x = 
             case_when(
               game_info_data$season %in% c("20072008", "20082009") ~ coords_x - 1, 
               as.numeric(game_info_data$season) >= 20162017 ~ coords_x + 1, 
               TRUE ~ coords_x
               ), 
           coords_y = 
             case_when(
               game_info_data$season %in% c("20072008", "20082009") ~ coords_y + 1, 
               as.numeric(game_info_data$season) >= 20162017 ~ coords_y - 1, 
               TRUE ~ coords_y
               )
           ) %>% 
    filter(event_type %in% c("TAKE", "GIVE", "MISS", "HIT", "SHOT", "BLOCK", "GOAL", "PENL", "FAC")) %>% 
    # Determine if simultaneous events are present
    group_by(game_period, game_seconds, event_type, event_team) %>% 
    mutate(group_count = n()) %>% 
    select(game_period, game_seconds, event_type, event_team, event_description, coords_x, coords_y, group_count, team_ID_ESPN, ESPN_type) %>% 
    data.frame()
  
  
  
  ## ----------------------------------- ##
  ##   Add Coords to HTM Events - Base   ##
  ## ----------------------------------- ##
  
  combine_events_okay <- events_data_HTM %>% 
    select(eventIdx, game_period, game_seconds, event_type, event_team) %>% 
    filter(event_type %in% unique(na.omit(pbp_ESPN_events$event_type))) %>% 
    left_join(., pbp_ESPN_events %>% 
                filter(group_count == 1),  ## filter out concurrent events
              by = c("game_period", "game_seconds", "event_type", "event_team")
              ) %>% 
    filter(!is.na(group_count)) %>% 
    select(-c(group_count)) %>% 
    data.frame()
  
  
  
  ## ---------------------------------------- ##
  ##   Add Coords to HTM Events - Reconcile   ##
  ## ---------------------------------------- ##
  
  pbp_ESPN_events_prob <- NULL
  
  # Reconcile coordinates for concurrent events (same type, same second)
  if (nrow(filter(pbp_ESPN_events, group_count > 1, event_type != "PENL")) > 0) { 
    
    # Prepare concurrent events data (ESPN)
    pbp_ESPN_events_prob <- pbp_ESPN_events %>% 
      filter(group_count > 1,           ## only act on problem rows
             event_type != "PENL"       ## not adding coordinates for concurrent penalties
             ) %>%  
      mutate(index = row_number()) %>% 
      # Extract the first event player
      mutate(player_match = 
               case_when(
                 # Normal Strengths
                 event_type == "SHOT" & game_period < 5 ~  gsub("Shot\\s*on\\s*goal\\s*by\\s*", "", event_description) %>% gsub("\\s*saved by.+|\\s*[(]+.*", "", .), 
                 event_type == "MISS" & game_period < 5 ~  gsub("Shot\\s*missed\\s*by\\s*", "", event_description) %>% gsub("\\s*[(]+.*", "", .), 
                 event_type == "BLOCK" & game_period < 5 ~ gsub("\\s*shot\\s*blocked\\s*by.*", "", event_description), 
                 ## blocked shots do not have the shooter prior to 2010-2011
                 
                 event_type == "HIT" ~  gsub("\\s*credited\\s*.*", "", event_description), 
                 event_type == "GIVE" ~ gsub("Giveaway\\s*by\\s*", "", event_description) %>% gsub("\\s*\\bin\\b\\s*.*", "", .), 
                 
                 # Shootouts
                 event_type %in% c("SHOT", "MISS") & game_period == 5 & grepl("Shootout\\s*attempt\\s*by\\s*", event_description) ~ gsub("Shootout\\s*attempt\\s*by\\s*", "", event_description) %>% gsub("\\s*saved by.+", "", .), 
                 event_type %in% c("SHOT", "MISS") & game_period == 5 & grepl("shootout\\s*attempt\\s*against", event_description) ~ gsub("\\s*shootout\\s*attempt\\s*against.*", "", event_description), 
                 
                 event_type == "GOAL" & game_period == 5 & grepl("Shootout\\s*GOAL\\s*scored\\s*by\\s*", event_description) ~ gsub("Shootout\\s*GOAL\\s*scored\\s*by\\s*", "", event_description) %>% gsub("\\s*\\bon\\b\\s*.*", "", .), 
                 event_type == "GOAL" & game_period == 5 & grepl("shootout\\s*attempt\\s*against", event_description) ~       gsub("\\s*shootout\\s*attempt\\s*against.*", "", event_description)
                 )
             ) %>% 
      group_by(index) %>% 
      mutate(last_name =      toupper(strsplit(player_match, "\\s")[[1]][2]), 
             event_player_1 = roster_data$player_team_num[match(paste0(event_team, last_name), paste0(roster_data$Team, roster_data$lastName))],  ## match by team and player last name
             
             # Attempt to address two-string last names
             name_length =    max(as.numeric(lapply(strsplit(player_match, "\\s"), length))), 
             event_player_1 = ifelse(is.na(event_player_1) & name_length > 2, 
                                     roster_data$player_team_num[match(
                                       paste0(event_team, 
                                              toupper(strsplit(player_match, "\\s")[[1]][2]), 
                                              toupper(strsplit(player_match, "\\s")[[1]][3])
                                              ), 
                                       paste0(roster_data$Team, 
                                              gsub("\\s*", "", roster_data$lastName))
                                       )], 
                                     event_player_1
                                     )
             ) %>% 
      select(-c(name_length)) %>% 
      group_by(game_period, game_seconds, event_type, event_player_1) %>% 
      mutate(same_check = n()) %>% 
      data.frame()
    
    
    # Fix non-penalty events using pbp_ESPN_events_prob object
    if (nrow(pbp_ESPN_events_prob) > 0) { 
      
      # Initial join with HTM events
      hold_df_ESPN <- events_data_HTM %>% 
        select(eventIdx, game_period, game_seconds, event_type, event_team, event_player_1) %>% 
        filter(event_type %in% unique(na.omit(pbp_ESPN_events$event_type))) %>% 
        left_join(., pbp_ESPN_events_prob, 
                  by = c("game_period", "game_seconds", "event_type", "event_team", "event_player_1")
                  ) %>% 
        filter(!is.na(group_count))
      
      # Each event_player_1 is separate
      done_df_ESPN <- hold_df_ESPN %>% 
        filter(same_check == 1) %>% 
        select(eventIdx, game_period, game_seconds, event_type, event_team, event_description, coords_x, coords_y)
      
      # 2 concurrent events - same event_player_1
      fix_df_ESPN_2 <- hold_df_ESPN %>% 
        filter(same_check == 2) %>% 
        # Determine rows to keep
        group_by(game_period, game_seconds, event_type) %>% 
        mutate(row =      row_number(),  
               filtered = ifelse(row_number() == max(row) | row_number() == min(row), 1, 0)
               ) %>% 
        filter(filtered == 1) %>% 
        # Ensure new rows created from join are filtered out if something went wrong
        group_by(eventIdx) %>% 
        mutate(remove = row_number() - 1) %>%  ##  (any "created" row marked with a 1, 2, 3...)
        filter(remove == 0) %>%                ##  ("created" rows filtered out)
        ungroup() %>% 
        select(eventIdx, game_period, game_seconds, event_type, event_team, event_description, coords_x, coords_y) %>% 
        data.frame()
      
      # 3 concurrent events - same event_player_1
      fix_df_ESPN_3 <- hold_df_ESPN %>% 
        filter(same_check == 3) %>% 
        # Determine rows to keep
        group_by(game_period, game_seconds, event_type) %>% 
        mutate(row =      row_number(),  
               filtered = ifelse(row_number() == max(row) | row_number() == min(row) | row_number() == min(row) + 4, 1, 0)
               ) %>% 
        filter(filtered == 1) %>% 
        # Ensure new rows created from join are filtered out if something went wrong
        group_by(eventIdx) %>% 
        mutate(remove = row_number() - 1) %>%  ##  (any "created" row marked with a 1, 2, 3...)
        filter(remove == 0) %>%                ##  ("created" rows filtered out)
        ungroup() %>% 
        select(eventIdx, game_period, game_seconds, event_type, event_team, event_description, coords_x, coords_y) %>% 
        data.frame()
      
      # 4 concurrent events - same event_player_1
      fix_df_ESPN_4 <- hold_df_ESPN %>% 
        filter(same_check == 4) %>% 
        # Determine rows to keep
        group_by(game_period, game_seconds, event_type) %>% 
        mutate(row =      row_number(),  
               filtered = ifelse(row_number() == max(row) | row_number() == min(row) | row_number() == min(row) + 5 | row_number() == min(row) + 10, 1, 0)
               ) %>% 
        filter(filtered == 1) %>% 
        # Ensure new rows created from join are filtered out if something went wrong
        group_by(eventIdx) %>% 
        mutate(remove = row_number() - 1) %>%  ##  (any "created" row marked with a 1, 2, 3...)
        filter(remove == 0) %>%                ##  ("created" rows filtered out)
        ungroup() %>% 
        select(eventIdx, game_period, game_seconds, event_type, event_team, event_description, coords_x, coords_y) %>% 
        data.frame()
      
      # 5 concurrent events - *** NO EXAMPLES TO CORRECT
      
      
      # Combine to final object for joining
      combine_events_prob <- bind_rows(
        done_df_ESPN, 
        fix_df_ESPN_2, 
        fix_df_ESPN_3, 
        fix_df_ESPN_4
        ) %>% 
        arrange(eventIdx)
      
      } 
    
    
    # Join to data not effected by concurrent event issues
    if (exists("combine_events_prob")) { 
      combine_events_reconcile <- bind_rows(
        combine_events_okay, 
        combine_events_prob
        ) %>% 
        arrange(eventIdx) %>% 
        data.frame()
      
      } 
    else {
      combine_events_reconcile <- combine_events_okay
      
      }
    
    }
  
  
  
  ## ---------------------------------- ##
  ##   Join ESPN Coords w/ HTM Events   ##
  ## ---------------------------------- ##
  
  # Determine object to join
  if (nrow(filter(pbp_ESPN_events, group_count > 1, event_type != "PENL")) == 0) { 
    combine_events_final <- combine_events_okay
    
    } else {
      combine_events_final <- combine_events_reconcile
    
      }
  
  
  # Final Join
  pbp_events_full <- events_data_HTM %>% 
    left_join(., combine_events_final %>% 
                select(eventIdx, game_period, game_seconds, event_type, coords_x, coords_y, event_team, 
                       event_description_alt = event_description
                       ), 
              by = c("eventIdx", "game_period", "game_seconds", "event_type", "event_team")
              ) %>% 
    data.frame()
  
  
  # Override reconciliation process if duplicate rows created
  if (nrow(events_data_HTM) != nrow(pbp_events_full)) {
    warning("additional events created in coordinate reconciliation, NAs created in lieu")
    
    pbp_events_full <- events_data_HTM %>% 
      left_join(., combine_events_okay %>% 
                  select(eventIdx, game_period, game_seconds, event_type, coords_x, coords_y, event_team, 
                         event_description_alt = event_description
                         ), 
                by = c("eventIdx", "game_period", "game_seconds", "event_type", "event_team")
                ) %>% 
      data.frame()
    
    }
  
  
  return(pbp_events_full)
  
  }


## -------------------- ##
##   Combine All Data   ##
## -------------------- ##

# Combine / Process Shifts and Events Data 
sc.pbp_combine <- function(events_data, shifts_data, roster_data, game_info_data) { 
  
  # Join / Arrange
  pbp_df <- bind_rows(
    events_data, 
    shifts_data
    ) %>% 
    mutate(season =    game_info_data$season, 
           game_date = game_info_data$game_date, 
           session =   game_info_data$session, 
           home_team = game_info_data$home_team, 
           away_team = game_info_data$away_team
           ) %>% 
    # determine priority for arranging events, shootouts excluded (modified from Manny Perry's code)
    mutate(priority = 
             1 * (event_type %in% c("TAKE", "GIVE", "MISS", "HIT", "SHOT", "BLOCK") & (game_period < 5 & session == "R")) +
             2 * (event_type == "GOAL" & (game_period < 5 & session == "R")) +
             3 * (event_type == "STOP" & (game_period < 5 & session == "R")) +
             4 * (event_type == "PENL" & (game_period < 5 & session == "R")) +
             5 * (event_type == "OFF" &  (game_period < 5 & session == "R")) +
             6 * (event_type %in% c("PEND", "GEND") & (game_period < 5 & session == "R")) +
             7 * (event_type == "ON" &   (game_period < 5 & session == "R")) +
             8 * (event_type == "FAC" &  (game_period < 5 & session == "R"))
           ) %>% 
    arrange(game_period, game_seconds, priority) %>% 
    mutate(event_index = cumsum(!is.na(game_id))) %>%
    select(-c(priority)) %>% 
    data.frame()
  
  
  # Loop to construct home matrix for players on ice
  is_on_matrix_home <- foreach(i = 1:length(roster_data$player_team_num), .combine = cbind) %do% { 
    
    vec <- cumsum(
      1 * (grepl(paste0("\\b", roster_data$player_team_num[i], "\\b"), pbp_df$players_substituted) &  ## "\\b" forces exact match
             pbp_df$event_type == "ON" &  
             pbp_df$event_team == pbp_df$home_team) - 
        1 * (grepl(paste0("\\b", roster_data$player_team_num[i], "\\b"), pbp_df$players_substituted) &  ## "\\b" forces exact match
               pbp_df$event_type == "OFF" & 
               pbp_df$event_team == pbp_df$home_team)
      )
    
    }
  
  # Loop to construct away matrix for players on ice
  is_on_matrix_away <- foreach(i = 1:length(roster_data$player_team_num), .combine = cbind) %do% { 
    
    vec <- cumsum(
      1 * (grepl(paste0("\\b", roster_data$player_team_num[i], "\\b"), pbp_df$players_substituted) &  ## "\\b" forces exact match
             pbp_df$event_type == "ON" &  
             pbp_df$event_team == pbp_df$away_team) - 
        1 * (grepl(paste0("\\b", roster_data$player_team_num[i], "\\b"), pbp_df$players_substituted) &  ## "\\b" forces exact match
               pbp_df$event_type == "OFF" & 
               pbp_df$event_team == pbp_df$away_team)
      )
    
    }
  
  
  # Set column names of matrices
  colnames(is_on_matrix_home) <- roster_data$player
  colnames(is_on_matrix_away) <- roster_data$player
  
  
  # Combine matrices (from Manny Perry's code)
  is_on_df_home <- which(is_on_matrix_home == 1, 
                         arr.ind = TRUE
                         ) %>%
    data.frame() %>%
    group_by(row) %>%
    summarise(home_on_1 = colnames(is_on_matrix_home)[unique(col)[1]],
              home_on_2 = colnames(is_on_matrix_home)[unique(col)[2]],
              home_on_3 = colnames(is_on_matrix_home)[unique(col)[3]],
              home_on_4 = colnames(is_on_matrix_home)[unique(col)[4]],
              home_on_5 = colnames(is_on_matrix_home)[unique(col)[5]],
              home_on_6 = colnames(is_on_matrix_home)[unique(col)[6]]
              ) %>%
    data.frame()
  
  is_on_df_away <- which(is_on_matrix_away == 1, 
                         arr.ind = TRUE
                         ) %>%
    data.frame() %>%
    group_by(row) %>%
    summarise(away_on_1 = colnames(is_on_matrix_away)[unique(col)[1]],
              away_on_2 = colnames(is_on_matrix_away)[unique(col)[2]],
              away_on_3 = colnames(is_on_matrix_away)[unique(col)[3]],
              away_on_4 = colnames(is_on_matrix_away)[unique(col)[4]],
              away_on_5 = colnames(is_on_matrix_away)[unique(col)[5]],
              away_on_6 = colnames(is_on_matrix_away)[unique(col)[6]]
              ) %>%
    data.frame()
  
  
  # Create vector to use for goalie determination
  goalie_vec <- c(as.character(filter(roster_data, position_type == "G", Team == game_info_data$home_team)$player), 
                  as.character(filter(roster_data, position_type == "G", Team == game_info_data$away_team)$player))
  
  
  # Add home/away goalies
  is_on_df_home <- is_on_df_home %>% 
    mutate(home_goalie = ifelse(home_on_1 %in% goalie_vec, home_on_1, 
                                ifelse(home_on_2 %in% goalie_vec, home_on_2, 
                                       ifelse(home_on_3 %in% goalie_vec, home_on_3, 
                                              ifelse(home_on_4 %in% goalie_vec, home_on_4, 
                                                     ifelse(home_on_5 %in% goalie_vec, home_on_5, 
                                                            ifelse(home_on_6 %in% goalie_vec, home_on_6, NA))))))
           ) %>% 
    rename(event_index = row) %>%  ## row determined above in matrix construction, renamed for joining
    data.frame()
  
  is_on_df_away <- is_on_df_away %>% 
    mutate(away_goalie = ifelse(away_on_1 %in% goalie_vec, away_on_1, 
                                ifelse(away_on_2 %in% goalie_vec, away_on_2, 
                                       ifelse(away_on_3 %in% goalie_vec, away_on_3, 
                                              ifelse(away_on_4 %in% goalie_vec, away_on_4, 
                                                     ifelse(away_on_5 %in% goalie_vec, away_on_5, 
                                                            ifelse(away_on_6 %in% goalie_vec, away_on_6, NA))))))
           ) %>% 
    rename(event_index = row) %>%  ## row determined above in matrix construction, renamed for joining
    data.frame()
  
  # Return data as list
  return_list <- list(pbp_final =     pbp_df,  
                      is_on_df_home = is_on_df_home, 
                      is_on_df_away = is_on_df_away)
  
  }

# Finalize PBP Data
sc.pbp_finalize <- function(pbp_data, on_data_home, on_data_away, roster_data, game_info_data) { 
  
  # Create vectors to be used below
  skater_vec_home <- as.character(filter(roster_data, position_type != "G", Team == game_info_data$home_team)$player)
  skater_vec_away <- as.character(filter(roster_data, position_type != "G", Team == game_info_data$away_team)$player)
  
  goalie_vec_home <- as.character(filter(roster_data, position_type == "G", Team == game_info_data$home_team)$player_team_num)
  goalie_vec_away <- as.character(filter(roster_data, position_type == "G", Team == game_info_data$away_team)$player_team_num)
  goalie_vec <-      c(goalie_vec_home, goalie_vec_away)
  
  
  # Combine and modify to form final pbp data frame
  pbp_combined <- pbp_data %>% 
    left_join(., on_data_home, by = "event_index") %>% 
    left_join(., on_data_away, by = "event_index") %>% 
    group_by(game_id) %>%
    arrange(event_index) %>%
    mutate(home_skaters = 6 - 
             1 * (is.na(home_on_1)) - 1 * (is.na(home_on_2)) - 1 * (is.na(home_on_3)) - 
             1 * (is.na(home_on_4)) - 1 * (is.na(home_on_5)) - 1 * (is.na(home_on_6)) - 
             1 * (!is.na(home_goalie)), 
           away_skaters = 6 - 
             1 * (is.na(away_on_1)) - 1 * (is.na(away_on_2)) - 1 * (is.na(away_on_3)) - 
             1 * (is.na(away_on_4)) - 1 * (is.na(away_on_5)) - 1 * (is.na(away_on_6)) - 
             1 * (!is.na(away_goalie)),
           home_score =   cumsum(event_type == "GOAL" & event_team == home_team) - 1 * (event_type == "GOAL" & event_team == home_team),
           away_score =   cumsum(event_type == "GOAL" & event_team == away_team) - 1 * (event_type == "GOAL" & event_team == away_team),
           # Determine event length (remove time for OFF events - a result of subsequent OFF events)
           event_length = lead(game_seconds, 1) - game_seconds, 
           event_length = 
             case_when(
               is.na(event_length) ~ 0, 
               event_type == "OFF" ~ 0,  ## all OFF events should have no event_length
               TRUE ~ event_length
               ), 
           # Set goalies for shootouts (last regulation goalie for each team)
           home_goalie =  ifelse(game_period == 5 & event_type %in% st.fenwick_events & event_team == away_team & is.na(home_goalie), last(na.omit(home_goalie)), home_goalie), 
           away_goalie =  ifelse(game_period == 5 & event_type %in% st.fenwick_events & event_team == home_team & is.na(away_goalie), last(na.omit(away_goalie)), away_goalie)
           ) %>% 
    # Indicate a goalie changed (ON / OFF event types)
    group_by(event_index) %>% 
    mutate(home_G_change =   ifelse(sum(1 * (goalie_vec %in% strsplit(players_substituted, ", ")[[1]])) == 1, 
                                    ifelse(goalie_vec[goalie_vec %in% strsplit(players_substituted, ", ")[[1]]] %in% goalie_vec_home,
                                           goalie_vec[goalie_vec %in% strsplit(players_substituted, ", ")[[1]]], NA), 
                                    NA), 
           away_G_change =   ifelse(sum(1 * (goalie_vec %in% strsplit(players_substituted, ", ")[[1]])) == 1, 
                                    ifelse(goalie_vec[goalie_vec %in% strsplit(players_substituted, ", ")[[1]]] %in% goalie_vec_away,
                                           goalie_vec[goalie_vec %in% strsplit(players_substituted, ", ")[[1]]], NA), 
                                    NA), 
           home_G_change =   roster_data$player[match(home_G_change, roster_data$player_team_num)], 
           away_G_change =   roster_data$player[match(away_G_change, roster_data$player_team_num)]
           ) %>%
    group_by(game_id) %>%
    mutate(game_strength_state = paste(ifelse(is.na(home_goalie), "E", home_skaters), 
                                       ifelse(is.na(away_goalie), "E", away_skaters), 
                                       sep = "v"
                                       ),
    game_score_state =    paste(home_score, away_score, sep = "v"), 
    # Alternate strength state calculation (disregards goalies)
    home_skaters_alt = 
      1 * (home_on_1 %in% skater_vec_home) + 1 * (home_on_2 %in% skater_vec_home) + 1 * (home_on_3 %in% skater_vec_home) +
      1 * (home_on_4 %in% skater_vec_home) + 1 * (home_on_5 %in% skater_vec_home) + 1 * (home_on_6 %in% skater_vec_home), 
    away_skaters_alt = 
      1 * (away_on_1 %in% skater_vec_away) + 1 * (away_on_2 %in% skater_vec_away) + 1 * (away_on_3 %in% skater_vec_away) +
      1 * (away_on_4 %in% skater_vec_away) + 1 * (away_on_5 %in% skater_vec_away) + 1 * (away_on_6 %in% skater_vec_away), 
    game_strength_state_alt = gsub("6", "E", paste0(home_skaters_alt, "v", away_skaters_alt)) , 
    # Match full player names for event_player columns
    event_player_1 = roster_data$player[match(event_player_1, roster_data$player_team_num)], 
    event_player_2 = roster_data$player[match(event_player_2, roster_data$player_team_num)], 
    event_player_3 = roster_data$player[match(event_player_3, roster_data$player_team_num)]
    ) %>% 
    select(
      # Main selections
      season, game_id, game_date, session, event_index, game_period, game_seconds, event_type, 
      event_description, event_detail, event_zone, event_team, event_player_1:event_player_3, event_length, coords_x, 
      coords_y, players_substituted, home_on_1:home_on_6, away_on_1:away_on_6, home_goalie, away_goalie, 
      home_team, away_team, home_skaters, away_skaters, home_score, away_score, game_score_state, game_strength_state,
      
      # Additional selections (to be split out)
      changes, home_skaters_alt, away_skaters_alt, game_strength_state_alt, 
      home_G_change, away_G_change, 
      event_description_alt
      ) %>% 
    arrange(game_id, event_index) %>% 
    data.frame()
  
  
  # Split into base and extra data
  pbp_base <- pbp_combined %>% 
    select(season:game_strength_state)
  
  pbp_extras <- pbp_combined %>% 
    select(game_id, event_index, changes:event_description_alt)
  
  
  # Return data as a list 
  return_list <- list(pbp_base =   pbp_base, 
                      pbp_extras = pbp_extras)
  
  
  }


## --------------------- ##
##   Compile Functions   ##
## --------------------- ##

# Run All Functions to Scrape Game Data
sc.scrape_game <- function(game_id, season_id) { 
  
  ## --------------- ##
  ##   Scrape Data   ##
  ## --------------- ##
  
  # Scrape events - HTM
  events_HTM <- sc.scrape_events_HTM(game_id_fun = game_id, season_id_fun = season_id, attempts = 3)
  
  # Scrape events - API
  events_API <- sc.scrape_events_API(game_id_fun = game_id, attempts = 3)
  
  # Scrape shifts
  shifts_HTM <- sc.scrape_shifts(game_id_fun = game_id, season_id_fun = season_id, attempts = 3)
  
  # Scrape rosters
  rosters_HTM <- sc.scrape_rosters(game_id_fun = game_id, season_id_fun = season_id, attempts = 3)
  
  # Scrape Event Summary
  event_summary_HTM <- sc.scrape_event_summary(game_id_fun = game_id, season_id_fun = season_id, attempts = 3)
  
  
  ## ---------------------------- ##
  ##   Create Basic Data Frames   ##
  ## ---------------------------- ##
  
  # Create game information data frame
  game_info_df <- sc.game_info(game_id_fun = game_id, season_id_fun = season_id, events_data = events_HTM, roster_data = rosters_HTM)
  
  # Create rosters data frame
  rosters_list <- sc.roster_info(game_id_fun = game_id, season_id_fun = season_id, roster_data = rosters_HTM, game_info_data = game_info_df, shifts_list = shifts_HTM)
  
  # Create event summary data frame
  event_summary_df <- sc.event_summary(game_id_fun = game_id, season_id_fun = season_id, event_summary_data = event_summary_HTM, roster_data = rosters_list$roster_df, game_info_data = game_info_df)
  
  
  ## ------------------------------- ##
  ##   Prepare Events & Shift Data   ##
  ## ------------------------------- ##
  
  # Prepare Shifts Data
  prepare_shifts_list <- sc.prepare_shifts(game_id_fun = game_id, season_id_fun = season_id, shifts_list = shifts_HTM, roster_data = rosters_list$roster_df, game_info_data = game_info_df)
  
  # Prepare Events Data (HTM)
  prepare_events_HTM_df <- sc.prepare_events_HTM(game_id_fun = game_id, season_id_fun = season_id, events_data = events_HTM, game_info_data = game_info_df)
  
  # Prepare Events Data (API)
  if (!is.null(events_API$liveData$plays$allPlays$coordinates)) { 
    
    if (ncol(events_API$liveData$plays$allPlays$coordinates) > 0) { 
      prepare_events_API_df <- sc.prepare_events_API(game_id_fun = game_id, events_data_API = events_API, game_info_data = game_info_df)
      coord_type <- "NHL_API"
      
      } else {
        prepare_events_API_df <- NULL
      
        }
    
    } else {
      prepare_events_API_df <- NULL
    
      }
  
  
  ## -------------------- ##
  ##   Join Coordinates   ##
  ## -------------------- ##
  
  if (!is.null(prepare_events_API_df)) {  ## NHL API SOURCE
    events_full_df <- sc.join_coordinates_API(events_data_API = prepare_events_API_df, events_data_HTM = prepare_events_HTM_df)
    
    } else {                              ## ESPN XML SOURCE
      prepare_events_ESPN_df <- sc.scrape_events_ESPN(game_id_fun = game_id, season_id_fun = season_id, game_info_data = game_info_df, attempts = 3)
      
      if (class(prepare_events_ESPN_df) == "data.frame") { 
      
        if (nrow(prepare_events_ESPN_df) > 0) { 
          events_full_df <- sc.join_coordinates_ESPN(season_id_fun = season_id, events_data_ESPN = prepare_events_ESPN_df, events_data_HTM = prepare_events_HTM_df, 
                                                     roster_data = rosters_list$roster_df, game_info_data = game_info_df)
          coord_type <- "ESPN"
          
          } else {  ## COORDINATES NOT AVAILABLE
            events_full_df <- prepare_events_HTM_df %>% 
              mutate(coords_x =              NA, 
                     coords_y =              NA, 
                     event_description_alt = NA)
            
            coord_type <- "NO_COORDS"
        
            }
      
        } else {    ## COORDINATES NOT AVAILABLE
          events_full_df <- prepare_events_HTM_df %>% 
            mutate(coords_x =              NA, 
                   coords_y =              NA, 
                   event_description_alt = NA)
      
          coord_type <- "NO_COORDS"
          
          }
    
      }
  
  
  ## -------------------- ##
  ##   Combine All Data   ##
  ## -------------------- ##
  
  # Combine / Process Shifts and Events Data 
  pbp_combine_list <- sc.pbp_combine(events_data = events_full_df, shifts_data = prepare_shifts_list$on_off_events, roster_data = rosters_list$roster_df, game_info_data = game_info_df)
  
  # Finalize PBP Data
  pbp_finalize_list <- sc.pbp_finalize(pbp_data =       pbp_combine_list$pbp_final, 
                                       on_data_home =   pbp_combine_list$is_on_df_home, 
                                       on_data_away =   pbp_combine_list$is_on_df_away, 
                                       roster_data =    rosters_list$roster_df, 
                                       game_info_data = game_info_df)
  
  # Return data as a list
  return_list <- list(pbp_base =         pbp_finalize_list$pbp_base, 
                      pbp_extras =       pbp_finalize_list$pbp_extras, 
                      player_shifts =    prepare_shifts_list$player_shifts_final, 
                      player_periods =   prepare_shifts_list$player_period_sums, 
                      roster_df =        rosters_list$roster_df_final, 
                      scratches_df =     rosters_list$scratches_df, 
                      game_info_df =     game_info_df %>% mutate(coord_source = coord_type), 
                      event_summary_df = event_summary_df)
  
  }

# Run sc.scrape_game function in loop for multiple games
sc.scrape_pbp <- function(games, sleep = 0) { 
  
  # Label games to be scraped
  if (length(games) > 1) cat(paste0("Processing ", 
                                    length(games), 
                                    " Games: ",
                                    min(sort(as.numeric(games))), 
                                    " - ", 
                                    max(sort(as.numeric(games))), 
                                    "\n", "-------------", 
                                    "\n")
                             )
  else cat(paste0("Processing Game:", 
                  "\n")
           )
  
  # Create report data frame
  scrape_report_df <- data.frame(matrix(ncol = 10))
  
  # Loop to scrape games 
  for(i in 1:length(games)) {
    
    Sys.sleep(sleep)
    
    cat(paste0(games[i], "...", "\n"))
    
    start_time <- Sys.time()
    
    tryCatch({
      
      pbp_list <- sc.scrape_game(game_id =   games[i], 
                                 season_id = paste0(as.numeric(substr(games[i], 1, 4)), 
                                                    as.numeric(substr(games[i], 1, 4)) + 1
                                                    )
                                 )
      
      hold_pbp_base <-         pbp_list$pbp_base
      hold_pbp_extras <-       pbp_list$pbp_extras
      hold_player_shifts <-    pbp_list$player_shifts
      hold_player_periods <-   pbp_list$player_periods
      hold_roster_df <-        pbp_list$roster_df 
      hold_scratches_df <-     pbp_list$scratches_df 
      hold_game_info_df <-     pbp_list$game_info_df
      hold_event_summary_df <- pbp_list$event_summary_df
      
      if (i == 1 | !exists("new_pbp_base")) { 
        new_pbp_base <-         pbp_list$pbp_base
        new_pbp_extras <-       pbp_list$pbp_extras
        new_player_shifts <-    pbp_list$player_shifts
        new_player_periods <-   pbp_list$player_periods
        new_roster_df <-        pbp_list$roster_df 
        new_scratches_df <-     pbp_list$scratches_df 
        new_game_info_df <-     pbp_list$game_info_df
        new_event_summary_df <- pbp_list$event_summary_df
        
        }
      else if (i > 1) { 
        new_pbp_base <-         bind_rows(hold_pbp_base, new_pbp_base)
        new_pbp_extras <-       bind_rows(hold_pbp_extras, new_pbp_extras)
        new_player_shifts <-    bind_rows(hold_player_shifts, new_player_shifts)
        new_player_periods <-   bind_rows(hold_player_periods, new_player_periods)
        new_roster_df <-        bind_rows(hold_roster_df, new_roster_df)
        new_scratches_df <-     bind_rows(hold_scratches_df, new_scratches_df)
        new_game_info_df <-     bind_rows(hold_game_info_df, new_game_info_df)
        new_event_summary_df <- bind_rows(hold_event_summary_df, new_event_summary_df)
        
        }
      
      }, 
      
      error = function(e) cat("ERROR :", conditionMessage(e), "\n")
      
      )
    
    # Scrape report data frame
    if (exists("hold_pbp_base")) { 
      
      if (games[i] == unique(hold_pbp_base$game_id)) { 
        scrape_report_df[i, 1] <-  games[i]
        scrape_report_df[i, 2] <-  nrow(pbp_list$pbp_base)
        scrape_report_df[i, 3] <-  nrow(pbp_list$pbp_extras)
        scrape_report_df[i, 4] <-  nrow(pbp_list$player_shifts)
        scrape_report_df[i, 5] <-  nrow(pbp_list$player_periods)
        scrape_report_df[i, 6] <-  nrow(pbp_list$roster_df)
        scrape_report_df[i, 7] <-  nrow(pbp_list$scratches_df)
        scrape_report_df[i, 8] <-  nrow(pbp_list$game_info_df)
        scrape_report_df[i, 9] <-  nrow(pbp_list$event_summary_df)
        scrape_report_df[i, 10] <- as.numeric(round(Sys.time() - start_time, 2))
        
        } else {
          scrape_report_df[i, 1] <-       games[i]
          scrape_report_df[i, c(2, 9)] <- NA
          scrape_report_df[i, 10] <-      as.numeric(round(Sys.time() - start_time, 2))
        
          }
      
      } else {
        scrape_report_df[i, 1] <-       games[i]
        scrape_report_df[i, c(2, 8)] <- NA
        scrape_report_df[i, 9] <-       as.numeric(round(Sys.time() - start_time, 2))
      
        }
    
    }
  
  # Add Names
  names(scrape_report_df) <- c("game_id", names(pbp_list), "time_elapsed")
  
  # Print results
  cat("-------------", 
      "\n", 
      paste0(length(unique(new_pbp_base$game_id)), 
             " of ", 
             length(games), 
             " games returned // Avg Time Per Game: ", 
             round(mean(scrape_report_df$time_elapsed), 2), 
             "\n")
      )
  
  # Return List
  return_list <- list(pbp_base =         new_pbp_base %>% arrange(game_id),  
                      pbp_extras =       new_pbp_extras %>% arrange(game_id), 
                      player_shifts =    new_player_shifts %>% arrange(game_id), 
                      player_periods =   new_player_periods %>% arrange(game_id), 
                      roster_df =        new_roster_df %>% arrange(game_id), 
                      scratches_df =     new_scratches_df %>% arrange(game_id), 
                      game_info_df =     new_game_info_df %>% arrange(game_id), 
                      event_summary_df = new_event_summary_df %>% arrange(game_id), 
                      report =           scrape_report_df %>% arrange(game_id))
  
  }


###########################



