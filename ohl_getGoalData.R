library(tidyverse)
library(stringr)
library(RJSONIO)
library(jsonlite)
library(RODBC)
library(DBI)
library(odbc)

# connect to database
conn1 <- odbcConnect("SQLServer_DSN")

test <- read_csv("gameIDs.csv") %>%
  filter(team == "PBO") %>%
  filter(home_score != "0" | visitor_score != "0")

# create an empty place to add each game data
#output_goals <- NULL

# iterative process to read each game id from all games data frame and then run code
for (i in 1:nrow(test)) {
  game_ID <- (test[i, 3])
  joined <- NULL
  
  # this game breaks the scraper because the minus list produces a dataframe 
  # for the penalty shot goal of nothing
  # game_ID <- 24109
  
  # Automate the link based on event_ID
  str1 <- "https://cluster.leaguestat.com/feed/index.php?feed=gc&key=2976319eb44abe94&client_code=ohl&game_id="
  str2 <- "&lang_code=en&fmt=json&tab=gamesummary"
  game_url <- paste0(str1,game_ID,str2)
  gameLogsURL <- game_url #read the webpage
  
  # Import game logs data from JSON
  # use jsonlite::fromJSON to handle NULL values
  goalLogsData <- jsonlite::fromJSON(gameLogsURL, simplifyDataFrame = TRUE)
  
  # Pull out the data frame for goal data
  goals <- goalLogsData[["GC"]][["Gamesummary"]][["goals"]]
  goalscorer <- goalLogsData[["GC"]][["Gamesummary"]][["goals"]][["goal_scorer"]]
  assist1 <- goalLogsData[["GC"]][["Gamesummary"]][["goals"]][["assist1_player"]]
  assist2 <- goalLogsData[["GC"]][["Gamesummary"]][["goals"]][["assist2_player"]]
  plus <- goalLogsData[["GC"]][["Gamesummary"]][["goals"]][["plus"]]
  minus <- goalLogsData[["GC"]][["Gamesummary"]][["goals"]][["minus"]]
  game_date <- goalLogsData[["GC"]][["Gamesummary"]][["game_date"]]
  
  # Clear the game date
  game_date <- as_tibble(game_date) %>% 
    mutate(game_date = stringr::str_split(game_date,
                                          "\\,", simplify = TRUE, n = 2)[,2]) %>% 
    mutate(game_date = mdy(game_date)) %>% 
    select(game_date)
  
  # Clean goal scorer data frame
  goalscorerDF <- goalscorer %>%
    mutate(goal_id = as.integer(row_number())) %>% 
    select(goal_id, team_code, goal_scorer_id = "player_id") %>%
    mutate(goal_scorer_id = as.integer(goal_scorer_id)) %>% 
    as_tibble()
  
  # Clean assist1 data frame
  assist1DF <- assist1 %>%
    mutate(goal_id = as.integer(row_number())) %>%
    #mutate(assist1 = paste(first_name, last_name)) %>%
    #mutate(assist1 = stringr::str_split(assist1, "NA NA ", simplify = TRUE, n = 2)[,1]) %>%
    select(goal_id, a1_player_id = "player_id") %>%
    mutate(a1_player_id = as.integer(a1_player_id)) %>%  
    as_tibble()
  
  # Clean assist2 data frame
  assist2DF <- assist2 %>%
    mutate(goal_id = as.integer(row_number())) %>%
    #mutate(assist2 = paste(first_name, last_name)) %>%
    #mutate(assist2 = stringr::str_split(assist2, "NA NA", simplify = TRUE, n = 2)[,1]) %>%
    select(goal_id, a2_player_id = "player_id") %>%
    mutate(a2_player_id = as.integer(a2_player_id)) %>% 
    as_tibble()
  
  # Clean goals data frame
  goalsDF <- goals %>%
    mutate(goal_id = as.integer(row_number())) %>%
    mutate(game_id = game_ID$game_id) %>%
    mutate_all(na_if,"") %>%
    mutate_at(vars(goal_type), ~replace_na(., "EV")) %>%
    select(goal_id, game_id, event, period_id, time, goal_type, game_winning,
           penalty_shot, x_location, y_location) %>%
    mutate(period_id = as.numeric(period_id),
           game_winning = as.numeric(game_winning),
           penalty_shot = as.numeric(penalty_shot),
           x_location = as.numeric(x_location),
           y_location = as.numeric(y_location)) %>% 
    as_tibble()
  

# deal with the minus players  
  new_minus <- bind_rows(minus, .id = "goal_event")
  
# Create a new variable that represents the number of the player on the ice
# Then pivot
  minusDF <- new_minus %>%
    group_by(goal_event) %>%
    mutate(player_on_ice = row_number(),
           player_on_ice = str_c("on_ice_minus_", player_on_ice)) %>%
    ungroup() %>%
    select(-c(jersey_number:last_name)) %>%
    pivot_wider(values_from = player_id, names_from = player_on_ice) %>%
    select(goal_id = "goal_event", on_ice_minus_1:on_ice_minus_5) %>%
    mutate(goal_id = as.numeric(goal_id)) %>% 
    mutate_if(is.character,as.numeric)
    
# deal with the plus players
  new_plus <- bind_rows(plus, .id = "goal_event")
  
# Create a new variable that represents the number of the player on the ice
# Then pivot
  plusDF <- new_plus %>%
    group_by(goal_event) %>%
    mutate(player_on_ice = row_number(),
           player_on_ice = str_c("on_ice_plus_", player_on_ice)) %>%
    ungroup() %>%
    select(-c(jersey_number:last_name)) %>%
    pivot_wider(values_from = player_id, names_from = player_on_ice) %>%
    select(goal_id = "goal_event", on_ice_plus_1:on_ice_plus_5) %>% 
    mutate(goal_id = as.numeric(goal_id)) %>% 
    mutate_if(is.character,as.numeric)
  
# join all the dataframes together
  joined <- list(goalsDF,
                 goalscorerDF,
                 assist1DF,
                 assist2DF,
                 plusDF,
                 minusDF) %>% 
    reduce(left_join, by = "goal_id") %>% 
    mutate(game_date = game_date$game_date) %>% 
    select(game_date, everything())
  
  # save to database  
  sqlSave(conn1,
          joined,
          tablename = "ohl_goals",
          rownames = F,
          append = T)

  # save as data frame
  #output_goals <- rbind(output_goals, (joined))

}

