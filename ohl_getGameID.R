library(tidyverse)
library(stringr)
library(RJSONIO)
library(jsonlite)

# this code takes the team ids and gets the game data for each game
# they play.  this data is stored in gameID_data json file and then
# extract what we want

# OHL team_IDs
id <- c(1,2,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,34)
ohl_teams <- data.frame(id)

# create an empty place to add each game data
output <- NULL

# iterative process to read each team id from ohl_teams data frame and then run code
for (i in 1:nrow(ohl_teams)) {
  team_ID <- (ohl_teams[i, 1])

# game information based on team ID.  Enter the team ID as team_ID
# use this team_ID if you only want to run script for one team
#team_ID <- 6

# Automate the link based on team_ID
str1 <- "https://lscluster.hockeytech.com/feed/?feed=modulekit&view=schedule&key=2976319eb44abe94&fmt=json&client_code=ohl&lang=en&season_id=68&team_id="
str2 <- "&league_code=&fmt=json"
gameID_url <- paste0(str1,team_ID,str2)

# Import gameID data from JSON
# use jsonlite::fromJSON to handle NULL values
gameID_data <- jsonlite::fromJSON(gameID_url, simplifyDataFrame = TRUE)

# Pull out the data frame for gameID
game_data <- gameID_data[["SiteKit"]][["Schedule"]]
# Pull out the data frame for team_id
team_id <- gameID_data[["SiteKit"]][["Parameters"]][["team_id"]]
# create a data frame with team_id
team <- data.frame(team_id)

# clean data
games <- game_data %>%
  mutate(team_id = as.numeric(team_id)) %>%
  mutate(team = case_when(
    team_id == 1 ~ "HAM",
    team_id == 2 ~ "KGN",
    team_id == 4 ~ "OSH",
    team_id == 5 ~ "OTT",
    team_id == 6 ~ "PBO",
    team_id == 7 ~ "BAR",
    team_id == 8 ~ "ER",
    team_id == 9 ~ "GUE",
    team_id == 10 ~ "KIT",
    team_id == 11 ~ "OS",
    team_id == 12 ~ "SBY",
    team_id == 13 ~ "FLNT",
    team_id == 14 ~ "LDN",
    team_id == 15 ~ "SAR",
    team_id == 16 ~ "SOO",
    team_id == 17 ~ "WSR",
    team_id == 18 ~ "MISS",
    team_id == 19 ~ "NB",
    team_id == 20 ~ "NIAG",
    team_id == 34 ~ "SAG",
    TRUE ~ "NULL")) %>% 
  select(team_id, team, game_id, date_played,
         home = "home_team_code", home_score = "home_goal_count",
         visitor = "visiting_team_code", visitor_score = "visiting_goal_count") %>% 
  mutate(game_id = as.numeric(game_id),
         date_played = as.Date(date_played),
         home_score = as.numeric(home_score),
         visitor_score = as.numeric(visitor_score))

output <- rbind(output, (games))

}


# remove columns not needed to combine with goal data frame
output_combine <- output %>% 
  select(-c(team_id, team))

# create data frame of 
# just game_id so it can be used to iterate for code_goal_data file
output_gameID <- output %>%
  filter(team == "FLNT",
         #home == "GUE" | visitor == "GUE",
         home_score >0 | visitor_score >0) %>% 
  select(game_id)

# save csv file of all games
write.csv(output,
          file = "~/Documents/hockey/OHL_OIR/all_games.csv",
          row.names = F)

  
