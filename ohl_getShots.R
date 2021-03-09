library(tidyverse)
library(janitor)
library(lubridate)
library(stringr)
library(RJSONIO)
library(jsonlite)
library(RODBC)
library(DBI)
library(odbc)

#set random system sleep variable
tmsleep <- sample(5:10,1)

# set progress bar
pb <- txtProgressBar(min = 0, max = 62, style = 3)

# connect to database
conn1 <- odbcConnect("SQLServer_DSN")

# read in the ohl game ids to iterate through
gameIDs <- read_csv("ohl_gameIDs.csv") %>%
  filter(home == "PBO" | visitor == "PBO")

# iterative process to read each game id from all games data frame and then run code
for (i in 1:nrow(gameIDs)) {
  game_ID <- (gameIDs[i, 1])
  game_id <- as.numeric(gameIDs[i,1])
  Sys.sleep(tmsleep)
  shots <- NULL


str1 <- "https://cluster.leaguestat.com/feed/index.php?feed=gc&key=2976319eb44abe94&client_code=ohl&game_id="
str2 <- "&lang_code=en&fmt=json&tab=pxpverbose"
game_url <- paste0(str1,game_ID,str2)
url <- game_url

# Import pxp data from JSON
# use jsonlite::fromJSON to handle NULL values
json_data <- jsonlite::fromJSON(url, simplifyDataFrame = TRUE)

# create a tiblle
events <- as_tibble(json_data[["GC"]][["Pxpverbose"]]) %>% 
  mutate(game_id = game_id) %>% 
  select(game_id, everything())

# get shot data
shots <- events %>% 
  filter(event == "shot") %>% 
  select(game_id, event, time, s, team_id, x_location, y_location,
         shot_player_id = "player_id", home, shot_type, shot_type_description,
         shot_quality_description)

# save to database  
sqlSave(conn1,
        shots,
        tablename = "ohl_shots",
        rownames = F,
        append = T)


setTxtProgressBar(pb, i)

}

