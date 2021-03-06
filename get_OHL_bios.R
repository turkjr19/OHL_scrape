library(tidyverse)
library(janitor)
library(lubridate)
library(stringr)
library(RJSONIO)
library(jsonlite)

# create dataframe to create start and finish requirements
start_num <- c("0", "100", "200", "300", "400", "500")
df <- data.frame(start_num)

# create an empty place to add each game data
output <- NULL

# iterative process to read each page with a start_num in data frame above
for (i in 1:nrow(df)) {
  page <- (df[i, 1])
  Sys.sleep(2)

str1 <- "https://lscluster.hockeytech.com/feed/?feed=modulekit&view=statviewtype&type=topscorers&key=2976319eb44abe94&fmt=json&client_code=ohl&lang=en&league_code=&season_id=68&first="
str2 <- page
str3 <- "&limit=100&sort=active&stat=all&order_direction="

url <- paste0(str1,str2,str3)

# Import gameID data from JSON
# use jsonlite::fromJSON to handle NULL values
json_data <- jsonlite::fromJSON(url, simplifyDataFrame = TRUE) 

# Pull out the data required from json data
player_id<- json_data[["SiteKit"]][["Statviewtype"]][["player_id"]]
first_name <- json_data[["SiteKit"]][["Statviewtype"]][["first_name"]]
last_name <- json_data[["SiteKit"]][["Statviewtype"]][["last_name"]]
birthdate <- json_data[["SiteKit"]][["Statviewtype"]][["birthdate"]]
birthdate_year <- json_data[["SiteKit"]][["Statviewtype"]][["birthdate_year"]]
shoots <- json_data[["SiteKit"]][["Statviewtype"]][["shoots"]]
position <- json_data[["SiteKit"]][["Statviewtype"]][["position"]]
player_page_link <- json_data[["SiteKit"]][["Statviewtype"]][["player_page_link"]]

#clean up json
player_id <- as_tibble(player_id) %>% 
  rename(ohl_player_id = "value")

first_name<- as_tibble(first_name) %>% 
  rename(first_name = "value")

last_name<- as_tibble(last_name) %>% 
  rename(last_name = "value")

birthdate <- as_tibble(birthdate) %>% 
  rename(birthdate = "value")

birthdate_year <- as_tibble(birthdate_year) %>% 
  rename(birthdate_year = "value")

shoots <- as_tibble(shoots) %>% 
  rename(shoots = "value")

position <- as_tibble(position) %>% 
  rename(position = "value")

player_page_link <- as_tibble(player_page_link) %>% 
  rename(player_page_link = "value")


#combine together
ohl_player_bio <- bind_cols(player_id, first_name, last_name, birthdate,
                           birthdate_year, shoots, position, player_page_link) %>% 
  mutate(full_name = paste(first_name, last_name)) %>%
  select(ohl_player_id, full_name, first_name, last_name,
         birthdate:player_page_link) %>% 
  mutate(ohl_player_id = as.numeric(ohl_player_id)) %>%
  mutate(birthdate = mdy(birthdate)) 

output <- rbind(output, (ohl_player_bio))

}


