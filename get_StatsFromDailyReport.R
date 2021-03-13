# OHL Daily Report Scraper

library(tidyverse)
library(rvest)
library(stringr)
library(janitor)
library(RODBC)
library(DBI)
library(odbc)

# The daily report is the same URL all season
url_dailyreport <- "http://chlcluster.leaguestat.com/download.php?client_code=ohl&file_path=daily-report/daily-report.html"

# **** Get team stats ****
# get standings (can use this to create relative players stats)
table_return <- url_dailyreport %>%
  read_html() %>%
  html_nodes(xpath = '/html/body/table[5]') %>%
  html_table()

# clean the scraped stats table
table_return <- table_return[[1]]
colnames(table_return)[1] = "team"
# pull out the teams with playoff designations
playoffTeams <- table_return %>% 
  mutate(team = str_split(team, "\\ ", simplify = TRUE, n = 2)[,2]) %>% 
  filter(str_detect(team, 'x|xy|xyz')) %>% 
  mutate(team = str_split(team, "\\ ", simplify = TRUE, n = 2)[,2])
# pull out teams without playoff designations
nonPlayoffTeams <- table_return %>% 
  mutate(team = str_split(team, "\\ ", simplify = TRUE, n = 2)[,2]) %>% 
  filter(!str_detect(team, 'x|xy|xyz'))
# combine tables
ohlTeamStats <- bind_rows(playoffTeams, nonPlayoffTeams)

#amount of goals scored by team
ohlTeamsGoalsFor <- ohlTeamStats %>% 
  select(team, team_GF = "GF")

# Function for retrieving specific table by number from xpath
# Input param tablenum: the number of the table from the xpath
# Default value 87 will retrieve Texas Stars skater stats
fnOhlTable <- function(tablenum = 87, cleanSkaters = TRUE) {
  xpathnum = paste('/html/body/table[',as.character(tablenum),']', sep = "")
  table_return <- url_dailyreport %>%
    read_html() %>%
    html_nodes(xpath = xpathnum) %>%
    html_table()
  df_Table <- table_return[[1]]
  
  if (cleanSkaters == TRUE) {
    # Consolidate multiple teams onto only total line
    df_Table <- df_Table %>%
      filter(No. != "NA") %>%
      # Remove goalies and total
      filter(POS != "G")
    # Add designation for Rookie
    df_Table$Rookie <- str_detect(df_Table$PLAYER, "\\*")
    # Add designation for Active
    df_Table$Active <- !str_detect(df_Table$PLAYER, "^(x )|^(\\*x )")
    # Clean names
    df_Table$PLAYER <- str_replace_all(df_Table$PLAYER, "^(x )|^(\\*x )|^\\* |\\(total\\)$", "")
  } else {}
  df_Table
  
}



# ListTables: none

ListTables <- function() {
  all_h2 <- url_dailyreport %>% 
    read_html() %>% 
    html_nodes("h2") %>% 
    html_text()
  teams <- str_detect(all_h2, "^[A-Z].*\\W[:alpha:].*\\WStatistics")
  all_teams <- trimws(unlist(str_extract_all(all_h2[teams], "^[A-Z].*\\W[:alpha:].*\\W")))
  skater_tables <- seq(35,73,2)
  goalie_tables <- seq(36,74,2)
  dfTeams <- data.frame("Teams" = all_teams, "Skater Table" = skater_tables, "Goalie Table" = goalie_tables)
  dfTeams
}



# fnAllTeams: cleanSkaters

fnAllTeams <- function(cleanSkaters = TRUE){
  
  alltables <- ListTables()
  
  # Seed first set in data frame for all teams
  dfAllTeams <- fnOhlTable(alltables[1,2], cleanSkaters = cleanSkaters)
  dfAllTeams$TEAM <- rep(as.character(alltables[1,]$Teams),nrow(dfAllTeams))
  
  # Populate with remaining teams
  for (team in 2:nrow(alltables)) {
    tempRow <- alltables[team,]
    tempTeam <- fnOhlTable(tempRow[2], cleanSkaters = cleanSkaters)
    tempTeam$TEAM <- rep(as.character(tempRow$Teams),nrow(tempTeam))
    dfAllTeams <- rbind(dfAllTeams, tempTeam)
  }
  
  # Return the full data frame
  dfAllTeams
  
}


# run this line to get all skater stats
ohlAllSkaters <- fnAllTeams(cleanSkaters = TRUE) %>% 
  rename(jersey = "No.") %>% 
  clean_names() %>% 
  rename(plus_minus = "x") %>% 
  mutate(ppg = round(pts/gp, 2),
         evg = g-pp-shg,
         eva = a-ppa-sha,
         evpts = evg+eva,
         eppg = round((evg+eva)/gp, 2)) %>% 
  select(team, jersey:pts, ppg, evg, eva, evpts, eppg,
         plus_minus:active) %>% 
  mutate(team = str_split(team, "\\ ", simplify = TRUE, n = 2)[,1]) %>% 
  mutate(team = case_when(
    team == "North" ~ "North Bay",
    team == "Owen" ~ "Owen Sound",
    team == "Soo" ~ "Sault Ste. Marie",
    TRUE ~ (as.character(.$team))
  )) %>% 
  left_join(ohlTeamsGoalsFor, by = "team") %>% 
  mutate(involve = round(pts/team_GF, 2)) %>% 
  select(team:pts, involve, ppg:active)

# connect to database
conn1 <- odbcConnect("SQLServer_DSN")

# save to database  
sqlSave(conn1,
        ohlAllSkaters,
        tablename = "ohl_GameReport",
        rownames = F,
        append = T)


