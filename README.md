# OHL_scrape
scrape scripts for OHL website

1. ohl_getBios - goes to the player stats page and retreives player bio information.
2. ohl_getGameID - goes to the schedule page and returns gameIDs for each team.  This is required for scrapes in 3,4 and 5
3. ohl_getGoalData scrape
4. ohl_getShots scrape
5. ohl_getFaceoffs

# ****************************
# Daily Report Scrape
scrape script

Functions in this script:
1. ListTables() will show all teams and the index of their table. It takes no arguments.
2. fnOhlTable is the core function for downloading tables. It takes two arguments: the table number (as tablenum defaulting to 87 for the Texas Stars skaters), and whether it should clean it as a skater table (as cleanSkaters defaulting to TRUE).
3. fnAllTeams(cleanSkaters = TRUE) returns an entire dataframe of all stats.

Examples:
# run this line to get all skater stats
ohlAllSkaters <- fnAllTeams(cleanSkaters = TRUE)

# run this line to get only a team level stats
ListTables() # this will get you a list of tables by team
pboSkaters <- fnOhlTable(tablenum = 63, cleanSkaters = TRUE)
View(pboSkaters) # to view as a spreadsheet

# individual skater (need correct spelling of name)
player <- pboSkaters %>% filter(PLAYER == "Nick Robertson")
View(player) # to view as a spreadsheet
# *****************************
