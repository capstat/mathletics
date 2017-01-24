#for data scraping
library(rvest)
#for string manipulation
library(stringr)
#for data wrangling
library(dplyr)
#empty data frame
team_stats_df = data.frame()
for(i in seq(1950, 2016, 1)){
  #for tracking purposes
  print(i)
  #read the html page
  team_stats_html = read_html(
    sprintf("http://www.basketball-reference.com/leagues/NBA_%s.html", i))
  team_stats = team_stats_html %>%
    #find all the tables
    html_nodes("table") %>%
    #get them tables
    html_table()
  #change the 1st column headers
  colnames(team_stats[[1]])[1] = "Team"
  temp = team_stats[[1]]
  #more than 1 conference
  if(length(team_stats) > 1){
    colnames(team_stats[[2]])[1] = "Team"
    #combine the conferences
    temp = rbind(temp, team_stats[[2]])
  }
  #add the year
  temp$Year = i
  #combine to other data
  team_stats_df = rbind(team_stats_df, temp)
}
#do some clean up - remove * and + from team name
team_stats_df$Team = str_trim(str_replace_all(
  team_stats_df$Team, "[\\*\\+\\(\\)\\d]", ""))
#remove unnecessary rows and columns
team_stats_df = team_stats_df[nchar(team_stats_df$W)<3,]
#remove "-" for 1st place teams
team_stats_df$GB = ifelse(
  !is.na(as.numeric(team_stats_df$GB)),
  team_stats_df$GB, 0)
#change columns to numeric
team_stats_df[, c(2:8)] = sapply(team_stats_df[, c(2:8)], as.numeric)
#points ratioPS/G","PA/G
team_stats_df$Ratio = team_stats_df$`PS/G`/team_stats_df$`PA/G`
write.csv(team_stats_df, "nba_standings.csv", row.names=FALSE)