#for data scraping
library(rvest)
#for string manipulation
library(stringr)
#for data wrangling
library(dplyr)
#get NFL team stats for 2005, 2006, 2007
#empty data frame
team_stats_df = data.frame()
for(i in seq(1922, 2016, 1)){
  #for tracking purposes
  print(i)
  #read the html page
  team_stats_html = read_html(
    sprintf("http://www.pro-football-reference.com/years/%s/", i))
  team_stats = team_stats_html %>%
    #find all the tables
    html_nodes("table") %>%
    #get them tables
    html_table()
  #before AFC - NFC divisions were created
  if(length(team_stats)==1){ 
    temp = team_stats[[1]] 
  } else{
    #combine the AFC and NFC tables
    temp = rbind(team_stats[[1]], team_stats[[2]])
  }
  #remove any bad rows
  temp = temp[temp$Tm != "",]
  #add a tie column if its not there already
  if(!("T" %in% colnames(temp))){ temp$T = 0 }
  #add the year
  temp$Year = i
  #combine to other data
  team_stats_df = rbind(team_stats_df, temp)
}
#do some clean up - remove * and + from team name
team_stats_df$Tm = str_replace_all(team_stats_df$Tm, "\\*|\\+", "")
#remove unnecessary rows and columns
team_stats_df = team_stats_df[nchar(team_stats_df$W)<3, c(14,1:7)]
#change columns to numeric
team_stats_df[, c(3:8)] = sapply(team_stats_df[, c(3:8)], as.numeric)
#remove any team without a W L T 
#(not sure why they would be in there but there are some)
team_stats_df = team_stats_df[!is.na(team_stats_df$`W-L%`),]
#actual win %
team_stats_df$Win.Pct = ifelse(team_stats_df$W==0, 0, 
                               team_stats_df$W/
                                 (team_stats_df$W+team_stats_df$L))
#points ratio
team_stats_df$Ratio = team_stats_df$PF/team_stats_df$PA
#save so you dont have to scrape again
write.csv(team_stats_df, "nfl_standings.csv", row.names=FALSE)