#there are def easier more straight forward ways to do this
#for scraping the web
library(rvest)
#for string manipulation
library(stringr)
#for data wrangling
library(dplyr)

#create an empty vector to capture team stat links
stat_links = c()
#create an empty data frame to hold all our data
playoffs = data.frame()
#web address for each playoff series - the year will replace the %s
ws_url = "http://www.baseball-reference.com/postseason/%s_WS.shtml"
urls = c(ws_url)
#for every year since 1st world series
for(i in seq(1903, 2016, 1)){
  #skip the years without world series
  if(i == 1904 | i == 1994){ next }
  #set urls based upon ployoff structure
  if(i == 1969 | i == 1982){ 
    urls = c(ws_url, str_replace(ws_url, "WS", "ALCS"), 
             str_replace(ws_url, "WS", "NLCS"))
  }
  #there were division series in 1981 because of the strike
  if(i == 1981){ 
    urls = c(urls, str_replace(ws_url, "WS", "AEDIV"), 
             str_replace(ws_url, "WS", "AWDIV"),
             str_replace(ws_url, "WS", "NEDIV"), 
             str_replace(ws_url, "WS", "NWDIV"))
  }
  if(i == 1995){ 
    urls = c(urls, str_replace(ws_url, "WS", "ALDS1"), 
             str_replace(ws_url, "WS", "ALDS2"),
             str_replace(ws_url, "WS", "NLDS1"), 
             str_replace(ws_url, "WS", "NLDS2"))
  }
  for(each in urls){
    #read the website
    temp_html = read_html(sprintf(each, i))
    #keep track of any bad web address
    if(str_detect(temp_html, "404 - File Not Found")){ 
      print(sprintf(each, i)) 
      next
    }
    #add the team stat links to use later
    links = temp_html %>% 
      html_nodes("h2 a") %>% 
      html_attr("href")
    stat_links = c(stat_links, links)
    #get the series results
    results = temp_html %>% 
      html_nodes("h2") %>% 
      html_text()
    #sample result:
    #"2007 NLCS (4-0): Colorado Rockies (90-73) over Arizona Diamondbacks (90-72)"
    #extract all the numbers for later use
    extract_numbers = as.numeric(unlist(str_extract_all(results[1], "([0-9]+)")))
    #remove the tie from the 1907 and 1922 worls series
    if(i==1907 | i==1922){ extract_numbers = extract_numbers[-4] }
    #create a temp mini data frame, one row for each team
    temp = data.frame(
      "year"=as.character(rep(extract_numbers[1], 2)),
      "series"=rep(str_trim(str_match(results[1], "\\d{4}\\s([a-zA-Z\\s]+)")[,2]), 2),
      "team"=c(str_trim(str_match(results[1], ":\\s([a-zA-Z\\s\\.]+)")[,2]),
               str_trim(str_match(results[1], "over\\s([a-zA-Z\\s\\.]+)")[,2])),
      "abbr"=c(substr(links, 8, 10)),
      "opponent"=c(str_trim(str_match(results[1], "over\\s([a-zA-Z\\s\\.]+)")[,2]),
                   str_trim(str_match(results[1], ":\\s([a-zA-Z\\s\\.]+)")[,2])),
      "series.wins"=c(extract_numbers[2], extract_numbers[3]),
      "Wpct"=c(extract_numbers[4]/(extract_numbers[4]+extract_numbers[5]),
               extract_numbers[6]/(extract_numbers[6]+extract_numbers[7])),
      "won_series"=c(TRUE,FALSE)
    )
    #combine the temp df with our initial df
    playoffs = rbind(playoffs, temp)
    #track progress
    print(paste(i, temp$series[1]))
  }
}
#empty data frame for team stats
team_stats = data.frame(stringsAsFactors=FALSE)
#base url for team stats
team_url = "http://www.baseball-reference.com"
#get stats from each unique link
for(each_link in unique(stat_links)){
  #to track progress
  print(each_link)
  temp_html = read_html(paste0(team_url, each_link))
  team_tables = temp_html %>%
    html_nodes("table") %>%
    html_table()
  team_info = data.frame("abbr"=substr(each_link, 8, 10), 
                         "year"=substr(each_link, 12, 15))
  batting = team_tables[[2]][team_tables[[2]]$Name=="Team Totals",-c(1:4)]
  pitching = team_tables[[3]][team_tables[[3]]$Name=="Team Totals",-c(1:4)]
  #change the column names before the join
  colnames(pitching) = paste0("p", colnames(pitching))
  team_stats = rbind(team_stats, cbind(team_info, batting, pitching))
}
#join the table
playoffs = left_join(playoffs, team_stats, by=c("abbr", "year"))
#change some columns to numbers to do some math
playoffs$R = as.numeric(playoffs$R)
playoffs$pR = as.numeric(playoffs$pR)
playoffs$`pW-L%` = as.numeric(playoffs$`pW-L%`)
#add some columns
playoffs$Predicted.Win.Pct = ((playoffs$R/playoffs$pR)^2)/
  (1+((playoffs$R/playoffs$pR)^2))
playoffs$Absolute.Error = abs(playoffs$`pW-L%`-playoffs$Predicted.Win.Pct)
#save so you dont have to scrape again
write.csv(playoffs, "mlb_playoffs.csv", row.names=FALSE)