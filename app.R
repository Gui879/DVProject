#install.packages('leaflet')
#install.packages('htmltools')
#install.packages('dplyr')
#install.packages('chron')
#install.packages('visNetwork')
#install.packages('shinyjs')
#install.packages('plyr')
#install.packages('ggplot2')
#install.packages('waffle')
#install.packages('lawn')
#install.packages('geosphere')
#install.packages('shiny')
#install.packages('leaflet')
#install.packages('shinyWidgets')
#install.packages('plotly')
#install.packages('DT')
#install.packages('shiny')
#install.packages('shinydashboard')
#install.packages('shinyWidgets')
#install.packages('leaflet')
#install.packages('htmltools')
#install.packages('shinyWidgets')
#install.packages('leaflet.extras')
#install.packages('lubridate')
#install.packages('geosphere')
#install.packages('waffle')
#install.packages('tm')
#install.packages('RJSONIO')

#used packages
library(plotly)
library(DT)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(leaflet)
library(htmltools)
library(shinyWidgets)
library(leaflet.extras)
library(lubridate)
library(geosphere)
library(waffle)
require(leaflet)
require(htmltools)
require(dplyr)
require(chron)
require(visNetwork)
require(shinyjs)
require(plyr)
require(ggplot2)
require(waffle)
require(lawn)
require(geosphere)
require(shiny)
require(leaflet)
require(shinyWidgets)


#PlayersInfo Data
data<-read.csv("data-2.csv",header=TRUE)

#Data RaidsLocation
dt<-read.csv("dataM.csv",header=TRUE,sep=',', encoding = "UTF-8")
raids <-read.csv("raids.csv",header=TRUE,sep=',', encoding = "UTF-8")
raids$date <- as.Date(raids$date)
raids <- raids[order(raids$date),]
dates <- raids[!duplicated(raids[,'date']),'date']
raids$hour_start <- format(strptime(raids$hour_start, "%H:%M"), "%H:%M")
raids$hour_end <- format(strptime(raids$hour_end, "%H:%M"), "%H:%M")
loc <- read.csv("loc.csv",header=TRUE, sep = ',', encoding = "UTF-8")
raids$username <- NULL
raids$nmr_raids <- 1:nrow(raids)
raids$total <- 1:nrow(raids)

perhour <- data.frame(matrix(ncol = 2, nrow = 24))
hours <- 0:23
perhour$X1 <- hours
perhour$X1 <- paste(format(as.POSIXct(sprintf("2016/08/12 %s:00:00",perhour$X1)),"%H:%M"),"-",format(as.POSIXct(sprintf("2016/08/12 %s:00:00",perhour$X1))+59*60,"%H:%M"))
for(i in 0:24){
  perhour[i,'X2'] <- nrow(raids[i == hour(c(as.POSIXct(paste("2016-03-01",raids$hour_start)))) | i ==hour(c(as.POSIXct(paste("2016-03-01",raids$hour_end)))),])
}
levels(perhour$X1) <- 0:24

perhour <- perhour[5:22,]
perhour[1,'X1'] <- "00:00 - 04:59"
perhour[18,'X1'] <- "21:00 - 23:59"

raids$weekday <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")[as.POSIXlt(raids$date)$wday + 1]
perday <- data.frame(table(raids$weekday))
names(perday) <- c('X1','X2')
perday$X1 <- factor(perday$X1, levels= c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
perday <- perday[order(perday$X1),]

permonth <- c("Jan", "Feb", "Mar", "Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")[as.POSIXlt(raids$date)$mon + 1]
permonth <- data.frame(table(permonth))
names(permonth) <- c('X1','X2')
permonth$X1 <- factor(permonth$X1, levels= c("Jan", "Feb", "Mar", "Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
permonth <- permonth[order(permonth$X1),]


#import dataset
df<-read.csv("data.csv",header=TRUE,sep=',')
players_raids <- data.frame(table(df$username))
#select useful columns
df<-df[c("date","location","hour_start","hour_end","lat",'log')]

#remove duplicates
df<-df[!duplicated(df),]


#get location's coordinates
locations <- readRDS(file='locations.rds')

#import players information
players_info <- read.csv('players_info.csv', header=TRUE, sep=',')
players_info$recorded_raids = players_raids$Freq[players_raids$Var1 %in% players_info$username]
#import the friendships
friendships <- read.csv('friendships.csv', header=TRUE, sep=',')

#columns with the reference to each player
friendships['from'] <- 0
friendships['to'] <- 0

#import all the diferent centralities
dcentrality <- read.csv('degree_centrality.csv', header=TRUE, sep=',')
ccentrality <- read.csv('close_centrality.csv', header=TRUE, sep=',')
bcentrality <- read.csv('between_centrality.csv', header=TRUE, sep=',')

#create colum player id
dcentrality['player_id']=0
ccentrality['player_id']=0
bcentrality['player_id']=0

friendships[friendships$row_name==as.vector(players_info$username[players_info$X==1]),'from']=1

#create an index to each player and insert it in the columns 'from' and 'to'
for(i in 1:nrow(players_info)){
  friendships[friendships$row_name==as.vector(players_info$username[players_info$X==i]),'from']=i
  friendships[friendships$col_name==as.vector(players_info$username[players_info$X==i]),'to']=i
  ccentrality[ccentrality$username==as.vector(players_info$username[players_info$X==i]),'player_id']=i
  dcentrality[dcentrality$username==as.vector(players_info$username[players_info$X==i]),'player_id']=i
  bcentrality[bcentrality$username==as.vector(players_info$username[players_info$X==i]),'player_id']=i
}




load("pokemon.RData")

df$date = as.Date(df$date)

df$hour_start = as.POSIXct(df$hour_start,format="%H:%M")
df$hour_end = as.POSIXct(df$hour_end,format="%H:%M")

df = df[order(as.Date(df$date)),]

#convert addresses to coordinates
geocodeAdddress <- function(address) {
  require(RJSONIO)
  url <- "https://eu1.locationiq.com/v1/search.php?key=789d3cbd8fc4f1&q="
  url <- URLencode(paste(url, address, '&format=json',sep = ""))
  print(url)
  coord <- NULL
  coord <- tryCatch({
    x <- fromJSON(url, simplify = FALSE)
    return(list('lat'=as.numeric(x[[1]]$lat),'log'=as.numeric(x[[1]]$lon)))
  }, warning = function(w) {
    return(NULL)
  }, error = function(e) {
    return(NULL)
  })
  
  Sys.sleep(0.2)  # API only allows 5 requests per second
  return(coord)
}


#get pictures' link
getpicture <- function(log,lat,address) {
  require(RJSONIO)
  url <- "https://maps.googleapis.com/maps/api/place/nearbysearch/json?key=AIzaSyACUFajZiUFfxAlvLVxVC4wtfQedMvYRmo&sensor=false&location="
  url <- URLencode(paste(url, lat, ',', log, '&radius=100&keyword=',address,sep = ""))
  print(url)
  x <- fromJSON(url, simplify = FALSE)
  if(x$status=='OK'){
    reference = x$results[[1]]$photos[[1]]$photo_reference
    url <- 'https://maps.googleapis.com/maps/api/place/photo?maxwidth=400&key=AIzaSyACUFajZiUFfxAlvLVxVC4wtfQedMvYRmo&photoreference='
    url <- URLencode(paste(url,reference,sep = ""))
    return(url)
  }else{
    return(NULL)
  }
  
  Sys.sleep(0.2)  # API only allows 5 requests per second
  
}


gymIcon <- makeIcon(
  iconUrl = 'https://cdn2.iconfinder.com/data/icons/pokemon-go-addict/100/011-_Pokestop_-_PokeBall_-_Game_-_Pokemon_-_Pokemongo-512.png',
  iconWidth = 32,
  iconHeight = 32,
  #control anchor
  iconAnchorX = 12,
  iconAnchorY = 8
)

runApp()