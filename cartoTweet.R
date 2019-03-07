library(twitteR) 
library(dplyr)
library(splus2R)

##### Authentification ####
consumer_key <- "lcGTDWiZinC7SgmLCGBubjtMN"
consumer_secret <-"5zofQ1d3cvl8I329s3XTdkg7p7XQdNZKp4JoosNH38EuYQlbx2"
access_token <- "3109736956-yy6PAHBmGbk3WgnEGpSWXfFUMdTBHgx1G4FLGaO"
access_secret <- "WoEjlfdkPoVxRyW9MtjeoC3BtCZb7va34I6Q8md84T9xX" 

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

##### Get tweets for the first circle####

u<-twitteR::getUser('@Valtrier')

u$location

twFromUser1 = userTimeline('@Valtrier', n = 100)

d = twListToDF(twFromUser1)

user1 <- d %>%
  group_by(replyToSN) %>%
  filter(!is.na(replyToSN))%>%
  summarise(from=u$location, to=getUser(replyToSN)$location, lwd = n())%>%
  filter(!to=="")

twToUser1 = searchTwitter('@Valtrier', n = 100)

d = twListToDF(twToUser1)

users2 <- d %>%
  group_by(screenName) %>%
  summarise(from=getUser(screenName)$location, to=u$location,lwd = n())%>%
  filter(!from=="")

user1

users2<-users2%>%
  filter(!from=="\"\"")

users <- rbind(user1[,-1], users2[,-1]) %>% 
  filter(!lowerCase(to) == lowerCase(from))

users

library(opencage)
location = NULL
lng1 = NULL
lat1 = NULL


for(i in 1:nrow(users)){
  results<-opencage_forward(users$from[i], "71f0409b179a40ddb21f233dd80b5e26",limit = 1)
  
  location = c(location,users$from[i])
  lng1 = c(lng1,results$results$geometry.lng)
  lat1 = c(lat1,results$results$geometry.lat)
  
}

location2 = NULL
lng2 = NULL
lat2 = NULL

for(i in 1:nrow(users)){
  results<-opencage_forward(users$to[i], "71f0409b179a40ddb21f233dd80b5e26",limit = 1)
  
  results$results$geometry.lng
  
  location2 = c(location2,users$to[i])
  lng2 = c(lng2,results$results$geometry.lng)
  lat2 = c(lat2,results$results$geometry.lat)
  
}



final = cbind(lng1,lng2, lat1, lat2)%>%as.data.frame()

library(tidyverse)
library(maps)
library(geosphere)
library(leaflet)

plot_all_connection=function(map, data, opacity = 0.2, color="orange"){
  for(i in 1:nrow(data)){
    map <- addPolylines(map, lng = c(final$lng1[i],final$lng2[i]), lat = c(final$lat1[i], final$lat2[i]), opacity = opacity, color=color)
  }
  
  return(map)
}


map<-leaflet() %>% 
  addProviderTiles("OpenStreetMap") %>% 
  addCircleMarkers(lng = final$lng1, lat = final$lat1, radius = 2.5, color = "purple", opacity = 0.3) %>% 
  plot_all_connection(data=final)%>%
  addCircleMarkers(lng = final$lng2, lat = final$lat2, radius = 3, color = "darkgreen", opacity = 0.05)

map


tmpUsers<-users2
for(y in 1:5){
  print("start")
  Sys.sleep(30)
  print("debut recup user")
  for(i in 1:nrow(tmpUsers)){
    print(tmpUsers$screenName)  
    twToUser3 = searchTwitter(paste("@",tmpUsers$screenName[i], sep = ""), n = 50)
    
    if(i == 1){
      d3 = twListToDF(twToUser1)
    }else{
      d3 = rbind(d3, twListToDF(twToUser1))
    }
  }
  
Sys.sleep(30)
print("start recup user")
users3 <- d3 %>%
  group_by(screenName) %>%
  summarise(from=getUser(screenName)$location,lwd = n())%>%
  filter(!from=="")


location3 = NULL
lng3 = NULL
lat3 = NULL

Sys.sleep(30)
print("début recup geocode forward")
  for(i in 1:nrow(users3)){
    results<-opencage_forward(users3$from[i], "71f0409b179a40ddb21f233dd80b5e26",limit = 1)
    
    location3 = c(location3,users3$from[i])
    lng3 = c(lng3,results$results$geometry.lng)
    lat3 = c(lat3,results$results$geometry.lat)
    
  }

addCircleMarkers(map,lng = lng3, lat = lat3, radius = 2, color = "red", opacity = 0.5) 

tmpUsers<- users3
print(y)
}
  
