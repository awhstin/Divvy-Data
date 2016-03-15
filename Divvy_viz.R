#Below you will see the Quarters section is commented out except for the first Quarter, 
#I wanted to provide all the data but I only use Q1 in my viz.

library(plyr)
library(ggplot2)
library(ggalt) 
library(rgdal)
library(rgeos)
library(maptools)
library(waffle)

#Quarters
Q1<-read.csv('Divvy_Trips_2015-Q1.csv', na.strings = FALSE)
#Q2<-read.csv('Divvy_Trips_2015-Q2.csv', na.strings = FALSE)
#Q4<-read.csv('Divvy_Trips_2015_Q4.csv', na.strings = FALSE)
#combine the three months into Q3
#Q3a<-read.csv('Divvy_Trips_2015_07.csv', na.strings = FALSE)
#Q3b<-read.csv('Divvy_Trips_2015_08.csv', na.strings = FALSE)
#Q3c<-read.csv('Divvy_Trips_2015_09.csv', na.strings = FALSE)
#Q3<-rbind(Q3a,Q3b,Q3c)

#Stations
stations<-read.csv('Divvy_Stations_2015.csv', na.strings = FALSE)

#test
rides<-rbind(Q1)

#get lat and lon for to & froms
temp<-rename(stations, c("id"="from_station_id"))
temp2<-rename(stations, c("id"="to_station_id","name"="name1","latitude"="lat1","longitude"="lon1"))
ride<-join(rides,temp,by='from_station_id', type='left')
ride<-join(ride,temp2,by='to_station_id', type='left')

#
#visualize
#

#shapefile stuff
na<-readOGR(dsn=".", layer="geo_export_6211654a-eb37-4139-831c-36da914d0e9e")
na@data$id <- rownames(na@data)
na.points<- fortify(na, region="id")
na.df <- join(na.points, na@data, by="id")

#create the map
map<-ggplot(data=na.df)+geom_polygon(aes(long,lat,group=group),alpha=.5,color="gray")+
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(), panel.background = element_rect(fill="white"), axis.ticks=element_blank(), axis.text=element_blank(), axis.title= element_blank())+
  scale_x_continuous(limits = c(-87.8, -87.55))+ scale_y_continuous(limits = c(41.8, 42))


#put ages into ranges
age<-as.data.frame(2016-ride$birthyear)
ride$age<-age$`2016 - ride$birthyear`
ride<-rename(ride,c("2016-ride$birthyear"="age"))
tens <- ride[ which(ride$age <= 19), ]
tens$range<-"Tens"
twenties <- ride[ which(ride$age >= 20 & ride$age < 30 ), ]
twenties$range<-"Twenties"
thirties <- ride[ which(ride$age >= 30 & ride$age < 40 ), ]
thirties$range<-"Thirties"
forties <- ride[ which(ride$age >= 40 & ride$age < 50 ), ]
forties$range<-"Forties"
fifties <- ride[ which(ride$age >= 50 & ride$age < 60 ), ]
fifties$range<-"Fifties"
sixties <- ride[ which(ride$age >= 60 & ride$age < 70 ), ]
sixties$range<-"Sixties"
seventy <- ride[ which(ride$age >= 70  ), ]
seventy$range<-"Seventy Plus"

theages<-rbind(tens,twenties,thirties,forties,fifties,sixties,seventy)

#bring it all together.
map+geom_segment(data=theages, aes(x=longitude, y=latitude, xend=lon1, yend=lat1, colour=range), alpha=.0025)+
  scale_x_continuous(limits = c(-87.8, -87.55))+
  scale_y_continuous(limits = c(41.8, 42))+
  scale_colour_brewer(palette="Dark2")+
  guides(colour = guide_legend(title="Age Range",override.aes = list(alpha = 1)))+
  theme(legend.position="none")
  
#More Divvy Themed Map
map<-ggplot(data=na.df)+geom_polygon(aes(long,lat, group=group),color="black", alpha=.25)+
  scale_x_continuous(limits = c(-87.725, -87.575))+ scale_y_continuous(limits = c(41.825, 41.95))

map<-map+geom_segment(data = theages,aes(x=longitude, y=latitude, xend=lon1, yend=lat1, colour=range), alpha=.25)+
  scale_colour_brewer(palette="Blues")+
  guides(colour = guide_legend(title="Age Range",override.aes = list(alpha = 1)))+
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(), panel.background = element_rect(fill="#2FCAFC"), axis.ticks=element_blank(), axis.text=element_blank(), axis.title= element_blank(), legend.position="none")

map+geom_point(data=stationsa,aes(x=longitude, y=latitude), color="white" ,alpha=.55, size=I(5))

  
#waffle
time<-c(`Thirties (71657)`=71657,`Twenties (40582)`=40582, `Forties (33556)`=33556,`Fifties (23520)`=23520,`Sixties (7544)`=7544,`Seventy Plus (725)`=725,`Tens (274)`=274)
waffles<-waffle(time/250, rows=10, size=0.25, colors=c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F", "#E5C494"), xlab="1 square == 250 riders", legend_pos = "top")




