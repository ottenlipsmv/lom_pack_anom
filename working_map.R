library(ggmap)
library(tidyverse)

setwd("~/thesis_start/Chapter 2/analysis/map_geography")


ggmap(get_googlemap())
register_google(key = "AIzaSyBfpKZJCXgeUaLud7PWACqzQt_bNLxmIgM", account_type = "standard")

map_file<-read.csv("48_map.csv")

molec.master<-read_csv("molec_master.csv")


#remove thom/hmout; maybe leave them in but color them differently? 
#stacey.only<-filter(molec.master, "Y" == Stacey)

#join stacey data
all.molec.2<-mutate(map_file, collection = map_file$Collection)

stacey.join<-inner_join(molec.master, all.molec.2, by = "collection")

#make a clean file with only lat, long, stacey_clade: fine and coarse, and collection

clean_data<-select(stacey.join, collection, Stacey_coarse, Stacey_clade, lat=Lat.y, lon=Long.y)
  

#make a bbox based on my collections only

lon.lat.only<-clean_data[,4:5]
box<-make_bbox(lon = lon, lat = lat, data = lon.lat.only, f=0.25)

?make_bbox

#get/make basemap

#simple with roads
base_map<-get_map(location = box, maptype = "toner-background", source= "stamen")

#terrian, but too green
base_map2<-get_map(location = box, maptype = "terrain-background", source= "stamen")

#simple with rivers instead of roads.
base_map3<-get_map(location = box, maptype = "terrain-lines", source= "stamen")


#not bad google hybrid map, need to fiddle w/ box though
base_map4<-get_map(location = box, maptype = "hybrid", source= "google")

base_map5<-base_map3<-get_map(location = box, maptype = "satellite", source= "google")


#color them
cols<-c("andrus"="red", "camas"="blue" , "centralOR"="forestgreen", 
"mtsp3"="steelblue", "pack"="hotpink", "tritri"="black", 
"washcoN"="brown", "washcoS"="orange", "mvo_59"="purple", "OUT"= "limegreen")

shapes<-c("N"="triangle", "S"="square", "Andrus"="circle", "OUT"="diamond")


#cols<-c("andrus"="red", "camas"="blue" , "centralOR"="forestgreen", "mtsp3"="steelblue", "pack"="hotpink", "tritri"="black", "washcoN"="brown", "washcoS"="orange", "mvo_59"="purple", "OUT"= "limegreen")
#shapes<-c("N"="triangle", "S"="square", "Andrus"="circle", "OUT"="diamond")


label<-(c("Andrusianum","Camas Praire", "East-Central Oregon", "Western Montana", "MVO_59", "Out Group", "Packardiae",
          "TriTri", "Hell's Canyon", "Mann Creek"))

labels_coarse<-c("Andrusianum", "Northern", "Out Group", "Southern")



ggmap(base_map3)+
  geom_point(data = clean_data, mapping = aes(x=lon, y = lat, color = Stacey_clade, shape = Stacey_coarse, size = 5))+
  scale_color_manual(values=cols, labels = label, name = "Fine Clade")+
  scale_shape_manual(values=shapes, labels = labels_coarse, name = "Coarse Clade")


?size
# geom_text(data = clean_data, aes(x = lon, y = lat, label = collection), 
#size = 3, vjust = 0, hjust = -0.5)


#removed shape = coarse
#could also just not include outgroups in stacey clade and just have them
#be shapes 

write.csv(clean_data, "clean_map_data.csv")
