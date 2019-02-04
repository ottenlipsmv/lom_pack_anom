########################################################
##########climate niche analysis bioclim var > PCA######
########################################################

#load packages
library(rosm)
library(prettymapr)
library(raster)
library(ggmap)
library(rgdal)
library(ggfortify)
library(ggplot2)
library(Hmisc)


#import data (GPS corrdiantes for the 48 collections include in molecular analysis)
map_file<-read.csv("~/thesis_start/Chapter 2/analysis/map_geography/48_map.csv")


#Download and extract BIOCLIM variables for each GPS point

#isolate just lat/long
all_lat_long_only<-map_file[,7:6]



#download BIOCLIM variables

#download  UTM zone 12 for bioclim
climate_data_12<- getData("worldclim",var="bio",res=.5, lon=-117, lat=45)

#download UTM zone 11 for bioclim
climate_data_missing<-getData("worldclim",var="bio",res=.5, lon=-120.5393, lat=47.53360)

#combine climate rasters for zones 11 and 12
all_climate<-merge(climate_data_missing, climate_data_12)

#extract variables at each gps point
pack.anom.bioCLIM<-extract(all_climate, all_lat_long_only)


#save extracted variables to computer for safekeeping
#write.csv(pack.anom.bioclim, "~/thesis_start/Chapter 2/analysis/all.csv")


#load csv saved above
ALL.MOLEC<-read.csv("~/thesis_start/Chapter 2/analysis/all.csv")

#remove thom and hm (outgroups) and then join with molecular clade data

#read in molecular clade data
molec.master<-read_csv("molec_master.csv")

#remove thom/hmout (out groups) from data
stacey.only<-filter(molec.master, "Y" == Stacey)

#join molecular clade data with bioclim variables data

#create new col. with Collection #s so join can occur
all.molec.2<-mutate(ALL.MOLEC, collection = map_file$Collection)

#join bioclim data with moelcular clade data
stacey.join.pca.in<-inner_join(stacey.only, all.molec.2, by = "collection")


#perform pca
pca1<-prcomp(scale(stacey.join.pca.in[,10:28], center = TRUE, scale =TRUE ))

#inital plot
autoplot(pca1, data=stacey.join.pca.in, colour = "Stacey_coarse")

# View loadings
autoplot(pca1, data=stacey.join.pca.in, colour = "Stacey_coarse", loadings = TRUE, loadings.label =TRUE)

#created loadings object and write to fule
loadigns<-pca1$rotation
#write.csv(loadigns, "loadings.csv")


###Prez quality plot###


#prepare input for plot
Pc_pop<-data.frame(pca1$x, pop=stacey.join.pca.in$Stacey_clade, coarse=stacey.join.pca.in$Stacey_coarse, collection=stacey.join.pca.in$collection)

#create color and shape objects to group by molec clade
cols<-c("andrus"="red", "camas"="blue" , "centralOR"="forestgreen", "mtsp3"="steelblue", "pack"="hotpink", "tritri"="black", "washcoN"="brown", "washcoS"="orange", "mvo_59"="purple")

shapes<-c("N"="triangle", "S"="square", "Andrus"="circle")


#prepare labels for molec clade
label<-(c("Andrusianum","Camas Praire", "East-Central Oregon", "Western Montana", "MVO_59","Packardiae",
          "TriTri", "Hell's Canyon", "Mann Creek"))

labels_coarse<-c("Andrusianum", "Northern","Southern")


#plot with color and shape by molecular clade

ggplot(Pc_pop, aes(x=PC1, y= PC2, col=pop, shape=coarse))+
  geom_point(size = 2.5)+
  scale_color_manual(values=cols, labels = label, name = "Fine Clade")+
  scale_shape_manual(values=shapes, labels = labels_coarse, name = "Coarse Clade")
