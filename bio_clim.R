#climate niche analysis of pack/anom
library(rosm)
library(prettymapr)
library(raster)
library(ggmap)
library(rgdal)
library(ggfortify)
library(ggplot2)
library(Hmisc)


#import data (only 48 molecular colletions now)
map_file<-read.csv("~/thesis_start/Chapter 2/analysis/map_geography/48_map.csv")


##########################ALREADYDONE###########################
#all_lat_long_only<-map_file[,7:6]

#all_lat_long_only


#download and extract BIOCLIM variables

#climate_data_12<- getData("worldclim",var="bio",res=.5, lon=-117, lat=45)
#climate_data_missing<-getData("worldclim",var="bio",res=.5, lon=-120.5393, lat=47.53360)

#just do it for 1 then add it back in.

#all_climate<-merge(climate_data_missing, climate_data_12)


#setwd("~/alpine_climate_final/")

#bio1_12<-raster("bio_12/bio1_12.bil")
#bio2_12<-raster("bio_12/bio2_12.bil")
#bio3_12<-raster("bio_12/bio3_12.bil")
#bio4_12<-raster("bio_12/bio4_12.bil")
#bio5_12<-raster("bio_12/bio5_12.bil")
#bio6_12<-raster("bio_12/bio6_12.bil")
#bio7_12<-raster("bio_12/bio7_12.bil")
#bio8_12<-raster("bio_12/bio8_12.bil")
#bio9_12<-raster("bio_12/bio9_12.bil")
#bio10_12<-raster("bio_12/bio10_12.bil")
#bio11_12<-raster("bio_12/bio11_12.bil")
#bio12_12<-raster("bio_12/bio12_12.bil")
#bio13_12<-raster("bio_12/bio13_12.bil")
#bio14_12<-raster("bio_12/bio14_12.bil")
#bio15_12<-raster("bio_12/bio15_12.bil")
#bio16_12<-raster("bio_12/bio16_12.bil")
#bio17_12<-raster("bio_12/bio17_12.bil")
#bio18_12<-raster("bio_12/bio18_12.bil")
#bio19_12<-raster("bio_12/bio19_12.bil")

#bio_12_all<-brick(bio1_12,bio2_12,bio3_12,bio4_12,bio5_12,bio6_12, bio7_12,bio8_12, bio9_12, bio10_12,bio11_12,bio12_12,bio13_12,bio14_12,bio15_12,bio16_12,bio17_12,bio18_12,bio19_12)


#pack.anom.bioCLIM<-extract(bio_12_all, all_lat_long_only)

#one<-all_lat_long_only[1,]

#four.eight<-all_lat_long_only[48,]

#left.over<-rbind(one, four.eight)

#extract.left.over<-extract(climate_data_missing, left.over)

#setwd("")
#write.csv(pack.anom.bioCLIM, "most.csv")
#write.csv(extract.left.over, "rest.csv")
########ALREADY DONE ABOVE####################


####START HERE######
#make sure to load map_file 1st!

ALL.MOLEC<-read.csv("~/thesis_start/Chapter 2/analysis/all.csv")

#need to remove thom and hm and then join with molec
molec.master<-read_csv("molec_master.csv")

#remove thom/hmout
stacey.only<-filter(molec.master, "Y" == Stacey)

#join stacey data
all.molec.2<-mutate(ALL.MOLEC, collection = map_file$Collection)

stacey.join.pca.in<-inner_join(stacey.only, all.molec.2, by = "collection")

#perform pca
pca1<-prcomp(scale(stacey.join.pca.in[,10:28], center = TRUE, scale =TRUE ))

#inital plot
autoplot(pca1, data=stacey.join.pca.in, colour = "Stacey_coarse")

#loadings
autoplot(pca1, data=stacey.join.pca.in, colour = "Stacey_coarse", loadings = TRUE, loadings.label =TRUE)

loadigns<-pca1$rotation
write.csv(loadigns, "loadings.csv")

#by 'pop'

Pc_pop<-data.frame(pca1$x, pop=stacey.join.pca.in$Stacey_clade, coarse=stacey.join.pca.in$Stacey_coarse, collection=stacey.join.pca.in$collection)

cols<-c("andrus"="red", "camas"="blue" , "centralOR"="forestgreen", "mtsp3"="steelblue", "pack"="hotpink", "tritri"="black", "washcoN"="brown", "washcoS"="orange", "mvo_59"="purple")

shapes<-c("N"="triangle", "S"="square", "Andrus"="circle")


label<-(c("Andrusianum","Camas Praire", "East-Central Oregon", "Western Montana", "MVO_59","Packardiae",
          "TriTri", "Hell's Canyon", "Mann Creek"))

labels_coarse<-c("Andrusianum", "Northern","Southern")



#can I add the north/south frame to this somehow??
ggplot(Pc_pop, aes(x=PC1, y= PC2, col=pop, shape=coarse))+
  geom_point(size = 2.5)+
  scale_color_manual(values=cols, labels = label, name = "Fine Clade")+
  scale_shape_manual(values=shapes, labels = labels_coarse, name = "Coarse Clade")
    
  
  
  #geom_text(data = Pc_pop, aes(x=PC1, y= PC2, label= collection))


?stat_ellipse

?frame

?stat_ellipse





#lets remove the correlated varaibles?? #doesn't seems to make a lot of sense

#bio.only<-scale(stacey.join.pca.in[,10:28], center = TRUE, scale =TRUE)

#cor<-rcorr(bio.only, type = "pearson")

#print(cor)







####BS JUNK BELOW##########
pca<-prcomp(ALL.MOLEC, center = TRUE, scale = TRUE)

#north-south astral split
autoplot(pca, data = map_file, colour = "Astral", frame = TRUE, frame.type = "norm", label = TRUE, shape = FALSE)



#need to join with molec_master to plot diff colors.



#autoplot(pca, data = map_file, colour = "NGS_clade_coarse")
mole



?loadings.label

?labels

map_file

pca

###############################
cor<-rcorr(as.matrix(ALL.MOLEC), type = "pearson")
print(cor)

#still need to remove correlated variables?

#print(cor)

