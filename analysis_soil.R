setwd("~/thesis_start/Chapter 2/analysis/Soil_data/")
library(ggplot2)
library(ggfortify)
library(tidyverse)

#really quick and dirty TEST/visulaztion of semi-raw data
soils<-read.csv("raw_all_soils.csv")

soils2<-soils[,2:22]
soils2
soils2pca<-prcomp(soils2, center=TRUE, scale = TRUE)
autoplot(soils2pca)
?autoplot
soils2pca
###########TESTABOVE##########
###########Real_below########

#read in main soils data
#raw<-read_csv("R_final_input.csv")

#join with p-size estimation
#particle.size.ott<-read_csv("soils_particle_size_R.csv")
#?rename

#particle.size.ott2<-rename(particle.size.ott, Collection = Sample)

#psize.include<-left_join(raw, particle.size.ott2, by = "Collection")

#write.csv(psize.include, "joined.csv")

#fix outside R, move around a few cols and add in molecul hypos

############STARTHERE##############

#read in soils_data
ready<-read.csv("ready_for_pca_molec_only.csv")

#read in species data
molec.master<-read_csv("molec_master.csv")

#join species and soils data
molec.only<-inner_join(molec.master, ready, by="collection")




pca.im<-molec.only[,12:27]


#fucking sand and silt was character
#pca.im$sand_and_silt<-as.numeric(pca.im$sand_and_silt)

#perform the pca
pca<-prcomp(scale(pca.im, center = TRUE, scale = TRUE))

pca$rotation

#basic stuff
autoplot(pca)


#prettier prez quality 

soils_pca<-data.frame(pca$x, pop=molec.only$Stacey_clade, coarse=molec.only$Stacey_coarse, collection =molec.only$collection)

#stacey_clade
cols<-c("andrus"="red", "camas"="blue" , "centralOR"="forestgreen", "mtsp3"="steelblue", "pack"="hotpink", "tritri"="black", "washcoN"="brown", "washcoS"="orange", "mvo_59"="purple")
shapes<-c("N"="triangle", "S"="square", "Andrus"="circle")


label<-(c("Andrusianum","Camas Praire", "East-Central Oregon", "Western Montana", "Packardiae", "TriTri",
          "Hell's Canyon", "Mann Creek", "MVO_59"))

labels_coarse<-c("Andrusianum", "Northern", "Southern")

#need to add these labels to other plots

#ggplot(soils_pca, aes(x=PC1, y= PC2, col=pop, shape = coarse))+
#  geom_point(size = 2.5)+
#  scale_color_manual(values=cols, labels = label, name = "Fine Clade")

ggplot(soils_pca, aes(x=PC1, y= PC2, col=pop, shape = coarse))+
  geom_point(size = 2.5)+
  scale_color_manual(values=cols, labels = label, name = "Fine Clade")+
  scale_shape_manual(values = shapes, labels = labels_coarse, name = "Coarse Clade")

  #geom_text(data = soils_pca, aes(x=PC1, y= PC2, label= collection))

#is there a way to just label one?
#what is with mvo65
#MVO 73 is not with the other tri tris.

#geom_text(data = clean_data, aes(x = lon, y = lat, label = collection), 
  #        size = 3, vjust = 0, hjust = -0.5)


#coarse 
autoplot(pca, data = molec.only, colour = "Stacey_coarse", frame=TRUE, frame.type="norm")

autoplot(pca, data = molec.only, colour = "Stacey_coarse", label = TRUE, shape =FALSE, loadings = TRUE, loadings.label = TRUE)


#pca.in3<-as.numeric(pca.in2)

#need to join upp with molec_master

#proably worthwhile to tease apart the whole anomalum clade

###JUNKBELWO########

#now I need to trasnfer the data from the joined cols into the orginal cols

#psize.include.rename<-mutate(psize.include, gravel.x = gravel.y)
#psize.include.rename2<-mutate(psize.include.rename, clay.x = clay.y)
#psize.include.rename3<-mutate(psize.include.rename2, sand_and_silt.x = coarse)
#shit need to calculate # sand_and_silt for DONS!####

#remove three cols
#final.ish<-select(psize.include.rename3, -gravel.y, -clay.y, -coarse, -sand, -sand_and_silt)

#join with molecular??

#export so I can add spphypo and molec y or n

# graph and plot pca