#####################################################
####PCA on soil mineral content and particle size####
#####################################################




#load packages and set working dir.
setwd("~/thesis_start/Chapter 2/analysis/Soil_data/")
library(ggplot2)
library(ggfortify)
library(tidyverse)

##quick and dirty visualization of data 
#load data
soils<-read.csv("raw_all_soils.csv")

#select only soil variables
soils2<-soils[,2:22]

#perform PCA
soils2pca<-prcomp(soils2, center=TRUE, scale = TRUE)

#basic plot
autoplot(soils2pca)



############MAIN ANALYSIS##############

#read in soils_data
ready<-read.csv("ready_for_pca_molec_only.csv")

#read in species data
molec.master<-read_csv("molec_master.csv")

#join species and soils data
molec.only<-inner_join(molec.master, ready, by="collection")


#prepare input for pca
#retain rows with data only
pca.im<-molec.only[,12:27]


#change sand and silt from numeric to character data 
#pca.im$sand_and_silt<-as.numeric(pca.im$sand_and_silt)

#perform the pca
pca<-prcomp(scale(pca.im, center = TRUE, scale = TRUE))


#basic plots

#very basic
autoplot(pca)

#create ellipse and view variation 
autoplot(pca, data = molec.only, colour = "Stacey_coarse", frame=TRUE, frame.type="norm")

#view loadings
autoplot(pca, data = molec.only, colour = "Stacey_coarse", label = TRUE, shape =FALSE, loadings = TRUE, loadings.label = TRUE)



#Presentation quality plot with ggplot2


# 1st step: create a data frame w/ relevant data
soils_pca<-data.frame(pca$x, pop=molec.only$Stacey_clade, coarse=molec.only$Stacey_coarse, collection =molec.only$collection)

#2nd step: create the shapes and colors 
cols<-c("andrus"="red", "camas"="blue" , "centralOR"="forestgreen", "mtsp3"="steelblue", "pack"="hotpink", "tritri"="black", "washcoN"="brown", "washcoS"="orange", "mvo_59"="purple")
shapes<-c("N"="triangle", "S"="square", "Andrus"="circle")

#3rd step: create the labels
label<-(c("Andrusianum","Camas Praire", "East-Central Oregon", "Western Montana", "Packardiae", "TriTri",
          "Hell's Canyon", "Mann Creek", "MVO_59"))

labels_coarse<-c("Andrusianum", "Northern", "Southern")


#4th step: build the plot that is color and shape coordinated base on molecular clades 
ggplot(soils_pca, aes(x=PC1, y= PC2, col=pop, shape = coarse))+
  geom_point(size = 2.5)+
  scale_color_manual(values=cols, labels = label, name = "Fine Clade")+
  scale_shape_manual(values = shapes, labels = labels_coarse, name = "Coarse Clade")

  

