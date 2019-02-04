##################################################
##########REPRO CHAR ANALYSIS/VISULIZE/PCA########
##################################################

#set working dir and load packages
setwd("~/thesis_start/Chapter 2/analysis/morphometrics/")
library(tidyverse)
library(ggfortify)


#read in raw repro morpho data
repro_matrix<-read_csv("final_reproR.csv")

#read in molec clade data
molec.master<-read_csv("molec_master.csv")

#filter to only samples included in STACEY analysis (ie. remove outgroups)
stacey.only<-filter(molec.master, "Y" == Stacey)

#join stacey and  
stacey.join.pca.in<-inner_join(stacey.only, repro_matrix, by = "collection")

#perform pca 
pca<-prcomp(scale(stacey.join.pca.in[11:18], center = TRUE, scale = TRUE))

#basic plot
autoplot(pca, data = stacey.join.pca.in, colour = "Stacey_coarse", loadings = TRUE, loadings.label= TRUE)


###Prez quality plot###


#prepare input for plot

Pc_pop<-data.frame(pca$x, pop=stacey.join.pca.in$Stacey_clade, coarse=stacey.join.pca.in$Stacey_coarse)

#create color and shape objects to group by molec (STACEY) clade

cols<-c("andrus"="red", "camas"="blue" , "centralOR"="forestgreen", 
        "mtsp3"="steelblue", "pack"="hotpink", "tritri"="black", 
        "washcoN"="brown", "washcoS"="orange", "MVO_59"="purple")
shapes<-c("N"="triangle", "S"="square", "Andrus"="circle")


#prepare labels for molec clade

label<-(c("Andrusianum","Camas Praire", "East-Central Oregon",
          "Western Montana", "Packardiae",
          "TriTri", "Hell's Canyon", "Mann Creek"))
labels_coarse<-c("Andrusianum", "Northern","Southern")


#plot with color and shape by molecular clade

ggplot(Pc_pop, aes(x=PC1, y= PC2, col=pop, shape=coarse))+
  geom_point(size = 2.5)+
  scale_color_manual(values=cols, labels = label, name = "Fine Clade")+
  scale_shape_manual(values = shapes, labels= labels_coarse, name = "Coarse Clade")
