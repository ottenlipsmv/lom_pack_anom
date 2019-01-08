setwd("~/thesis_start/Chapter 2/analysis/morphometrics/")
library(tidyverse)
library(ggfortify)

repro_matrix<-read_csv("final_reproR.csv")
#doublecheck MVo_154 and MVO_180

#read in molec
molec.master<-read_csv("molec_master.csv")
stacey.only<-filter(molec.master, "Y" == Stacey)

#join
stacey.join.pca.in<-inner_join(stacey.only, repro_matrix, by = "collection")

#the deed
pca<-prcomp(scale(stacey.join.pca.in[11:18], center = TRUE, scale = TRUE))

#basic plot
autoplot(pca, data = stacey.join.pca.in, colour = "Stacey_coarse", loadings = TRUE, loadings.label= TRUE)


#by 'pop'

Pc_pop<-data.frame(pca$x, pop=stacey.join.pca.in$Stacey_clade, coarse=stacey.join.pca.in$Stacey_coarse)

cols<-c("andrus"="red", "camas"="blue" , "centralOR"="forestgreen", "mtsp3"="steelblue", "pack"="hotpink", "tritri"="black", "washcoN"="brown", "washcoS"="orange", "MVO_59"="purple")

shapes<-c("N"="triangle", "S"="square", "Andrus"="circle")


label<-(c("Andrusianum","Camas Praire", "East-Central Oregon", "Western Montana", "Packardiae",
          "TriTri", "Hell's Canyon", "Mann Creek"))

labels_coarse<-c("Andrusianum", "Northern","Southern")

ggplot(Pc_pop, aes(x=PC1, y= PC2, col=pop, shape=coarse))+
  geom_point(size = 2.5)+
  scale_color_manual(values=cols, labels = label, name = "Fine Clade")+
  scale_shape_manual(values = shapes, labels= labels_coarse, name = "Coarse Clade")
#probably change the size of the points? size = 
#?scale_colour_manual

# old colour command  scale_color_manual(values=c("red","blue","green","steelblue","hotpink","black","brown", "orange", "purple"))

#geom_point

#mine
#cols<-c("andrus"="red", "camas"="blue" , "centralOR"="forestgreen", "mtsp3"="steelblue", "pack"="hotpink", "tritri"="black", "washcoN"="brown", "washcoS"="orange", "mvo_59"="purple")
#change green and steelblue?
#don't forget MVO_59

#example to came colours below
#cols <- c("8" = "red", "4" = "blue", "6" = "darkgreen", "10" = "orange")
#p + scale_colour_manual(values = cols)