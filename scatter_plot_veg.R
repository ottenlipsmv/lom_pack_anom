##################################################
##########VEG MORPHO CHAR ANALYSIS/VISULIZE########
##################################################

#read in new season (2018) veg morpho data
more_veg<-read_csv("veg_finish.csv")


#average the 5 width measurments
more_veg2<-mutate(more_veg, widthAvg = 
                    rowMeans(subset(more_veg, select= 
                        c(width1,width2, width3, width4, width5))))



#average the 5 lengths measurments
more_veg3<-mutate(more_veg2, LenAvg = 
                    rowMeans(subset(more_veg2, select = 
                                      c(length1, length2, length3, length4, length5))))

#create ratio col.
more_veg3<-mutate(more_veg3, Ratio = more_veg3$widthAvg/more_veg3$LenAvg)


#remove extraneous variables 
more_veg4<-select(more_veg3, collection, Ratio, LenAvg, widthAvg)


# join with molecular clade information
more_veg5<-full_join(more_veg4, molec_master)


#again remove extraneous/select relevant vairables
final_more_veg<-select(more_veg5, collection, Ratio, 
                       Stacey_coarse, Stacey_clade, LenAvg, widthAvg)

#remove NAs
final_more_veg<-drop_na(final_more_veg)


#read in previous season's (2017) veg morpho data
molec_only<-read.csv("...")

#again remove extraneous/select relevant vairables
other_old_veg<-select(molec_only, collection, 'Aspect Ratio', 
                      Stacey_coarse, Stacey_clade, 'Leaflet lengths', 
                      'leaflet widths')

#rename cols to match new collections/measurments
other_old_veg<-mutate(other_old_veg, Ratio = other_old_veg$`Aspect Ratio`, 
                      LenAvg = other_old_veg$`Leaflet lengths`, 
                      widthAvg = other_old_veg$`leaflet widths`)

#again remove extraneous/select relevant vairables

final_old_veg<-select(other_old_veg, -'Aspect Ratio', -'Leaflet lengths',
                      -'leaflet widths')


#join 2017 and 2018
all_ratio<-rbind(final_more_veg, final_old_veg)

#remove thom and hm (outgroups)

all_ratio_in<-subset(all_ratio, Stacey_clade != 'Hmout')
all_ratio_in<-subset(all_ratio_in, Stacey_clade != 'thom')


#PREZ QUALITY GRAPH#

#create color and shape objects to group by molec (STACEY) clade

cols<-c("andrus"="red", "camas"="blue" , "centralOR"="forestgreen", 
        "mtsp3"="steelblue", "pack"="hotpink", "tritri"="black", 
        "washcoN"="brown", "washcoS"="orange", "mvo_59"="purple")

size<-c("andrus"="4", "camas"="4" , "centralOR"="4", 
        "mtsp3"="4", "pack"="4", "tritri"="4", 
        "washcoN"="4", "washcoS"="4", "mvo_59"="4")


shapes<-c("N"="triangle", "S"="square", "Andrus"="circle")

#prepare labels for molec clade

label<-(c("Andrusianum","Camas Praire", "East-Central Oregon", "Western Montana", "MVO_59","Packardiae",
          "TriTri", "Hell's Canyon", "Mann Creek"))

labels_coarse<-c("Andrusianum", "Northern","Southern")

#ratio boxplot 

ggplot(all_ratio_in, aes(Stacey_clade,log(Ratio), fill=Stacey_clade))+
  geom_boxplot()+
  scale_fill_manual(values=cols)

#final scatter plot (len vs. width)

ggplot(data=all_ratio_in)+
  geom_point(mapping = aes(x=LenAvg, y=`widthAvg`, color=Stacey_clade, shape=Stacey_coarse))+
  scale_color_manual(values = cols, labels = label, name = "Fine Clade")+
  scale_shape_manual(values=shapes, labels = labels_coarse, name = "Coarse Clade")+
  scale_size_manual(values=size, guide = "none")
