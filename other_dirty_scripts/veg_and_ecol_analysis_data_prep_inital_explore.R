library(tidyverse)
library(ggfortify)
#set wd to thesis data collection
setwd("thesis data collection/")
cover_data<-read_csv("cover_data_R.csv")
#make 'tidy'
#ie. by collector #  
by_coll<- cover_data %>% group_by(`coll #`)

#that prob needs to be a function

#test summarry
#bare<-summarise(by_coll, average_bare = mean(as.numeric(`% bare`),na.rm = TRUE, n=n()))


#test('%woody')
#summarize cover data
summary<-summarise_all(by_coll, mean)


#summarize soil compac data
         
compac<-summary[,17:21]
compac_means<-rowMeans(compac)

summary2<-add_column(summary, compac_mean=compac_means)




aspect_ratio<-read_csv("aspect_ratio.csv")
morpho_spp<-aspect_ratio[,4:5]

#join with summary

#need make Coll capital in summary 2

summary3<-rename(summary2, `Coll #` = "coll #")

#as.characeter

#mtcars$qsec <-as.integer(mtcars$qsec)

#summary3$`coll #`<-as.integer(summary3$`coll #`)


morpho.summary2<-left_join(summary3, morpho_spp, by="Coll #")



#cleanup/remove cols not of interest

#PCA
#just cover


#cover_pca_input<-morpho.summary2[6:14]

cover_pca_input<-select(morpho.summary2, 6:14, 27)

#nas into zeros

cover_pca_input[is.na(cover_pca_input)] <- 0

cover_pca_input2<-scale(cover_pca_input[1:9], scale = TRUE, center = TRUE)

PCA_veg<-prcomp(cover_pca_input2)


autoplot(PCA_veg, data = cover_pca_input, colour = 'spp')


#sort of two clusters. do they match up with morphology or genetics??

#not with morpho? maybe try without 0s for species

pack_anom_only<-filter(cover_pca_input, spp != "0")

pack_anom_PCA_input<-scale(pack_anom_only[1:9], scale = TRUE, center = TRUE)

PCA_pack_anom<-prcomp(pack_anom_PCA_input)

autoplot(PCA_pack_anom, data = pack_anom_only, colour = 'spp')

#I need to spend some time with the trees tomorrow coming up with some species hypothesis
#also I need to prune/include other for this data set to what is just in the molecular analysis.

?filter
#I need two more cols, one about 'geneitc species',  one about 'morpho species',

#not very exciting results
#need to seee if they match up with 'species'

#autoplot example below
#autoplot(alpine.pca,  frame=TRUE, frame.type="euclid", data = master_spp_list, colour = 'Species',loadings=TRUE, loadings.label=TRUE)








#all ecological variables?

#need to important soils data.
#first lets check what compaction only looks like

hist(summary2$compac_mean)


