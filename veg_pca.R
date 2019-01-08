library(tidyverse)
library(ggfortify)



veg_least_missing<-read_csv("corase_clade.csv")

#nas into zeros
veg_least_missing[is.na(veg_least_missing)] <- 0

corase_clade2<-scale(veg_least_missing[2:7], scale = TRUE, center = TRUE)

PCA_corase<-prcomp(corase_clade2)


autoplot(PCA_corase, data = veg_least_missing, colour = 'coarse_clade', label= TRUE)

#ones down in the corner are mvo 22 and 25

autoplot(PCA_corase, data = veg_least_missing, colour='morph')


group1<-select(veg_least_missing, 'veg group'=1)

?select
