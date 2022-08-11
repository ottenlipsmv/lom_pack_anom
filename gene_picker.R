#import data
gene_stats<-read.csv("~/NGS stats/AMAS_stats_post_BMGE.txt", sep = "")

genes_w_paralogs<-readLines("NGS stats/genesWparalogs_fasta.txt")

#at least 75% of samples present

gene1<-subset(gene_stats, gene_stats$No_of_taxa>=37)

#less than 10% missing data

gene2<-subset(gene1, gene1$Missing_percent<=10)


#alignment length greater than 200
gene3<-subset(gene2, gene2$Alignment_length>=200)


# remove genes that have paralog warnings
gene4<-gene3[! gene3$Alignment_name %in% genes_w_paralogs,]


#remove genes that are over 15% variable

gene5<-subset(gene4, gene4$Proportion_variable_sites<=.15)

#average missing data, but doesnt include those missing complete sequences

#return taxa without full sequences

#what is their alignment legnth?

#add it together???

#weight for missing %???

mean(gene5$Missing_percent)
#2.6%; not quite right.

#probably easier to use AMAS concat feature.

#export list
Final_gene_list <-gene5$Alignment_name

write.csv(Final_gene_list, "~/NGS stats/GENES_TO_ANALYZE.csv")
