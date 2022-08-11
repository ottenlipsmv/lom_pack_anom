require(ape)
require(phylotools)
require(ips)
setwd("~/Ottenlips_new_analysis/")

#remove 49-56 and associated Rs


#import list of duplicates to remove
#duplicates<-readLines("scripts/Remove Dupes in R/duplicate_individuals_to_remove.txt")


duplicates<-readLines("scripts/Remove Dupes in R/duplicate_introns.txt")

#still one R remaining 

#need to remove everything after the L

gsub()

?regex

#this will have to be a loop


#works in one at a time....
#rm.sequence.fasta(infile = "exons/aligned/4527.FNA.fasta", outfile = "exons/aligned/test_4527.fasta", to.rm =  c(duplicates))


#rm.sequence.fasta(test_4527, outfile =paste(fasta,".remove.fasta", sep =""), to.rm = duplicates)


#loop to do all of them!

#setwd("exons/aligned/")

setwd("introns/aligned/post_bmge/all_introns/")

FNA <- list.files(pattern="fasta")

for (i in FNA){
  rm.sequence.fasta(i, outfile =paste(i,".remove.fasta", sep =""), to.rm = duplicates)
}


