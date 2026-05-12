library(ape)
library(phytools)
library(pracma)
library(tidyverse)
source("../utility/functions.R")

nrep=20
ncharHist=100



outFile <- file("output/rb_likelihoods.csv", "a")
for (i in 1:ncharHist){
  rb_liks <- readLines(paste0("output/rb_likelihoods_charHist_", i, ".txt"))
  rb_liks <- gsub("\\[ ", "", rb_liks)
  rb_liks <- gsub("\\ ]", "", rb_liks)
  writeLines(rb_liks, outFile)
  file.remove(paste0("output/rb_likelihoods_charHist_", i, ".txt"))
}
close(outFile)

rb_liks <- read.csv("output/rb_likelihoods.csv", header = FALSE)
rb_liks$charHist <- as.numeric(gsub("V", "", rownames(rb_liks)))
rb_liks <- rb_liks %>% pivot_longer(names_to = "nrep", cols = starts_with("V"), values_to = "rb_lik") %>% 
  mutate(nrep = as.numeric(gsub("V", "", nrep))) %>% 
  relocate(charHist, .after = nrep)

write.csv(rb_liks, "output/likelihoods_rb.csv", row.names = FALSE, quote = FALSE)
file.remove("output/rb_likelihoods.csv")
