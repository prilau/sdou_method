library(ape)
library(phytools)
library(tidyverse)
source("../utility/functions.R")



# read tree
history <- read.simmap("../hypsodonty/output/ase_SCM/r100.trees", format="phylip")
cont_char <- read.csv("data/artiodactyla_Continuous.csv")
sp <- cont_char$species
cont_char <- unlist(cont_char$HI)
names(cont_char) <- sp

reps       = 20

alphas <- read.csv("data/alphas.csv", header = TRUE)
thetas <- read.csv("data/thetas.csv", header = TRUE)
sigma2s <- read.csv("data/sigma2s.csv", header = TRUE)


rpruning_liks <- expand.grid(nrep=1:20, charHist=1:100, r_pruning=NA)

bar = txtProgressBar(style=3, width=40)
for(i in 1:reps) {
  alpha <- unlist(alphas[i,])
  theta <- unlist(thetas[i,])
  sigma2 <- unlist(sigma2s[i,])

  
  names(alpha) <- names(sigma2) <- names(theta) <- c("0", "1", "2")
  
  for (j in 1:100){
    rpruning_liks$r_pruning[which(rpruning_liks$nrep==i & rpruning_liks$charHist==j)] <- sd_logL_pruning(history[[j]], cont_char, alpha, sigma2, theta)
    setTxtProgressBar(bar, ((i-1)*100+j) / (100*reps))
  }
  
  # increment the progress bar
}
write.csv(rpruning_liks, "output/likelihoods_rpruning.csv", row.names = FALSE, quote = FALSE)


rvcv_liks <- expand.grid(nrep=1:20, charHist=1:100, r_vcv=NA)
outFile <- file("output/likelihoods_rvcv.csv", "w")
writeLines("nrep,charHist,r_vcv", outFile)
close(outFile)

bar = txtProgressBar(style=3, width=40)
for(i in 1:reps) {
  alpha <- unlist(alphas[i,])
  theta <- unlist(thetas[i,])
  sigma2 <- unlist(sigma2s[i,])
  
  
  names(alpha) <- names(sigma2) <- names(theta) <- c("0", "1", "2")
  
  for (j in 1:100){
    rvcv_liks$r_vcv[which(rvcv_liks$nrep==i & rvcv_liks$charHist==j)] <- sd_logL_vcv(history[[j]], cont_char, alpha, sigma2, theta)
    outFile <- file("output/likelihoods_rvcv.csv", "a")
    writeLines(paste0(i,",",j,",",rvcv_liks$r_vcv[which(rvcv_liks$nrep==i & rvcv_liks$charHist==j)]), outFile)
    close(outFile)
    setTxtProgressBar(bar, ((i-1)*100+j) / (100*reps))
  }
  
  # increment the progress bar
}
#write.csv(rvcv_liks, "output/likelihoods_rvcv.csv", row.names = FALSE, quote = FALSE)
