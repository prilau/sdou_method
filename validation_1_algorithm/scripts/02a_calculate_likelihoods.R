library(ape)
library(phytools)
library(pracma)
library(tidyverse)
source("../utility/functions.R")


# read tree
history <- read.simmap("data/charhist.trees", format="phylip")
cont_char <- read.csv("data/artiodactyla_Continuous.csv")
sp <- cont_char$species
cont_char <- unlist(cont_char$brain_mass_g_log_mean)
names(cont_char) <- sp

reps       = 1000

pars <- read.csv("data/parameters.csv")


rb_output <- read_file(paste0("output/rb_likelihoods.txt"))
rb_output <- gsub("\\[ ", "", rb_output)
rb_output <- gsub(" \\]", "", rb_output)
rb_output <- as.numeric(strsplit(rb_output, ", ")[[1]])

liks <- tibble(rb=rb_output, r_pruning=NA, r_vcv=NA, .rows=1000)

bar = txtProgressBar(style=3, width=40)
for(i in 1:reps) {
  alpha <- unlist(pars[i,1:3])
  theta <- unlist(pars[i, 4:6])
  sigma2 <- unlist(pars[i, 7:9])

  
  names(alpha) <- names(sigma2) <- names(theta) <- c("0", "1", "2")
  
  
  liks$r_pruning[i] <- sd_logL_pruning(history[[i]], cont_char, alpha, sigma2, theta)
  liks$r_vcv[i] <- sd_logL_vcv(history[[i]], cont_char, alpha, sigma2, theta)
  # increment the progress bar
  setTxtProgressBar(bar, i / reps)
  
}

write.csv(liks, "output/likelihoods.csv", row.names = FALSE)
