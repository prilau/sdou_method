library(ape)
library(phytools)
library(tidyverse)
source("../utility/functions.R")

charhist_discOnly <- read.simmap("output/ase/MAP.tree", format="phylip")

charhist_discCont <- read.simmap(paste0("output/joint_SDOU/marginal_character.tre"), format="phylip")
for(i in 1:length(charhist_discCont$maps)){
  if(length(charhist_discCont$maps[[i]]) == 1) next
  
  map <- c(charhist_discCont$maps[[i]][1])
  old_state <- names(charhist_discCont$maps[[i]][1])
  
  for (j in 2:length(charhist_discCont$maps[[i]])){
    new_state <- names(charhist_discCont$maps[[i]][j])
    if (isTRUE(new_state == old_state)){
      map[length(map)] <- map[length(map)] + charhist_discCont$maps[[i]][j]
    } else {
      map <- append(map, charhist_discCont$maps[[i]][j])
    }
    old_state <- new_state
  }
  charhist_discCont$maps[[i]] <- map
}


# posterior medians from joint-SDOU (mixed of 2 runs)
alpha  <- c(0.0275, 0.0459, 0.0301)
sigma2 <-	c(0.2143, 0.0865, 0.2205)
theta  <-	c(4.5429, 4.9544, 6.3673)
names(alpha) <- names(sigma2) <- names(theta) <- c("0", "1", "2")

cont_discOnly <- simulateContinuous(charhist_discOnly, alpha, sigma2, theta)
cont_discCont <- simulateContinuous(charhist_discCont, alpha, sigma2, theta)

write.nexus.data(cont_discOnly, "data/artiodactyla_Continuous_simDiscOnly.nex", format="Continuous")
write.nexus.data(cont_discCont, "data/artiodactyla_Continuous_simDiscCont.nex", format="Continuous")
