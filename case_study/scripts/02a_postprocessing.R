library(ape)
library(phytools)
library(tidyverse)



# create a tree file with simmap trees
treetrace <- read.table("output/ase/charhist.log", header = TRUE)

# create a tree file with the MAP tree
maptree <- read.simmap(paste0("output/ase/marginal_character.tre"), format="phylip")
for(i in 1:length(maptree$maps)){
  if(length(maptree$maps[[i]]) == 1) next
  
  map <- c(maptree$maps[[i]][1])
  old_state <- names(maptree$maps[[i]][1])
  
  for (j in 2:length(maptree$maps[[i]])){
    new_state <- names(maptree$maps[[i]][j])
    if (isTRUE(new_state == old_state)){
      map[length(map)] <- map[length(map)] + maptree$maps[[i]][j]
    } else {
      map <- append(map, maptree$maps[[i]][j])
    }
    old_state <- new_state
  }
  maptree$maps[[i]] <- map
}
write.simmap(maptree, paste0("output/ase/MAP.tree"))

# for step 03a
nmaps <- length(treetrace$simmap)

# randomly draw one tree 
set.seed(1083)
#stochmaps_r1   <- treetrace$simmap[sample(1:nmaps, 1, replace=FALSE)]
stochmaps_r100 <- treetrace$simmap[sample(1:nmaps, 100, replace=FALSE)]
set.seed(NULL)

#write.table(stochmaps_r1, file = paste0("output/ase/", CLADE_NTAXA, "_simmap_r1.tree"), 
#            col.names = FALSE, row.names = FALSE, quote = FALSE)
write.table(stochmaps_r100, file = paste0("output/ase/r100.trees"), 
             col.names = FALSE, row.names = FALSE, quote = FALSE)
