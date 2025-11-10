library(ape)
library(tidyverse)

tree <- read.nexus("data/raw/cetartiodactyla_gtr.tre")

df <- read.table("data/raw/ruminants.txt", sep="\t", header = TRUE)
species_keep <- intersect(tree$tip.label, df$species)

df <- df %>% filter(!is.na(species),
                    species %in% species_keep) %>% 
  mutate(species = gsub("^([^_]*_[^_]*)_.*$", "\\1", species),
         diet = ifelse(diet == "Br", 0, ifelse(diet == "MF", 1, 2)))

tree <- keep.tip(tree, species_keep)
tree$tip.label <- gsub("^([^_]*_[^_]*)_.*$", "\\1", tree$tip.label)
tree_rescaled <- tree
tree_rescaled$edge.length <- tree_rescaled$edge.length / max(node.depth.edgelength(tree))
write.tree(tree, "data/artiodactyla.tree")
write.tree(tree_rescaled, "data/artiodactyla_rescaled.tree")



disc <- cont <- list()
for (i in 1:nrow(df)){
  sp <- df$species[i]
  disc[[sp]] <- df$diet[i]
  cont[[sp]] <- df$HI[i]
}

df_cont <- df %>% select(species, HI)

write.nexus.data(disc, "data/artiodactyla_Discrete.nex", format="standard")
write.nexus.data(cont, "data/artiodactyla_Continuous.nex", format="continuous")
write.csv(df_cont, "data/artiodactyla_Continuous.csv", row.names = FALSE)
