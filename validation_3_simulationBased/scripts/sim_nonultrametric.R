library(ape)
library(phytools)
tree <- read.tree("data/artiodactyla_rescaled.tree")
char_hist <- read.simmap("data/artiodactyla_character_history.tree", format="phylip")

set.seed(8567)
extinct_tips <- sample(1:length(tree$tip.label), 5)
branches <- which(tree$edge[,2] %in% extinct_tips)
bl_ratio <- runif(5, 0.3, 0.7)
tree$edge.length[branches] <- tree$edge.length[branches] * bl_ratio
plot(tree, show.tip.label = FALSE)
write.tree(tree, "data/artiodactyla_sim_nonultrametric.tree")

char_hist$edge.length[branches] <- char_hist$edge.length[branches] * bl_ratio
for (i in 1:length(branches)){
  branch <- branches[i]
  char_hist$maps[[branch]] <- char_hist$maps[[branch]] * bl_ratio[i]
}
plot(char_hist)
write.simmap(char_hist, "data/artiodactyla_sim_character_history.tree")
