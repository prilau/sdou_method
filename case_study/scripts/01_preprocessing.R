library(ape)
library(slouch)
library(tidyverse)

data("artiodactyla")
data("neocortex")

artiodactyla$node.label <- NULL

artiodactyla$tip.label[11] <- "Connochaetes_taurinus"
artiodactyla$tip.label[12] <- "Eudorcas_rufifrons"
artiodactyla$tip.label[26] <- "Philantomba_monticola"
artiodactyla$tip.label[27] <- "Raphicerus_campestris"
artiodactyla$tip.label[30] <- "Rupicapra_rupicapra"
artiodactyla$tip.label[38] <- "Tragelaphus_scriptus"
artiodactyla$tip.label[42] <- "Giraffa_camelopardalis_angolensis"


write.tree(artiodactyla, "data/artiodactyla.tree")



neocortex$species[11] <- "Connochaetes_taurinus"
neocortex$species[12] <- "Eudorcas_rufifrons"
neocortex$species[14] <- "Giraffa_camelopardalis_angolensis"
neocortex$species[29] <- "Philantomba_monticola"
neocortex$species[30] <- "Raphicerus_campestris"
neocortex$species[33] <- "Rupicapra_rupicapra"
neocortex$species[41] <- "Tragelaphus_scriptus"

neocortex <- neocortex %>% mutate(diet = ifelse(diet=="Br", 0, ifelse(diet=="MF", 1, 2)))

disc_list <- list()
cont_list <- list()

for(i in 1:length(neocortex$species))
{
  sp <- neocortex$species[i]
  cont_list[[sp]] <- neocortex$brain_mass_g_log_mean[i]
  disc_list[[sp]] <- neocortex$diet[i]
}

write.nexus.data(cont_list, "data/artiodactyla_Continuous.nex", format="continuous")
write.nexus.data(disc_list, "data/artiodactyla_Discrete.nex", format="standard")
