library(ape)
library(phytools)
library(tidyverse)
source("../utility/functions.R")

t <- read.tree("data/artiodactyla.tree")
data <- read.table("output/data_augmentation/charhist.log", header = TRUE)
simmap_to_ancStates(input_simmap = data$char_hist,
                    output_path = "output/data_augmentation/anc_states.log",
                    tree = t)


