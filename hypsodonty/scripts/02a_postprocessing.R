library(ape)
library(phytools)
library(tidyverse)
source("../utility/functions.R")

tree <- read.tree("data/artiodactyla_rescaled.tree")
data <- read.table("output/ase_DA/charhist.log", header = TRUE)
simmap_to_ancStates(input_simmap = data$char_hist,
                    output_path = "output/ase_DA/anc_states.log",
                    tree = tree)


