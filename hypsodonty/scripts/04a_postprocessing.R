library(ape)
library(phytools)
library(tidyverse)
source("../utility/functions.R")

tree <- read.tree("data/artiodactyla_rescaled.tree")
data <- read.table("output/sdou_joint/charhist.log", header = TRUE)
simmap_to_ancStates(input_simmap = data$char_hist,
                    output_path = "output/sdou_joint/anc_states.log",
                    tree = tree)