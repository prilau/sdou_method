library(ape)
library(phytools)
library(tidyverse)
source("../utility/functions.R")

t <- read.tree("data/artiodactyla.tree")
data <- read.table("output/joint_SDOU/charhist.log", header = TRUE)
simmap_to_ancStates(input_simmap = data$char_hist,
                    output_path = "output/joint_SDOU/anc_states.log",
                    tree = t)

# for joint_SDOU_validation/
simmap_to_stochmap(input_path = "output/joint_SDOU/charhist.log",
                   output_path = "output/joint_SDOU/charhist_cleanedup.log",
                   tree = t)
