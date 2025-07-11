library(ape)
library(phytools)
library(tidyverse)
source("../utility/functions.R")

t <- read.tree("data/artiodactyla.tree")

##### long runs
data <- read.table("output/joint_SDOU_validation/long_charhist_run_1.log", header = TRUE)
simmap_to_ancStates(input_simmap = data$char_hist,
                    output_path = "output/joint_SDOU_validation/long_anc_states_run_1.log",
                    tree = t)

data <- read.table("output/joint_SDOU_validation/long_charhist_run_2.log", header = TRUE)
simmap_to_ancStates(input_simmap = data$char_hist,
                    output_path = "output/joint_SDOU_validation/long_anc_states_run_2.log",
                    tree = t)


data <- read.table("output/joint_SDOU_validation/long_charhist_run_3.log", header = TRUE)
simmap_to_ancStates(input_simmap = data$char_hist,
                    output_path = "output/joint_SDOU_validation/long_anc_states_run_3.log",
                    tree = t)


data <- read.table("output/joint_SDOU_validation/long_charhist_run_4.log", header = TRUE)
simmap_to_ancStates(input_simmap = data$char_hist,
                    output_path = "output/joint_SDOU_validation/long_anc_states_run_4.log",
                    tree = t)


##### large and small alphas
data <- read.table("output/joint_SDOU_validation/largeAlpha_charhist.log", header = TRUE)
simmap_to_ancStates(input_simmap = data$char_hist,
                    output_path = "output/joint_SDOU_validation/largeAlpha_anc_states.log",
                    tree = t)

data <- read.table("output/joint_SDOU_validation/smallAlpha_charhist.log", header = TRUE)
simmap_to_ancStates(input_simmap = data$char_hist,
                    output_path = "output/joint_SDOU_validation/smallAlpha_anc_states.log",
                    tree = t)


####### simulate continuous characters with reconstructed character history
data <- read.table("output/joint_SDOU_validation/simDiscOnly_charhist.log", header = TRUE)
simmap_to_ancStates(input_simmap = data$char_hist,
                    output_path = "output/joint_SDOU_validation/simDiscOnly_anc_states.log",
                    tree = t)

data <- read.table("output/joint_SDOU_validation/simDiscCont_charhist.log", header = TRUE)
simmap_to_ancStates(input_simmap = data$char_hist,
                    output_path = "output/joint_SDOU_validation/simDiscCont_anc_states.log",
                    tree = t)