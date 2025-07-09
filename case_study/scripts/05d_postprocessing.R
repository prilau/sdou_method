library(ape)
library(phytools)
library(tidyverse)
source("../utility/functions.R")

t <- read.tree("data/artiodactyla.tree")
data <- read.table("output/joint_SDOU/charhist.log", header = TRUE)
simmap_to_ancStates(input_simmap = data$char_hist,
                    output_path = "output/joint_SDOU/anc_states.log",
                    tree = t)


data <- read.table("output/data_augmentation/charhist.log", header = TRUE)
simmap_to_ancStates(input_simmap = data$char_hist,
                    output_path = "output/data_augmentation/anc_states.log",
                    tree = t)



##### long runs
data <- read.table("output/joint_SDOU/long_charhist_run_1.log", header = TRUE)
simmap_to_ancStates(input_simmap = data$char_hist,
                    output_path = "output/joint_SDOU/long_anc_states_run_1.log",
                    tree = t)

data <- read.table("output/joint_SDOU/long_charhist_run_2.log", header = TRUE)
simmap_to_ancStates(input_simmap = data$char_hist,
                    output_path = "output/joint_SDOU/long_anc_states_run_2.log",
                    tree = t)


data <- read.table("output/joint_SDOU/long_charhist_run_3.log", header = TRUE)
simmap_to_ancStates(input_simmap = data$char_hist,
                    output_path = "output/joint_SDOU/long_anc_states_run_3.log",
                    tree = t)


data <- read.table("output/joint_SDOU/long_charhist_run_4.log", header = TRUE)
simmap_to_ancStates(input_simmap = data$char_hist,
                    output_path = "output/joint_SDOU/long_anc_states_run_4.log",
                    tree = t)


##### large and small alphas
data <- read.table("output/joint_SDOU/largeAlpha_charhist.log", header = TRUE)
simmap_to_ancStates(input_simmap = data$char_hist,
                    output_path = "output/joint_SDOU/largeAlpha_anc_states.log",
                    tree = t)

data <- read.table("output/joint_SDOU/smallAlpha_charhist.log", header = TRUE)
simmap_to_ancStates(input_simmap = data$char_hist,
                    output_path = "output/joint_SDOU/smallAlpha_anc_states.log",
                    tree = t)
