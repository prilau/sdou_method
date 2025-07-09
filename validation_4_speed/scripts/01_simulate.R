library(ape)
library(phytools)
library(pracma)
library(tidyverse)
library(TESS)
source("../utility//functions.R")


# simulate trees of different sizes
num_tips   = c(1e2, 2e2, 5e2, 1e3, 5e3, 1e4, 1e5)
reps       = 10

grid = expand.grid(tree=1:reps, num_tips=num_tips,
                   stringsAsFactors=FALSE)

# simulate trees
set.seed(168)
bar = txtProgressBar(style=3, width=40)
for(i in 1:nrow(grid)) {
  
  this_row = grid[i,]
  this_num_tip      = this_row[[2]]
  this_tree       = this_row[[1]]
  
  # create the directories if necessary
  this_dir = paste0("data/n_", this_num_tip, "/trees")
  if ( !dir.exists(this_dir) ) {
    dir.create(this_dir, recursive=TRUE, showWarnings=FALSE)
  }
  
  # simulate the tree
  tree = ladderize(tess.sim.taxa(1, this_num_tip, 10, 1, 0.5)[[1]])
  
  # rescale the tree
  tree$edge.length = tree$edge.length / max(branching.times(tree))
  
  # write the tree
  write.tree(tree, file=paste0(this_dir,"/t", this_tree,".tree"))
  
  # increment the progress bar
  setTxtProgressBar(bar, i / nrow(grid))
  
}

# simulate discrete character histories
grid$expected_num_changes <- grid$num_tips / 20

Q = matrix(1, 2, 2)
diag(Q) = -1
rownames(Q) = colnames(Q) = 0:1

bar = txtProgressBar(style=3, width=40)
for(i in 1:nrow(grid)) {
  #if(grid[i,1]!=1e+05) next

  this_row = grid[i,]
  this_num_tip      = this_row[[2]]
  this_tree         = this_row[[1]]
  this_num_change   = this_row[[3]]
  
  this_dir = paste0("data/n_", this_num_tip)
  
  tree <- read.tree(paste0(this_dir, "/trees/t", this_tree, ".tree"))
  cat("successfully read tree.\n")
  
  tree_length = sum(tree$edge.length)
  rate = this_num_change / tree_length
  
  
  cat("simulating character history.\n")
  history = sim.history(tree, rate * Q, nsim=10, message=FALSE)
  for (j in 1:10){
    write.simmap(history[[j]], file = paste0(this_dir, "/character_histories/t", this_tree, "_charhist_", j, ".tree"))
  }
  
  setTxtProgressBar(bar, i / nrow(grid))

}


# simulate continuous traits: one for each tree size
for(i in num_tips) {
  this_dir = paste0("data/n_", i)
  cont <- rnorm(i, mean=0, sd=4)
  cont_list <- list()
  for (j in 1:i){
    tip <- paste0("t", j)
    cont_list[[tip]] <- cont[j]
  }
  write.nexus.data(cont_list, paste0(this_dir, "/Continuous.nex"), format = "continuous")
}

set.seed(NULL)