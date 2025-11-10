library(ape)
library(phytools)
library(pracma)
library(tidyverse)
library(TESS)
source("../utility/functions.R")


# simulate trees of different sizes
num_tips   = c(50, 100, 150)
disc_states = c(2,3,4)
reps       = 100

grid = expand.grid(tree=1:reps, num_tips=num_tips, disc_states=disc_states,
                   stringsAsFactors=FALSE)

# simulate trees
set.seed(168)
bar = txtProgressBar(style=3, width=40)
for(i in 1:nrow(grid)) {
  
  this_row = grid[i,]
  this_num_tip      = this_row[[2]]
  this_tree       = this_row[[1]]
  this_disc_state       = this_row[[3]]
  
  # create the directories if necessary
  this_dir = paste0("data/n_", this_num_tip, "/k_", this_disc_state, "/trees")
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
grid <- grid %>% mutate(exp_num_change = ifelse(num_tips==50, 8, ifelse(num_tips==100, 16, 24)))

Q2 = matrix(1, 2, 2)
diag(Q2) = -1
rownames(Q2) = colnames(Q2) = 0:1 + 1

Q3 = matrix(0.5, 3, 3)
diag(Q3) = -1
rownames(Q3) = colnames(Q3) = 0:2 + 1

Q4 = matrix(1/3, 4, 4)
diag(Q4) = -1
rownames(Q4) = colnames(Q4) = 0:3 + 1

rates <- c()

bar = txtProgressBar(style=3, width=40)
for(i in 1:nrow(grid)) {
  #if(grid[i,1]!=1e+05) next

  this_row = grid[i,]
  this_tree         = this_row[[1]]
  this_num_tip      = this_row[[2]]
  this_disc_state       = this_row[[3]]
  this_num_change   = this_row[[4]]
  
  this_dir = paste0("data/n_", this_num_tip, "/k_", this_disc_state)
  
  tree <- read.tree(paste0(this_dir, "/trees/t", this_tree, ".tree"))
  cat("\nSuccessfully read tree", this_tree, "with", this_num_tip, "taxa.\n")
  
  tree_length = sum(tree$edge.length)
  rate = this_num_change / tree_length

  if (this_disc_state == 2){
    Q <- Q2
  } else if (this_disc_state == 3){
    Q <- Q3
  } else { Q <- Q4 }
  
  history = sim.history(tree, rate * Q, nsim=1, message=FALSE)
  
  if (this_disc_state == 2){
    while (!(mean(history$states == "1") > 0.10 & mean(history$states == "2") > 0.10) ) {
      history = sim.history(tree, rate * Q, nsim=1, message=FALSE)
    }
  } else if (this_disc_state == 3){
    while (!(mean(history$states == "1") > 0.10 & mean(history$states == "2") > 0.10 & mean(history$states == "3") > 0.10) ) {
      history = sim.history(tree, rate * Q, nsim=1, message=FALSE)
    }
  } else {
    while (!(mean(history$states == "1") > 0.10 & mean(history$states == "2") > 0.10 & mean(history$states == "3") > 0.10 & mean(history$states == "4") > 0.10) ) {
      history = sim.history(tree, rate * Q, nsim=1, message=FALSE)
    }
  }
  
  
  if (this_disc_state == 2){
    maps = history$mapped.edge[,c("1","2")]
  } else if (this_disc_state == 3){
    maps = history$mapped.edge[,c("1","2","3")]
  } else {
    maps = history$mapped.edge[,c("1","2","3","4")]
  }
  
  # save the discrete trait as a nexus file
  writeCharacterData(t(t(history$states)), file=paste0(this_dir, "/discrete_characters/t", this_tree, "_Discrete.nex"), type="Standard")
  
  filepath = paste0(this_dir, "/character_histories/t", this_tree, "_charhist.tree")
  if (!file.exists(filepath)){
    file.create(filepath)
  }
  write.simmap(history,
               file = filepath,
               map.order = "right-to-left")
  
  outFile <- file(paste0(this_dir, "/rates/t", this_tree, "_rate.txt"), "w")
  writeLines(as.character(rate), outFile)
  close(outFile)
  

  setTxtProgressBar(bar, i / nrow(grid))

}


# simulate continuous traits
bar = txtProgressBar(style=3, width=40)
for(i in 1:nrow(grid)) {
  #if(grid[i,1]!=1e+05) next
  
  this_row = grid[i,]
  this_tree         = this_row[[1]]
  this_num_tip      = this_row[[2]]
  this_disc_state       = this_row[[3]]
  this_num_change   = this_row[[4]]
  
  this_dir = paste0("data/n_", this_num_tip, "/k_", this_disc_state)
  
  history <- read.simmap(paste0(this_dir, "/character_histories/t", this_tree, "_charhist.tree"), format = "phylip")
  cat("\nSuccessfully read character history", this_tree, "with", this_num_tip, "taxa.\n")
  
 
  if (this_disc_state == 2){
    alpha <- log(2) / c(0.35, 0.45)
    sigma2 <- c(0.05, 0.07)
    theta <- c(-1, 1)
    names(alpha) <- names(sigma2) <- names(theta) <- c("1", "2")
  } else if (this_disc_state == 3){
    alpha <- log(2) / c(0.3, 0.4, 0.5)
    sigma2 <- c(0.04, 0.06, 0.08)
    theta <- c(-2, 0, 2)
    names(alpha) <- names(sigma2) <- names(theta) <- c("1", "2", "3")
  } else {
    alpha <- log(2) / c(0.25, 0.35, 0.45, 0.55)
    sigma2 <- c(0.03, 0.05, 0.07, 0.09)
    theta <- c(-3, -1, 1, 3)
    names(alpha) <- names(sigma2) <- names(theta) <- c("1", "2", "3", "4")
  }
  
  cont <- simulateContinuous(history, alpha, sigma2, theta)
  write.nexus.data(cont, paste0(this_dir, "/continuous_traits/t", this_tree, "_Continuous.nex"), format = "continuous")
  
  setTxtProgressBar(bar, i / nrow(grid))
  
}

set.seed(NULL)
