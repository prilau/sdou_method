library(ape)
library(phytools)
library(pracma)
library(tidyverse)
source("../utility/functions.R")


# read tree
tree <- read.tree("data/artiodactyla_rescaled.tree")
reps       = 1000

pars <- tibble(alpha_1=NA, alpha_2=NA, alpha_3=NA,
               theta_1=NA, theta_2=NA, theta_3=NA,
               sigma2_1=NA, sigma2_2=NA, sigma2_3=NA,
               .rows = 1000)

set.seed(NULL)
set.seed(2048)
Q = matrix(0.5, 3, 3)
diag(Q) = -1
rownames(Q) = colnames(Q) = 0:2
tree_length = sum(tree$edge.length)
rate = 5 / tree_length # expected changes on the tree = 5

history = sim.history(tree, rate * Q, nsim=1000, message=FALSE)

file.create("/data/charhist.trees", showWarnings = FALSE)

bar = txtProgressBar(style=3, width=40)
for(i in 1:reps) {
  
  pars[i,] <- as.list(c( runif(3, 0.005, 0.05), runif(3, -5, 5), runif(3, 0.01, 10) ))
  
  write.simmap(history[[i]],
               file = paste0("data/charhist_temp.trees"),
               map.order = "right-to-left")
  outFile <- file("data/charhist.trees", "a")
  x <- readLines("data/charhist_temp.trees")
  writeLines(x, outFile)
  close(outFile)
 
  # increment the progress bar
  setTxtProgressBar(bar, i / reps)
  
}
file.remove("data/charhist_temp.trees")

set.seed(NULL)

write.csv(pars, "data/parameters.csv", row.names = FALSE)
