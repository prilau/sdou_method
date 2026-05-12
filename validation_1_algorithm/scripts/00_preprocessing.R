library(tidyverse)

# read tree
cont_char <- read.csv("data/artiodactyla_Continuous.csv")
vy <- var(cont_char$HI)
a <- log(2)
s <- 2 * vy * a
reps       = 20

pars <- tibble(alpha_0=NA, alpha_1=NA, alpha_2=NA,
               theta_0=NA, theta_1=NA, theta_2=NA,
               sigma2_0=NA, sigma2_1=NA, sigma2_2=NA,
               .rows = 20)

set.seed(NULL)
set.seed(2048)


bar = txtProgressBar(style=3, width=40)
for(i in 1:reps) {
  
  pars[i,] <- as.list(round(c( rlnorm(3, log(a), 0.587405), runif(3, 0.5, 8), rlnorm(3, log(s), 0.587405) ), digits = 3))

  # increment the progress bar
  setTxtProgressBar(bar, i / reps)
  
}
set.seed(NULL)


write.csv(pars %>% select(starts_with("alpha")), "data/alphas.csv", row.names = FALSE)
write.csv(pars %>% select(starts_with("theta")), "data/thetas.csv", row.names = FALSE)
write.csv(pars %>% select(starts_with("sigma2")), "data/sigma2s.csv", row.names = FALSE)
