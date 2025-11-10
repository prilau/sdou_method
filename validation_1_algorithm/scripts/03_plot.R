library(ggplot2)
library(tidyverse)

rb_liks <- read.csv("output/likelihoods_rb.csv")
rpruning_liks <- read.csv("output/likelihoods_rpruning.csv")
rvcv_liks <- read.csv("output/likelihoods_rvcv.csv")

liks <- rb_liks
liks$rpruning_lik <- rpruning_liks$r_pruning
liks$rvcv_lik <- rvcv_liks$r_vcv

set.seed(2048)
x1 <- sample(1:20, size=5, replace=FALSE)
x2 <- sample(1:100, size=5, replace=FALSE)
liks_small <- liks[which(liks$nrep %in% x1 & liks$charHist %in% x2),]

liks_small <- liks_small %>%
  arrange(rb_lik) %>%
  mutate(rep=1:n()) %>% 
  pivot_longer(cols=c("rb_lik", "rpruning_lik", "rvcv_lik"),
               names_to = "algorithm", values_to = "likelihood") %>% 
  mutate(algorithm=ifelse(algorithm=="rb_lik", "RevBayes pruning",
                          ifelse(algorithm=="rpruning_lik", "R pruning", "R VCV")))
p <- ggplot() +
  geom_point(data=liks_small %>% filter(algorithm == "R VCV"),
             aes(x=rep, y=likelihood), size = 3.5, shape=0, color="#EC7014", alpha=0.6) +
  geom_point(data=liks_small %>% filter(algorithm == "R pruning"),
             aes(x=rep, y=likelihood), size = 3, shape=16, color="#FEC44F", alpha=0.6) +
  geom_point(data=liks_small %>% filter(algorithm == "RevBayes pruning"),
             aes(x=rep, y=likelihood), size = 2, shape=3, color="#993404", alpha=0.6) +
  theme_classic() +
  theme(axis.title = element_text(size=16),
        axis.text = element_text(size=12)) +
  ylab("Likelihood") +
  xlab("Character history replicate")



ggsave("figures/likelihood_comparison.pdf", p, width=4.5, height=4, unit="in")
