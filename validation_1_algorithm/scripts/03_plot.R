library(ggplot2)
library(tidyverse)

rb_liks <- read.csv("output/likelihoods_rb.csv")
rpruning_liks <- read.csv("output/likelihoods_rpruning.csv")
rvcv_liks <- read.csv("output/likelihoods_rvcv.csv")

liks <- rb_liks
liks <- left_join(liks, rpruning_liks, by=c("nrep", "charHist"))
liks <- left_join(liks, rvcv_liks, by=c("nrep", "charHist"))

set.seed(2048)
x1 <- sample(1:20, size=5, replace=FALSE)
x2 <- sample(1:100, size=5, replace=FALSE)
liks_small <- liks[which(liks$nrep %in% x1 & liks$charHist %in% x2),]

liks_small <- liks_small %>%
  arrange(rb_lik) %>%
  mutate(nrep=1:n()) %>% 
  pivot_longer(cols=c("rb_lik", "r_pruning", "r_vcv"),
               names_to = "algorithm", values_to = "lnP") %>% 
  mutate(algorithm=ifelse(algorithm=="rb_lik", "Pruning (RevBayes)",
                          ifelse(algorithm=="r_pruning", "Pruning (R)", "VCV (R)")))

p <- ggplot() +
  geom_point(data=liks_small %>% filter(algorithm == "VCV (R)"),
             aes(x=nrep, y=lnP), size = 3.5, shape=0, color="#EC7014", alpha=0.6) +
  geom_point(data=liks_small %>% filter(algorithm == "Pruning (R)"),
             aes(x=nrep, y=lnP), size = 3, shape=16, color="#FEC44F", alpha=0.6) +
  geom_point(data=liks_small %>% filter(algorithm == "Pruning (RevBayes)"),
             aes(x=nrep, y=lnP), size = 2, shape=3, color="#993404", alpha=0.6) +
  theme_bw() +
  theme(axis.title = element_text(size=16),
        axis.text.y = element_text(size=12),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  ylab("ln(Probability)") +
  xlab("Character history replicate")
p


ggsave("figures/likelihood_comparison.pdf", p, width=4.5, height=4, unit="in")
