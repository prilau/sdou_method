library(ggplot2)
library(tidyverse)

df <- read.csv("output/likelihoods.csv")

set.seed(3721)
x <- sample(1:1000, size=25, replace=FALSE)
df_small <- df[x,]

df_small <- df_small %>%
  arrange(rb) %>%
  mutate(rep=1:n()) %>% 
  pivot_longer(cols=c("rb", "r_pruning", "r_vcv"),
               names_to = "algorithm", values_to = "likelihood")
df_small <- df_small %>% 
  mutate(algorithm=ifelse(algorithm=="rb", "RevBayes pruning",
                          ifelse(algorithm=="r_pruning", "R pruning", "R MVN")))
p <- ggplot() +
  geom_point(data=df_small %>% filter(algorithm == "R MVN"),
             aes(x=rep, y=likelihood), size = 3.5, shape=0, color="#EC7014", alpha=0.6) +
  geom_point(data=df_small %>% filter(algorithm == "R pruning"),
             aes(x=rep, y=likelihood), size = 3, shape=16, color="#FEC44F", alpha=0.6) +
  geom_point(data=df_small %>% filter(algorithm == "RevBayes pruning"),
             aes(x=rep, y=likelihood), size = 2, shape=3, color="#993404", alpha=0.6) +
  theme_classic() +
  theme(axis.title = element_text(size=16),
        axis.text = element_text(size=12)) +
  ylab("Likelihood") +
  xlab("Character history replicate")


ggsave("figures/likelihood_comparison.pdf", p, width=4.5, height=4, unit="in")
