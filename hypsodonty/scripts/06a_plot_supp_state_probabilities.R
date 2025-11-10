library(cowplot)
library(khroma)
library(ggplot2)
library(ggridges)
library(latex2exp)
library(phytools)
library(RevGadgets)
library(tidyverse)
source("../utility/functions.R")

tree <- readTrees("data/artiodactyla_rescaled.tree")
t <- read.tree("data/artiodactyla_rescaled.tree")


# read ancestral states
ase_MAP <- processAncStates("output/ase_SCM/anc_states.tre",
                            state_labels=c("0"="Browsers", "1"="Mixed feeders", "2"="Grazers"))
ase_da <- processAncStates("output/ase_DA/anc_states_marginal.log",
                           state_labels=c("0"="Browsers", "1"="Mixed feeders", "2"="Grazers"))
ase_joint <- processAncStates("output/sdou_joint/anc_states_marginal.log",
                              state_labels=c("0"="Browsers", "1"="Mixed feeders", "2"="Grazers"))




########################################
# x-y plot for MAP state probabilities #
########################################
ntip <- length(t$tip.label)
df_scm   <- ase_MAP@data[-(1:ntip),]
colnames(df_scm) <- c(paste0("scm_", colnames(df_scm)[1:(ncol(df_scm)-1)]), "node")

df_da <- ase_da@data[-(1:ntip),]
colnames(df_da) <- c(paste0("da_", colnames(df_da)[1:(ncol(df_da)-1)]), "node")

df_joint <- ase_joint@data[-(1:ntip),]
colnames(df_joint) <- c(paste0("joint_", colnames(df_joint)[1:(ncol(df_joint)-1)]), "node")


df_scm_joint1 <- full_join(df_scm, df_joint, by = "node")
df_scm_joint1 <- df_scm_joint1 %>% mutate(joint_pp = as.numeric(ifelse(joint_anc_state_1==scm_anc_state_1, joint_anc_state_1_pp,
                                                          ifelse(joint_anc_state_2==scm_anc_state_1, joint_anc_state_2_pp,
                                                                 joint_anc_state_3_pp))),
                                        scm_pp = as.numeric(scm_anc_state_1_pp),
                                        same_state = ifelse(joint_anc_state_1==scm_anc_state_1, "Y", "N"),
                                        same_state = as.factor(same_state),
                                        diff_pp = joint_pp-scm_pp,
                                        sig_diff = as.factor(ifelse(abs(diff_pp) < 0.05, "N", "Y")))



abline <- tibble(x=c(0,1), y=c(0,1))
  
p_scm_joint <- df_scm_joint %>%
  ggplot() +
  geom_ribbon(data=abline, aes(x=x, y=y, ymin = y - 0.05, ymax = y + 0.05), fill = "grey70", alpha=0.5) +
  geom_line(data=abline, aes(x=x, y=y), linetype="dashed") +
  geom_point(aes(x=scm_pp, y=joint_pp, colour = sig_diff), alpha=0.7, size=0.8) +
  theme_classic() +
  scale_color_manual(values = c("N"="black", "Y"="darkred")) +
  theme(legend.position = "none",
        plot.background = element_rect(colour = "black"),
        plot.title = element_text(hjust=0.5)) +
  xlab("Stochastic character mapping") +
  ylab("Joint inference") +
  #coord_cartesian(xlim=c(0.3, 1), ylim=c(0.3,1)) +
  ggtitle("Posterior node state probability")

p_scm_joint


df_scm_da <- full_join(df_scm, df_da, by = "node")
df_scm_da <- df_scm_da %>% mutate(da_pp = as.numeric(ifelse(da_anc_state_1==scm_anc_state_1, da_anc_state_1_pp,
                                                                     ifelse(da_anc_state_2==scm_anc_state_1, da_anc_state_2_pp,
                                                                            da_anc_state_3_pp))),
                                        scm_pp = as.numeric(scm_anc_state_1_pp),
                                        same_MAP_state = ifelse(da_anc_state_1==scm_anc_state_1, "Y", "N"),
                                        same_MAP_state = as.factor(same_MAP_state),
                                        diff_pp = da_pp-scm_pp,
                                        sig_diff = as.factor(ifelse(abs(diff_pp) < 0.05, "N", "Y")))



p_scm_da <- df_scm_da %>%
  ggplot() +
  geom_ribbon(data=abline, aes(x=x, y=y, ymin = y - 0.05, ymax = y + 0.05), fill = "grey70", alpha=0.5) +
  geom_line(data=abline, aes(x=x, y=y), linetype="dashed") +
  geom_point(aes(x=scm_pp, y=da_pp, colour = sig_diff), alpha=0.7, size=0.8) +
  theme_classic() +
  scale_color_manual(values = c("N"="black", "Y"="darkgreen"), labels=c("No", "yes"), name="Significant difference") +
  theme(legend.position = "none") +
  xlab("Stochastic character mapping") +
  ylab("Data augmentation") +
  coord_cartesian(xlim=c(0.3, 1), ylim=c(0.3,1))


p_scm_da


p_all <- plot_grid(p_scm_da, p_scm_joint)

ggsave("figures/map_pp.pdf", p_all, width = 4.5, height = 2.5, unit = "in")

