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
                            state_labels=c("0"="Browsers", "2"="Grazers", "1"="Mixed feeders"))
ase_da <- processAncStates("output/ase_DA/anc_states_marginal_run_1.log",
                           state_labels=c("0"="Browsers", "2"="Grazers", "1"="Mixed feeders"))
ase_joint <- processAncStates("output/sdou_joint/anc_states_marginal_run_1.log",
                              state_labels=c("0"="Browsers", "2"="Grazers", "1"="Mixed feeders"))




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


df_scm_joint <- full_join(df_scm, df_joint, by = "node")
df_scm_joint <- df_scm_joint %>% mutate(joint_pp = as.numeric(ifelse(joint_anc_state_1==scm_anc_state_1, joint_anc_state_1_pp,
                                                          ifelse(joint_anc_state_2==scm_anc_state_1, joint_anc_state_2_pp,
                                                                 joint_anc_state_3_pp))),
                                        scm_pp = as.numeric(scm_anc_state_1_pp),
                                        same_MAP_state = ifelse(joint_anc_state_1==scm_anc_state_1, "Y", "N"),
                                        same_MAP_state = as.factor(same_MAP_state),
                                        diff_pp = joint_pp-scm_pp,
                                        sig_diff = as.factor(ifelse(abs(diff_pp) < 0.05, "N", "Y")))



p_scm_joint <- df_scm_joint %>%
  ggplot() +
  geom_abline(intercept = 0, slope = 1, linetype="dashed") +
  geom_point(aes(x=scm_pp, y=joint_pp, colour = sig_diff), alpha=0.7) +
  theme_classic() +
  scale_color_manual(values = c("N"="black", "Y"="darkgreen"), labels=c("No", "yes"), name="Significant difference") +
  #scale_shape_manual(values=c("Y"=16,"N"=17), labels=c("same", "different"), name="MAP state") +
  xlab("Stochastic character mapping") +
  ylab("Joint inference") +
  coord_cartesian(xlim=c(0.3, 1), ylim=c(0.3,1))


p_scm_joint

ggsave("figures/map_p_scm_da.pdf", p_scm_joint, width = 5, height=3, units = "in")





df_compare$da_pp  <- as.numeric(df_da$anc_state_1_pp)
df_compare$joint_pp <- as.numeric(df_joint$anc_state_1_pp)
df_compare$diff_scm_da   <- ifelse(abs(df_compare$scm_pp-df_compare$da_pp) > 0.025, "Y", "N")
df_compare$diff_scm_joint <- ifelse(abs(df_compare$scm_pp-df_compare$joint_pp) > 0.025, "Y", "N")




p_joint_da <- df_compare %>%
  ggplot() +
  geom_abline(intercept = 0, slope = 1, linetype="dashed") +
  geom_point(aes(x=da_pp, y=joint_pp, colour = diff_scm_joint), alpha=0.8) +
  theme_classic() +
  scale_x_continuous(breaks=c(0.6, 0.7, 0.8, 0.9, 1)) +
  scale_y_continuous(breaks=c(0.6, 0.7, 0.8, 0.9, 1)) +
  scale_color_manual(values = c("Y"="darkred", "N"="black")) +
  #scale_shape_manual(values = c("Y"=16, "N"=1))
  #xlab("Stochastic mapping") +
  #ylab(TeX("SD-OU (estimated $\\alpha$)")) +
  theme(legend.position = "none",
        axis.title = element_blank()) +
  coord_cartesian(xlim=c(0.6, 1), ylim=c(0.6,1))

p_da_scm <- df_compare %>%
  ggplot() +
  geom_abline(intercept = 0, slope = 1, linetype="dashed") +
  geom_point(aes(x=scm_pp, y=da_pp, colour = diff_scm_da), alpha=0.8) +
  theme_classic() +
  scale_x_continuous(breaks=c(0.6, 0.7, 0.8, 0.9, 1)) +
  scale_y_continuous(breaks=c(0.6, 0.7, 0.8, 0.9, 1)) +
  scale_color_manual(values = c("Y"="darkred", "N"="black")) +
  #scale_shape_manual(values = c("Y"=16, "N"=1))
  #xlab("Stochastic mapping") +
  #ylab("Data augmentation") +
  theme(legend.position = "none",
        axis.title = element_blank()) +
  coord_cartesian(xlim=c(0.6, 1), ylim=c(0.6,1))

#p_compare_largeAlpha <- df_compare %>%
#  ggplot() +
#  geom_abline(intercept = 0, slope = 1, linetype="dashed") +
#  geom_point(aes(x=scm_pp, y=largeAlpha_pp, colour = diff_scm_largeAlpha), alpha=0.8) +
#  theme_classic() +
#  scale_x_continuous(breaks=c(0.6, 0.7, 0.8, 0.9, 1)) +
#  scale_y_continuous(breaks=c(0.6, 0.7, 0.8, 0.9, 1)) +
#  scale_color_manual(values = c("Y"="darkred", "N"="black")) +
#  #scale_shape_manual(values = c("Y"=16, "N"=1))
#  #xlab("Stochastic mapping") +
#  #ylab(TeX("SD-OU (large $\\alpha$)")) +
#  theme(legend.position = "none",
#        axis.title = element_blank()) +
#  coord_cartesian(xlim=c(0.6, 1), ylim=c(0.6,1))
#
#p_compare_smallAlpha <- df_compare %>%
#  ggplot() +
#  geom_abline(intercept = 0, slope = 1, linetype="dashed") +
#  geom_point(aes(x=scm_pp, y=smallAlpha_pp, colour = diff_scm_smallAlpha), alpha=0.8) +
#  theme_classic() +
#  scale_x_continuous(breaks=c(0.6, 0.7, 0.8, 0.9, 1)) +
#  scale_y_continuous(breaks=c(0.6, 0.7, 0.8, 0.9, 1)) +
#  scale_color_manual(values = c("Y"="darkred", "N"="black")) +
#  #scale_shape_manual(values = c("Y"=16, "N"=1))
#  #xlab("Stochastic mapping") +
#  #ylab(TeX("SD-OU (small $\\alpha$)")) +
#  theme(legend.position = "none",
#        axis.title = element_blank()) +
#  coord_cartesian(xlim=c(0.6, 1), ylim=c(0.6,1))



legend <- get_legend2(p_compare_smallAlpha + theme(legend.position = "right",
                                                   legend.title = element_blank(),
                                         legend.box.margin = margin(0, 0, 0, 12)) +
                        scale_fill_manual(labels = c(TeX("\\Delta pp \\leq 0.05"), TeX("\\Delta pp > 0.05")), values = c("black", "darkred")) +
                        scale_color_manual(labels = c(TeX("\\Delta pp \\leq 0.05"), TeX("\\Delta pp > 0.05")), values = c("black", "darkred")) +
                        guides(color = guide_legend(override.aes = list(alpha = 0.8)),
                               fill = guide_legend(override.aes = list(alpha = 0.8))))

p_first_row <- plot_grid(p0g, p0w2, p0g,
                       rel_widths=c(0.98, 0.08, 0.98*3), ncol=3) +
  draw_plot_label(label=c("Discrete only",
                          "Discrete + Continuous"),
                  x=c(0.49/4,
                      (0.49*5+0.08)/4),
                  y=0.5,
                  hjust=.5, vjust=.5, size=10)

p_second_row <- plot_grid(p0w, p0w2, p0w, p0w, p0w,
                          rel_widths=c(0.98, 0.08, 0.98, 0.98, 0.98), ncol=5) +
  draw_plot_label(label=c("Data augmentation",
                          "SD-OU (small alpha)",
                          "SD-OU (est. alpha)",
                          "SD-OU (large alpha)"),
                  x=c(0.49/4,
                      (0.49*3+0.08)/4,
                      (0.49*5+0.08)/4,
                      (0.49*7+0.08)/4),
                  y=0.5,
                  hjust=.5, vjust=.5, size=8)
p_third_row <- plot_grid(p_compare_da, p0w2, p_compare_smallAlpha,
                           p_compare_joint, p_compare_largeAlpha,
                           rel_widths=c(0.98, 0.08, 0.98, 0.98, 0.98), ncol=5)
p_first_col <- plot_grid(p_first_row, p_second_row, p_third_row,
                         rel_heights=c(1,1,4), ncol=1)
p_second_col <- plot_grid(p0w2, legend,
                          rel_heights=c(2,4), ncol=1)
p_compare_scm <- plot_grid(p_first_col, p_second_col,
                           rel_widths=c(5,1), ncol=2)


ggsave("figures/case_study_node_posterior_cf_scm.pdf", p_compare_scm, width = 7.6, height = 2.65, units = "in")



p_compare_joint_smallAlpha <- df_compare %>%
  ggplot() +
  geom_abline(intercept = 0, slope = 1, linetype="dashed") +
  geom_point(aes(x=joint_pp, y=smallAlpha_pp, colour = diff_joint_smallAlpha), alpha=0.8) +
  theme_classic() +
  scale_x_continuous(breaks=c(0.6, 0.7, 0.8, 0.9, 1)) +
  scale_y_continuous(breaks=c(0.6, 0.7, 0.8, 0.9, 1)) +
  scale_color_manual(values = c("Y"="darkred", "N"="black")) +
  #scale_shape_manual(values = c("Y"=16, "N"=1))
  #xlab(TeX("SD-OU (estimated $\\alpha$)")) +
  #ylab(TeX("SD-OU (small $\\alpha$)")) +
  theme(legend.position = "none",
        axis.title = element_blank()) +
  coord_cartesian(xlim=c(0.6, 1), ylim=c(0.6,1))

p_compare_joint_largeAlpha <- df_compare %>%
  ggplot() +
  geom_abline(intercept = 0, slope = 1, linetype="dashed") +
  geom_point(aes(x=joint_pp, y=largeAlpha_pp, colour = diff_joint_largeAlpha), alpha=0.8) +
  theme_classic() +
  scale_x_continuous(breaks=c(0.6, 0.7, 0.8, 0.9, 1)) +
  scale_y_continuous(breaks=c(0.6, 0.7, 0.8, 0.9, 1)) +
  scale_color_manual(values = c("Y"="darkred", "N"="black")) +
  #scale_shape_manual(values = c("Y"=16, "N"=1))
  #xlab(TeX("SD-OU (estimated $\\alpha$)")) +
  #ylab(TeX("SD-OU (large $\\alpha$)")) +
  theme(legend.position = "none",
        axis.title = element_blank()) +
  coord_cartesian(xlim=c(0.6, 1), ylim=c(0.6,1))



p_first_row <- plot_grid(p0w, p0w, ncol=2) +
  draw_plot_label(label=c("SD-OU (small alpha)",
                          "SD-OU (large alpha)"),
                  x=c(0.25, 0.75),
                  y=0.5,
                  hjust=.5, vjust=.5, size=8)

p_second_row <- plot_grid(p_compare_joint_smallAlpha, p_compare_joint_largeAlpha, ncol=2)


p_first_col <- plot_grid(p_first_row, p_second_row,
                           rel_heights=c(1,4), ncol=1)
p_second_col <- plot_grid(p0w2, legend,
                          rel_heights=c(1,4), ncol=1)
p_compare_joint <- plot_grid(p_first_col, p_second_col,
                             rel_widths=c(3.1,1), ncol=2)

ggsave("figures/case_study_node_posterior_cf_joint.pdf", p_compare_joint, width = 5, height = 2.5, units = "in")

