library(cowplot)
library(khroma)
library(ggplot2)
library(ggridges)
library(latex2exp)
library(phytools)
library(RevGadgets)
library(tidyverse)
source("../utility/functions.R")

tree <- readTrees("data/artiodactyla.tree")
t <- read.tree("data/artiodactyla.tree")

###############
# plot traces #
###############

seq_map  <- readTrace("output/seq_SDOU/trace_MAP.log", burnin = 0.0)
seq_map[[1]]$model <- "seq-MAP"
seq_r100 <- readTrace(path = paste0("output/seq_SDOU/trace_r100_sampled.log"), burnin = 0.0)
seq_r100[[1]]$model <- "seq-r100"
joint <- readTrace("output/joint_SDOU/trace.log", burnin=0.0)
joint[[1]]$model <- "joint"
joint[[1]] <- joint[[1]] %>%
  select(!starts_with(c("num_changes", "rate", "total", "lambda", "rf"))) %>% 
  mutate(Replicate_ID = NA)
traces <- rbind(
  seq_map[[1]],
  seq_r100[[1]],
  joint[[1]]) %>% 
  mutate(model = factor(model, levels = c("joint", "seq-r100", "seq-MAP")))



colors_sdou <- c("#CC6677", "#44AA99", "#DDCC77")

# alpha
names(colors_sdou) <- c("alpha[1]", "alpha[2]", "alpha[3]")
plot_alpha <- traces %>% 
  pivot_longer(cols=c("alpha[1]", "alpha[2]", "alpha[3]"),
               names_to = "parameter", values_to = "value") %>% 
  mutate(parameter = factor(parameter, levels = c("alpha[1]",
                                                  "alpha[2]",
                                                  "alpha[3]"))) %>% 
  ggplot(aes(x = value, y=model, fill=parameter)) +
  geom_density_ridges(scale=0.9, alpha=0.6, show.legend = FALSE, 
                      ### The only change is here
                      #aes(fill = NULL),
                      bandwidth = 0.01) +
  #scale_color_manual(values = colors_sdou) +   
  scale_fill_manual(values = colors_sdou) + 
  theme_classic() +
  #scale_y_discrete(labels=c("OU" = "SD-OU", "MuSSCRat_1" = "MuSSCRat",
  #                          "SD-BM_0" = "SD-BM", "SD-BM_1"="SD-BM",
  #                          "SD-OU_0"="SD-OU", "SD-OU_1"="SD-OU")) +
  ylab("Posterior density") +
  xlab(TeX("Rate of attraction $\\alpha$")) +
  scale_x_continuous(breaks=c(0, 0.1, 0.2)) +
  coord_cartesian(xlim=c(0, 0.2))


plot_alpha

# sigma2
names(colors_sdou) <- c("sigma2[1]", "sigma2[2]", "sigma2[3]")
plot_sigma2 <- traces %>% 
  pivot_longer(cols=c("sigma2[1]", "sigma2[2]", "sigma2[3]"),
               names_to = "parameter", values_to = "value") %>% 
  mutate(parameter = factor(parameter, levels = c("sigma2[1]",
                                                  "sigma2[2]",
                                                  "sigma2[3]"))) %>% 
  ggplot(aes(x = value, y=model, fill=parameter)) +
  geom_density_ridges(scale=0.9, alpha=0.6, show.legend = FALSE, 
                      ### The only change is here
                      #aes(fill = Model),
                      bandwidth = 0.025) +
  #scale_color_manual(values = colors_sdou) +   
  scale_fill_manual(values = colors_sdou) + 
  theme_classic() +
  scale_x_continuous(breaks=c(0, 0.25, 0.5, 0.75)) +
  ylab("") +
  xlab(TeX("Diffusion variance $\\sigma^2$")) +
  coord_cartesian(xlim=c(0, 0.75))
plot_sigma2

# theta
names(colors_sdou) <- c("theta[1]", "theta[2]", "theta[3]")
plot_theta <- traces %>% 
  pivot_longer(cols=c("theta[1]", "theta[2]", "theta[3]"),
               names_to = "parameter", values_to = "value") %>% 
  mutate(parameter = factor(parameter, levels = c("theta[1]",
                                                  "theta[2]",
                                                  "theta[3]"))) %>% 
  ggplot(aes(x = value, y=model, fill=parameter)) +
  geom_density_ridges(scale=0.9, alpha=0.7, show.legend = FALSE, 
                      ### The only change is here
                      #aes(fill = Model),
                      bandwidth = 0.4) +
  #scale_color_manual(values = colors_sdou) +   
  scale_fill_manual(values = colors_sdou) + 
  theme_classic() +
  #scale_y_discrete(labels=c("OU" = "SD-OU", "MuSSCRat_1" = "MuSSCRat",
  #                          "SD-BM_0" = "SD-BM", "SD-BM_1"="SD-BM",
  #                          "SD-OU_0"="SD-OU", "SD-OU_1"="SD-OU")) +
  ylab("") +
  xlab(TeX("Optimum $\\theta$")) #+
#coord_cartesian(xlim=c(0, 0.8))
plot_theta

plot_dummy <- traces %>% 
  pivot_longer(cols=c("theta[1]", "theta[2]", "theta[3]"),
               names_to = "Diet", values_to = "value") %>% 
  ggplot(aes(x = value, fill=Diet)) +
  geom_density() + 
  scale_fill_manual(values = colors_sdou, labels = c("Browsers",
                                                     "Mixed feeders",
                                                     "Grazers"))

legend <- get_legend2(plot_dummy + theme(legend.position = "right",
                                         legend.box.margin = margin(0, 0, 0, 12)) +
                        guides(fill = guide_legend(override.aes = list(alpha = 0.6))))


plot_all <- cowplot::plot_grid(plot_alpha, plot_sigma2, plot_theta, legend,
                               ncol=4, rel_widths=c(1,1,1,0.65))
plot_all


ggsave("figures/case_study_posterior.pdf", plot_all, width=8, height=3, units="in")




######################
# stochastic mapping #
######################
ase_MAP <- processAncStates("output/ase/anc_states.tre",
                            state_labels=c("0"="Browsers", "2"="Grazers", "1"="Mixed feeders"))
p1 <- plotAncStatesPie(t = ase_MAP,
                       tip_labels = FALSE,
                       #tip_labels_size = 2,
                       #tip_labels_offset = 1,
                       pie_colors = c("Browsers"="#CC6677",
                                      "Mixed feeders"="#44AA99",
                                      "Grazers"="#ddcc77"),
                       tip_pies = TRUE,
                       node_pie_size = 2,
                       tip_pie_size = 2,
                       tree_layout = "rectangular",
                       state_transparency = 0.9,
                       tree_linewidth = 0.75) +
  theme(legend.position = "none")

tb_MAP <- read.table("output/ase/charhist.log", header = TRUE)
simmap_MAP <- read.simmap(text=tb_MAP$simmap, format = "phylip")
processed_MAP <- processStochMaps(tree=tree, simmap = simmap_MAP, states = c("0", "1", "2"))
colnames(processed_MAP)[6] = "Browsers"
colnames(processed_MAP)[8] = "Grazers"
colnames(processed_MAP)[7] = "Mixed feeders"
p2 <- plotStochMaps(tree=tree, maps = processed_MAP, color_by = "MAP",
                    colors = c("Browsers"="#CC6677",
                               "Mixed feeders"="#44aa99",
                               "Grazers"="#ddcc77"),
                    tip_labels = FALSE#,
                    #tip_labels_size = 2,
                    #tip_labels_offset = 1
) +
  theme(legend.position = "none")

#####################
# data augmentation #
#####################
ase_da <- processAncStates("output/data_augmentation/anc_states_marginal.log",
                           state_labels=c("0"="Browsers", "2"="Grazers", "1"="Mixed feeders"))
p3 <- plotAncStatesPie(t = ase_da,
                       tip_labels = FALSE,
                       #tip_labels_size = 2,
                       #tip_labels_offset = 1,
                       pie_colors = c("Browsers"="#CC6677",
                                      "Mixed feeders"="#44AA99",
                                      "Grazers"="#ddcc77"),
                       tip_pies = TRUE,
                       node_pie_size = 2,
                       tip_pie_size = 2,
                       tree_layout = "rectangular",
                       state_transparency = 0.9,
                       tree_linewidth = 0.75) +
  theme(legend.position = "none")

tb_da <- read.table("output/data_augmentation/charhist.log", header = TRUE)
simmap_da <- read.simmap(text=tb_da$char_hist, format = "phylip")
processed_da <- processStochMaps(tree=tree, simmap = simmap_da, states = c("0", "1", "2"))
colnames(processed_da)[6] = "Browsers"
colnames(processed_da)[8] = "Grazers"
colnames(processed_da)[7] = "Mixed feeders"
p4 <- plotStochMaps(tree=tree, maps = processed_da, color_by = "MAP",
                    colors = c("Browsers"="#CC6677",
                               "Mixed feeders"="#44aa99",
                               "Grazers"="#ddcc77"),
                    tip_labels = FALSE#,
                    #tip_labels_size = 2,
                    #tip_labels_offset = 1
) +
  theme(legend.position = "none")


#########
# joint #
#########
ase_joint <- processAncStates("output/joint_SDOU/anc_states_marginal.log",
                              state_labels=c("0"="Browsers", "2"="Grazers", "1"="Mixed feeders"))

p5 <- plotAncStatesPie(t = ase_joint,
                       pie_colors = c("Browsers"="#CC6677",
                                      "Mixed feeders"="#44AA99",
                                      "Grazers"="#ddcc77"),
                       tip_labels = FALSE,
                       tip_pies = TRUE,
                       node_pie_size = 2,
                       tip_pie_size = 2,
                       tree_layout = "rectangular",
                       state_transparency = 0.9,
                       tree_linewidth = 0.75) +
  theme(legend.position = "none")

tb_joint <- read.table("output/joint_SDOU/charhist.log", header = TRUE)
simmap_joint <- read.simmap(text=tb_joint$char_hist, format = "phylip")
processed_joint <- processStochMaps(tree=tree, simmap = simmap_joint, states = c("0", "1", "2"))
colnames(processed_joint)[6] = "Browsers"
colnames(processed_joint)[8] = "Grazers"
colnames(processed_joint)[7] = "Mixed feeders"
p6 <- plotStochMaps(tree=tree, maps = processed_joint, color_by = "MAP",
                    colors = c("Browsers"="#CC6677",
                               "Mixed feeders"="#44aa99",
                               "Grazers"="#ddcc77"),
                    tip_labels = FALSE
) +
  theme(legend.position = "none")


#####
# helper #
##########

p0g <- ggplot() +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "grey92", colour = NA),
    plot.background = element_rect(fill = "grey92", colour = NA)) 

p0w <- ggplot() + theme_bw() + theme(
  panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank(),
  panel.background = element_rect(fill = "white", colour = NA),
  plot.background = element_rect(fill = "white", colour = NA),
  axis.line = element_line(colour = "black")
) 

p0w2 <- ggplot() + theme(
  panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank(),
  panel.background = element_rect(fill = "white", colour = NA),
  plot.background = element_rect(fill = "white", colour = NA),
  axis.line = element_line(colour = "black")
) 


df_tips <- data.frame(xpos = 0, ypos = 1:43*2-2, label=rev(ggtree::get_taxa_name(p6)))

ptips <- ggplot(df_tips, aes(x=xpos, y=ypos, label=label)) + 
  geom_text(size=1.5, hjust=0, fontface = "italic") +
  theme_void() +
  coord_cartesian(xlim=c(0,1))
ptips

#######
# all #
#######
plot_left <- plot_grid(p0w, p0w, p1, p3, p2, p4,
                       rel_heights = c(1,6,6),
                       ncol=2) +
  draw_plot_label(label=c("(a) Stochastic mapping", "(b) Data augmentation"),
                  x=c(0.25, 0.75),
                  y=12.5/13,
                  hjust=.5, vjust=.5, size=10)
plot_left <- plot_grid(p0g, plot_left,
                       ncol = 1, align = 'v',
                       rel_heights = c(1,13)) +
  draw_plot_label(label=c("Discrete only"),
                  x=0.5,
                  y=13.5/14,
                  hjust=.5, vjust=.5, size=12)
plot_mid <- p0w2
plot_right <- plot_grid(p0w, p5, p6,
                        rel_heights = c(1,6,6),
                        ncol=1) +
  draw_plot_label(label=c("(c) Joint inference"),
                  x=0.5,
                  y=12.5/13,
                  hjust=.5, vjust=.5, size=10)
plot_right <- plot_grid(p0g, plot_right,
                        ncol = 1, align = 'v',
                        rel_heights = c(1,13)) +
  draw_plot_label(label=c("Discrete + Continuous"),
                  x=0.5,
                  y=13.5/14,
                  hjust=.5, vjust=.5, size=12)
plot_rightmost <- plot_grid(p0w2, ptips, ptips, p0w2,
                            rel_heights = c(2,6,6,0.08), ncol=1)

plot_maps_all <- cowplot::plot_grid(plot_left, plot_mid, plot_right, plot_rightmost,
                                    rel_widths = c(2,0.01,1,0.8), align = "v",
                                    ncol=4)


ggsave("figures/case_study_maps.pdf", plot_maps_all, width=7.5, height=6, units="in")

########
# long #
########
ase_long <- tb_long <-
  simmap_long <- processed_long <-
  p_ase_long <- p_stochmap_long <- list()

for (i in 1:4){
  ase_long[[i]] <-
    processAncStates(paste0("output/joint_SDOU_validation/long_anc_states_marginal_run_", i, ".log"),
                     state_labels=c("0"="Browsers",
                                    "1"="Mixed feeders",
                                    "2"="Grazers"))
  tb_long[[i]] <- read.table(paste0("output/joint_SDOU_validation/long_charhist_run_", i, ".log"), header = TRUE)
  simmap_long[[i]] <- read.simmap(text=tb_long[[i]]$char_hist, format = "phylip")
  processed_long[[i]] <- processStochMaps(tree=tree, simmap = simmap_long[[i]],
                                          states = c("0", "1", "2"))
  colnames(processed_long[[i]])[6] = "Browsers"
  colnames(processed_long[[i]])[7] = "Mixed feeders"
  colnames(processed_long[[i]])[8] = "Grazers"
  
  p_ase_long[[i]] <- plotAncStatesPie(t = ase_long[[i]],
                                      tip_labels = FALSE,
                                      #tip_labels_size = 2,
                                      #tip_labels_offset = 1,
                                      pie_colors = c("Browsers"="#CC6677",
                                                     "Mixed feeders"="#44AA99",
                                                     "Grazers"="#ddcc77"),
                                      tip_pies = TRUE,
                                      node_pie_size = 2,
                                      tip_pie_size = 2,
                                      tree_layout = "rectangular",
                                      state_transparency = 0.9,
                                      tree_linewidth = 0.75) +
    theme(legend.position = "none")
  
  
  p_stochmap_long[[i]] <- plotStochMaps(tree=tree, maps = processed_long[[i]], color_by = "MAP",
                                        colors = c("Browsers"="#CC6677",
                                                   "Mixed feeders"="#44aa99",
                                                   "Grazers"="#ddcc77"),
                                        tip_labels = FALSE#,
                                        #tip_labels_size = 2,
                                        #tip_labels_offset = 1
  ) +
    theme(legend.position = "none")
}


plot_long_top <- plot_grid(p0w, p0w, p0w, p0w, ncol=4) +
  draw_plot_label(label=c("Long run 1", "Long run 2", "Long run 3", "Long run 4"),
                  x=c(1,3,5,7)/8,
                  y=0.5,
                  hjust=0.5, vjust=.5, size=10)
plot_long_bot <- plot_grid(p_ase_long[[1]], p_ase_long[[2]],
                            p_ase_long[[3]], p_ase_long[[4]],
                            p_stochmap_long[[1]], p_stochmap_long[[2]],
                            p_stochmap_long[[3]], p_stochmap_long[[4]],
                            ncol=4)
plot_long <- plot_grid(plot_long_top, plot_long_bot,
                       rel_heights = c(1,12), ncol=1)

ggsave("figures/case_study_maps_long.pdf", plot_long, width=7.5, height=6, units="in")



################################
# large and small fixed alphas #
################################
ase_largeAlpha <-
  processAncStates(paste0("output/joint_SDOU_validation/largeAlpha_anc_states_marginal.log"),
                   state_labels=c("0"="Browsers",
                                  "1"="Mixed feeders",
                                  "2"="Grazers"))
tb_largeAlpha <- read.table(paste0("output/joint_SDOU_validation/largeAlpha_charhist.log"), header = TRUE)
simmap_largeAlpha <- read.simmap(text=tb_largeAlpha$char_hist, format = "phylip")
processed_largeAlpha <- processStochMaps(tree=tree, simmap = simmap_largeAlpha,
                                         states = c("0", "1", "2"))
colnames(processed_largeAlpha)[6] = "Browsers"
colnames(processed_largeAlpha)[7] = "Mixed feeders"
colnames(processed_largeAlpha)[8] = "Grazers"

p7 <- plotAncStatesPie(t = ase_largeAlpha ,
                       pie_colors = c("Browsers"="#CC6677",
                                      "Mixed feeders"="#44AA99",
                                      "Grazers"="#ddcc77"),
                       tip_labels = FALSE,
                       tip_pies = TRUE,
                       node_pie_size = 2,
                       tip_pie_size = 2,
                       tree_layout = "rectangular",
                       state_transparency = 0.9,
                       tree_linewidth = 0.75) +
  theme(legend.position = "none")
p8 <- plotStochMaps(tree=tree, maps = processed_largeAlpha, color_by = "MAP",
                    colors = c("Browsers"="#CC6677",
                               "Mixed feeders"="#44aa99",
                               "Grazers"="#ddcc77"
                    ),
                    tip_labels = FALSE
) +
  theme(legend.position = "none")


ase_smallAlpha <-
  processAncStates(paste0("output/joint_SDOU_validation/smallAlpha_anc_states_marginal.log"),
                   state_labels=c("0"="Browsers",
                                  "1"="Mixed feeders",
                                  "2"="Grazers"))
tb_smallAlpha <- read.table(paste0("output/joint_SDOU_validation/smallAlpha_charhist.log"), header = TRUE)
simmap_smallAlpha <- read.simmap(text=tb_smallAlpha$char_hist, format = "phylip")
processed_smallAlpha <- processStochMaps(tree=tree, simmap = simmap_smallAlpha,
                                         states = c("0", "1", "2"))
colnames(processed_smallAlpha)[6] = "Browsers"
colnames(processed_smallAlpha)[7] = "Mixed feeders"
colnames(processed_smallAlpha)[8] = "Grazers"

p9 <- plotAncStatesPie(t = ase_smallAlpha ,
                       pie_colors = c("Browsers"="#CC6677",
                                      "Mixed feeders"="#44AA99",
                                      "Grazers"="#ddcc77"),
                       tip_labels = FALSE,
                       tip_pies = TRUE,
                       node_pie_size = 2,
                       tip_pie_size = 2,
                       tree_layout = "rectangular",
                       state_transparency = 0.9,
                       tree_linewidth = 0.75) +
  theme(legend.position = "none")
p10 <- plotStochMaps(tree=tree, maps = processed_smallAlpha, color_by = "MAP",
                     colors = c("Browsers"="#CC6677",
                                "Mixed feeders"="#44aa99",
                                "Grazers"="#ddcc77"
                     ),
                     tip_labels = FALSE
) +
  theme(legend.position = "none")


plot_row1 <- plot_grid(p0g, p0w2, p0g,
                       rel_widths=c(0.98,0.08,2.94), ncol=3) +
  draw_plot_label(label=c("Stochastic mapping",
                          "State-dependent OU model"),
                  x=c(0.49/4, (0.49*5+0.08)/4),
                  y=0.5,
                  hjust=.5, vjust=.5, size=12)
plot_row2 <- plot_grid(p0w, p0w2, p0w, p0w, p0w,
                       rel_widths=c(0.98,0.08,0.98,0.98,0.98), ncol=5) +
  draw_plot_label(label=c("No continuous character",
                          "Fixed, large alpha",
                          "Estimated alpha",
                          "Fixed, small alpha"),
                  x=c(0.49/4,
                      (0.49*3+0.08)/4,
                      (0.49*5+0.08)/4,
                      (0.49*7+0.08)/4),
                  y=0.5,
                  hjust=.5, vjust=.5, size=8)

plot_row34 <- plot_grid(p1, p0w2, p7, p5, p9, p2, p0w2, p8, p6, p10,
                        rel_widths=c(0.98,0.08,0.98,0.98,0.98), ncol=5)
plot_alphas <- plot_grid(plot_row1, plot_row2, plot_row34,
                         rel_heights = c(1,1,12), ncol=1)





plot_alphas
ggsave("figures/case_study_maps_alphas.pdf", plot_alphas, width=7.5, height=6, units="in")



#################################################
# simulate cont char from reconstructed history #
#################################################
#ase_simDiscOnly <-
#  processAncStates(paste0("output/joint_SDOU_validation/simDiscOnly_anc_states_marginal.log"),
#                   state_labels=c("0"="Browsers",
#                                  "1"="Mixed feeders",
#                                  "2"="Grazers"))
#tb_simDiscOnly <- read.table(paste0("output/joint_SDOU_validation/simDiscOnly_charhist.log"), header = TRUE)
#simmap_simDiscOnly <- read.simmap(text=tb_simDiscOnly$char_hist, format = "phylip")
#processed_simDiscOnly <- processStochMaps(tree=tree, simmap = simmap_simDiscOnly,
#                                         states = c("0", "1", "2"))
#colnames(processed_simDiscOnly)[6] = "Browsers"
#colnames(processed_simDiscOnly)[7] = "Mixed feeders"
#colnames(processed_simDiscOnly)[8] = "Grazers"
#
#p11 <- plotAncStatesPie(t = ase_simDiscOnly ,
#                       pie_colors = c("Browsers"="#CC6677",
#                                      "Mixed feeders"="#44AA99",
#                                      "Grazers"="#ddcc77"),
#                       tip_labels = FALSE,
#                       tip_pies = TRUE,
#                       node_pie_size = 2,
#                       tip_pie_size = 2,
#                       tree_layout = "rectangular",
#                       state_transparency = 0.9,
#                       tree_linewidth = 0.75) +
#  theme(legend.position = "none")
#p12 <- plotStochMaps(tree=tree, maps = processed_simDiscOnly, color_by = "MAP",
#                    colors = c("Browsers"="#CC6677",
#                               "Mixed feeders"="#44aa99",
#                               "Grazers"="#ddcc77"
#                    ),
#                    tip_labels = FALSE
#) +
#  theme(legend.position = "none")
#
#
#ase_simDiscCont <-
#  processAncStates(paste0("output/joint_SDOU_validation/simDiscCont_anc_states_marginal.log"),
#                   state_labels=c("0"="Browsers",
#                                  "1"="Mixed feeders",
#                                  "2"="Grazers"))
#tb_simDiscCont <- read.table(paste0("output/joint_SDOU_validation/simDiscCont_charhist.log"), header = TRUE)
#simmap_simDiscCont <- read.simmap(text=tb_simDiscCont$char_hist, format = "phylip")
#processed_simDiscCont <- processStochMaps(tree=tree, simmap = simmap_simDiscCont,
#                                         states = c("0", "1", "2"))
#colnames(processed_simDiscCont)[6] = "Browsers"
#colnames(processed_simDiscCont)[7] = "Mixed feeders"
#colnames(processed_simDiscCont)[8] = "Grazers"
#
#p13 <- plotAncStatesPie(t = ase_simDiscCont ,
#                       pie_colors = c("Browsers"="#CC6677",
#                                      "Mixed feeders"="#44AA99",
#                                      "Grazers"="#ddcc77"),
#                       tip_labels = FALSE,
#                       tip_pies = TRUE,
#                       node_pie_size = 2,
#                       tip_pie_size = 2,
#                       tree_layout = "rectangular",
#                       state_transparency = 0.9,
#                       tree_linewidth = 0.75) +
#  theme(legend.position = "none")
#p14 <- plotStochMaps(tree=tree, maps = processed_simDiscCont, color_by = "MAP",
#                     colors = c("Browsers"="#CC6677",
#                                "Mixed feeders"="#44aa99",
#                                "Grazers"="#ddcc77"
#                     ),
#                     tip_labels = FALSE
#) +
#  theme(legend.position = "none")
#
#
#plot_row1 <- plot_grid(p0g, p0w2, p0g,
#                       rel_widths=c(0.98,0.08,2.94), ncol=3) +
#  draw_plot_label(label=c("Stochastic mapping",
#                          "State-dependent OU model"),
#                  x=c(0.49/4, (0.49*5+0.08)/4),
#                  y=0.5,
#                  hjust=.5, vjust=.5, size=12)
#plot_row2 <- plot_grid(p0w, p0w2, p0w, p0w, p0w,
#                       rel_widths=c(0.98,0.08,0.98,0.98,0.98), ncol=5) +
#  draw_plot_label(label=c("No continuous character",
#                          "Continuous character\nsimulated under MAP history\nfrom stochastic mapping",
#                          "Empirical continuous character",
#                          "Continuous character\nsimulated under MAP history\nfrom the SD-OU model"),
#                  x=c(0.49/4,
#                      (0.49*3+0.08)/4,
#                      (0.49*5+0.08)/4,
#                      (0.49*7+0.08)/4),
#                  y=0.5,
#                  hjust=.5, vjust=.5, size=8)
#
#plot_row34 <- plot_grid(p1, p0w2, p11, p5, p13, p2, p0w2, p12, p6, p14,
#                        rel_widths=c(0.98,0.08,0.98,0.98,0.98), ncol=5)
#plot_sims <- plot_grid(plot_row1, plot_row2, plot_row34,
#                         rel_heights = c(1,1.5,12), ncol=1)
#
#
#
#
#
#plot_sims
#ggsave("figures/case_study_maps_sims.pdf", plot_sims, width=7.5, height=6, units="in")

########################################
# x-y plot for MAP state probabilities #
########################################
ntip <- length(t$tip.label)
df_scm   <- ase_MAP@data[-(1:ntip),]
df_da <- ase_da@data[-(1:ntip),]
df_joint <- ase_joint@data[-(1:ntip),]
df_large_alpha <- ase_largeAlpha@data[-(1:ntip),]
df_small_alpha <- ase_smallAlpha@data[-(1:ntip),]
# check if all MAP states are the same
if (!identical(df_scm$anc_state_1,df_joint$anc_state_1)){
  cat("some MAP states are different! Proceed with plotting is not recommended.")
} else {
  cat("Good to go.")
}





df_compare        <- tibble(.rows=nrow(df_scm))
df_compare$state  <- df_scm$anc_state_1
df_compare$scm_pp <- as.numeric(df_scm$anc_state_1_pp)
df_compare$da_pp  <- as.numeric(df_da$anc_state_1_pp)
df_compare$largeAlpha_pp  <- as.numeric(df_large_alpha$anc_state_1_pp)
df_compare$smallAlpha_pp  <- as.numeric(df_small_alpha$anc_state_1_pp)
df_compare$joint_pp <- as.numeric(df_joint$anc_state_1_pp)
df_compare$diff_scm_da   <- ifelse(abs(df_compare$scm_pp-df_compare$da_pp) > 0.05, "Y", "N")
df_compare$diff_scm_largeAlpha   <- ifelse(abs(df_compare$scm_pp-df_compare$largeAlpha_pp) > 0.05, "Y", "N")
df_compare$diff_scm_joint <- ifelse(abs(df_compare$scm_pp-df_compare$joint_pp) > 0.05, "Y", "N")
df_compare$diff_scm_smallAlpha <- ifelse(abs(df_compare$scm_pp-df_compare$smallAlpha_pp) > 0.05, "Y", "N")
df_compare$diff_joint_smallAlpha <- ifelse(abs(df_compare$joint_pp-df_compare$smallAlpha_pp) > 0.05, "Y", "N")
df_compare$diff_joint_largeAlpha <- ifelse(abs(df_compare$joint_pp-df_compare$largeAlpha_pp) > 0.05, "Y", "N")


p_compare_joint <- df_compare %>%
  ggplot() +
  geom_abline(intercept = 0, slope = 1, linetype="dashed") +
  geom_point(aes(x=scm_pp, y=joint_pp, colour = diff_scm_joint), alpha=0.8) +
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

p_compare_da <- df_compare %>%
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

p_compare_largeAlpha <- df_compare %>%
  ggplot() +
  geom_abline(intercept = 0, slope = 1, linetype="dashed") +
  geom_point(aes(x=scm_pp, y=largeAlpha_pp, colour = diff_scm_largeAlpha), alpha=0.8) +
  theme_classic() +
  scale_x_continuous(breaks=c(0.6, 0.7, 0.8, 0.9, 1)) +
  scale_y_continuous(breaks=c(0.6, 0.7, 0.8, 0.9, 1)) +
  scale_color_manual(values = c("Y"="darkred", "N"="black")) +
  #scale_shape_manual(values = c("Y"=16, "N"=1))
  #xlab("Stochastic mapping") +
  #ylab(TeX("SD-OU (large $\\alpha$)")) +
  theme(legend.position = "none",
        axis.title = element_blank()) +
  coord_cartesian(xlim=c(0.6, 1), ylim=c(0.6,1))

p_compare_smallAlpha <- df_compare %>%
  ggplot() +
  geom_abline(intercept = 0, slope = 1, linetype="dashed") +
  geom_point(aes(x=scm_pp, y=smallAlpha_pp, colour = diff_scm_smallAlpha), alpha=0.8) +
  theme_classic() +
  scale_x_continuous(breaks=c(0.6, 0.7, 0.8, 0.9, 1)) +
  scale_y_continuous(breaks=c(0.6, 0.7, 0.8, 0.9, 1)) +
  scale_color_manual(values = c("Y"="darkred", "N"="black")) +
  #scale_shape_manual(values = c("Y"=16, "N"=1))
  #xlab("Stochastic mapping") +
  #ylab(TeX("SD-OU (small $\\alpha$)")) +
  theme(legend.position = "none",
        axis.title = element_blank()) +
  coord_cartesian(xlim=c(0.6, 1), ylim=c(0.6,1))



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

