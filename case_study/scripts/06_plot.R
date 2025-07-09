library(cowplot)
library(khroma)
library(ggplot2)
library(ggridges)
library(latex2exp)
library(phytools)
library(RevGadgets)
library(tidyverse)
source("scripts/functions.R")


# plot ancestral state map of stochastic mapping and data augmentation respectively
#tb_node <- read.table("output/ase/charhist.log", header = TRUE)
#sm_node <- read.simmap(text=tb_node$simmap, format="phylip")
#obj_node<-summary(sm_node)
#plot(obj_node)
#
#tb_node2 <- read.table("output/data_augmentation/charhist.log", header = TRUE)
#sm_node2 <- read.simmap(text=tb_node2$char_hist, format="phylip")
#obj_node2<-summary(sm_node2)
#plot(obj_node2)
#
#tb_node3 <- read.table("output/joint_SDOU/augch.trees", header = TRUE)
#sm_node3 <- read.simmap(text=tb_node3$char_hist, format="phylip")
#obj_node3<-summary(sm_node3)
#plot(obj_node3)


#plot(sm_node3[[1]], color=c("0"="#44aa99", "1"="#ddcc77", "2"="#882255"))
#
#tree <- read.tree("data/artiodactyla.tree")
#
#max(node.depth.edgelength(tree))



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
bounds <- ci(x, ci = 0.95, method = "HDI", verbose = FALSE) %>% unlist()
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



tree <- readTrees("data/artiodactyla.tree")
t <- read.tree("data/artiodactyla.tree")

######################
# stochastic mapping #
######################
ase_MAP <- processAncStates("output/ase/anc_states.tre",
                            state_labels=c("0"="Browsers", "2"="Grazers", "1"="Mixed feeders"))
p1 <- plotAncStatesPie(t = ase_MAP,
                       tip_labels = FALSE,
                       #tip_labels_size = 2,
                       #tip_labels_offset = 1,
                       pie_colors = c("Browsers"="#CC6677", "Grazers"="#ddcc77", "Mixed feeders"="#44AA99"),
                       tip_pies = TRUE,
                       node_pie_size = 2,
                       tip_pie_size = 2,
                       tree_layout = "rectangular",
                       state_transparency = 0.9) +
  theme(legend.position = "none")

tb_MAP <- read.table("output/ase/charhist.log", header = TRUE)
simmap_MAP <- read.simmap(text=tb_MAP$simmap, format = "phylip")
processed_MAP <- processStochMaps(tree=tree, simmap = simmap_MAP, states = c("0", "1", "2"))
colnames(processed_MAP)[6] = "Browsers"
colnames(processed_MAP)[8] = "Grazers"
colnames(processed_MAP)[7] = "Mixed feeders"
p2 <- plotStochMaps(tree=tree, maps = processed_MAP, color_by = "MAP",
                    colors = c("Browsers"="#CC6677",
                               "Grazers"="#ddcc77",
                               "Mixed feeders"="#44aa99"),
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
                       pie_colors = c("Browsers"="#CC6677", "Grazers"="#ddcc77", "Mixed feeders"="#44AA99"),
                       tip_pies = TRUE,
                       node_pie_size = 2,
                       tip_pie_size = 2,
                       tree_layout = "rectangular",
                       state_transparency = 0.9) +
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
                       pie_colors = c("Browsers"="#CC6677", "Grazers"="#ddcc77", "Mixed feeders"="#44AA99"),
                       tip_labels = FALSE,
                       tip_pies = TRUE,
                       node_pie_size = 2,
                       tip_pie_size = 2,
                       tree_layout = "rectangular",
                       state_transparency = 0.9) +
  theme(legend.position = "none")

tb_joint <- read.table("output/joint_SDOU/charhist.log", header = TRUE)
simmap_joint <- read.simmap(text=tb_joint$char_hist, format = "phylip")
processed_joint <- processStochMaps(tree=tree, simmap = simmap_joint, states = c("0", "1", "2"))
colnames(processed_joint)[6] = "Browsers"
colnames(processed_joint)[8] = "Grazers"
colnames(processed_joint)[7] = "Mixed feeders"
p6 <- plotStochMaps(tree=tree, maps = processed_joint, color_by = "MAP",
                    colors = c("Browsers"="#CC6677",
                               "Grazers"="#ddcc77",
                               "Mixed feeders"="#44aa99"),
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


#p0 <- ggplot() + theme_void()
#row1 <- cowplot::plot_grid(p3, p0,
#                                rel_widths = c(2.225,1),
#                                #labels = c("Joint", "Data augmentation", "Stochastic mapping"),
#                                #label_size = 10,
#                                ncol=2)
#cowplot::plot_grid(row1, p5,
#                   #labels = c("Joint", "Data augmentation", "Stochastic mapping"),
#                   #label_size = 10,
#                   ncol=1)

########
# long #
########
ase_long <- tb_long <-
  simmap_long <- processed_long <-
  p_ase_long <- p_stochmap_long <- list()

for (i in 1:4){
  ase_long[[i]] <-
    processAncStates(paste0("output/joint_SDOU/long_anc_states_marginal_run_", i, ".log"),
                            state_labels=c("0"="Browsers",
                                           "1"="Mixed feeders",
                                           "2"="Grazers"))
  tb_long[[i]] <- read.table(paste0("output/joint_SDOU/long_charhist_run_", i, ".log"), header = TRUE)
  simmap_long[[i]] <- read.simmap(text=tb_long[[i]]$char_hist, format = "phylip")
  processed_long[[i]] <- processStochMaps(tree=tree, simmap = simmap_long[[i]],
                                          states = c("0", "1", "2"))
  colnames(processed_long[[i]])[6] = "Browsers"
  colnames(processed_long[[i]])[7] = "Mixed feeders"
  colnames(processed_long[[i]])[8] = "Grazers"
  
  if (i<4){
    p_ase_long[[i]] <- plotAncStatesPie(t = ase_long[[i]],
                           tip_labels = FALSE,
                           #tip_labels_size = 2,
                           #tip_labels_offset = 1,
                           pie_colors = c("Browsers"="#CC6677", "Grazers"="#ddcc77", "Mixed feeders"="#44AA99"),
                           tip_pies = TRUE,
                           node_pie_size = 2,
                           tip_pie_size = 2,
                           tree_layout = "rectangular",
                           state_transparency = 0.9) +
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
  else {
    p_ase_long[[i]] <- plotAncStatesPie(t = ase_joint,
                           pie_colors = c("Browsers"="#CC6677", "Grazers"="#ddcc77", "Mixed feeders"="#44AA99"),
                           tip_labels = TRUE,
                           tip_labels_size = 1.5,
                           tip_labels_offset = 1,
                           tip_pies = TRUE,
                           node_pie_size = 2/(1+1/2.225),
                           tip_pie_size = 2/(1+1/2.225),
                           tree_layout = "rectangular",
                           state_transparency = 0.9) +
      theme(legend.position = "none")
    p_stochmap_long[[i]] <- plotStochMaps(tree=tree, maps = processed_joint, color_by = "MAP",
                        colors = c("Browsers"="#CC6677",
                                   "Grazers"="#ddcc77",
                                   "Mixed feeders"="#44aa99"),
                        tip_labels = TRUE,
                        tip_labels_size = 1.5,
                        tip_labels_offset = 0.5
    ) +
      theme(legend.position = "none")
  }
}


plot_maps_long <- cowplot::plot_grid(p_ase_long[[1]], p_ase_long[[2]], p_ase_long[[3]], p_ase_long[[4]],
                                     p_stochmap_long[[1]], p_stochmap_long[[2]], p_stochmap_long[[3]], p_stochmap_long[[4]],
                                rel_widths = c(1,1,1, 1+1/2.225,1,1,1,1,1+1/2.225),
                                labels = c("Run 1", "Run 2", "Run 3", "Run 4"),
                                hjust = 0,
                                label_size = 10,
                                ncol=4)

ggsave("figures/case_study_maps_long.pdf", plot_maps_long, width=7.5, height=7.5, units="in")



