library(cowplot)
library(ggplot2)
library(latex2exp)
library(phytools)
library(RevGadgets)
library(tidyverse)
source("../utility/functions.R")

tree <- readTrees("data/artiodactyla_rescaled.tree")
t <- read.tree("data/artiodactyla_rescaled.tree")



# process output
ase_MAP <- processAncStates("output/ase_SCM/anc_states.tre",
                            state_labels=c("0"="Browsers", "1"="Mixed feeders", "2"="Grazers"))
ase_da <- processAncStates("output/ase_DA/anc_states_marginal.log",
                           state_labels=c("0"="Browsers", "1"="Mixed feeders", "2"="Grazers"))
ase_joint <- processAncStates("output/sdou_joint/anc_states_marginal.log",
                              state_labels=c("0"="Browsers", "1"="Mixed feeders", "2"="Grazers"))
tb_MAP <- read.table("output/ase_SCM/charhist.log", header = TRUE, sep = "\t")
simmap_MAP <- read.simmap(text=tb_MAP$simmap, format = "phylip")
processed_MAP <- processStochMaps(tree=tree, simmap = simmap_MAP, states = c("0", "1", "2"))

tb_da <- read.table("output/ase_DA/charhist.log", header = TRUE)
simmap_da <- read.simmap(text=tb_da$char_hist, format = "phylip")
processed_da <- processStochMaps(tree=tree, simmap = simmap_da, states = c("0", "1", "2"))

tb_joint <- read.table("output/sdou_joint/charhist.log", header = TRUE)
simmap_joint <- read.simmap(text=tb_joint$char_hist, format = "phylip")
processed_joint <- processStochMaps(tree=tree, simmap = simmap_joint, states = c("0", "1", "2"))

colnames(processed_MAP)[6] = colnames(processed_da)[6] = 
  colnames(processed_joint)[6] = "Browsers"
colnames(processed_MAP)[7] = colnames(processed_da)[7] = 
  colnames(processed_joint)[7] = "Mixed feeders"
colnames(processed_MAP)[8] = colnames(processed_da)[8] = 
  colnames(processed_joint)[8] = "Grazers"

######################
# stochastic mapping #
######################
p1 <- plotAncStatesPie(t = ase_MAP,
                       tip_labels = FALSE,
                       #tip_labels_size = 2,
                       #tip_labels_offset = 1,
                       pie_colors = c("Browsers"="#2c6e49",
                                      "Mixed feeders"="#adc178",
                                      "Grazers"="#7f4f24"),
                       tip_pies = FALSE,
                       node_pie_size = 2,
                       tree_layout = "rectangular",
                       state_transparency = 0.9,
                       tree_linewidth = 0.5) +
  theme(legend.position = "none") +
  coord_flip() +
  scale_x_reverse()

p2 <- plotStochMaps(tree=tree, maps = processed_MAP, color_by = "MAP",
                    colors = c("Browsers"="#2c6e49",
                               "Mixed feeders"="#adc178",
                               "Grazers"="#7f4f24"),
                    tip_labels = FALSE,
                    line_width=0.5
                    #tip_labels_size = 2,
                    #tip_labels_offset = 1
) +
  theme(legend.position = "none") +
  coord_flip()

#####################
# data augmentation #
#####################
p3 <- plotAncStatesPie(t = ase_da,
                       tip_labels = FALSE,
                       #tip_labels_size = 2,
                       #tip_labels_offset = 1,
                       pie_colors = c("Browsers"="#2c6e49",
                                      "Mixed feeders"="#adc178",
                                      "Grazers"="#7f4f24"),
                       tip_pies = FALSE,
                       node_pie_size = 2,
                       tree_layout = "rectangular",
                       state_transparency = 0.9,
                       tree_linewidth = 0.5) +
  theme(legend.position = "none") +
  coord_flip() +
  scale_x_reverse()



p4 <- plotStochMaps(tree=tree, maps = processed_da, color_by = "MAP",
                    colors = c("Browsers"="#2c6e49",
                               "Mixed feeders"="#adc178",
                               "Grazers"="#7f4f24"),
                    tip_labels = FALSE,
                    line_width=0.5
                    #tip_labels_size = 2,
                    #tip_labels_offset = 1
) +
  theme(legend.position = "none") +
  coord_flip()


#########
# joint #
#########

p5 <- plotAncStatesPie(t = ase_joint,
                       pie_colors = c("Browsers"="#2c6e49",
                                      "Mixed feeders"="#adc178",
                                      "Grazers"="#7f4f24"),
                       tip_labels = FALSE,
                       tip_pies = FALSE,
                       node_pie_size = 2,
                       tree_layout = "rectangular",
                       state_transparency = 0.9,
                       tree_linewidth = 0.5) +
  theme(legend.position = "none") +
  coord_flip() +
  scale_x_reverse()



p6 <- plotStochMaps(tree=tree, maps = processed_joint, color_by = "MAP",
                    colors = c("Browsers"="#2c6e49",
                               "Mixed feeders"="#adc178",
                               "Grazers"="#7f4f24"),
                    tip_labels = FALSE,
                    line_width=0.5
                    ) +
  theme(legend.position = "none") +
  coord_flip()


##########
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


df_tips <- data.frame(xpos = 0, ypos = 1:82*2-2, label=rev(ggtree::get_taxa_name(p6)))

ptips <- ggplot(df_tips, aes(x=xpos, y=ypos, label=label)) + 
  geom_text(size=1.5, hjust=0, fontface = "italic") +
  theme_void() +
  coord_cartesian(xlim=c(0,1))
ptips


p0 <- p2 +
  scale_color_manual(name = "Diet", labels=c("Browsers", "Mixed feeders", "Grazers"),
                     values=c("#2c6e49", "#adc178", "#7f4f24"))
legend <- get_legend2(p0 + theme(legend.position = "right",
                                  legend.box.margin = margin(0, 0, 0, 12))
                       + guides(color = guide_legend(override.aes = list(linewidth = 2),
                                                     title='Feeding behavior')))
#############
# scm+joint #
#############
plot_left <- plot_grid(p0w, p1, p2,
                       rel_heights = c(1,6,6),
                       ncol=1) +
  draw_plot_label(label=c("(a) Stochastic mapping"),
                  x=c(0.5),
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
  draw_plot_label(label=c("(b) Joint inference"),
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

plot_maps_scm_joint <- cowplot::plot_grid(plot_left, plot_mid, plot_right, legend,
                                    rel_widths = c(1,0.01,1,0.6),
                                    ncol=4)


ggsave("figures/case_study_maps_scm_joint.pdf", plot_maps_scm_joint, width=6.5, height=5, units="in")


##########
# scm+da #
##########
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

plot_maps_scm_da <- cowplot::plot_grid(plot_left, legend,
                                       rel_widths = c(1,0.3),
                                       ncol=2)


ggsave("figures/supp_case_study_maps_scm_da.pdf", plot_maps_scm_da, width=6, height=5, units="in")
