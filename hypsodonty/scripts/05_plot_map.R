library(cowplot)
library(ggplot2)
library(latex2exp)
library(phytools)
library(RevGadgets)
library(tidyverse)
source("../utility/functions.R")

t <- read.tree("data/artiodactyla_rescaled.tree")
unscaled_t <- read.tree("data/artiodactyla.tree")
tree <- readTrees("data/artiodactyla_rescaled.tree")
unscaled_tree <- tree
unscaled_tree[[1]][[1]]@phylo$edge.length <- tree[[1]][[1]]@phylo$edge.length * max(node.depth.edgelength(unscaled_t))



# process output
ase_MAP <- processAncStates("output/ase_SCM/anc_states.tre",
                            state_labels=c("0"="Browsers", "1"="Mixed feeders", "2"="Grazers"))
ase_MAP@phylo$edge.length <- ase_MAP@phylo$edge.length * max(node.depth.edgelength(unscaled_t))
ase_da <- processAncStates("output/ase_DA/anc_states_marginal.log",
                           state_labels=c("0"="Browsers", "1"="Mixed feeders", "2"="Grazers"))
ase_da@phylo$edge.length <- ase_da@phylo$edge.length * max(node.depth.edgelength(unscaled_t))
ase_joint <- processAncStates("output/sdou_joint/anc_states_marginal.log",
                              state_labels=c("0"="Browsers", "1"="Mixed feeders", "2"="Grazers"))
ase_joint@phylo$edge.length <- ase_joint@phylo$edge.length * max(node.depth.edgelength(unscaled_t))

tb_MAP <- read.table("output/ase_SCM/charhist.log", header = TRUE, sep = "\t")
simmap_MAP <- read.simmap(text=tb_MAP$simmap, format = "phylip")
processed_MAP <- processStochMaps(tree=tree, simmap = simmap_MAP, states = c("0", "1", "2"))
processed_MAP$bl <- processed_MAP$bl * max(node.depth.edgelength(unscaled_t))
processed_MAP$x0 <- processed_MAP$x0 * max(node.depth.edgelength(unscaled_t))
processed_MAP$x1 <- processed_MAP$x1 * max(node.depth.edgelength(unscaled_t))

tb_da <- read.table("output/ase_DA/charhist.log", header = TRUE)
simmap_da <- read.simmap(text=tb_da$char_hist, format = "phylip")
processed_da <- processStochMaps(tree=tree, simmap = simmap_da, states = c("0", "1", "2"))
processed_da$bl <- processed_da$bl * max(node.depth.edgelength(unscaled_t))
processed_da$x0 <- processed_da$x0 * max(node.depth.edgelength(unscaled_t))
processed_da$x1 <- processed_da$x1 * max(node.depth.edgelength(unscaled_t))

tb_joint <- read.table("output/sdou_joint/charhist_run_1.log", header = TRUE)
simmap_joint <- read.simmap(text=tb_joint$char_hist, format = "phylip")
processed_joint <- processStochMaps(tree=tree, simmap = simmap_joint, states = c("0", "1", "2"))
processed_joint$bl <- processed_joint$bl * max(node.depth.edgelength(unscaled_t))
processed_joint$x0 <- processed_joint$x0 * max(node.depth.edgelength(unscaled_t))
processed_joint$x1 <- processed_joint$x1 * max(node.depth.edgelength(unscaled_t))

colnames(processed_MAP)[6] = colnames(processed_da)[6] = 
  colnames(processed_joint)[6] = "Browsers"
colnames(processed_MAP)[7] = colnames(processed_da)[7] = 
  colnames(processed_joint)[7] = "Mixed feeders"
colnames(processed_MAP)[8] = colnames(processed_da)[8] = 
  colnames(processed_joint)[8] = "Grazers"

write_rds(ase_MAP, "output/ase_MAP.rds")
write_rds(ase_da, "output/ase_da.rds")
write_rds(ase_joint, "output/ase_joint.rds")
write_rds(processed_MAP, "output/processed_MAP.rds")
write_rds(processed_da, "output/processed_da.rds")
write_rds(processed_joint, "output/processed_joint.rds")

ase_MAP <- read_rds("output/ase_MAP.rds")
ase_da <- read_rds("output/ase_da.rds")
ase_joint <- read_rds("output/ase_joint.rds")
processed_MAP <- read_rds("output/processed_MAP.rds")
processed_da <- read_rds("output/processed_da.rds")
processed_joint <- read_rds("output/processed_joint.rds")

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
                       timeline = FALSE,
                       time_bars = FALSE,
                       geo=FALSE,
                       tip_pies = FALSE,
                       node_pie_size = 2,
                       tree_layout = "rectangular",
                       state_transparency = 0.9,
                       tree_linewidth = 0.5) +
  theme(legend.position = "none") +
  coord_flip() +
  scale_x_reverse()

p2 <- plotStochMaps(tree=unscaled_tree, maps = processed_MAP, color_by = "MAP",
                    colors = c("Browsers"="#2c6e49",
                               "Mixed feeders"="#adc178",
                               "Grazers"="#7f4f24"),
                    tip_labels = FALSE,
                    tip_labels_size=3,
                    timeline = TRUE,
                    geo=FALSE,
                    time_bars = FALSE,
                    line_width=0.5
                    #tip_labels_size = 2,
                    #tip_labels_offset = 1
) +
  theme(legend.position = "none") +
  geom_cladelab(node=cervidae_node, label="", align=TRUE, angle=0, barcolour="#9a9a98ff",
                hjust=0, barsize=1.25, offset=4, offset.text=0.025, extend=0.2) +
  geom_cladelab(node=bovidae_node, label="", align=TRUE, angle=0, barsize=1.25, barcolour="#494a45ff",
                hjust=0, vjust=6.5, offset=4, offset.text=0.025, extend=0.2) +
  geom_cladelab(node=bovini_node, label="", align=TRUE, angle=0, barsize=1.5, barcolour="#f0cf91ff",
                hjust=0, offset=2, offset.text=0.01, extend=0.2, fontsize=2.88) +
  geom_cladelab(node=antilopini_node, label="", align=TRUE, angle=0, barsize=1.5, barcolour="#b2c5cfff",
                hjust=0, offset=2, offset.text=0.01, extend=0.2, fontsize=2.88) +
  geom_cladelab(node=cephalophini_node, label="", align=TRUE, angle=0, barsize=1.5, barcolour="#76a7abff",
                hjust=0, offset=2, offset.text=0.01, extend=0.2, fontsize=2.88) +
  geom_cladelab(node=caprini_node, label="", align=TRUE, angle=0, barsize=1.5, barcolour="#c5b0beff",
                hjust=0, offset=2, offset.text=0.01, extend=0.2, fontsize=2.88)  +
  coord_flip() +
  xlim(-45,5)

p2

p2a <- plot_grid(p0w2, p2, ncol = 2, rel_widths = c(1,9)) +
  draw_plot_label(label=c("Age (Ma)"),
                  x=0.08,
                  y=0.5,
                  hjust=.5, vjust=.5, size=10, angle=90, fontface = "plain")
# +
#  coord_flip()

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
                       timeline = TRUE,
                       time_bars = FALSE,
                       geo=FALSE,
                       tip_pies = FALSE,
                       node_pie_size = 2,
                       tree_layout = "rectangular",
                       state_transparency = 0.9,
                       tree_linewidth = 0.5) +
  theme(legend.position = "none") +
  coord_flip() +
  scale_x_reverse()



p4 <- plotStochMaps(tree=unscaled_tree, maps = processed_da, color_by = "MAP",
                    colors = c("Browsers"="#2c6e49",
                               "Mixed feeders"="#adc178",
                               "Grazers"="#7f4f24"),
                    tip_labels = FALSE,
                    timeline = TRUE,
                    geo=FALSE,
                    time_bars = FALSE,
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
                       tip_labels = FALSE,
                       #tip_labels_size = 2,
                       #tip_labels_offset = 1,
                       pie_colors = c("Browsers"="#2c6e49",
                                      "Mixed feeders"="#adc178",
                                      "Grazers"="#7f4f24"),
                       timeline = FALSE,
                       time_bars = FALSE,
                       geo=FALSE,
                       tip_pies = FALSE,
                       node_pie_size = 2,
                       tree_layout = "rectangular",
                       state_transparency = 0.9,
                       tree_linewidth = 0.5) +
  theme(legend.position = "none") +
  coord_flip() +
  scale_x_reverse()



p6 <- plotStochMaps(tree=unscaled_tree, maps = processed_joint, color_by = "MAP",
                    colors = c("Browsers"="#2c6e49",
                               "Mixed feeders"="#adc178",
                               "Grazers"="#7f4f24"),
                    tip_labels = FALSE,
                    timeline = FALSE,
                    geo=TRUE,
                    time_bars = FALSE,
                    line_width=0.5
                    ) +
  theme(legend.position = "none") +
  geom_cladelab(node=cervidae_node, label="", align=TRUE, angle=0, barcolour="#9a9a98ff",
                hjust=0, barsize=1.25, offset=4, offset.text=0.025, extend=0.2) +
  geom_cladelab(node=bovidae_node, label="", align=TRUE, angle=0, barsize=1.25, barcolour="#494a45ff",
                hjust=0, vjust=6.5, offset=4, offset.text=0.025, extend=0.2) +
  geom_cladelab(node=bovini_node, label="", align=TRUE, angle=0, barsize=1.5, barcolour="#f0cf91ff",
                hjust=0, offset=2, offset.text=0.01, extend=0.2, fontsize=2.88) +
  geom_cladelab(node=antilopini_node, label="", align=TRUE, angle=0, barsize=1.5, barcolour="#b2c5cfff",
                hjust=0, offset=2, offset.text=0.01, extend=0.2, fontsize=2.88) +
  geom_cladelab(node=cephalophini_node, label="", align=TRUE, angle=0, barsize=1.5, barcolour="#76a7abff",
                hjust=0, offset=2, offset.text=0.01, extend=0.2, fontsize=2.88) +
  geom_cladelab(node=caprini_node, label="", align=TRUE, angle=0, barsize=1.5, barcolour="#c5b0beff",
                hjust=0, offset=2, offset.text=0.01, extend=0.2, fontsize=2.88)  +
  coord_flip() +
  xlim(-45,5)


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


#p0 <- p2 +
#  scale_color_manual(name = "Feeding behavior", labels=c("Browsers", "Mixed feeders", "Grazers"),
#                     values=c("#2c6e49", "#adc178", "#7f4f24"))
#legend <- get_legend2(p0 + theme(legend.position = "right",
#                                  legend.box.margin = margin(0, 0, 0, 12),
#                                 legend.text = element_text(size=8))
#                       + guides(color = guide_legend(override.aes = list(linewidth = 2),
#                                                     title='Feeding behavior')))
legend <- ggplot() +
  annotate("text", x = 1, y = -0.0, label = "Browsers", hjust = 0      , size=3.5) +
  annotate("text", x = 1, y = -0.5, label = "Mixed feeders", hjust = 0, size=3.5  ) +
  annotate("text", x = 1, y = -1.0, label = "Grazers", hjust = 0, size=3.5    ) +
  annotate("segment", x = 0.925, y = -0.0, xend = 0.975, color = "#2c6e49", linewidth=2) +
  annotate("segment", x = 0.925, y = -0.5, xend = 0.975, color = "#adc178", linewidth=2) +
  annotate("segment", x = 0.925, y = -1.0, xend = 0.975, color = "#7f4f24", linewidth=2) +
  
  annotate("text", x = 1, y = -2.0-.25, label = "Cervidae", hjust = 0, size=3.5  ) +
  annotate("text", x = 1, y = -2.5-.25, label = "Bovidae", hjust = 0, size=3.5  ) +
  annotate("text", x = 1, y = -3.0-.25, label = "Bovini", hjust = 0, size=3.5    ) +
  annotate("text", x = 1, y = -3.5-.25, label = "Antilopini", hjust = 0, size=3.5) +
  annotate("text", x = 1, y = -4.0-.25, label = "Cephalophini", hjust = 0, size=3.5) +
  annotate("text", x = 1, y = -4.5-.25, label = "Caprini", hjust = 0, size=3.5   ) +
  annotate("segment", x = 0.925, y = -2.0-0.25, xend = 0.975, color = "#9a9a98ff", linewidth=2) +
  annotate("segment", x = 0.925, y = -2.5-0.25, xend = 0.975, color = "#494a45ff", linewidth=2) +
  annotate("segment", x = 0.925, y = -3.0-0.25, xend = 0.975, color = "#f0cf91ff", linewidth=2) +
  annotate("segment", x = 0.925, y = -3.5-0.25, xend = 0.975, color = "#b2c5cfff", linewidth=2) +
  annotate("segment", x = 0.925, y = -4.0-0.25, xend = 0.975, color = "#76a7abff", linewidth=2) +
  annotate("segment", x = 0.925, y = -4.5-0.25, xend = 0.975, color = "#c5b0beff", linewidth=2) +
  theme_void() +
  annotate("text", x = 0.905, y = 0.5, label = "Feeding behavior", hjust = 0, size=4.5) +
  annotate("text", x = 0.905, y = -1.5-0.25, label = "Clade", hjust = 0, size=4.5) +
  coord_cartesian(xlim=c(0.9,1.25))
legend
#############
# scm+joint #
#############
p1a <- plot_grid(p0w2, p1, rel_widths = c(19,81))
plot_left_lab <- plot_grid(p0w) +
  draw_plot_label(label=c("(a) Stochastic mapping"),
                  x=c(0.5),
                  y=0.5,
                  hjust=.5, vjust=.5, size=10)
  
plot_left_lab <-  plot_grid(p0w2, plot_left_lab, rel_widths = c(19,81))
  

plot_left <- plot_grid(plot_left_lab, p1a, p2a,
                       rel_heights = c(1,6,7),
                       ncol=1, align="r")

plot_left_lab_grey <- plot_grid(p0g) + 
  draw_plot_label(label=c("Discrete only"),
                  x=0.5,
                  y=0.5,
                  hjust=.5, vjust=.5, size=12)
plot_left_lab_grey <- plot_grid(p0w2, plot_left_lab_grey, rel_widths = c(19,81))

plot_left <- plot_grid(plot_left_lab_grey, plot_left,
                       ncol = 1, align = 'v',
                       rel_heights = c(1,13))

plot_mid <- p0w2
plot_right <- plot_grid(p0w, p5, p6,
                        rel_heights = c(1,6,7),
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

legend_w_space <- plot_grid(p0w2, legend, p0w2, ncol=1, rel_heights = c(1,3,1))
plot_maps_scm_joint <- cowplot::plot_grid(plot_left, plot_mid, plot_right, legend_w_space,
                                    rel_widths = c(1/0.9,0.01,1,0.8),
                                    ncol=4)

plot_maps_scm_joint


ggsave("figures/case_study_maps_scm_joint2.pdf", plot_maps_scm_joint, width=6.75, height=5, units="in")


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
