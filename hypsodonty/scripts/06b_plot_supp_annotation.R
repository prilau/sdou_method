library(ggtree)
library(RevGadgets)
library(tidytree)

source("../utility/functions.R")

tb_joint <- read.table("output/sdou_joint/charhist.log", header = TRUE)
simmap_joint <- read.simmap(text=tb_joint$char_hist, format = "phylip")
processed_joint <- processStochMaps(tree=tree, simmap = simmap_joint, states = c("0", "1", "2"))

tb <- as_tibble(t)

cervidae_node <- MRCA(tree[[1]][[1]], which(tb$label == "Hippocamelus_antisensis"),
     which(tb$label == "Muntiacus_muntjak"))

bovidae_node <- MRCA(tree[[1]][[1]], which(tb$label == "Moschus_moschiferus"),
                     which(tb$label == "Tetracerus_quadricornis"))

bovini_node <- MRCA(tree[[1]][[1]], which(tb$label == "Syncerus_caffer"),
                     which(tb$label == "Bos_gaurus"))

antilopini_node <- MRCA(tree[[1]][[1]], which(tb$label == "Procapra_gutturosa"),
                    which(tb$label == "Rahicerus_campestris"))
cephalophini_node <- MRCA(tree[[1]][[1]], which(tb$label == "Philantomba_monticola"),
                     which(tb$label == "Sylvicapra_grimmia"))
caprini_node <- MRCA(tree[[1]][[1]], which(tb$label == "Pantholops_hodgsonii"),
                     which(tb$label == "Ovibos_moschatus"))



p2 <- plotStochMaps(tree=tree, maps = processed_joint, color_by = "MAP",
                    colors = c("Browsers"="#2c6e49",
                               "Mixed feeders"="#adc178",
                               "Grazers"="#7f4f24"),
                    tip_labels = TRUE,
                    line_width=0.5,
                    tip_labels_size = 2,
                    #tip_labels_offset = 1
) +
  geom_cladelab(node=cervidae_node, label="Cervidae", align=TRUE, angle=0, barcolour="#222222", textcolor="#222222",
                  hjust=0, barsize=1.5, offset=0.01+0.175, offset.text=0.025, extend=0.2) +
  geom_cladelab(node=bovidae_node, label="Bovidae", align=TRUE, angle=0, barsize=1.5, barcolour="#222222", textcolor="#222222",
                hjust=0, vjust=6.5, offset=0.01+0.175, offset.text=0.025, extend=0.2) +
  geom_cladelab(node=bovini_node, label="Bovini", align=TRUE, angle=0, barsize=1, barcolour="#E69F00", textcolor="#E69F00",
                hjust=0, offset=0.02+0.175, offset.text=0.01, extend=0.2, fontsize=2.88) +
  geom_cladelab(node=antilopini_node, label="Antilopini", align=TRUE, angle=0, barsize=1, barcolour="#56B4E9", textcolor="#56B4E9",
                hjust=0, offset=0.02+0.175, offset.text=0.01, extend=0.2, fontsize=2.88) +
  geom_cladelab(node=cephalophini_node, label="Cephalophini", align=TRUE, angle=0, barsize=1, barcolour="#D55E00", textcolor="#D55E00",
                hjust=0, offset=0.02+0.175, offset.text=0.01, extend=0.2, fontsize=2.88) +
  geom_cladelab(node=caprini_node, label="Caprini", align=TRUE, angle=0, barsize=1, barcolour="#0072B2", textcolor="#0072B2",
                hjust=0, offset=0.02+0.175, offset.text=0.01, extend=0.2, fontsize=2.88) +
  theme(legend.position = "none") +
  xlim(-1,0.3)


ggsave("figures/supp_case_study_maps_joint_annotated.pdf", width=8, height=6, units = "in")
