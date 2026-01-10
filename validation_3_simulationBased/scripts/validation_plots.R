library(cowplot)
library(tidyverse)
source("../utility/functions.R")

# sequential models
processValidation("seq_rtOp", n_reps = 1000)
p1s <- generate_coverage_plots("seq_rtOp",
                               results_dir = "results/seq_rtOp",
                               figs_dir = "figures/seq_rtOp")
p1_all <- plot_grid(p1s[[1]], p1s[[2]], p1s[[3]],
                    p1s[[4]], p1s[[5]], p1s[[6]],
                    p1s[[7]], p1s[[8]], p1s[[9]], ncol=3)
ggsave(paste0("figures/seq_rtOp.pdf"),
       plot=p1_all, width=6, height=6, units="in")

processValidation("seq_rtEq",  n_reps = 1000)
p2s <- generate_coverage_plots("seq_rtEq_rfPa",
                               results_dir = "results/seq_rtEq",
                               figs_dir = "figures/seq_rtEq")
p2_all <- plot_grid(p2s[[1]], p2s[[2]], p2s[[3]],
                    p2s[[4]], p2s[[5]], p2s[[6]],
                    p2s[[7]], p2s[[8]], p2s[[9]], ncol=3)
ggsave(paste0("figures/seq_rtEq.pdf"),
       plot=p2_all, width=6, height=6, units="in")


processValidation("seq_rtPa", n_reps = 1000)
p3s <- generate_coverage_plots("seq_rtPa_rfPa",
                               results_dir = "results/seq_rtPa",
                               figs_dir = "figures/seq_rtPa")
p3_all <- plot_grid(p3s[[1]], p3s[[2]], p3s[[3]],
                    p3s[[4]], p3s[[5]], p3s[[6]],
                    p3s[[7]], p3s[[8]], p3s[[9]],
                    p3s[[10]], ncol=3)
ggsave(paste0("figures/seq_rtPa.pdf"),
       plot=p3_all, width=6, height=8, units="in")


# joint models
## root frequencies = Parameter
processSimplexParameters("rtOp", rf=TRUE, rates=TRUE)
processValidation("rtOp", n_reps = 1000)
p4s <- generate_coverage_plots("rtOp",
                               results_dir = "results/rtOp",
                               figs_dir = "figures/rtOp")
p4_all <- plot_grid(p4s[[1]], p4s[[2]], p4s[[3]], p4s[[4]], p4s[[5]],
                    p4s[[6]], p4s[[7]], p4s[[8]], p4s[[9]], p4s[[10]],
                    p4s[[11]], p4s[[12]], p4s[[13]], p4s[[14]], p4s[[15]],
                    p4s[[16]], p4s[[17]], p4s[[18]], p4s[[19]],
                    ncol=5)
ggsave(paste0("figures/joint_rtOp_rfPa.pdf"),
       plot=p4_all, width=10, height=8, units="in")


processSimplexParameters("rtEq", rf=TRUE, rates=TRUE)
processValidation("rtEq", n_reps = 1000)
p5s <- generate_coverage_plots("rtEq",
                               results_dir = "results/rtEq",
                               figs_dir = "figures/rtEq")

p5_all <- plot_grid(p5s[[1]], p5s[[2]], p5s[[3]], p5s[[4]], p5s[[5]],
                    p5s[[6]], p5s[[7]], p5s[[8]], p5s[[9]], p5s[[10]],
                    p5s[[11]], p5s[[12]], p5s[[13]], p5s[[14]], p5s[[15]],
                    p5s[[16]], p5s[[17]], p5s[[18]], p5s[[19]],
                    ncol=5)
ggsave(paste0("figures/joint_rtEq_rfPa.pdf"),
       plot=p5_all, width=10, height=8, units="in")


processSimplexParameters("rtPa", rf=TRUE, rates=TRUE)
processValidation("rtPa", n_reps = 1000)
p6s <- generate_coverage_plots("rtPa",
                               results_dir = "results/rtPa",
                               figs_dir = "figures/rtPa")
p6_all <- plot_grid(p6s[[1]], p6s[[2]], p6s[[3]], p6s[[4]], p6s[[5]],
                    p6s[[6]], p6s[[7]], p6s[[8]], p6s[[9]], p6s[[10]],
                    p6s[[11]], p6s[[12]], p6s[[13]], p6s[[14]], p6s[[15]],
                    p6s[[16]], p6s[[17]], p6s[[18]], p6s[[19]], p6s[[20]],
                    ncol=5)
ggsave(paste0("figures/joint_rtPa_rfPa.pdf"),
       plot=p6_all, width=10, height=8, units="in")


# root frequencies = Stationary
processSimplexParameters("rtOp_rfSt", rf=FALSE, rates=TRUE)
processValidation("rtOp_rfSt", n_reps = 1000)
p7s <- generate_coverage_plots("rtOp_rfSt",
                               results_dir = "results/rtOp_rfSt",
                               figs_dir = "figures/rtOp_rfSt")
p7_all <- plot_grid(p7s[[1]], p7s[[2]], p7s[[3]], p7s[[4]],
                    p7s[[5]], p7s[[6]], p7s[[7]], p7s[[8]],
                    p7s[[9]], p7s[[10]], p7s[[11]], p7s[[12]],
                    p7s[[13]], p7s[[14]], p7s[[15]], p7s[[16]],
                    ncol=4)
ggsave(paste0("figures/joint_rtOp_rfSt.pdf"),
       plot=p7_all, width=8, height=8, units="in")


processSimplexParameters("rtEq_rfSt", rf=FALSE, rates=TRUE)
processValidation("rtEq_rfSt", n_reps = 1000)
p8s <- generate_coverage_plots("rtEq_rfSt",
                               results_dir = "results/rtEq_rfSt",
                               figs_dir = "figures/rtEq_rfSt")

p8_all <- plot_grid(p8s[[1]], p8s[[2]], p8s[[3]], p8s[[4]],
                    p8s[[5]], p8s[[6]], p8s[[7]], p8s[[8]],
                    p8s[[9]], p8s[[10]], p8s[[11]], p8s[[12]],
                    p8s[[13]], p8s[[14]], p8s[[15]], p8s[[16]],
                    ncol=4)
ggsave(paste0("figures/joint_rtEq_rfSt.pdf"),
       plot=p8_all, width=8, height=8, units="in")


processSimplexParameters("rtPa_rfSt", rf=FALSE, rates=TRUE)
processValidation("rtPa_rfSt", n_reps = 248)
p9s <- generate_coverage_plots("rtPa_rfSt",
                               results_dir = "results/rtPa_rfSt",
                               figs_dir = "figures/rtPa_rfSt")
p9_all <- plot_grid(p9s[[1]], p9s[[2]], p9s[[3]], p9s[[4]],
                    p9s[[5]], p9s[[6]], p9s[[7]], p9s[[8]],
                    p9s[[9]], p9s[[10]], p9s[[11]], p9s[[12]],
                    p9s[[13]], p9s[[14]], p9s[[15]], p9s[[16]],
                    p9s[[17]], ncol=4)
ggsave(paste0("figures/joint_rtPa_rfSt.pdf"),
       plot=p9_all, width=8, height=10, units="in")
