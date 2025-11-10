run1 <- read.table("output/ase_SCM/trace_run_1.log", header = TRUE) %>% 
  select(starts_with(c("lambda")))
run2 <- read.table("output/ase_SCM/trace_run_2.log", header = TRUE) %>% 
  select(starts_with(c("lambda")))

pars <- colnames(run1)

df_scm <- tibble(par = pars,
                 ks = NA,
                 analysis = "SCM")

for(i in 1:length(pars)){
  df_scm$ks[i] <- ks.test(run1[[pars[i]]], run2[[pars[i]]], "two.sided")$statistic %>% unname %>% unlist
}

run1 <- read.table("output/ase_DA/trace_run_1.log", header = TRUE) %>% 
  select(starts_with(c("lambda")))
run2 <- read.table("output/ase_DA/trace_run_2.log", header = TRUE) %>% 
  select(starts_with(c("lambda")))

pars <- colnames(run1)

df_da <- tibble(par = pars,
                 ks = NA,
                analysis = "DA")

for(i in 1:length(pars)){
  df_da$ks[i] <- ks.test(run1[[pars[i]]], run2[[pars[i]]], "two.sided")$statistic %>% unname %>% unlist
}


run1 <- read.table("output/sdou_seq/trace_MAP_run_1.log", header = TRUE) %>% 
  select(starts_with(c("alpha", "sigma", "theta")))
run2 <- read.table("output/sdou_seq/trace_MAP_run_2.log", header = TRUE) %>% 
  select(starts_with(c("alpha", "sigma", "theta")))

pars <- colnames(run1)

df_map <- tibble(par = pars,
                   ks = NA,
                 analysis = "seq-single")

for(i in 1:length(pars)){
  df_map$ks[i] <- ks.test(run1[[pars[i]]], run2[[pars[i]]], "two.sided")$statistic %>% unname %>% unlist
}

run1 <- read.table("output/sdou_seq/trace_r100_run_1.log", header = TRUE) %>% 
  select(starts_with(c("alpha", "sigma", "theta")))
run2 <- read.table("output/sdou_seq/trace_r100_run_2.log", header = TRUE) %>% 
  select(starts_with(c("alpha", "sigma", "theta")))

pars <- colnames(run1)

df_r100 <- tibble(par = pars,
                 ks = NA,
                 analysis = "seq-multi")

for(i in 1:length(pars)){
  df_r100$ks[i] <- ks.test(run1[[pars[i]]], run2[[pars[i]]], "two.sided")$statistic %>% unname %>% unlist
}


run1 <- read.table("output/sdou_joint/trace_run_1.log", header = TRUE) %>% 
  select(starts_with(c("alpha", "lambda", "sigma", "theta")))
run2 <- read.table("output/sdou_joint/trace_run_2.log", header = TRUE) %>% 
  select(starts_with(c("alpha", "lambda", "sigma", "theta")))

pars <- colnames(run1)

df_joint <- tibble(par = pars,
             ks = NA,
             analysis = "joint")

for(i in 1:length(pars)){
  df_joint$ks[i] <- ks.test(run1[[pars[i]]], run2[[pars[i]]], "two.sided")$statistic %>% unname %>% unlist
}

df_all <- rbind(df_scm, df_da, df_map, df_r100, df_joint) %>% 
  mutate(analysis_par = paste0(analysis, "_", par),
         analysis_par = factor(analysis_par, levels = c("SCM_lambda", "DA_lambda",
                                                        "seq-single_alpha.1.", "seq-single_alpha.2.", "seq-single_alpha.3.",
                                                        "seq-single_theta.1.",  "seq-single_theta.2.",  "seq-single_theta.3.",
                                                        "seq-single_sigma2.1.", "seq-single_sigma2.2.", "seq-single_sigma2.3.", 
                                                        "seq-multi_alpha.1.",   "seq-multi_alpha.2.", "seq-multi_alpha.3.",
                                                        "seq-multi_theta.1.", "seq-multi_theta.2.", "seq-multi_theta.3.",
                                                        "seq-multi_sigma2.1.", "seq-multi_sigma2.2.", "seq-multi_sigma2.3.",
                                                        "joint_alpha.1.", "joint_alpha.2.", "joint_alpha.3.",
                                                        "joint_theta.1.", "joint_theta.2.", "joint_theta.3.",
                                                        "joint_sigma2.1.", "joint_sigma2.2.", "joint_sigma2.3.",
                                                        "joint_lambda")),
         analysis = factor(analysis, levels=c("SCM", "DA", "seq-single", "seq-multi", "joint")))


ggplot(df_all) + geom_point(aes(y=ks, x=analysis_par, color=analysis)) +
  geom_hline(yintercept = 0.0921, linetype="dashed") +
  ylab("Kolmogorov-Smirnov score") +
  scale_color_manual(values=c("#332288", "#117733", "#88CCEE", "#DDCC77", "#CC6677"), labels=c("SCM", "DA", "seq-single", "seq-multi", "joint")) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle=40, vjust=1, hjust=1)) +
  coord_cartesian(ylim = c(0, 0.1))

ggsave("figures/convergence.pdf", width = 6, height=4, units = "in")
