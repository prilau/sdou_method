library(ape)
library(cowplot)
library(ggplot2)
library(ggridges)
library(khroma)
library(latex2exp)
library(RevGadgets)
library(tidyverse)
source("../utility/functions.R")

trace_1a <- readTrace(path = "output/3a_state_dependent_BM.log", burnin = 0.0)
trace_1b <- readTrace(path = "output/3b_MuSSCRat.log", burnin = 0.0)
trace_1c <- readTrace(path = "output/3c_state_dependent_OU.log", burnin = 0.0)

trace_2a <- readTrace(path = "output/2a_state_independent_OU.log", burnin = 0.0)
trace_2b <- readTrace(path = "output/2b_state_dependent_OU.log", burnin = 0.0)

trace_3a <- readTrace(path = "output/1a_state_independent_BM.log", burnin = 0.0)
trace_3b <- readTrace(path = "output/1b_state_dependent_OU.log", burnin = 0.0)
trace_3c <- readTrace(path = "output/1c_state_dependent_BM.log", burnin = 0.0)
trace_3d <- readTrace(path = "output/1d_state_independent_OU.log", burnin = 0.0)

colors_traces <- as.character(c(color("high contrast")(3)[-1], color("medium contrast")(6),
                                color("muted")(9)[c(5,6,8)]))
colors_traces <- colors_traces[c(11,10,1,3,6,2,4,7,5,9,8)]


#######################
# Comparison to SD-BM #
#######################

traces_1 <- list(tibble(.rows = 20002))

traces_1[[1]]$sigma2_1_1a <- trace_1a[[1]]$`sigma2[1]`
traces_1[[1]]$sigma2_1_1b <- trace_1b[[1]]$`sigma2[1]`
traces_1[[1]]$sigma2_1_1c <- trace_1c[[1]]$`sigma2[1]`
traces_1[[1]]$sigma2_2_1a <- trace_1a[[1]]$`sigma2[2]`
traces_1[[1]]$sigma2_2_1b <- trace_1b[[1]]$`sigma2[2]`
traces_1[[1]]$sigma2_2_1c <- trace_1c[[1]]$`sigma2[2]`
traces_1[[1]]$sigma2_3_1a <- trace_1a[[1]]$`sigma2[3]`
traces_1[[1]]$sigma2_3_1b <- trace_1b[[1]]$`sigma2[3]`
traces_1[[1]]$sigma2_3_1c <- trace_1c[[1]]$`sigma2[3]`

traces_1_long <- traces_1[[1]] %>% 
  pivot_longer(cols=c("sigma2_1_1a", "sigma2_1_1b", "sigma2_1_1c",
                      "sigma2_2_1a", "sigma2_2_1b", "sigma2_2_1c",
                      "sigma2_3_1a", "sigma2_3_1b", "sigma2_3_1c"),
               names_to = "par", values_to = "value") %>% 
  mutate(model = ifelse(endsWith(par, "1a"), "SD-BM", ifelse(endsWith(par, "1b"), "MuSSCRat", "SD-OU")),
         model = factor(model, levels = c("SD-OU", "MuSSCRat", "SD-BM")),
         par = factor(par, levels = rev(c("sigma2_1_1a", "sigma2_1_1b", "sigma2_1_1c",
                                          "sigma2_2_1a", "sigma2_2_1b", "sigma2_2_1c",
                                          "sigma2_3_1a", "sigma2_3_1b", "sigma2_3_1c"))))


colors_traces_1 <- colors_traces[c(5,8,11,4,7,10,3,6,9)]


p1 <- ggplot(traces_1_long, aes(x = value, y=model)) +
  geom_density_ridges(scale=1.2, alpha=0.8, show.legend = FALSE, 
                      ### The only change is here
                      aes(fill = par),
                      bandwidth = 0.075) +
  scale_color_manual(values = colors_traces_1) +   
  scale_fill_manual(values = colors_traces_1) + 
  theme_classic() +
  ylab("") +
  xlab(TeX("Diffusion variance $\\sigma^2$")) +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14)) +
  scale_x_continuous(breaks=c(0,1,2)) +
  coord_cartesian(xlim=c(0.0, 2.5))

p1

#######################
# Comparison to SI-OU #
#######################

traces_2 <- list(tibble(.rows = 20002))
traces_2[[1]]$sigma2_2a <- trace_2a[[1]]$sigma2
traces_2[[1]]$sigma2_2b <- trace_2b[[1]]$`sigma2[1]`
traces_2[[1]]$alpha_2a <- trace_2a[[1]]$alpha
traces_2[[1]]$alpha_2b <- trace_2b[[1]]$`alpha[1]`
traces_2[[1]]$theta_2a <- trace_2a[[1]]$theta
traces_2[[1]]$theta_2b <- trace_2b[[1]]$`theta[1]`


traces_2_long <- traces_2[[1]] %>% 
  pivot_longer(cols=c("sigma2_2a", "sigma2_2b",
                      "theta_2a", "theta_2b",
                      "alpha_2a", "alpha_2b"),
               names_to = "par", values_to = "value") %>% 
  mutate(model = ifelse(endsWith(par, "2a"), "SI-OU", "SD-OU"),
         model = factor(model, levels = c("SD-OU", "SI-OU")))


colors_traces_2 <- colors_traces[c(3,2)]

# alpha
p2_alpha <- traces_2_long %>% 
  filter(par %in% c("alpha_2a", "alpha_2b")) %>% 
  ggplot(aes(x = value, y=model)) +
  geom_density_ridges(scale=1.2, alpha=0.8, show.legend = FALSE, 
                      aes(fill = model),
                      bandwidth = 0.075) +
  scale_color_manual(values = colors_traces_2) +   
  scale_fill_manual(values = colors_traces_2) + 
  theme_classic() +
  ylab("") +
  xlab(TeX("Rate of attraction $\\alpha$")) +
  scale_x_continuous(breaks=c(0,1,2)) +
  coord_cartesian(xlim=c(0.0, 2.5)) +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14))
p2_alpha

# sigma2
p2_sigma2 <- traces_2_long %>% 
  filter(par %in% c("sigma2_2a", "sigma2_2b")) %>% 
  ggplot(aes(x = value, y=model)) +
  geom_density_ridges(scale=1.2, alpha=0.8, show.legend = FALSE, 
                      aes(fill = model),
                      bandwidth = 0.1) +
  scale_color_manual(values = colors_traces_2) +   
  scale_fill_manual(values = colors_traces_2) + 
  theme_classic() +
  ylab("") +
  xlab(TeX("Diffusion variance $\\sigma^2$")) +
  coord_cartesian(xlim=c(0.0, 3)) +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14))
p2_sigma2

# theta
p2_theta <- traces_2_long %>% 
  filter(par %in% c("theta_2a", "theta_2b")) %>% 
  ggplot(aes(x = value, y=model)) +
  geom_density_ridges(scale=1.2, alpha=0.8, show.legend = FALSE, 
                      aes(fill = model),
                      bandwidth = 0.05) +
  scale_color_manual(values = colors_traces_2) +   
  scale_fill_manual(values = colors_traces_2) + 
  theme_classic() +
  ylab("") +
  xlab(TeX("Optimum $\\theta$")) +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14))
p2_theta



#######################
# Comparison to SI-BM #
#######################
traces_3 <- list(tibble(.rows = 20002))
traces_3[[1]]$sigma2_3a <- trace_3a[[1]]$sigma2
traces_3[[1]]$sigma2_3b <- trace_3b[[1]]$`sigma2[1]`
traces_3[[1]]$sigma2_3c <- trace_3c[[1]]$`sigma2[1]`
traces_3[[1]]$sigma2_3d <- trace_3d[[1]]$sigma2

traces_3_summary <- traces_3[[1]] %>%
  pivot_longer(cols=c("sigma2_3a", "sigma2_3b",
                      "sigma2_3c", "sigma2_3d"),
               names_to = "model", values_to = "value") %>% 
  group_by(model) %>% 
  summarise(hdi(value)[2],
            hdi(value)[3]) %>% 
  arrange(model)

traces_3_long <- traces_3[[1]] %>% 
  pivot_longer(cols=c("sigma2_3a", "sigma2_3b",
                      "sigma2_3c", "sigma2_3d"
  ),
  names_to = "model", values_to = "value") %>% 
  mutate(model = ifelse(model == "sigma2_3a", "SI-BM", model),
         model = ifelse(model == "sigma2_3b", "SD-OU", model),
         model = ifelse(model == "sigma2_3c", "SD-BM", model),
         model = ifelse(model == "sigma2_3d", "SI-OU", model),
         model = factor(model, levels = c("SD-OU", "SI-OU", "SD-BM", "SI-BM")))

colors_traces_3 <- colors_traces[c(3,2,9,1)]
names(colors_traces_3) <- c("SD-OU", "SI-OU", "SD-BM", "SI-BM")
  
p3 <- traces_3_long %>% 
  ggplot(aes(y = value, x=model, fill=model)) +
  geom_half_violin(draw_quantiles = 0.5, position=position_dodge(width=0.8),
                   linewidth=0.25, width=1.5) +
  scale_fill_manual(values = colors_traces_3) + 
  scale_x_discrete(labels=c("SD-OU", "SI-OU", "SD-BM", "SI-BM")) +
  scale_y_continuous(breaks=c(1,3,5)) +
  theme_classic() +
  ylab("") +
  xlab("") +
  ggtitle(TeX("Diffusion variance $\\sigma^2$")) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(ylim=c(1,5))

# obtain x-values of each half-violin element
p3_data <- ggplot_build(p3)$data[[1]]
unique(p3_data$x)

# combine the start and end of each horizontal line for the HPD interval
traces_3_summary$x_start <- unique(p3_data$x) - 0.025
traces_3_summary$x_end   <- unique(p3_data$x)

# add HPD interval to plot
p3 <- p3 +
  geom_segment(data = traces_3_summary, 
               aes(x = x_start, y = CI_low, xend = x_end, yend = CI_low)) +
  geom_segment(data = traces_3_summary, 
               aes(x = x_start, y = CI_high, xend = x_end, yend = CI_high))

p3


# sigma2
#p3 <- traces_3_long %>% 
#  ggplot(aes(x = value, y=model)) +
#  geom_density_ridges(scale=1.2, alpha=0.8, show.legend = FALSE, 
#                      ### The only change is here
#                      aes(fill = model),
#                      bandwidth = 0.05) +
#  scale_color_manual(values = colors_traces_3) +   
#  scale_fill_manual(values = colors_traces_3) + 
#  theme_classic() +
#  ylab("") +
#  xlab(TeX("Diffusion variance $\\sigma^2$")) +
#  #coord_cartesian(xlim=c(0.3, 2)) +
#  theme(axis.title = element_text(size = 16),
#        axis.text = element_text(size = 14))
#p3


########################
# stick plots together #
########################

p0w <- ggplot() +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))
p0w2 <- ggplot() +
  theme(panel.background = element_rect(fill = 'white', colour = 'white'))



val_case1_row1 <- plot_grid(p0w) +
  draw_plot_label(label=c("Comparison to state-dependent BM models"),
                  x=0.5,
                  y=0.5,
                  hjust=.5, vjust=.5, size=16)

val_case1 <- cowplot::plot_grid(val_case1_row1, p1, ncol=1, rel_heights = c(0.1, 0.9))

val_cases23 <- cowplot::plot_grid(p2_alpha, p2_sigma2, p0w2, p0w2, p2_theta, p3, ncol=2,
                                  labels=c("(a)", "(b)", "", "", "(c)", "(d)"),
                                  rel_heights = c(10,1,10))



ggsave("figures/validation_compare_SDBM.pdf", val_case1, width=5, height=5, units="in")
ggsave("figures/validation_compare_SIOU_SIBM.pdf", val_cases23, width=7, height=6, units="in")


