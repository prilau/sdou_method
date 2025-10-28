library(bayestestR)
library(cowplot)
library(gghalves)
library(latex2exp)
source("../utility/functions.R")

# read traces
seq_map  <- readTrace("output/sdou_seq/trace_MAP.log", burnin = 0.0)
seq_map[[1]]$model <- "seq-MAP"
seq_r100 <- readTrace(path = paste0("output/sdou_seq/trace_r100_sampled.log"), burnin = 0.0)
seq_r100[[1]]$model <- "seq-r100"
joint <- readTrace("output/sdou_joint/trace.log", burnin=0.0)
joint[[1]]$model <- "joint"
joint[[1]] <- joint[[1]] %>%
  select(!starts_with(c("num_changes", "rate", "total", "lambda", "rf", "allowed", "all")))

# combine traces from three models
traces <- rbind(
  seq_map[[1]],
  seq_r100[[1]],
  joint[[1]]) %>% 
  mutate(model = factor(model, levels = c("seq-MAP", "seq-r100", "joint")))

# summarise the 95% highest posterior density interval
trace_summary <- traces %>%
  pivot_longer(cols=c("alpha[1]", "alpha[2]", "alpha[3]",
                      "theta[1]", "theta[2]", "theta[3]",
                      "sigma2[1]", "sigma2[2]", "sigma2[3]"),
               names_to = "parameter", values_to = "value") %>% 
  group_by(model, parameter) %>% 
  summarise(hdi(value)[2],
            hdi(value)[3]) %>% 
  arrange(parameter, model)

# plot alpha
plot_alpha <- traces %>% 
  pivot_longer(cols=c("alpha[1]", "alpha[2]", "alpha[3]"),
               names_to = "parameter", values_to = "value") %>% 
  mutate(parameter = factor(parameter, levels = c("alpha[1]",
                                                  "alpha[2]",
                                                  "alpha[3]"))) %>% 
  ggplot(aes(y = value, x=parameter, fill=model)) +
  geom_half_violin(draw_quantiles = 0.5, position=position_dodge(width=0.8),
                   linewidth=0.25, width=1.5) +
  scale_fill_manual(values = c("seq-MAP"="#CC6677", "seq-r100"="#6699CC", "joint"="#DDCC77")) + 
  scale_x_discrete(labels=c("", "", "")) +
  scale_y_continuous(breaks=c(0,2,4)) +
  theme_classic() +
  ylab("") +
  xlab("") +
  ggtitle(TeX("Rate of attraction $\\alpha$")) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(ylim=c(0,5))

# obtain x-values of each half-violin element
p_alpha_data <- ggplot_build(plot_alpha)$data[[1]]
unique(p_alpha_data$x)

# combine the start and end of each horizontal line for the HPD interval
summary_alpha <- trace_summary %>% filter(parameter %in% c("alpha[1]", "alpha[2]", "alpha[3]"))
summary_alpha$x_start <- unique(p_alpha_data$x) - 0.025
summary_alpha$x_end <- unique(p_alpha_data$x)

# add HPD interval to plot
plot_alpha <- plot_alpha +
  geom_segment(data = summary_alpha, 
               aes(x = x_start, y = CI_low, xend = x_end, yend = CI_low)) +
  geom_segment(data = summary_alpha, 
               aes(x = x_start, y = CI_high, xend = x_end, yend = CI_high))

plot_alpha

# plot theta
plot_theta <- traces %>% 
  pivot_longer(cols=c("theta[1]", "theta[2]", "theta[3]"),
               names_to = "parameter", values_to = "value") %>% 
  mutate(parameter = factor(parameter, levels = c("theta[1]",
                                                  "theta[2]",
                                                  "theta[3]"))) %>% 
  ggplot(aes(y = value, x=parameter, fill=model)) +
  geom_half_violin(draw_quantiles = 0.5, position=position_dodge(width=0.8),
                   linewidth=0.25, width=1.5) +
  scale_fill_manual(values = c("seq-MAP"="#CC6677", "seq-r100"="#6699CC", "joint"="#DDCC77")) + 
  scale_x_discrete(labels=c("", "", "")) +
  theme_classic() +
  ylab("") +
  xlab("") +
  ggtitle(TeX("Optimum $\\theta$")) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(ylim=c(0,8))

p_theta_data <- ggplot_build(plot_theta)$data[[1]]
unique(p_theta_data$x)

summary_theta <- trace_summary %>% filter(parameter %in% c("theta[1]", "theta[2]", "theta[3]"))
summary_theta$x_start <- unique(p_theta_data$x) - 0.025
summary_theta$x_end <- unique(p_theta_data$x)

plot_theta <- plot_theta +
  geom_segment(data = summary_theta, 
               aes(x = x_start, y = CI_low, xend = x_end, yend = CI_low)) +
  geom_segment(data = summary_theta, 
               aes(x = x_start, y = CI_high, xend = x_end, yend = CI_high))

plot_theta

# plot sigma2
plot_sigma2 <- traces %>% 
  pivot_longer(cols=c("sigma2[1]", "sigma2[2]", "sigma2[3]"),
               names_to = "parameter", values_to = "value") %>% 
  mutate(parameter = factor(parameter, levels = c("sigma2[1]",
                                                  "sigma2[2]",
                                                  "sigma2[3]"))) %>% 
  ggplot(aes(y = value, x=parameter, fill=model)) +
  geom_half_violin(draw_quantiles = 0.5, position=position_dodge(width=0.8),
                   linewidth=0.25, width=1.5) + 
  scale_fill_manual(values = c("seq-MAP"="#CC6677", "seq-r100"="#6699CC", "joint"="#DDCC77")) + 
  scale_x_discrete(labels=c("Browsers", "Mixed feeders", "Grazers")) +
  theme_classic() +
  ylab("") +
  xlab("") +
  ggtitle(TeX("Diffusion variance $\\sigma^2$")) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(ylim=c(0,7))

p_sigma2_data <- ggplot_build(plot_sigma2)$data[[1]]
unique(p_sigma2_data$x)

summary_sigma2 <- trace_summary %>% filter(parameter %in% c("sigma2[1]", "sigma2[2]", "sigma2[3]"))
summary_sigma2$x_start <- unique(p_sigma2_data$x) - 0.025
summary_sigma2$x_end <- unique(p_sigma2_data$x)

plot_sigma2<- plot_sigma2 +
  geom_segment(data = summary_sigma2, 
               aes(x = x_start, y = CI_low, xend = x_end, yend = CI_low)) +
  geom_segment(data = summary_sigma2, 
               aes(x = x_start, y = CI_high, xend = x_end, yend = CI_high))
  
plot_sigma2

# grab legend object
legend <- get_legend2(plot_sigma2 + theme(legend.position = "bottom",
                                         legend.box.margin = margin(0, 0, 0, 12)))

# combine all plots and the legend
plot_all <- plot_grid(plot_alpha, plot_theta, plot_sigma2, legend, ncol=1, rel_heights = c(1,1,1,0.3))
plot_all

# save figure
ggsave("figures/case_study_ou.pdf", plot_all, width=5, height=5, units="in")


