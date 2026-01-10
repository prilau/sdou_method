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
               names_to = "parameter", values_to = "value") %>%
  mutate(model = ifelse(endsWith(parameter, "1a"), "SD-BM", ifelse(endsWith(parameter, "1b"), "MuSSCRat", "SD-OU")),
         model = factor(model, levels = c("SD-OU", "MuSSCRat", "SD-BM")),
         par = factor(parameter, levels = rev(c("sigma2_1_1a", "sigma2_1_1b", "sigma2_1_1c",
                                          "sigma2_2_1a", "sigma2_2_1b", "sigma2_2_1c",
                                          "sigma2_3_1a", "sigma2_3_1b", "sigma2_3_1c"))))

trace_1_summary <- traces_1_long %>%
  group_by(model, parameter) %>%
  summarise(hdi(value)[2],
            hdi(value)[3]) %>%
  arrange(parameter, model)

colors_traces_1 <- colors_traces[c(5,8,11,4,7,10,3,6,9)]

p1 <- ggplot(traces_1_long, aes(x = value, y=model)) +
  geom_density_ridges(scale=1.2, alpha=0.8, show.legend = FALSE,
                      ### The only change is here
                      aes(fill = par),
                      bandwidth = 0.175) +
  scale_color_manual(values = colors_traces_1) +
  scale_fill_manual(values = colors_traces_1) +
  theme_classic() +
  ylab("") +
  xlab(TeX("State-dependent diffusion variance $\\sigma^2_k$")) +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14)) +
  #scale_x_continuous(breaks=c(0,1,2)) +
  coord_cartesian(xlim=c(0.5, 11))

p1_data <- ggplot_build(p1)$data[[1]]
unique(p1_data$y)

# combine the start and end of each horizontal line for the HPD interval
trace_1_summary$y_start <- c(unique(p1_data$y) - 0.2, unique(p1_data$y), unique(p1_data$y) + 0.2)
trace_1_summary$y_end   <- c(unique(p1_data$y) + 0.5, unique(p1_data$y) + 0.7, unique(p1_data$y) + 0.9)

# add HPD interval to plot
p1 <- p1 +
  geom_segment(data = trace_1_summary,
               aes(y = y_start, x = CI_low, yend = y_end, xend = CI_low)) +
  geom_segment(data = trace_1_summary,
               aes(y = y_start, x = CI_high, yend = y_end, xend = CI_high))


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
               names_to = "parameter", values_to = "value") %>%
  mutate(model = ifelse(endsWith(parameter, "2a"), "SI-OU", "SD-OU"),
         model = factor(model, levels = c("SD-OU", "SI-OU")))

trace_2_summary <- traces_2_long %>%
  group_by(model, parameter) %>%
  summarise(hdi(value)[2],
            hdi(value)[3]) %>%
  arrange(parameter, model)


colors_traces_2 <- colors_traces[c(3,2)]

# alpha
p2_alpha <- traces_2_long %>%
  filter(parameter %in% c("alpha_2a", "alpha_2b")) %>%
  ggplot(aes(x = value, y=model)) +
  geom_density_ridges(scale=1.2, alpha=0.8, show.legend = FALSE,
                      aes(fill = model),
                      bandwidth = 0.05) +
  scale_color_manual(values = colors_traces_2) +
  scale_fill_manual(values = colors_traces_2) +
  theme_classic() +
  ylab("") +
  xlab(TeX("Rate of attraction $\\alpha$")) +
  scale_x_continuous(breaks=c(0,1,2)) +
  coord_cartesian(xlim=c(0.0, 2.5)) +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14))

p2_alpha_data <- ggplot_build(p2_alpha)$data[[1]]
unique(p2_alpha_data$y)

# combine the start and end of each horizontal line for the HPD interval
trace_2_alpha_summary <- trace_2_summary %>% filter(grepl(pattern = "alpha", parameter))
trace_2_alpha_summary$y_start <- unique(p2_alpha_data$y) - 0.2
trace_2_alpha_summary$y_end   <- unique(p2_alpha_data$y) + 0.5

# add HPD interval to plot
p2_alpha <- p2_alpha +
  geom_segment(data = trace_2_alpha_summary,
               aes(y = y_start, x = CI_low, yend = y_end, xend = CI_low)) +
  geom_segment(data = trace_2_alpha_summary,
               aes(y = y_start, x = CI_high, yend = y_end, xend = CI_high))


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
  coord_cartesian(xlim=c(1, 7)) +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14))

p2_sigma2_data <- ggplot_build(p2_sigma2)$data[[1]]
unique(p2_sigma2_data$y)

# combine the start and end of each horizontal line for the HPD interval
trace_2_sigma2_summary <- trace_2_summary %>% filter(grepl(pattern = "sigma2", parameter))
trace_2_sigma2_summary$y_start <- unique(p2_sigma2_data$y) - 0.2
trace_2_sigma2_summary$y_end   <- unique(p2_sigma2_data$y) + 0.5

# add HPD interval to plot
p2_sigma2 <- p2_sigma2 +
  geom_segment(data = trace_2_sigma2_summary,
               aes(y = y_start, x = CI_low, yend = y_end, xend = CI_low)) +
  geom_segment(data = trace_2_sigma2_summary,
               aes(y = y_start, x = CI_high, yend = y_end, xend = CI_high))


p2_sigma2

# theta
p2_theta <- traces_2_long %>%
  filter(par %in% c("theta_2a", "theta_2b")) %>%
  ggplot(aes(x = value, y=model)) +
  geom_density_ridges(scale=1.2, alpha=0.8, show.legend = FALSE,
                      aes(fill = model),
                      bandwidth = 0.1) +
  scale_color_manual(values = colors_traces_2) +
  scale_fill_manual(values = colors_traces_2) +
  theme_classic() +
  ylab("") +
  xlab(TeX("Optimum $\\theta$")) +
  coord_cartesian(xlim=c(0, 6)) +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14))

p2_theta_data <- ggplot_build(p2_theta)$data[[1]]
unique(p2_theta_data$y)

# combine the start and end of each horizontal line for the HPD interval
trace_2_theta_summary <- trace_2_summary %>% filter(grepl(pattern = "theta", parameter))
trace_2_theta_summary$y_start <- unique(p2_theta_data$y) - 0.2
trace_2_theta_summary$y_end   <- unique(p2_theta_data$y) + 0.5

# add HPD interval to plot
p2_theta <- p2_theta +
  geom_segment(data = trace_2_theta_summary,
               aes(y = y_start, x = CI_low, yend = y_end, xend = CI_low)) +
  geom_segment(data = trace_2_theta_summary,
               aes(y = y_start, x = CI_high, yend = y_end, xend = CI_high))


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

# sigma2
p3 <- traces_3_long %>%
  ggplot(aes(x = value, y=model)) +
  geom_density_ridges(scale=1.2, alpha=0.8, show.legend = FALSE,
                      ### The only change is here
                      aes(fill = model),
                      bandwidth = 0.075) +
  scale_color_manual(values = colors_traces_3) +
  scale_fill_manual(values = colors_traces_3) +
  theme_classic() +
  ylab("") +
  xlab(TeX("Diffusion variance $\\sigma^2$")) +
  coord_cartesian(xlim=c(1, 6)) +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14))

# obtain x-values of each half-violin element
p3_data <- ggplot_build(p3)$data[[1]]
unique(p3_data$y)

# combine the start and end of each horizontal line for the HPD interval
traces_3_summary$y_start <- unique(p3_data$y) - 0.2
traces_3_summary$y_end   <- unique(p3_data$y) + 0.5

# add HPD interval to plot
p3 <- p3 +
  geom_segment(data = traces_3_summary,
               aes(y = y_start, x = CI_low, yend = y_end, xend = CI_low)) +
  geom_segment(data = traces_3_summary,
               aes(y = y_start, x = CI_high, yend = y_end, xend = CI_high))

p3


########################
# stick plots together #
########################

p0w <- ggplot() +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))
p0w2 <- ggplot() +
  theme(panel.background = element_rect(fill = 'white', colour = 'white'))



#val_case1_row1 <- plot_grid(p0w) +
#  draw_plot_label(label=c("Comparison to state-dependent BM models"),
#                  x=0.5,
#                  y=0.5,
#                  hjust=.5, vjust=.5, size=16)

#val_case1 <- cowplot::plot_grid(val_case1_row1, p1, ncol=1, rel_heights = c(0.1, 0.9))

val_cases23 <- cowplot::plot_grid(p2_alpha, p2_sigma2, p0w2, p0w2, p2_theta, p3, ncol=2,
                                  labels=c("(a)", "(b)", "", "", "(c)", "(d)"),
                                  rel_heights = c(10,1,10))



ggsave("figures/validation_compare_SDBM.pdf", p1, width=5.5, height=4, units="in")
ggsave("figures/validation_compare_SIOU_SIBM.pdf", val_cases23, width=7, height=6, units="in")
