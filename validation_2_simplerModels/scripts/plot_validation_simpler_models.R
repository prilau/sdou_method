library(ape)
library(cowplot)
library(ggplot2)
library(ggridges)
library(khroma)
library(latex2exp)
library(RevGadgets)
library(tidyverse)
source("../utility/functions.R")

trace_1a <- readTrace(path = "output/1a_state_independent_BM.log", burnin = 0.0)
trace_1b <- readTrace(path = "output/1b_state_dependent_OU.log", burnin = 0.0)
trace_1c <- readTrace(path = "output/1c_state_dependent_BM.log", burnin = 0.0)
trace_1d <- readTrace(path = "output/1d_state_independent_OU.log", burnin = 0.0)
trace_2a <- readTrace(path = "output/2a_state_independent_OU.log", burnin = 0.0)
trace_2b <- readTrace(path = "output/2b_state_dependent_OU.log", burnin = 0.0)
trace_3a <- readTrace(path = "output/3a_state_dependent_BM.log", burnin = 0.0)
trace_3b <- readTrace(path = "output/3b_MuSSCRat.log", burnin = 0.0)
trace_3c <- readTrace(path = "output/3c_state_dependent_OU.log", burnin = 0.0)

colors_traces <- as.character(c(color("high contrast")(3)[-1], color("medium contrast")(6),
                                color("muted")(9)[c(5,6,8)]))
colors_traces <- colors_traces[c(11,10,1,3,6,2,4,7,5,9,8)]



#######################
# Comparison to SI-BM #
#######################
traces_1 <- list(tibble(.rows = 20002))
traces_1[[1]]$sigma2_1a <- trace_1a[[1]]$sigma2
traces_1[[1]]$sigma2_1b <- trace_1b[[1]]$`sigma2[1]`
traces_1[[1]]$sigma2_1c <- trace_1c[[1]]$`sigma2[1]`
traces_1[[1]]$sigma2_1d <- trace_1d[[1]]$sigma2



traces_1_long <- traces_1[[1]] %>% 
  pivot_longer(cols=c("sigma2_1a", "sigma2_1b",
                      "sigma2_1c", "sigma2_1d"
                      ),
               names_to = "model", values_to = "value") %>% 
  mutate(model = ifelse(model == "sigma2_1a", "SI-BM", model),
         model = ifelse(model == "sigma2_1b", "SD-OU", model),
         model = ifelse(model == "sigma2_1c", "SD-BM", model),
         model = ifelse(model == "sigma2_1d", "SI-OU", model),
         model = factor(model, levels = c("SD-OU", "SI-OU", "SD-BM", "SI-BM")))


colors_traces_1 <- colors_traces[c(3,2,9,1)]

# alpha
p1 <- traces_1_long %>% 
  ggplot(aes(x = value, y=model)) +
  geom_density_ridges(scale=1.2, alpha=0.8, show.legend = FALSE, 
                      ### The only change is here
                      aes(fill = model),
                      bandwidth = 0.05) +
  scale_color_manual(values = colors_traces_1) +   
  scale_fill_manual(values = colors_traces_1) + 
  theme_classic() +
  ylab("Posterior density") +
  xlab(TeX("Diffusion variance $\\sigma^2$")) +
  coord_cartesian(xlim=c(0.3, 2))
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
  ylab("Posterior density") +
  xlab(TeX("Rate of attraction $\\alpha$")) +
  scale_x_continuous(breaks=c(0,1,2)) +
  coord_cartesian(xlim=c(0.0, 2.5))
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
  coord_cartesian(xlim=c(0.0, 3))
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
  xlab(TeX("Optimum $\\theta$"))
p2_theta

#######################
# Comparison to SD-BM #
#######################

traces_3 <- list(tibble(.rows = 20002))

traces_3[[1]]$sigma2_1_3a <- trace_3a[[1]]$`sigma2[1]`
traces_3[[1]]$sigma2_1_3b <- trace_3b[[1]]$`sigma2[1]`
traces_3[[1]]$sigma2_1_3c <- trace_3c[[1]]$`sigma2[1]`
traces_3[[1]]$sigma2_2_3a <- trace_3a[[1]]$`sigma2[2]`
traces_3[[1]]$sigma2_2_3b <- trace_3b[[1]]$`sigma2[2]`
traces_3[[1]]$sigma2_2_3c <- trace_3c[[1]]$`sigma2[2]`
traces_3[[1]]$sigma2_3_3a <- trace_3a[[1]]$`sigma2[3]`
traces_3[[1]]$sigma2_3_3b <- trace_3b[[1]]$`sigma2[3]`
traces_3[[1]]$sigma2_3_3c <- trace_3c[[1]]$`sigma2[3]`

traces_3_long <- traces_3[[1]] %>% 
  pivot_longer(cols=c("sigma2_1_3a", "sigma2_1_3b", "sigma2_1_3c",
                      "sigma2_2_3a", "sigma2_2_3b", "sigma2_2_3c",
                      "sigma2_3_3a", "sigma2_3_3b", "sigma2_3_3c"),
               names_to = "par", values_to = "value") %>% 
  mutate(model = ifelse(endsWith(par, "3a"), "SD-BM", ifelse(endsWith(par, "3b"), "MuSSCRat", "SD-OU")),
         model = factor(model, levels = c("SD-OU", "MuSSCRat", "SD-BM")),
         par = factor(par, levels = rev(c("sigma2_1_3a", "sigma2_1_3b", "sigma2_1_3c",
                                          "sigma2_2_3a", "sigma2_2_3b", "sigma2_2_3c",
                                          "sigma2_3_3a", "sigma2_3_3b", "sigma2_3_3c"))))


colors_traces_3 <- colors_traces[c(5,8,11,4,7,10,3,6,9)]


p3 <- ggplot(traces_3_long, aes(x = value, y=model)) +
  geom_density_ridges(scale=1.2, alpha=0.8, show.legend = FALSE, 
                      ### The only change is here
                      aes(fill = par),
                      bandwidth = 0.075) +
  scale_color_manual(values = colors_traces_3) +   
  scale_fill_manual(values = colors_traces_3) + 
  theme_classic() +
  ylab("") +
  xlab(TeX("Diffusion variance $\\sigma^2$")) +
  #theme(axis.ticks.x = element_blank(),
  #      axis.text.x = element_blank()
  #      ) +
  scale_x_continuous(breaks=c(0,1,2)) +
  coord_cartesian(xlim=c(0.0, 2.5))

p3

## dummy plot with all color variables
#bm1[[1]]$bm <- bm1[[1]]$sigma2
#bm1[[1]]$sdbm <- bm1[[1]]$sigma2_sdou
#bm1[[1]]$ou <- bm1[[1]]$sigma2_ou
#bm1[[1]]$sdou <- bm1[[1]]$sigma2_sdbm
#bm1[[1]]$musscrat <- sdbm2020[[1]]$`sigma2s[1]`
#
#
#colors_all <- c('#66CCEE', '#EE6677', '#228833', '#4477AA', '#CCBB44') 
#names(colors_all) <- c("bm", "sdbm", "ou", "sdou", "musscrat")
#
#plot_dummy <- plotTrace(trace = bm1,
#                        color = colors_all,
#                        vars = c("bm", "sdbm", "ou", "sdou", "musscrat"))[[1]]
#
#legend <- get_legend2(plot_dummy + theme(legend.position = "left",
#                       legend.box.margin = margin(0, 0, 0, 12))
#                      + scale_color_manual(values=c('#66CCEE', '#EE6677', '#228833', '#4477AA', '#CCBB44'), 
#                                           name="Model",
#                                           labels=c("State-independent BM", "State-dependent BM",
#                                                    "State-independent OU", "State-dependent OU", "MuSSCRat"))
#                      + guides(size = "none",
#                                color = guide_legend(override.aes = list(size = 3),
#                                                     title='Model'),
#                                fill=guide_legend(title='Model')))
#
p0w <- ggplot() +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))

val_row1 <- plot_grid(p0w, p0w) +
  draw_plot_label(label=c("Comparison to state-independent BM model",
                          "Comparison to state-dependent BM models"),
                  x=c(0.25, 0.75),
                  y=0.5,
                  hjust=.5, vjust=.5, size=12)
val_row2 <- cowplot::plot_grid(p1, p3, ncol=2, labels=c("(a)", "(b)"))
val_row3 <- plot_grid(p0w) +
  draw_plot_label(label=c("Comparison to state-independent OU model"),
                  x=c(0.5),
                  y=c(0.5),
                  hjust=.5, vjust=.5, size=12)
val_row4 <- plot_grid(p2_alpha, p2_sigma2, p2_theta, ncol=3,
                      labels=c("(c)", "(d)", "(e)"))


val_all <- plot_grid(val_row1, val_row2, val_row3, val_row4,
                     rel_heights = c(1,5,1,5), ncol=1)

val_all

ggsave("figures/compare_simple_models.pdf", val_all, width=8, height=6, units="in")
