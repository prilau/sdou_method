library(ape)
library(phytools)
library(pracma)
library(tibble) 
library(tidyverse)
library(ggdist)
source("../utility/functions.R")


# simulate trees of different sizes
num_tips   = c(5e2, 1e3, 5e3, 1e4, 2.5e4, 5e4, 1e5)
reps       = 10


time_log <- tibble(num_tips=NA, avg_rep_time=NA, avg_overhead_time=NA, .rows = 700)

for (i in 1:length(num_tips)){
  temp <- read_file(paste0("output/n_", num_tips[i], "_per_rep_time.txt"))
  temp <- gsub("\\[ ", "", temp)
  temp <- gsub(" \\]", "", temp)
  temp <- strsplit(temp, ", ")[[1]]
  temp2 <- read_file(paste0("output/n_", num_tips[i], "_per_rep_time_overhead.txt"))
  temp2 <- gsub("\\[ ", "", temp2)
  temp2 <- gsub(" \\]", "", temp2)
  temp2 <- strsplit(temp2, ", ")[[1]]
  
  time_log$num_tips[((i-1)*100+1):(i*100)] <- num_tips[i]
  time_log$avg_rep_time[((i-1)*100+1):(i*100)] <- as.numeric(temp)
  time_log$avg_overhead_time[((i-1)*100+1):(i*100)] <- as.numeric(temp2)
}

time_log$avg_net_time <- time_log$avg_rep_time - time_log$avg_overhead_time


#############
## plotting #
#############




#
## Calculate intercept and slope of regression line
temp <- time_log %>%
  mutate(tree_size=log(num_tips),
         log_ns = log(avg_net_time)) %>%
  select(tree_size, log_ns)


reg_line <- coef(lm(temp$log_ns ~ temp$tree_size))


p <- temp %>% 
  group_by(tree_size) %>%
  median_qi(.width = c(.75, 1)) %>%
  ggplot(aes(x = tree_size, y = log_ns)) +
  geom_abline(linetype="longdash", alpha=0.3,
              intercept=reg_line[1],
              slope=reg_line[2],
              linewidth=1) + 
  geom_pointinterval(aes(y = log_ns, x = tree_size, ymin = .lower, ymax = .upper),
                     point_size = 2, interval_size_domain=c(0,0.2), interval_size_range=c(0,0.2),
                     interval_color="#6699CC", point_color="#004488") +
  scale_x_continuous(breaks=log(c(500, 1000, 5000, 10000, 25000, 50000, 100000)), label=c("500", "1k", "5k", "10k", "25k", "50k", "100k")) +
  scale_y_continuous(breaks=log(c(1e3, 1e4, 1e5)), label=c("100 ns", expression(paste("1 ", mu, "s")), expression(paste("10 ", mu, "s")))) +
  ylab("Time per calculation") +
  theme_bw() +
  xlab("Tree size") +
  theme(legend.position="none",
        axis.title = element_text(size=16),
        axis.text = element_text(size=12))

p


ggsave("figures/rb_speed.pdf", p, height=4, width=4.5, unit="in")


## remove white space on the sides
#coord_cartesian(xlim = c(1.3, 2.9))