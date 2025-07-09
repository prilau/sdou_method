library(ape)
library(phytools)
library(tidyverse)

# combine traces from r100 analyses
dir_in <- "output/seq_SDOU/"
traces <- list()
trace_files <- c(paste0(dir_in, list.files(dir_in, pattern=paste0("trace_r100_..log"))),
                 paste0(dir_in, list.files(dir_in, pattern=paste0("trace_r100_...log"))),
                 paste0(dir_in, list.files(dir_in, pattern=paste0("trace_r100_....log")))
                 )
for(i in 1:length(trace_files)){
  trace <- read.table(trace_files[i], header = TRUE) %>%
    select(-Iteration)
  traces <- rbind(traces, trace)
}

traces <- traces %>% 
  rename(`alpha[1]` = alpha.1.,
         `alpha[2]` = alpha.2.,
         `alpha[3]` = alpha.3.,
         `sigma2[1]` = sigma2.1.,
         `sigma2[2]` = sigma2.2.,
         `sigma2[3]` = sigma2.3.,
         `theta[1]` = theta.1.,
         `theta[2]` = theta.2.,
         `theta[3]` = theta.3.)

# shuffle rows
set.seed(1083)
traces <- traces[sample(nrow(traces), replace = FALSE),]
set.seed(NULL)
traces <- traces %>% 
  mutate(Iteration = 1:n()) %>% 
  relocate(Iteration, .before = Replicate_ID)

write.table(traces, paste0(dir_in, "trace_r100.log"),
            quote = FALSE, row.names = FALSE, sep = "\t")

# sample rows from each trace file
traces <- list()
nsample <- 500
for(i in 1:length(trace_files)){
  trace <- read.table(trace_files[i], header = TRUE) %>%
    select(-Iteration) %>% sample_n(nsample)
  traces <- rbind(traces, trace)
}

traces <- traces %>% 
  rename(`alpha[1]` = alpha.1.,
         `alpha[2]` = alpha.2.,
         `alpha[3]` = alpha.3.,
         `sigma2[1]` = sigma2.1.,
         `sigma2[2]` = sigma2.2.,
         `sigma2[3]` = sigma2.3.,
         `theta[1]` = theta.1.,
         `theta[2]` = theta.2.,
         `theta[3]` = theta.3.)

# shuffle rows
set.seed(1083)
traces <- traces[sample(nrow(traces), replace = FALSE),]
set.seed(NULL)
traces <- traces %>% 
  mutate(Iteration = 1:n()) %>% 
  relocate(Iteration, .before = Replicate_ID)

write.table(traces, paste0("output/seq_SDOU/trace_r100_sampled.log"),
            quote = FALSE, row.names = FALSE, sep = "\t")
