processValidation <- function(analysis_name, n_reps = 1000, n_bins = 50) {
  library(coda)
  
  if (missing(analysis_name)) {
    stop("Missing required argument: Analysis Name")
  }
  
  # Define directories
  results_dir = paste0("results/", analysis_name)
  output_dir = paste0("output/", analysis_name)
  
  # create directories
  dir.create(results_dir, showWarnings = FALSE)
  
  # function to read data from the file 
  read_data <- function(file_path) {
    if (file.exists(file_path)) {
      return(read.table(file_path, 
                        sep = "\t", 
                        header = TRUE, 
                        skip = 0, 
                        check.names = FALSE))
    } else {
      return(NULL)
    }
  }
  
  # get the list of parameters 
  parameters <- colnames(read_data(paste0(output_dir, 
                                          "/Validation_Sim_0/posterior_samples.var")))
  parameters <- parameters[-1]   # remove the first column ("Iteration")
  
  # exclude "branch_rates" if present 
  parameters <- parameters[!(parameters %in% c("rf", "rates", "branch_rates"))]
  
  # Print list of parameters 
  cat("parameters:\n", parameters, "\n\n")
  
  # Iterate over each parameter
  for (param in parameters) {
    # initialize variables 
    coverage_probs <- data.frame(total_count = numeric(0), 
                                 in_count = numeric(0), 
                                 hpd_width = numeric(0), 
                                 stringsAsFactors = FALSE)
    hpd_width <- seq(from = 0.0, to = 1.0, by = 1/n_bins)
    
    for (i in 1:(n_bins+1)) {
      coverage_probs[i,] = c(total_count = 0, in_count = 0, hpd_width = hpd_width[i])
    }
    
    # initialize progress bar 
    pb <- txtProgressBar(min = 0, max = n_reps, char = "*", style = 3)
    
    # iterate over each replication 
    for (i in 1:n_reps) {
      setTxtProgressBar(pb, i)
      
      # read in the data 
      data <- read_data(paste0(output_dir, 
                               "/Validation_Sim_", 
                               i-1, 
                               "/posterior_samples.var"))
      if (is.null(data)) next
      
      # extract samples 
      num_samples = length(data[,1])
      
      x <- as.mcmc(data[round(0.25*num_samples):num_samples, param])
      
      true_val_ext <- ifelse(param == "branch_rates", ".out", ".txt")
      true_val <- read.table(file=paste0(output_dir, 
                                         "/Validation_Sim_", 
                                         i-1, 
                                         "/", 
                                         param, 
                                         true_val_ext))[1,1]
      
      # calculate coverage probabilities 
      for (k in 1:(n_bins + 1)) {
        hpd <- HPDinterval(x, prob = hpd_width[k])
        if (true_val >= hpd[1,1] && true_val <= hpd[1,2]){
          coverage_probs[k, "in_count"] <- coverage_probs[k, "in_count"] + 1
        }
        coverage_probs[k, "total_count"] <- coverage_probs[k, "total_count"] + 1
      }      
    }
    close(pb)
    
    # calculate frequency of coverage
    coverage_probs$freq = coverage_probs$in_count / coverage_probs$total_count
    
    # save coverage probabilities 
    saveRDS(coverage_probs, file = paste0(results_dir, "/", param, ".rds"))
    
    # print results to the screen 
    cat(param,"\n")
    cat("HPD-width:\t\t",hpd_width,"\n")
    cat("Coverage-freq:\t\t",coverage_probs$freq,"\n")
  }
}

generate_coverage_plots <- function(analysis_name, 
                                    num_reps = 1000, 
                                    num_bins = 50, 
                                    results_dir, 
                                    figs_dir) {
  library(coda)
  library(ggplot2)
  
  # Create directories if they don't exist
  dir.create(figs_dir, showWarnings = FALSE)
  
  parameters <- gsub(".rds", "", list.files(results_dir))
  
  # Iterate over each parameter
  plots <- list()
  i = 1
  for (param in parameters) {
    # Read coverage probabilities
    coverage_probs <- readRDS(file = paste0(results_dir, "/", param, ".rds"))
    
    # Generate plot
    p <- ggplot(coverage_probs) +
      geom_bar(stat="identity", aes(x=hpd_width, y=freq), colour="lightgray", fill="lightgray") +
      theme_classic() +
      xlab("HPD width") + ylab("coverage probability") + ggtitle(param) +
      geom_segment(aes(x=0, y=0, xend=1, yend=1), linetype="dashed", size=1.5, show.legend=FALSE) +
      theme(legend.position="none", plot.title = element_text(hjust = 0.5))
    
    # Save plot
    plots[[i]] <- p
    i=i+1
    ggsave(paste0(figs_dir, "/hpd_width_vs_coverage_", param, ".pdf"), plot=p, width=10, height=10, units="cm")
  }
  return(plots)
}



processValidation("validation_rtOptimum_rfStationary")
p1s <- generate_coverage_plots("validation_rtOptimum_rfStationary",
                        results_dir = "results/validation_rtOptimum_rfStationary",
                        figs_dir = "figures/validation_rtOptimum_rfStationary")

processValidation("validation_rtEquilibrium_rfStationary")
p2s <- generate_coverage_plots("validation_rtEquilibrium_rfStationary",
                        results_dir = "results/validation_rtEquilibrium_rfStationary",
                        figs_dir = "figures/validation_rtEquilibrium_rfStationary")

processValidation("validation_rtParameter_rfStationary")
p3s <- generate_coverage_plots("validation_rtParameter_rfStationary",
                        results_dir = "results/validation_rtParameter_rfStationary",
                        figs_dir = "figures/validation_rtParameter_rfStationary")

library(cowplot)
p1_all <- plot_grid(p1s[[1]], p1s[[2]], p1s[[3]],
                    p1s[[5]], p1s[[6]], p1s[[7]],
                    p1s[[8]], p1s[[9]], p1s[[10]],
                    p1s[[4]], ncol=3)
ggsave(paste0("figures/validation_rtOptimum_rfStationary/hpd_width_vs_coverage.pdf"),
       plot=p1_all, width=6, height=8, units="in")

p2_all <- plot_grid(p2s[[1]], p2s[[2]], p2s[[3]],
                    p2s[[5]], p2s[[6]], p2s[[7]],
                    p2s[[8]], p2s[[9]], p2s[[10]],
                    p2s[[4]], ncol=3)
ggsave(paste0("figures/validation_rtEquilibrium_rfStationary/hpd_width_vs_coverage.pdf"),
       plot=p2_all, width=6, height=8, units="in")

p3_all <- plot_grid(p3s[[1]], p3s[[2]], p3s[[3]],
                    p3s[[5]], p3s[[6]], p3s[[7]],
                    p3s[[8]], p3s[[9]], p3s[[10]],
                    p3s[[4]], p3s[[11]], ncol=3)
ggsave(paste0("figures/validation_rtParameter_rfStationary/hpd_width_vs_coverage.pdf"),
       plot=p3_all, width=6, height=8, units="in")
