# Code archive for state-dependent OU models
```
├── README.md          # Project documentation
├── hypsodonty/                                           # Case study
│   ├── data/            
│   │   ├── artiodactyla_Continuous.csv                   # hypsodonty index of 82 ruminant species
│   │   ├── artiodactyla_Continuous.nex                   # hypsodonty index of 82 ruminant species in nexus format
│   │   ├── artiodactyla_Discrete.nex                     # feeding behavior of 82 ruminant species in nexus format
│   │   ├── artiodactyla_rescaled.tree                    # timetree of 82 ruminant species in the unit of tree height
│   │   ├── artiodactyla.tree                             # timetree of 82 ruminant species in the unit of million years
│   │   └── raw
│   │       ├── cetartiodactyla_gtr.tre                   # timetree obtained from Toljagić et al. (2018)
│   │       └── ruminants.txt                             # trait data curated by Toljagić et al. (2018)
│   └── scripts/            
│       ├── 00_preprocessing.R                            # R script to transform raw data to suitable input formats
│       ├── 01_mcmc_ase_SCM.Rev                           # Rev script to run stochastic character mapping
│       ├── 01a_postprocessing.R                          # R script to get character histories for 03_mcmc_sdou_seq_MAP.Rev and 03_mcmc_sdou_seq_r100.Rev
│       ├── 02_mcmc_ase_DA.Rev                            # Rev script to run data augmentation-based CTMC model
│       ├── 02a_postprocessing.R                          # R script to convert output datatype
│       ├── 02b_postprocessing.Rev                        # Rev script to summarize ancestral states
│       ├── 03_mcmc_sdou_seq_MAP.Rev                      # Rev script to run state-dependent OU model with fixed character history
│       ├── 03_mcmc_sdou_seq_r100.Rev                     # Rev script to run state-dependent OU model with 100 character histories
│       ├── 03a_postprocessing.R                          # R script to combine traces in 03_mcmc_sdou_seq_r100.Rev
│       ├── 04_mcmc_sdou_joint.Rev                        # Rev script to run state-dependent OU model with stochastic character history
│       ├── 04a_postprocessing.R                          # R script to convert output datatype
│       ├── 04b_postprocessing.Rev                        # Rev script to summarize ancestral states
│       ├── 05_plot_map.R                                 # R script to plot ancestral state maps and character history maps
│       ├── 05a_plot_ou.R                                 # R script to plot posterior distributions of state-dependent OU model parameters
│       ├── 06_plot_supp_convergence.R                    # R script to plot convergence of analyses
│       ├── 06a_plot_supp_state_probabilities.R           # R script to plot the posterior state probabilities of analyses
│       └── 06b_plot_supp_annotation.R                    # R script to plot phylogeny with tip labels
├── utility/
│   └── functions.R
├── validation_1_algorithm/
    ├── data/            
│   │   ├── artiodactyla_Continuous.csv
│   │   ├── artiodactyla_Continuous.nex
│   │   └── artiodactyla_rescaled.tree
│   └── scripts/            
│       ├── 00_preprocessing.R                            # R script to simulate the data
│       ├── 01_calculate_likelihoods.Rev                  # Rev script to calculate the likelihoods in RevBayes
│       ├── 01a_postprocessing.R                          # R script to convert the output datatype
│       ├── 02_calculate_likelihoods.R                    # R script to calculate the likelihoods in R
│       └── 03_plot.R                                     # R script to plot the likelihoods
├── validation_2_simplerModels/
    ├── data/            
│   │   ├── artiodactyla_Continuous.nex
│   │   ├── artiodactyla_Discrete.nex
│   │   └── artiodactyla_rescaled.tree
│   └── scripts/            
│       ├── 1a_mcmc_state_independent_BM.Rev               # Rev script to run the state-independent BM model
│       ├── 1b_mcmc_state_dependent_OU.Rev                 # Rev script to run the collapsed state-dependent OU model
│       ├── 1c_mcmc_state_dependent_BM.Rev                 # Rev script to run the collapsed state-dependent BM model
│       ├── 1d_mcmc_state_independent_OU.Rev               # Rev script to run the collapsed state-independent OU model
│       ├── 2a_mcmc_state_independent_OU.Rev               # Rev script to run the state-independent OU model
│       ├── 2b_mcmc_state_dependent_OU.Rev                 # Rev script to run the collapsed state-dependent OU model
│       ├── 3a_mcmc_state_dependent_BM.Rev                 # Rev script to run the state-dependent BM model
│       ├── 3b_mcmc_MuSSCRat.Rev                           # Rev script to run the MuSSCRat model
│       ├── 3c_mcmc_state_dependent_OU.Rev                 # Rev script to run the collapsed state-dependent OU model
│       └── plot_validation_simpler_models.R               # R script to plot the posterior distributions
├── validation_3_simulationBased/
    ├── data/            
│   │   ├── artiodactyla_Continuous.nex
│   │   ├── artiodactyla_Discrete.nex
│   │   ├── artiodactyla_character_history.tree
│   │   ├── artiodactyla_rescaled.tree
│   │   ├── artiodactyla_sim.tree
│   │   └── artiodactyla_sim_character_history.tree
│   └── scripts/            
│       ├── remove_ctmc_output.sh
│       ├── rtEq_rfPa.Rev                                 # Rev script to run simulation-based validation with root treatment = Equilibrium and root frequencies estimated
│       ├── rtEq_rfSt.Rev                                 # Rev script to run simulation-based validation with root treatment = Equilibrium and root frequencies at stationarity
│       ├── rtOp_rfPa.Rev                                 # Rev script to run simulation-based validation with root treatment = Optimum and root frequencies estimated
│       ├── rtOp_rfSt.Rev                                 # Rev script to run simulation-based validation with root treatment = Optimum and root frequencies at stationarity
│       ├── rtPa_rfPa.Rev                                 # Rev script to run simulation-based validation with root treatment = Parameter and root frequencies estimated
│       ├── rtPa_rfSt.Rev                                 # Rev script to run simulation-based validation with root treatment = Parameter and root frequencies at stationarity
│       ├── seq_rtEq_rfPa.Rev                             # Rev script to run simulation-based validation with fixed single character history and root treatment = Equilibrium
│       ├── seq_rtOp_rfPa.Rev                             # Rev script to run simulation-based validation with fixed single character history and root treatment = Optimum
│       ├── seq_rtPa_rfPa.Rev                             # Rev script to run simulation-based validation with fixed single character history and root treatment = Parameter
│       ├── sim_nonultrametric.R                          # R script to simulate non-ultrametric phylogeny and character history for root treatment = Parameter
│       └── validation_plots.R                            # R script to plot validation reults
└── validation_4_speed/
    ├── data/            
    └── scripts/            
        ├── 01_simulate.R                                 # R script to simulate data of different tree sizes
        ├── 02_computeSpeed_smallPhylo.Rev                # Rev script to compute overall speed for small phylogenies
        ├── 02a_computeSpeed_largePhylo.Rev               # Rev script to compute overall speed for large phylogenies
        ├── 03_overhead_smallPhylo.Rev                    # Rev script to compute overhead speed for small phylogenies
        ├── 03a_overhead_largePhylo.Rev                   # Rev script to compute overhead speed for large phylogenies
        ├── 04_plot.R                                     # R script to plot the net speed
        └── combine_charhist.sh                           # bash script to combine character histories as inputs
```
