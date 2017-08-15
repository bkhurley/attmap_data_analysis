# Analysis stack for attmap_altstim data
#
# 20 OCT 2016 - BH
##############

library(dplyr)

# SPECIFY WHICH EXPERIMENT TO ANALYZE (using analyze_expt):
exp_names = c('attmap_altstim_inc', 'attmap_altstim_intensitydec')
analyze_expt = 2 # change to index which experiment we are analyzing from list above
curr_exp_name = exp_names[analyze_expt]

# load custom functions
source('/experiment_3/attmap_altstim_plots.R')
source('/experiment_3/attmap_altstim_LMM.R')
source('/experiment_3/compare_model_fit.R')
source('/experiment_3/attmap_altstim_analyze_combined_data.R')
source('/experiment_3/attmap_thresh_compare.R')

# store the current directory
start.dir <- getwd()

# specify file paths
data.path <- "/data/attmap/attmap_altstim/tables/"
fig.path <- "/data/attmap/attmap_altstim/figs/local_figs/r_figs"
stats_out.path <- "/data/attmap/attmap_altstim/analyses"

# load data
setwd(data.path) # set data path as working directory
thresh.data <- read.csv(sprintf("%s_thresh_data.csv", curr_exp_name), 
                        header = T, sep = ",", na.strings = "")
trial.data <- read.csv(sprintf("%s_trial_data.csv", curr_exp_name), 
                       header = T, sep = ",", na.strings = "")


# create variables for abs value of thresholds to facilitate comparison between  
# increment (+) and decrement (-) thresholds
thresh.data <- mutate(thresh.data, abs_val_thresh = abs(mean_pdf),
                      abs_val_meanIso = abs(mean_iso_thresh))


# did threshold converge in <= 3 trials? create variable to track this
three_or_less_idx <- thresh.data$obs_num <= 3
thresh.data <- mutate(thresh.data, obs_num_categorical = character(length=length(thresh.data$obs_num)))
thresh.data$obs_num_categorical[three_or_less_idx] <- '<= 3 trials'
thresh.data$obs_num_categorical[!three_or_less_idx] <- '> 3 trias'
thresh.data$obs_num_categorical <- factor(thresh.data$obs_num_categorical)

# SPECIFY ANALYSES TO PERFORM:
perform.an <- c(4) # vector in which elements = analysis numbers

na <- 0 # initialize analysis counter

######################################
#
# ANALYSIS 1: compare model fit between RP and non-RP models 
na <- na + 1
if(!is.na(match(na,perform.an))) {
        cat(sprintf("\nPEROFRMING ANALYSIS %d . . .\n", na))
        params.compare_mods <- list()
        params.compare_mods$datafname.v1p2 <- 
                "/data/attmap/attmap_v1p2/tables/attmap_v1p2_thresh_data.csv"
        params.compare_mods$datafname.intensitydec <- 
                "/data/attmap/attmap_intensitydec/tables/attmap_intensitydec_thresh_data.csv"
        params.compare_mods$datafname.altstim_inc <- 
                "/data/attmap/attmap_altstim/tables/attmap_altstim_inc_thresh_data.csv"
        params.compare_mods$datafname.altstim_intensitydec <- 
                "/data/attmap/attmap_altstim/tables/attmap_altstim_intensitydec_thresh_data.csv"
        params.compare_mods$out_fpath <- stats_out.path
        params.compare_mods$out_fname <- "model_comparison.txt"
        params.compare_mods$write2file <- 0
        mod_compare <- compare_model_fit(params.compare_mods)
}

######################################

# ANALYSIS 2: statistical model
na <- na + 1
if(!is.na(match(na,perform.an))) {
        cat(sprintf("\nPEROFRMING ANALYSIS %d . . .\n", na))
        params.stats <- list()
        params.stats$exp_name <- curr_exp_name
        params.stats$out_fpath <- stats_out.path
        params.stats$fig_fpath <- fig.path
        params.stats$out_fname <- sprintf("%s_R_stat_results.txt", curr_exp_name)
        params.stats$write_outfile <- 1
        lmm.mod <- attmap_altstim_LMM(thresh.data, params.stats)
}

#######################################

# ANALYSIS 3: general plots
# Note: must run Analysis 2 first to get lmm.mod
na <- na + 1
if(!is.na(match(na,perform.an))) {
        cat(sprintf("\nPEROFRMING ANALYSIS %d . . .\n", na))
        params.plots <- list()
        params.plots$exp_name <- curr_exp_name
        params.plots$fpath <- fig.path
        params.plots$plot_RPboxplot <- 0
        params.plots$plot_scatter <- 0
        params.plots$plot_subjstats <- 0
        params.plots$plot_subjdata <- 1
        params.plots$plot_blockAnalysis <- 0
        params.plots$plot_bat <- 0
        params.plots$plot_iso <- 1
        params.plots$plot_obs_stop_hist <- 1
        params.plots$plot_stim_complexity_scatter <- 1
        params.plots$plot_musicianship <- 0
        p <- attmap_altstim_plots(thresh.data, lmm.mod, params.plots)
}

#######################################

# ANALYSIS 4: plots of combined experiments
na <- na + 1
if(!is.na(match(na,perform.an))) {
        cat(sprintf("\nPEROFRMING ANALYSIS %d . . .\n", na))
        params.combined_plots <- list()
        params.combined_plots$datafname.v1p2 <- 
                "/data/attmap/attmap_v1p2/tables/attmap_v1p2_thresh_data.csv"
        params.combined_plots$datafname.intensitydec <- 
                "/data/attmap/attmap_intensitydec/tables/attmap_intensitydec_thresh_data.csv"
        params.combined_plots$datafname.altstim_inc <- 
                "/data/attmap/attmap_altstim/tables/attmap_altstim_inc_thresh_data.csv"
        params.combined_plots$datafname.altstim_intensitydec <- 
                "/data/attmap/attmap_altstim/tables/attmap_altstim_intensitydec_thresh_data.csv"
        params.combined_plots$out_fpath <- fig.path
        attmap_altstim_analyze_combined_data(params.combined_plots)
}

##########################################

# ANALYSIS 5: compare subject thresholds and standard dev of their thresholds
# between increment experiments and between decrement experiments
na <- na + 1
if(!is.na(match(na,perform.an))){
        cat(sprintf("\nPERFORMING ANALYSIS %d . . .\n", na))
        params.thresh_compare <- list()
        params.thresh_compare$datafname.v1p2 <- 
                "/data/attmap/attmap_v1p2/tables/attmap_v1p2_thresh_data.csv"
        params.thresh_compare$datafname.intensitydec <- 
                "/data/attmap/attmap_intensitydec/tables/attmap_intensitydec_thresh_data.csv"
        params.thresh_compare$datafname.altstim_inc <- 
                "/data/attmap/attmap_altstim/tables/attmap_altstim_inc_thresh_data.csv"
        params.thresh_compare$datafname.altstim_intensitydec <- 
                "/data/attmap/attmap_altstim/tables/attmap_altstim_intensitydec_thresh_data.csv"
        params.thresh_compare$out_fpath <- stats_out.path
        params.thresh_compare$out_fname <- "thresh_comparison.txt"
        attmap_thresh_compare(params.thresh_compare)
}

setwd(start.dir)