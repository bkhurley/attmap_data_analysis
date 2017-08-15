# Analysis stack for attmap_intensitydec data
# Set analyses to be performed by populating perform.an with vector of prefered 
# analyses (e.g., to perform analyses 2 and 3, set perform.an <- c(2, 3))
#
# 07 DEC 2015 - BH
##############

# # load user-defined functions
source('/attmap_subj_analysis.R')
source('/experiment_2/attmap_intensitydec_plots.R')
source('/experiment_2/attmap_intensitydec_stats_v2.R')

# store the current directory
start.dir <- getwd()

# specify file paths
data.path <- "/data/attmap/attmap_intensitydec/tables/"
fig.path <- "/data/attmap/attmap_intensitydec/figures/r_figs"
stats_out.path <- "/data/attmap/attmap_intensitydec/analyses/"

# load data
setwd(data.path) # set data path as working directory
rp_sig.data <- read.csv("/data/attmap/attmap_v1p2/tables/attmap_v1p2_stim_peakReson.csv", header = T, sep = ",")
thresh.data <- read.csv("attmap_intensitydec_thresh_data.csv", header = T, sep = ",", na.strings = "")

# create factor with absolute value of thresholds (as decrement thresholds are negative)
thresh.data$abs_mean_pdf <- abs(thresh.data$mean_pdf)

# SPECIFY ANALYSES TO PERFORM:
perform.an <- c(2) # vector in which elements = analysis numbers

na <- 0 # initialize analysis counter

#######################################

# ANALYSIS 1: calculate subject-level metrics
na <- na + 1
if(!is.na(match(na,perform.an))) {
        cat(sprintf("\nPEROFRMING ANALYSIS %d . . .\n", na))
        params.subj_an <- list()
        params.subj_an$write2file <- 0
        subj_an <- attmap_subj_analysis(thresh.data,params.subj_an)
}

######################################

# ANALYSIS 2: statistical tests
na <- na + 1
if(!is.na(match(na,perform.an))) {
        cat(sprintf("\nPEROFRMING ANALYSIS %d . . .\n", na))
        params.stats <- list()
        params.stats$out_fpath <- stats_out.path
        params.stats$out_fname <- "attmap_intensitydec_R_stat_v2_results.txt"
        params.stats$write_outfile <- 1
        lmm.mod <- attmap_intensitydec_stats_v2(thresh.data, params.stats)
}

#######################################

# ANLAYSIS 3: generate plots
na <- na + 1
if(!is.na(match(na,perform.an))) {
        cat(sprintf("\nPEROFRMING ANALYSIS %d . . .\n", na))
        params.plots <- list()
        params.plots$fpath <- fig.path
        params.plots$plot_scatter <- 0
        params.plots$plot_subjstats <- 0
        params.plots$plot_subjdata <- 0
        params.plots$plot_blockAnalysis <- 1
        p <- attmap_intensitydec_plots(thresh.data, lmm.mod, subj_an, params.plots)
}

setwd(start.dir)