# Analysis stack for attmap_v1p2 data
# Set analyses to be performed by populating perform.an with vector of prefered 
# analyses (e.g., to perform analyses 2 and 3, set perform.an <- c(2, 3))
#
# 08 OCT 2015 - BH
##############

library(dplyr)

# load functions for this analysis
source('expeirment_1/attmap_v1p2_clean_data.R')
source('experiment_1/attmap_v1p2_plots.R')
source('experiment_1/attmap_v1p2_stats_v2.R')

##########################################################################
# import data
##########################################################################

# store the current directory
start.dir <- getwd()

# specify file paths
data.path <- "/data/attmap/attmap_v1p2/tables/"
fig.path <- "/data/attmap/attmap_v1p2/figures/local_figures/r_figs"
stats_out.path <- "/data/attmap/attmap_v1p2/analyses/"

# load data
setwd(data.path) # set data path as working directory
thresh.data <- read.csv("attmap_v1p2_thresh_data.csv", header = T, sep = ",", 
                        na.strings = "") %>% tbl_df()


##########################################################################
# Perform analyses
##########################################################################

# SPECIFY ANALYSES TO PERFORM:
perform.an <- c(1, 2) # vector in which elements = analysis numbers

na <- 0 # initialize analysis counter

#######################################
# ANALYSIS 1: clean data
na <- na + 1
if(!is.na(match(na,perform.an))) {
        cat(sprintf("\nPEROFRMING ANALYSIS %d . . .\n", na))
        clean.data <- attmap_v1p2_clean_data(thresh.data)
}

#######################################
# ANALYSIS 2: statistical tests
na <- na + 1
if(!is.na(match(na,perform.an))) {
        cat(sprintf("\nPEROFRMING ANALYSIS %d . . .\n", na))
        params.stats <- list()
        params.stats$out_fpath <- stats_out.path
        params.stats$out_fname <- "attmap_v1p2_R_stat_v2_results.txt"
        params.stats$write_outfile <- 1
        stats.v1p2 <- attmap_v1p2_stats_v2(clean.data, params.stats)
}

######################################
# ANLAYSIS 3: generate plots
na <- na + 1
if(!is.na(match(na,perform.an))) {
        cat(sprintf("\nPEROFRMING ANALYSIS %d . . .\n", na))
        params.plots <- list()
        params.plots$fpath <- fig.path
        params.plots$plot_scatter <- 1
        params.plots$plot_subjdata <- 0
        params.plots$plot_blockAnalysis <- 0
        p <- attmap_v1p2_plots(clean.data, stats.v1p2, params.plots)
}

setwd(start.dir)