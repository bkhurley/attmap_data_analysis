# Wrapper function for attmap_altstim statistical analyses
# Called from attmap_altstim_analyses.R 
# 
# 06 Nov 2016 - BKH

attmap_altstim_stats <- function(thresh.data, params) {
        
        # call function corresponding to the current experiment
        if(params$exp_name == "attmap_altstim_intensitydec"){
                source('~/svn/private/R/attmap/attmap_altstim/attmap_altstim_intensitydec_LMM.R')
                lmm.results <- attmap_altstim_intensitydec_LMM(thresh.data, params)
        } else {
                source('~/svn/private/R/attmap/attmap_altstim/attmap_altstim_inc_LMM.R')
                lmm.results <- attmap_altstim_inc_LMM(thresh.data, params)
        }
        
}