# analyze individual subject data
#
# 08 OCT 2015 - BH

attmap_subj_analysis <- function(in.df, params) {
        
        # load user-defined functions
        source('~/svn/private/R/utils/se.R')
        
        # calculate model/performance metrics for each subject
        sub_names <- as.character(unique(in.df$subject_id))
        nsubs <- length(sub_names)
        
        subj_an.df <- data.frame(matrix(NA, nrow = nsubs, ncol = 4))
        if(params$exp_name == "attmap_altstim_intensitydec"){
                colnames(subj_an.df) <- 
                        c("subj_name","thresh_X_reson_cor","mean_abs_thresh","thresh_SE") # output data frame
        } else {
                colnames(subj_an.df) <- 
                        c("subj_name","thresh_X_reson_cor","mean_thresh","thresh_SE") # output data frame
        }
        
        #subj_an.df$subj_name <- as.factor(subj_an.df$subj_name)
        
        # get/calc data for each subject
        for(isub in 1:nsubs) {
                
                subj_an.df$subj_name[isub] <- sub_names[isub]
                currsub.data <- in.df[in.df$subject_id==sub_names[isub],] #subset of subject's data
                
                # calculate correlation btw thresholds and resonator amplitude
                # mean threshold, and SE of mean threshold 
                if(params$exp_name == "attmap_altstim_intensitydec"){
                        subj_an.df$thresh_X_reson_cor[isub] <- 
                                cor(na.omit(currsub.data$abs_val_thresh), na.omit(currsub.data$reson_out))
                        subj_an.df$mean_thresh[isub] <- mean(currsub.data$abs_val_thresh, na.rm = T)
                        subj_an.df$thresh_SE[isub] <- se(currsub.data$abs_val_thresh)
                }
                else{
                        subj_an.df$thresh_X_reson_cor[isub] <- 
                                cor(na.omit(currsub.data$mean_pdf), na.omit(currsub.data$reson_out))
                        
                        subj_an.df$mean_thresh[isub] <- mean(currsub.data$mean_pdf, na.rm = T)
                        subj_an.df$thresh_SE[isub] <- se(currsub.data$mean_pdf)
                }
                
        }
        
        return(subj_an.df) # output results as data frame
        
}