compare_model_fit <- function(params) {
        
        # Load mixed effects package
        require(nlme)
        
        # set output file
        sink(file.path(params$out_fpath,params$out_fname))
        
        # load all data sets
        thresh.data.v1p2 <- read.csv(params$datafname.v1p2, 
                                     header = T, sep = ",", na.strings = "")
        thresh.data.dec <- read.csv(params$datafname.intensitydec, 
                                    header = T, sep = ",", na.strings = "")
        thresh.data.altstim_inc <- read.csv(params$datafname.altstim_inc, 
                                    header = T, sep = ",", na.strings = "")
        thresh.data.altstim_dec <- read.csv(params$datafname.altstim_intensitydec, 
                                    header = T, sep = ",", na.strings = "")
        
        # data variables to retain across data sets
        data_vars <- c("subject_id", "stim_name", "mean_pdf", "probe_time", "reson_out",
                       "metricHierarchy", "scene_density")
        
        # combine data sets into an increment set and decrement set
        inc_data <- rbind(thresh.data.v1p2[data_vars], thresh.data.altstim_inc[data_vars])
        dec_data <- rbind(thresh.data.dec[data_vars], thresh.data.altstim_dec[data_vars])
        dec_data["abs_thresh"] <- abs(dec_data$mean_pdf)
        
        #############################################
        # specify competing models for increment data
        
        cat("#####################\nMODEL COMPARISONS FOR INCREMENT DATA\n\n")
        
        ctrl <- lmeControl(opt='optim') # remove iteration limit from lme
        inc_mod.res <- lme(mean_pdf ~ reson_out, control = ctrl, random = ~1|subject_id, 
                           data=inc_data, na.action=na.omit, method = "ML")
        print(summary(inc_mod.res))
        inc_mod.mh <- lme(mean_pdf ~ metricHierarchy, control = ctrl, random = ~1|subject_id, 
                          data=inc_data, na.action=na.omit, method = "ML")
        print(summary(inc_mod.mh))
        inc_mod.sd <- lme(mean_pdf ~ scene_density, control = ctrl, random = ~1|subject_id, 
                          data=inc_data, na.action=na.omit, method = "ML")
        print(summary(inc_mod.sd))
        
        # compare AIC as metric of model fit to increment data
        print(AIC(inc_mod.res, inc_mod.mh, inc_mod.sd))
        
        #############################################
        # specify competing models for decrement data
        cat("\n\n\n#####################\nMODEL COMPARISONS FOR DECREMENT DATA\n\n")
        
        dec_mod.res <- lme(abs_thresh ~ reson_out, control = ctrl, random = ~1|subject_id, 
                           data=dec_data, na.action=na.omit, method = "ML")
        print(summary(dec_mod.res))
        dec_mod.mh <- lme(abs_thresh ~ metricHierarchy, control = ctrl, random = ~1|subject_id, 
                          data=dec_data, na.action=na.omit, method = "ML")
        print(summary(dec_mod.mh))
        dec_mod.sd <- lme(abs_thresh ~ scene_density, control = ctrl, random = ~1|subject_id, 
                          data=dec_data, na.action=na.omit, method = "ML")
        print(summary(dec_mod.sd))
        
        # compare model AICs
        print(AIC(dec_mod.res, dec_mod.mh, dec_mod.sd))
                
        sink()
                
}