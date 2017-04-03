##############################
# compare thresholds between experiments
# load all data sets

library(dplyr)

attmap_thresh_compare <- function(params) {
        
        # set output file
        sink(file.path(params$out_fpath,params$out_fname))
        
        # define function for calculating block duration
        calc_block_duration <- function(d) {
                # trial durations (s)
                stim_dur <- list()
                # get stim loop durations for exp 1 & 2 ...
                stim_dur$prA_4_3i_1v_107bpm.wav = 2.240
                stim_dur$prA_74_3i_1v_107bpm.wav = 2.234
                stim_dur$prC_54_3i_1v_107bpm.wav = 2.248
                stim_dur$prC_74_3i_1v_107bpm.wav = 2.229
                stim_dur$prC_124_3i_1v_107bpm.wav = 2.240
                # ... and for exp 3
                stim_dur$exp3 <- 2.24304
                
                d.stims <- unique(d$stim_name)
                # initiate block_duration vector
                d$block_duration <- double(length=length(d$trial_num))
                
                # calculate block duration (mins) using final trial number & stim loop duration
                if (d$exp[1] == "exp3_inc" || d$exp[1] == "exp3_dec") {
                        # all stims same duration. no need to loop through stims
                        d$block_duration <- d$trial_num * as.double(stim_dur$exp3)/60        
                } else {
                        # stims used in exps 1  & 2 differed in duration. we have 
                        # to calc for each stim
                        for (i in 1:length(d.stims)) {
                                i_stim <- d.stims[i]
                                stim_idx <- d$stim_name==i_stim
                                # calculate & assign block duration
                                d[stim_idx,"block_duration"] <- 
                                        (d[stim_idx, "trial_num"] * as.double(stim_dur[as.character(i_stim)])) /60
                        }
                }
                # return data frame with new variable block_duration
                return(d)
        } 
        
        # load all data sets
        # expt 1
        thresh.data.v1p2 <- read.csv(params$datafname.v1p2, 
                                     header = T, sep = ",", na.strings = "")
        thresh.data.v1p2$exp <- as.factor(rep("exp1", nrow(thresh.data.v1p2)))
        thresh.data.v1p2$abs_thresh <- abs(thresh.data.v1p2$mean_pdf)
        thresh.data.v1p2 <- calc_block_duration(thresh.data.v1p2)
        
        # expt 2
        thresh.data.dec <- read.csv(params$datafname.intensitydec, 
                                    header = T, sep = ",", na.strings = "")
        thresh.data.dec$exp <- as.factor(rep("exp2", nrow(thresh.data.dec)))
        thresh.data.dec$abs_thresh <- abs(thresh.data.dec$mean_pdf)
        thresh.data.dec <- calc_block_duration(thresh.data.dec)
        
        # expt 3 - increment
        thresh.data.altstim_inc <- read.csv(params$datafname.altstim_inc, 
                                            header = T, sep = ",", na.strings = "")
        thresh.data.altstim_inc$exp <- as.factor(rep("exp3_inc", nrow(thresh.data.altstim_inc)))
        thresh.data.altstim_inc$abs_thresh <- abs(thresh.data.altstim_inc$mean_pdf)
        thresh.data.altstim_inc$abs_mean_iso <- abs(thresh.data.altstim_inc$mean_iso_thresh)
        thresh.data.altstim_inc <- calc_block_duration(thresh.data.altstim_inc)
        
        # expt 3 - decrement
        thresh.data.altstim_dec <- read.csv(params$datafname.altstim_intensitydec, 
                                            header = T, sep = ",", na.strings = "")
        thresh.data.altstim_dec$exp <- as.factor(rep("exp3_dec", nrow(thresh.data.altstim_dec)))
        thresh.data.altstim_dec$abs_thresh <- abs(thresh.data.altstim_dec$mean_pdf)
        thresh.data.altstim_dec$abs_mean_iso <- abs(thresh.data.altstim_dec$mean_iso_thresh)
        thresh.data.altstim_dec <- calc_block_duration(thresh.data.altstim_dec)
        
        # get range of thresholds for isoch and experiment task across the two 
        # altstim subject samples
        cat("###################\nRANGE OF THRESHOLDS (ALTSTIM SAMPLES)\n\n")
        altstim_thresh <- rbind(thresh.data.altstim_inc, thresh.data.altstim_dec)
        print(summary(altstim_thresh$abs_thresh))
        print(summary(altstim_thresh$abs_mean_iso))
        
        # calculate mean block duration for each experiment
        cat("###################\nBLOCK DURATION\n\n")
        mean_blockdur_v1p2 <- mean(thresh.data.v1p2$block_duration)
        cat(sprintf("Experiment 1 mean block duration: %f mins\n\n", mean_blockdur_v1p2))
        mean_blockdur_dec <- mean(thresh.data.dec$block_duration)
        cat(sprintf("Experiment 2 mean block duration: %f mins\n\n", mean_blockdur_dec))
        mean_blockdur_altstim_inc <- mean(thresh.data.altstim_inc$block_duration)
        cat(sprintf("Experiment 3 (increment) mean block duration: %f mins\n\n", mean_blockdur_altstim_inc))
        mean_blockdur_altstim_dec <- mean(thresh.data.altstim_dec$block_duration)
        cat(sprintf("Experiment 3 (decrement) mean block duration: %f mins", mean_blockdur_altstim_dec))
        
        # data variables to retain across data sets
        data_vars <- c("subject_id", "abs_thresh", "exp", "block_duration")
        
        # compile increment subject-level threshold means & sd organized by exp
        inc_data <- rbind(thresh.data.v1p2[data_vars], thresh.data.altstim_inc[data_vars])
        # get subject-level means
        inc_data_summary <- aggregate(inc_data["abs_thresh"], by=inc_data[c("subject_id", "exp")], FUN=mean)
        names(inc_data_summary)[names(inc_data_summary)=="abs_thresh"] <- "mean_thresh"
        # get subject-level SD & combine to table with means
        sd_thresh <- aggregate(inc_data["abs_thresh"], by=inc_data[c("subject_id", "exp")], FUN=sd)
        names(sd_thresh)[names(sd_thresh)=="abs_thresh"] <- "sd_thresh"
        inc_data_summary <- merge(inc_data_summary, sd_thresh)
        # sort by exp first
        inc_data_summary <- inc_data_summary[order(inc_data_summary$exp),]
        inc_thresh_exp1 <- inc_data_summary$mean_thresh[inc_data_summary$exp=="exp1"]
        inc_thresh_exp3 <- inc_data_summary$mean_thresh[inc_data_summary$exp=="exp3_inc"]
        inc_sd_exp1 <- inc_data_summary$sd_thresh[inc_data_summary$exp=="exp1"]
        inc_sd_exp3 <- inc_data_summary$sd_thresh[inc_data_summary$exp=="exp3_inc"]
        # Welch t-test of subject-level threshold means between increment experiments
        cat("\n\n\n#####################\nCOMPARISON IN EXP1 V EXP3 SUBJECT THRESHOLDS\n\n")
        cat("SUBJECT MEANS\n")
        print(t.test(inc_thresh_exp3, inc_thresh_exp1, data=inc_data_summary, var.equal=0))
        # Welch t-test of subject-level threshold SDs between increment experiments
        cat("\n\nSUBJECT SD\n")
        print(t.test(inc_sd_exp3, inc_sd_exp1, data=inc_data_summary, var.equal=0))
        
        # compile decrement subject-level threshold means & sd organized by exp
        dec_data <- rbind(thresh.data.dec[data_vars], thresh.data.altstim_dec[data_vars])
        # get subject-level means
        dec_data_summary <- aggregate(dec_data["abs_thresh"], by=dec_data[c("subject_id", "exp")], FUN=mean)
        names(dec_data_summary)[names(dec_data_summary)=="abs_thresh"] <- "mean_thresh"
        # get subject-level SD & combine to table with means
        sd_thresh.dec <- aggregate(dec_data["abs_thresh"], by=dec_data[c("subject_id", "exp")], FUN=sd)
        names(sd_thresh.dec)[names(sd_thresh.dec)=="abs_thresh"] <- "sd_thresh"
        dec_data_summary <- merge(dec_data_summary, sd_thresh.dec)
        # sort by exp first
        dec_data_summary <- dec_data_summary[order(dec_data_summary$exp),]
        # get seperate vectors of subject means and subject SDs for each exp
        dec_thresh_exp2 <- dec_data_summary$mean_thresh[dec_data_summary$exp=="exp2"]
        dec_thresh_exp3 <- dec_data_summary$mean_thresh[dec_data_summary$exp=="exp3_dec"]
        dec_sd_exp2 <- dec_data_summary$sd_thresh[dec_data_summary$exp=="exp2"]
        dec_sd_exp3 <- dec_data_summary$sd_thresh[dec_data_summary$exp=="exp3_dec"]
        
        cat("\n\n\n#####################\nCOMPARISON IN EXP2 V EXP3-DEC SUBJECT THRESHOLDS\n\n")
        # Welch t-test of subject-level threshold means between decrement experiments
        cat("SUBJECT MEANS\n")
        print(t.test(dec_thresh_exp3, dec_thresh_exp2, data=dec_data_summary, var.equal=0))
        # Welch t-test of subject-level threshold SDs between decrement experiments
        cat("\n\nSUBJECT SD\n")
        print(t.test(dec_sd_exp3, dec_sd_exp2, data=dec_data_summary, var.equal=0))
        
        sink()
        
}