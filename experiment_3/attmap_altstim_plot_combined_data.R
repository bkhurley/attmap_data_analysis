# attmap_altstim_plot_combined_data
# plots combined data from increment and decrement altstim experiments 
#
# 24 Jan 2017 - BKH

attmap_altstim_plot_combined_data <- function(params) {
        
        # load graphics packages
        library(ggplot2)
        library(nlme)
        library(gridExtra)
        
        
        #############################
        # Define function for extracting subject slopes & intercepts & appending to data frame
        subj_slopes_intercepts <- function(lmm, data) {
                rand_ints <- lmm$coefficients$random$subject_id[,'(Intercept)']
                rand_slopes <- lmm$coefficients$random$subject_id[,'reson_out']
                fixed_int <- lmm$coefficients$fixed['(Intercept)']
                fixed_slope <- lmm$coefficients$fixed['reson_out']
                lmm_sub_names <- rownames(lmm$coefficients$random$subject_id)
                
                data$lmm_intercept <- rep(NA, length=length(data$subject_id))
                data$lmm_slope <- rep(NA, length=length(data$subject_id))
                subj_names = levels(data$subject_id)
                for(sub in 1:length(subj_names)){
                        lmm_sub_idx <- grep(subj_names[sub], lmm_sub_names)
                        
                        # if current subject not found in lmm data, skip this iteration
                        # (we don't have Ensemble data for one subject, so if Ensemble-dependent
                        # variables included in LMM model, that subject is omitted)
                        if(length(lmm_sub_idx)==0){
                                next
                        }
                        
                        data_sub_idx <- grep(subj_names[sub], data$subject_id)
                        # get this subject's intercept and the fixed LMM reson_out slope
                        data$lmm_intercept[data_sub_idx] <- fixed_int + rand_ints[lmm_sub_idx]
                        #thresh.data$lmm_slope[data_sub_idx] <- fixed_slope + rand_slopes[lmm_sub_idx]
                        data$lmm_slope[data_sub_idx] <- rand_slopes[lmm_sub_idx]
                }
                
                # return data fram with subject slope and intercept columns included
                return(data)
        }
        
        
        
        ##########################
        # load all data sets
        # expt 1
        thresh.data.v1p2 <- read.csv(params$datafname.v1p2, 
                                     header = T, sep = ",", na.strings = "")
        thresh.data.v1p2$exp <- as.factor(rep("exp1", nrow(thresh.data.v1p2)))
        # expt 2
        thresh.data.dec <- read.csv(params$datafname.intensitydec, 
                                    header = T, sep = ",", na.strings = "")
        thresh.data.dec$exp <- as.factor(rep("exp2", nrow(thresh.data.dec)))
        thresh.data.dec$abs_thresh <- abs(thresh.data.dec$mean_pdf)
        # expt 3 - increment
        thresh.data.altstim_inc <- read.csv(params$datafname.altstim_inc, 
                                            header = T, sep = ",", na.strings = "")
        thresh.data.altstim_inc$exp <- as.factor(rep("exp3_inc", nrow(thresh.data.altstim_inc)))
        thresh.data.altstim_inc$abs_thresh <- abs(thresh.data.altstim_inc$mean_pdf)
        thresh.data.altstim_inc$abs_mean_iso <- abs(thresh.data.altstim_inc$mean_iso_thresh)
        
        # expt 3 - decrement
        thresh.data.altstim_dec <- read.csv(params$datafname.altstim_intensitydec, 
                                            header = T, sep = ",", na.strings = "")
        thresh.data.altstim_dec$exp <- as.factor(rep("exp3_dec", nrow(thresh.data.altstim_dec)))
        thresh.data.altstim_dec$abs_thresh <- abs(thresh.data.altstim_dec$mean_pdf)
        thresh.data.altstim_dec$abs_mean_iso <- abs(thresh.data.altstim_dec$mean_iso_thresh)
        
        
        ##################
        # define simple slope & intercept LMMs for each data set
        # get LMM intercepts and slope for plotting and add to data frame
        ctrl <- lmeControl(opt='optim') # remove iteration limit from lme
        lmm.mod.exp1 <- lme(mean_pdf ~ 1, random = ~ reson_out | subject_id, 
                            control=ctrl, data=thresh.data.v1p2, 
                            na.action=na.exclude, method="REML")
        lmm.mod.exp2 <- lme(abs_thresh ~ 1, random = ~ reson_out | subject_id, 
                            control=ctrl, data=thresh.data.dec, 
                            na.action=na.exclude, method="REML")
        lmm.mod.exp3inc <- lme(abs_thresh ~ 1, random = ~ reson_out | subject_id, 
                               control=ctrl, data=thresh.data.altstim_inc, 
                               na.action=na.exclude, method="REML")
        lmm.mod.exp3dec <- lme(abs_thresh ~ 1, random = ~ reson_out | subject_id, 
                               control=ctrl, data=thresh.data.altstim_dec, 
                               na.action=na.exclude, method="REML")
        # append sopes & intercepts to data frame
        thresh.data.v1p2 <- subj_slopes_intercepts(lmm.mod.exp1, thresh.data.v1p2)
        thresh.data.dec <- subj_slopes_intercepts(lmm.mod.exp2, thresh.data.dec)
        thresh.data.altstim_inc <- subj_slopes_intercepts(lmm.mod.exp3inc, thresh.data.altstim_inc)
        thresh.data.altstim_dec <- subj_slopes_intercepts(lmm.mod.exp3dec, thresh.data.altstim_dec)
        
        # combine exp 3 data sets
        thresh.data.alt_all <- rbind(thresh.data.altstim_inc, thresh.data.altstim_dec)
        
        ###############################
        # 2-panel plot. Inc on left, Dec on right. In each panel, thresh as function of res for simple & complex
        
        # open PDF device for writing to file
        plot_fname = file.path(params$out_fpath,sprintf("complexity_isoThresh.pdf"))
        pdf(file = plot_fname, width=6.5, height=10, useDingbats=FALSE)
        
        levels(thresh.data.alt_all$exp) <- c("Increment", "Decrement") # change how experiment labels will display
        
        # complexity scatter
        sp1 <- ggplot(thresh.data.alt_all, aes(x=reson_out, y=abs_thresh, 
                                        shape=stim_complexity, 
                                        linetype = stim_complexity)) +
                ylab("Threshold (dB SPL)") +
                facet_wrap( ~ exp, nrow=1, ncol=2) + geom_point(alpha=0.75) +
                geom_smooth(size=0.8, color='black', method="lm", se=FALSE) +
                xlab("Resonator Amplitude") + ylim(c(0, 20)) +
                guides(shape=FALSE, linetype=FALSE) +
                theme_classic(base_size=16)
        
        # slope & intercept by mean isochronous thresh scatter
        
        sp2 <- ggplot(thresh.data.alt_all, aes(x=abs_mean_iso, y=lmm_slope)) +
                ylab("Threshold ~ Resonator Slope") + geom_point() + 
                facet_wrap(~ exp, nrow=1, ncol=2) +
                geom_smooth(size=0.8, color='black', method="lm", se=FALSE) +
                xlab("Mean Isochronous Threshold (dB SPL)") +
                theme_classic(base_size=16)
        sp3 <- ggplot(thresh.data.alt_all, aes(x=abs_mean_iso, y=lmm_intercept)) +
                ylab("Threshold ~ Resonator Intercept") + geom_point() +
                facet_wrap(~ exp, nrow=1, ncol=2) +
                geom_smooth(size=0.8, color='black', method="lm", se=FALSE) +
                xlab("Mean Isochronous Threshold (dB SPL)") +
                theme_classic(base_size=16) 
        
        grid.arrange(sp1, sp2, sp3, nrow=3, ncol=1)
        
        # close graphics device
        dev.off()
        remove(plot_fname)
        
        # plot histogram of thresh ~ reson slopes 
        # combine slope & experiment name data into 1 vector
        hist_data <- rbind(thresh.data.v1p2[,c("subject_id", "exp", "lmm_slope")], 
                           thresh.data.dec[,c("subject_id", "exp", "lmm_slope")], 
                           thresh.data.altstim_inc[,c("subject_id", "exp", "lmm_slope")],
                           thresh.data.altstim_dec[,c("subject_id", "exp", "lmm_slope")])
        hist_data <- aggregate(hist_data["lmm_slope"], by=hist_data[c("subject_id", "exp")], FUN=mean)
        levels(hist_data$exp) <- c("Experiment 1", "Experiment 2", 
                                   "Experiment 3 - Inc", "Experiment 3 - Dec") # change how experiment labels will display
        
        # open PDF device for writing to file
        plot_fname = file.path(params$out_fpath,sprintf("slope_hist.pdf"))
        pdf(file = plot_fname, width=5.5, height=5.5)
        sp1 <- ggplot(hist_data, aes(x=lmm_slope, color=)) +
                ylab("# Participants") +
                facet_wrap( ~ exp, nrow=2, ncol=2) + 
                geom_histogram(binwidth=0.5) +
                xlab("Threshold ~ Resonator Slope") +
                theme_classic(base_size=14)
        print(sp1)
        
        # close graphics device
        dev.off()
        remove(plot_fname)
        
}