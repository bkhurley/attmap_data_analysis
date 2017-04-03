# plot data from attmap_altstim experiment
# 
# 20 Oct 2016 - BH
##########################################

attmap_altstim_plots <- function(thresh.data, lmm.mod, params) {

        # load graphics packages
        library(ggplot2)
        library(lattice)
        
        # add column probe_time_ms to data table
        beat_dur = 60000/107 # beat duration in ms
        step_dur = beat_dur/4 # step duration (steps are at sixteenth note level)
        # probe time = step number * ms step duration
        ms_probe_time = thresh.data$probe_time * step_dur
        thresh.data$probe_time_ms = ms_probe_time
        
        # get LMM intercepts and slope for plotting and add to data frame
        rand_ints <- lmm.mod$simple_mod$coefficients$random$subject_id[,'(Intercept)']
        rand_slopes <- lmm.mod$simple_mod$coefficients$random$subject_id[,'reson_out']
        fixed_int <- lmm.mod$simple_mod$coefficients$fixed['(Intercept)']
        fixed_slope <- lmm.mod$simple_mod$coefficients$fixed['reson_out']
        lmm_sub_names <- rownames(lmm.mod$simple_mod$coefficients$random$subject_id)
        
        thresh.data$lmm_intercept <- rep(NA, length=length(thresh.data$subject_id))
        thresh.data$lmm_slope <- rep(NA, length=length(thresh.data$subject_id))
        subj_names = levels(thresh.data$subject_id)
        for(sub in 1:length(subj_names)){
                lmm_sub_idx <- grep(subj_names[sub], lmm_sub_names)
                
                # if current subject not found in lmm data, skip this iteration
                # (we don't have Ensemble data for one subject, so if Ensemble-dependent
                # variables included in LMM model, that subject is omitted)
                if(length(lmm_sub_idx)==0){
                        next
                }
                
                data_sub_idx <- grep(subj_names[sub], thresh.data$subject_id)
                # get this subject's intercept and the fixed LMM reson_out slope
                thresh.data$lmm_intercept[data_sub_idx] <- fixed_int + rand_ints[lmm_sub_idx]
                #thresh.data$lmm_slope[data_sub_idx] <- fixed_slope + rand_slopes[lmm_sub_idx]
                thresh.data$lmm_slope[data_sub_idx] <- rand_slopes[lmm_sub_idx]
        }
        
        #######################################
        # plot multi-panel boxplots of tresholds by probe position for each stim
        # box color is mapped to the RP reson amplitude that corresponds to probe time
        
        if(params$plot_RPboxplot) {
                # open PDF device for writing to file
                plot_fname = file.path(params$fpath,sprintf("%s_thresh_RP_boxplot.pdf", params$exp_name))
                pdf(file = plot_fname)
                
                # build plot layers
                if(params$exp_name == "attmap_altstim_intensitydec"){
                        p <- ggplot(thresh.data, aes(x = probe_time_ms, y = abs_val_thresh)) +
                                ylab("|Intensity Decrement Threshold| (dB SPL)")
                } else {
                        p <- ggplot(thresh.data, aes(x = probe_time_ms, y = mean_pdf)) +
                                ylab("Intensity Increment Threshold (dB SPL)")
                        }
                
                p <- p + geom_boxplot(aes(group = probe_time_ms, fill = reson_out)) + # map reson_out to box color
                        stat_summary(fun.y="mean", geom="point", size=2.5, shape = 23, fill = "white") +
                        facet_wrap(~ stim_name) +
                        xlab("Probe Time (ms)") +
                        theme_bw(base_size = 14) +
                        scale_fill_gradient2(low = "#0000FF", mid = "#FFFFFF", high ="#FF0000",
                                             midpoint = median(thresh.data$reson_out), space = "rgb", 
                                             guide = "colourbar")
                print(p)
                
                # close graphics device
                dev.off() 
                remove(plot_fname)
        }
        
        #####################################
        # boxplot of musicianship
        
        if(params$plot_musicianship){
                plot_fname = file.path(params$fpath,sprintf("%s_musician_boxplot.pdf", params$exp_name))
                pdf(file = plot_fname, width=4.5, height=4.5)
                
                # build plot layers
                if(params$exp_name == "attmap_altstim_intensitydec"){
                        p <- ggplot(data=subset(thresh.data, !is.na(musicianship)), 
                                    aes(x = musicianship, y = abs_val_thresh)) + 
                                ylab("|Intensity Decrement Threshold| (dB SPL)")
                } else {p <- ggplot(data=subset(thresh.data, !is.na(musicianship)),
                                    aes(x = musicianship, y = mean_pdf)) + 
                        ylab("Intensity Increment Threshold (dB SPL)")}
                
                p <- p + geom_boxplot() + stat_summary(fun.y="mean", geom="point", size=2.5, shape = 23) +
                        scale_x_discrete(labels = c('Musician', 'Non-Musician')) + 
                        xlab('') + ylim(c(0, 20)) + theme_classic(base_size = 16)
                print(p)
                
                # close graphics device
                dev.off() 
                remove(plot_fname)
                
        }
        
        ########################################
        # scatter plots of reson amplitude X threshold
        if(params$plot_scatter){
                # remove data where musicianship = NA
                mus_data = subset(thresh.data, !(musicianship %in% NA));
                # open PDF device for writing to file
                plot_fname = file.path(params$fpath,sprintf("%s_resonXthresh_mus_scatter.pdf", params$exp_name))
                pdf(file = plot_fname)
                
                # build plot layers
                if(params$exp_name == "attmap_altstim_intensitydec"){
                        sp <- ggplot(mus_data, aes(x=reson_out, y=abs_val_thresh, shape=musicianship, color=musicianship)) +
                                ylab("|Intensity Decrement Threshold| (dB SPL)")
                } else {
                        sp <- ggplot(mus_data, aes(x=reson_out, y=mean_pdf, shape=musicianship, color=musicianship)) +
                                ylab("Intensity Increment Threshold (dB SPL)")
                        }
                
                sp <- sp + geom_point(size=2, alpha=0.5, position=position_jitter(width=0.3, height=0.06)) + 
                        geom_smooth(method="lm", se=FALSE) + xlab("Reson Amplitude") + ylim(c(0, 20)) +
                        scale_x_continuous(breaks=seq(-0.6, 1.5, 0.3)) +
                        theme_classic(base_size=16) + theme(legend.position="top")
                print(sp)       
                
                # close graphics device
                dev.off()
                remove(plot_fname)
        }
        
        ########################################
        # scatter plots of reson amplitude X threshold grouped by stim complexity
        if(params$plot_stim_complexity_scatter){
                # open PDF device for writing to file
                plot_fname = file.path(params$fpath,sprintf("%s_resonXthresh_complexity_scatter.pdf", params$exp_name))
                pdf(file = plot_fname, width=6.5, height=4.5)
                
                # build plot layers
                if(params$exp_name == "attmap_altstim_intensitydec"){
                        sp <- ggplot(thresh.data, aes(x=reson_out, y=abs_val_thresh)) +
                                ylab("Intensity Decrement Threshold (|dB SPL|)")
                } else {
                        sp <- ggplot(thresh.data, aes(x=reson_out, y=mean_pdf)) +
                                ylab("Intensity Increment Threshold (dB SPL)")
                }
                
                sp <- sp + facet_wrap( ~ stim_complexity) + 
                        geom_point(size=1.5, alpha=0.5) +
                        geom_smooth(size=0.8, color='black', method="lm", se=FALSE) +
                        xlab("Reson Amplitude") + ylim(c(0, 20)) +
                        theme_classic(base_size=16)
                print(sp)       
                
                # close graphics device
                dev.off()
                remove(plot_fname)
        }
        
        ###########################################
        # plot of individual subjects' performance
        if(params$plot_subjdata){
                
                # open PDF device for writing to file
                plot_fname = file.path(params$fpath,sprintf("%s_subject_mod_data.pdf", params$exp_name))
                
                if(length(subj_names) != length(lmm_sub_names)){
                        nonmatching_sub <- setdiff(subj_names, lmm_sub_names)
                        thresh.data <- subset(thresh.data, !(subject_id %in% nonmatching_sub))
                }
                
                # plot subject scatter
                if(params$exp_name == "attmap_altstim_intensitydec"){
                        pdf(file = plot_fname)
                        sp <- ggplot(thresh.data, aes(x=reson_out, y=abs_val_thresh)) +
                                # LMM fit line
                                geom_abline(aes(intercept=lmm_intercept, slope=lmm_slope)) +
                                facet_wrap( ~ subject_id) +
                                ylab("Intensity Decrement Detection Threshold (|dB SPL|)") 
                } else {
                        pdf(file = plot_fname)
                        sp <- ggplot(thresh.data, aes(x=reson_out, y=mean_pdf)) +
                                # add LMM fit line
                                geom_abline(aes(intercept=lmm_intercept, slope=lmm_slope)) +
                                facet_wrap( ~ subject_id, ncol=5) +
                                ylab("Intensity Increment Detection Threshold (dB SPL)")
                        }
               
                sp <- sp + geom_point() + 
                        xlab("Reson Amplitude") + ylim(c(0, 20)) + theme_classic() +
                        theme(axis.title.y=element_text(size=14)) +
                        theme(axis.title.x=element_text(size=14))
                print(sp)
                
                # close graphics device
                dev.off()
                remove(plot_fname)
                
                ######################
                # compare w/ lm plot
                # open PDF device for writing to file
                plot_fname = file.path(params$fpath,sprintf("%s_subject_lm_data.pdf", params$exp_name))
                
                if(length(subj_names) != length(lmm_sub_names)){
                        nonmatching_sub <- setdiff(subj_names, lmm_sub_names)
                        thresh.data <- subset(thresh.data, !(subject_id %in% nonmatching_sub))
                }
                
                
                # plot subject scatter
                if(params$exp_name == "attmap_altstim_intensitydec"){
                        pdf(file = plot_fname)
                        sp <- ggplot(thresh.data, aes(x=reson_out, y=abs_val_thresh)) +
                                geom_smooth(method="lm", se=FALSE, color="black") +
                                facet_wrap( ~ subject_id) +
                                ylab("Intensity Decrement Detection Threshold (|dB SPL|)") 
                } else {
                        sp <- ggplot(thresh.data, aes(x=reson_out, y=mean_pdf)) +
                                pdf(file = plot_fname) +
                                geom_smooth(method="lm", se=FALSE, color="black") +
                                facet_wrap( ~ subject_id, ncol=5) +
                                ylab("Intensity Increment Detection Threshold (dB SPL)")
                }
                
                sp <- sp + geom_point() + 
                        xlab("Reson Amplitude") + ylim(c(0, 20)) + theme_classic() +
                        theme(axis.title.y=element_text(size=14)) +
                        theme(axis.title.x=element_text(size=14))
                print(sp)
                
                # close graphics device
                dev.off()
                remove(plot_fname)
                
        }
        
        ###########################################
        # subject slopes X iso, bat_tempo, bat_phase
        
        if(length(subj_names) != length(lmm_sub_names)){
                nonmatching_sub <- setdiff(subj_names, lmm_sub_names)
                thresh.data <- subset(thresh.data, !(subject_id %in% nonmatching_sub))
        }
        
        # plot subject scatter
        if(params$exp_name == "attmap_altstim_intensitydec"){
                # mean iso thresh X slope
                sp1 <- ggplot(thresh.data, aes(x= abs_val_meanIso, y=lmm_slope)) +
                        geom_point() + theme_classic() + 
                        geom_smooth(method="lm", se=FALSE, color="black") +
                        xlab("Mean Isochronous Decrement Detection Threshold (|db SPL|)") +
                        ylab("Reson Level X Increment Detection Threshold Slope") +
                        theme(axis.title.y=element_text(size=12)) +
                        theme(axis.title.x=element_text(size=12))
                print(sp1)
                
                # tempo BAT X slope
                sp2 <- ggplot(thresh.data, aes(x= bat_tempo_thresh, y=lmm_slope)) +
                        geom_point() + theme_classic() + 
                        geom_smooth(method="lm", se=FALSE, color="black") +
                        xlab("BAT (Tempo) Threshold") +
                        ylab("Reson Level X Increment Detection Threshold Slope") +
                        theme(axis.title.y=element_text(size=12)) +
                        theme(axis.title.x=element_text(size=12))
                print(sp2)
                
                # phase BAT X slope
                sp3 <- ggplot(thresh.data, aes(x= bat_phase_thresh, y=lmm_slope)) +
                        geom_point() + theme_classic() + 
                        geom_smooth(method="lm", se=FALSE, color="black") +
                        xlab("BAT (Phase) Threshold") +
                        ylab("Reson Level X Increment Detection Threshold Slope") +
                        theme(axis.title.y=element_text(size=12)) +
                        theme(axis.title.x=element_text(size=12))
                print(sp3)
        } else {
                # mean iso thresh X slope
                sp1 <- ggplot(thresh.data, aes(x= mean_iso_thresh, y=lmm_slope)) +
                        geom_point() + theme_classic() + 
                        geom_smooth(method="lm", se=FALSE, color="black") +
                        xlab("Mean Isochronous Increment Detection Threshold (db SPL)") +
                        ylab("Reson Level X Increment Detection Threshold Slope") +
                        theme(axis.title.y=element_text(size=12)) +
                        theme(axis.title.x=element_text(size=12))
                print(sp1)
                
                # tempo BAT X slope
                sp2 <- ggplot(thresh.data, aes(x= bat_tempo_thresh, y=lmm_slope)) +
                        geom_point() + theme_classic() + 
                        geom_smooth(method="lm", se=FALSE, color="black") +
                        xlab("BAT (Tempo) Threshold") +
                        ylab("Reson Level X Increment Detection Threshold Slope") +
                        theme(axis.title.y=element_text(size=12)) +
                        theme(axis.title.x=element_text(size=12))
                print(sp2)
                
                # phase BAT X slope
                sp3 <- ggplot(thresh.data, aes(x= bat_phase_thresh, y=lmm_slope)) +
                        geom_point() + theme_classic() + 
                        geom_smooth(method="lm", se=FALSE, color="black") +
                        xlab("BAT (Phase) Threshold") +
                        ylab("Reson Level X Increment Detection Threshold Slope") +
                        theme(axis.title.y=element_text(size=12)) +
                        theme(axis.title.x=element_text(size=12))
                print(sp3)
                
        }
        
        sp <- sp + geom_point( position=position_jitter(width=0.3, height=0.06)) + 
                xlab("Reson Amplitude") + ylim(c(0, 20)) + theme_classic() +
                theme(axis.title.y=element_text(size=14)) +
                theme(axis.title.x=element_text(size=14))
        print(sp)
        
        ###########################################
        # plot difference in performance over experiment blocks
        
        if(params$plot_blockAnalysis){
                
                # reson X thresh scatter by runs
                # open PDF device for writing to file
                plot_fname = file.path(params$fpath,sprintf("%s_block_analysis_scatter.pdf", params$exp_name))
                pdf(file = plot_fname)
                
                if(params$exp_name == "attmap_altstim_intensitydec"){
                        block_analysis = xyplot(abs_val_thresh ~ reson_out | as.factor(run_num), thresh.data,
                                                grid = TRUE,
                                                type = c("p","r"), col.line = "red")
                } else {
                        block_analysis = xyplot(mean_pdf ~ reson_out | as.factor(run_num), thresh.data,
                                                grid = TRUE,
                                                type = c("p","r"), col.line = "red")
                        }
                
                print(block_analysis)
                
                # close graphics device
                dev.off()
                remove(plot_fname)
                
                #####################################
                # boxplot of thresholds by run
                # open PDF device for writing to file
                plot_fname = file.path(params$fpath,sprintf("%s_threshbyblock_boxplot.pdf", params$exp_name))
                pdf(file = plot_fname)
                
                # build plot layers
                if(params$exp_name == "attmap_altstim_intensitydec"){
                        p <- ggplot(thresh.data, aes(x = as.factor(run_num), y = abs_val_thresh)) +
                                ylab("|Intensity Decrement Threshold| (dB SPL)")
                } else {p <- ggplot(thresh.data, aes(x = as.factor(run_num), y = mean_pdf)) + 
                        ylab("Intensity Increment Threshold (dB SPL)")}
                
                p <- p + geom_boxplot(aes(group = as.factor(run_num))) +
                        stat_summary(fun.y="mean", geom="point", size=2.5, shape = 23) +
                        xlab("Block #") +
                        theme_bw(base_size = 14)
                print(p)
                
                # close graphics device
                dev.off() 
                remove(plot_fname)
                
                #####################################
                # histogram of thresholds by run
                # open PDF device for writing to file
                plot_fname = file.path(params$fpath,sprintf("%s_threshbyblock_hist.pdf", params$exp_name))
                pdf(file = plot_fname)
                
                # build plot layers
                if(params$exp_name == "attmap_altstim_intensitydec"){
                        p <- ggplot(thresh.data, aes(x = abs_val_thresh)) +
                                xlab("|Intensity Decrement Threshold| (dB SPL)")
                } else {
                        p <- ggplot(thresh.data, aes(x = mean_pdf)) +
                                xlab("Intensity Increment Threshold (dB SPL)")
                        }
                
                p <- p + geom_histogram() + facet_wrap(~ run_num) + theme_bw(base_size = 14)
                print(p)
                
                # close graphics device
                dev.off() 
                remove(plot_fname)
        }
        
        ############################
        # BAT x thresh scatter
        
        # open PDF device for writing to file
        if(params$plot_bat){
                plot_fname = file.path(params$fpath,sprintf("%s_BATtempoXthresh_scatter.pdf", params$exp_name))
                pdf(file = plot_fname)
                
                # build plot layers
                if(params$exp_name == "attmap_altstim_intensitydec"){
                        sp <- ggplot(thresh.data, aes(x = bat_tempo_thresh, y = abs_val_thresh)) +
                                ylab("|Intensity Decrement Threshold| (dB SPL)")
                } else {
                        sp <- ggplot(thresh.data, aes(x = bat_tempo_thresh, y = mean_pdf)) +
                                ylab("Intensity Increment Threshold (dB SPL)")
                }
                
                sp <- sp + geom_point(size=2, shape=1, alpha=0.5, position=position_jitter(width=0.3, height=0.06)) + 
                        geom_smooth(method="lm", se=FALSE) + xlab("BAT Threshold (% Tempo)") + 
                        theme_classic(base_size = 16)
                print(sp)       
                
                # close graphics device
                dev.off()
                remove(plot_fname)
                
                # open PDF device for writing to file
                plot_fname = file.path(params$fpath,sprintf("%s_BATphaseXthresh_scatter.pdf", params$exp_name))
                pdf(file = plot_fname)
                
                # build plot layers
                if(params$exp_name == "attmap_altstim_intensitydec"){
                        sp <- ggplot(thresh.data, aes(x = bat_phase_thresh, y = abs_val_thresh)) +
                                ylab("|Intensity Decrement Threshold| (dB SPL)")
                } else {
                        sp <- ggplot(thresh.data, aes(x = bat_phase_thresh, y = mean_pdf)) +
                                ylab("Intensity Increment Threshold (dB SPL)")
                }
                
                sp <- sp + geom_point(size=2, shape=1, alpha=0.5, position=position_jitter(width=0.3, height=0.06)) + 
                        geom_smooth(method="lm", se=FALSE) + xlab("BAT Threshold (% Phase)") + 
                        theme_classic(base_size = 16)
                        
                print(sp)      
                
                # close graphics device
                dev.off()
                remove(plot_fname)
        
        }
        
        #######################
        # iso x thresh scatter
        
        if(params$plot_iso){
                # open PDF device for writing to file
                plot_fname = file.path(params$fpath, sprintf("%s_isoXthresh_scatter.pdf", params$exp_name))
                pdf(file = plot_fname)
                
                # build plot layers
                if(params$exp_name == "attmap_altstim_intensitydec"){
                        sp <- ggplot(thresh.data, aes(x = abs_val_meanIso, y = abs_val_thresh)) +
                                ylab("Intensity Decrement Threshold (|dB SPL|)") +
                                xlab("Mean Isoch. Decrement Detection Threshold (|db SPL|)")
                } else {
                        sp <- ggplot(thresh.data, aes(x = mean_iso_thresh, y = mean_pdf)) +
                                ylab("Intensity Increment Threshold (dB SPL)") +
                                xlab("Mean Isoch. Increment Detection Threshold (db SPL)")
                }
                
                sp <- sp + geom_point(size=2, shape=1, alpha=0.5, position=position_jitter(width=0.3, height=0.06)) + 
                        geom_smooth(method="lm", se=FALSE) + xlim(0, 20) + ylim(0, 20) + 
                        theme_classic(base_size = 16)
                print(sp)       
                
                # close graphics device
                dev.off()
                remove(plot_fname)
        }
        
        if(params$plot_obs_stop_hist){
                # open PDF device for writing to file
                plot_fname = file.path(params$fpath, sprintf("%s_obs_stop_hist.pdf", params$exp_name))
                pdf(file = plot_fname)
                
                h = ggplot(thresh.data, aes(x = obs_num)) +
                        xlab("Observation of Threshold Convergence") +
                        geom_histogram() + theme_bw(base_size = 14)
                print(h)
                
                # close graphics device
                dev.off() 
                remove(plot_fname)
        }
        
}