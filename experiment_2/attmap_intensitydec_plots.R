# plot data from attmap_intensitydec experiment
# 
# 22 Dec 2015 - BH
##########################################

attmap_intensitydec_plots <- function(thresh.data, lmm.mod, subj.data, params) {
        
        # load graphics packages
        library(ggplot2)
        library(lattice)
        library(grid)
        
        ########################################
        # scatter plots of reson amplitude X threshold
        if(params$plot_scatter){
                
                # remove data where musicianship = NA
                mus_data = subset(thresh.data, !(musicianship %in% NA));
                
                # open PDF device for writing to file
                plot_fname = file.path(params$fpath,"attmap_intensitydec_resonXthresh_scatter.pdf")
                pdf(file = plot_fname)
                
                # build plot layers
                sp <- ggplot(mus_data, aes(x = reson_out, y = abs_mean_pdf, color = musicianship)) +
                        geom_point(alpha = .3, size = 3) + 
                        geom_smooth(alpha = .2, size=1, method = "lm") +
                        xlab("Reson Amplitude") + ylab("Decrement Detection Threshold (|dB SPL|)") +
                        theme_classic(base_size = 16) +
                        theme(legend.position="top")
                print(sp)       
                
                # close graphics device
                dev.off()
                remove(plot_fname)
        }
        
        ########################################
        # scatter plot of model correlation X avg threshold for each subject
        if(params$plot_subjstats){
                
                # open PDF device for writing to file
                plot_fname = file.path(params$fpath,"attmap_intensitydec_subject_analysis.pdf")
                pdf(file = plot_fname)
                
                # build plot layers
                subject.sp <- ggplot(subj.data, aes(x = abs(mean_thresh), y = thresh_X_reson_cor)) +
                        geom_point(size = 4) +
                        geom_errorbarh(aes(xmin = abs(mean_thresh) - thresh_SE, xmax = abs(mean_thresh) + thresh_SE)) +
                        xlab("Mean Detection Threshold (abs. val.)") + 
                        ylab("Threshold X Resonator Correlation") +
                        ggtitle("Subject Performance") +
                        theme_bw(base_size = 14)
                print(subject.sp)
                
                # close graphics device
                dev.off()
                remove(plot_fname)
        }
        
        ###########################################
        # plot of individual subjects' performeance
        if(params$plot_subjdata){
                
                # open PDF device for writing to file
                plot_fname = file.path(params$fpath,"attmap_intensitydec_subject_data.pdf")
                pdf(file = plot_fname)
                
                subj_data = xyplot(abs_mean_pdf ~ reson_out | subject_id, thresh.data,
                                   grid = TRUE,
                                   type = c("p","r"), col.line = "red")
                print(subj_data)
                
                # close graphics device
                dev.off()
                remove(plot_fname)
                
        }
        
        ###########################################
        # plot difference in performance over experiment blocks
        
        if(params$plot_blockAnalysis){
                
                # reson X thresh scatter by runs
                # open PDF device for writing to file
                plot_fname = file.path(params$fpath,"attmap_intensitydec_block_analysis_scatter.pdf")
                pdf(file = plot_fname)
                
                block_analysis = xyplot(abs_mean_pdf ~ reson_out | as.factor(run_num), thresh.data,
                                        grid = TRUE,
                                        type = c("p","r"), col.line = "red")
                print(block_analysis)
                
                # close graphics device
                dev.off()
                remove(plot_fname)
                
                #####################################
                # boxplot of thresholds by run
                # open PDF device for writing to file
                plot_fname = file.path(params$fpath,"attmap_intensitydec_threshbyblock_boxplot.pdf")
                pdf(file = plot_fname)
                
                # build plot layers
                p <- ggplot(thresh.data, aes(x = as.factor(run_num), y = abs_mean_pdf)) + 
                        geom_boxplot(aes(group = as.factor(run_num))) +
                        stat_summary(fun.y="mean", geom="point", size=2.5, shape = 23) +
                        xlab("Block #") +
                        ylab("Intensity Deviant Threshold (|dB SPL|)") +
                        theme_bw(base_size = 14)
                print(p)
                
                # close graphics device
                dev.off() 
                remove(plot_fname)
                
                #####################################
                # histogram of thresholds by run
                # open PDF device for writing to file
                plot_fname = file.path(params$fpath,"attmap_intensitydec_threshbyblock_hist.pdf")
                pdf(file = plot_fname)
                
                # build plot layers
                p <- ggplot(thresh.data, aes(x = abs_mean_pdf)) + 
                        geom_histogram() +
                        facet_wrap(~ run_num) +
                        theme_bw(base_size = 14) +
                        xlab("Intensity Deviant Threshold (|dB SPL|)")
                print(p)
                
                # close graphics device
                dev.off() 
                remove(plot_fname)
        }
        
}