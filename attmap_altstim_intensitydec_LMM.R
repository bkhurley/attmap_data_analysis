# Statistical analysis for attmap_altstim experiments (attmap_altstim_inc & attmap_altstim_intensitydec)
# Called from attmap_altstim_analyses.R 
# 
# 06 Nov 2016 - BKH

attmap_altstim_intensitydec_LMM <- function(thresh.data, params) {
        
        # Load mixed effects package
        require(nlme)
        
        # set output file
        sink(file.path(params$out_fpath,params$out_fname))
        
        cat("/nEvaluating fixed effects of . . ./n/n")
        
        # LMM w/ reson amplitude as continuous fixed effect
        cat("/nResonator output/n")
        
        # remove iteration limit from lme
        ctrl <- lmeControl(opt='optim')
        
        # group data (will be needed for plotting model predictions)
        thresh.data.grp <- groupedData(abs_val_thresh ~ reson_out | subject_id, data = thresh.data)
        
        ##################
        # specify model w/ reson predictor and random intercept for subject
        thresh_mod.reson = lme(abs_val_thresh ~ reson_out, control=ctrl, 
                               data = thresh.data.grp, random = ~1|subject_id,
                               na.action=na.omit, method = "ML")
        
        # plot predictions against data
        ap = augPred(thresh_mod.reson)
        plot_fname = file.path(params$fig_path,sprintf("%s_LMM_randInt.pdf", params$exp_name))
        pdf(file = plot_fname)
        p <- plot(ap, aspects = "xy", main="Random Intercept LMM", 
                   xlab="Resonator Amplitude", ylab="Intensity Deviant Threshold (dB SPL)",
                   grid = T)
        print(p)
        # close graphics device
        dev.off() 
        remove(plot_fname)
        
        ###################
        # add slope to random effects
        thresh_mod.reson_randSlope = update(thresh_mod.reson, random = ~reson_out|subject_id)
        ap.rndSlp = augPred(thresh_mod.reson_randSlope, aspect = "xy", grid = T)
        # plot predictions against data
        plot_fname = file.path(params$fig_path,sprintf("%s_LMM_randSlopeInt.pdf", params$exp_name))
        pdf(file = plot_fname)
        p.slope <- plot(ap.rndSlp, aspects = "xy", main="Random Slope and Intercept LMM",
                   xlab="Resonator Amplitude", ylab="Intensity Deviant Threshold (dB SPL)",
                   grid = T)
        print(p.slope)
        # close graphics device
        dev.off() 
        remove(plot_fname)
        
        # compare models w/ respect to random effects
        print(anova(thresh_mod.reson, thresh_mod.reson_randSlope))
        
        cat("/nSignificantly better fit with random slope effect included/n")
        
        # given significant diff in fit, sticking with rand slope + int effects
        print(summary(thresh_mod.reson_randSlope))
        #print(summary(thresh_mod.reson))
        
        ##########################
        # check for musician effects 
        cat("/nReson*musicianship/n")
        
        thresh_mod.reson_musician = lme(abs_val_thresh ~ reson_out*musicianship, 
                                        control=ctrl, data=thresh.data, 
                                        random = ~reson_out|subject_id, na.action=na.omit,
                                        method='ML')
        
        # compare model fit
        print(anova(thresh_mod.reson_randSlope, thresh_mod.reson_musician))
        # model fit not significantly different.
        print(summary(thresh_mod.reson_musician))
        
        #########################
        # check for stim complexity effects
        thresh_mod.stim_complexity <- update(thresh_mod.reson, .~. + reson_out*stim_complexity)
        
        # compare model fit
        print(anova(thresh_mod.reson, thresh_mod.stim_complexity))
        cat("/nSignificant difference in fit with stim complexity interaction fixed effect included/n")
        print(summary(thresh_mod.stim_complexity))

        sink()
        
}