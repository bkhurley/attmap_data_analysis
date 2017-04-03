# Statistical analysis for attmap_altstim experiments (attmap_altstim_inc & attmap_altstim_intensitydec)
# Called from attmap_altstim_analyses.R 
# 
# 06 Nov 2016 - BKH

attmap_altstim_inc_LMM <- function(data, params) {
        
        # Load mixed effects package
        require(nlme)
        
        # set output file
        sink(file.path(params$out_fpath,params$out_fname))
        
        cat("#####################\nMODEL COMPARISONS WITH REGARD TO RANDOM & FIXED EFFECTS\n\n")
        
        # remove iteration limit from lme
        ctrl <- lmeControl(opt='optim')
        
        # specify the DV based on which experiment we are looking at
        if(params$exp_name == "attmap_altstim_intensitydec"){
                dv = data$abs_val_thresh
                iso = data$abs_val_meanIso
        } else {
                dv - data$mean_pdf
                iso = data$mean_iso_thresh
        }
        
        ##################
        # specify model w/ reson predictor and random intercept, and random slope for subjects
        lmm.rand_eff = lme(dv ~ reson_out, control = ctrl, random = ~reson_out|subject_id, 
                           data=data, na.action=na.omit, method = "ML")
        
        ###################
        # add slope to random effects
        # thresh_mod.reson_randSlope = update(thresh_mod.reson, random = ~reson_out|subject_id)
        
#         # plot predictions against data
#         if(params$generate_plots){
#                 ap.rndSlp <- augPred(thresh_mod.reson_randSlope, aspect = "xy", grid = T)
#                 plot_fname <- file.path(params$fig_fpath,sprintf("%s_LMM_randSlopeInt.pdf", params$exp_name))
#                 pdf(file = plot_fname)
#                 p.slope <- plot(ap.rndSlp, aspects = "xy", main="Random Slope and Intercept LMM",
#                                 xlab="Resonator Amplitude", ylab="Intensity Deviant Threshold (dB SPL)",
#                                 grid = T)
#                 print(p.slope)
#                 # close graphics device
#                 dev.off() 
#                 remove(plot_fname)
#         }
        
#         # compare models w/ respect to random effects
#         mod_compare.rand_eff <- anova(thresh_mod.reson, thresh_mod.reson_randSlope)
#         print(mod_compare.rand_eff)
#         
#         # if more complex model (randSlope) yields significantly better fit, 
#         # go with that one. Else, stay with simpler model
#         if(mod_compare.rand_eff$`p-value`[2] < params$crit_pval) {
#                 cat("\nSignificantly better fit with random slope effect included\n")
#                 lmm.rand_eff <- thresh_mod.randSlope
#         } else {
#                 cat("\nNo significant difference in fit with random slope effect included\n")
#                 lmm.rand_eff <- thresh_mod.reson
#         }
        
        #print(summary(thresh_mod.reson))
        
        #########################
        # check for musician fixed effects 
        
        # must remove subjects w/ no musicianship categorization so the two models 
        # have same # observations
        thresh.musNAremoved <- subset(data, !(musicianship %in% NA));
        reson_out.musNAremoved <- thresh.musNAremoved$reson_out
        subject_id.musNAremoved <- thresh.musNAremoved$subject_id
        musicianship.musNAremoved <- thresh.musNAremoved$musicianship
        if(params$exp_name == "attmap_altstim_intensitydec"){
                dv.musNAremoved = thresh.musNAremoved$abs_val_thresh
        } else {
                dv.musNAremoved - thresh.musNAremoved$mean_pdf
        }
        
        # re-specify model w/ subsetted data
        lmm.rand_eff.NAremoved = update(lmm.rand_eff, dv.musNAremoved ~ .,
                                        data=thresh.musNAremoved)
        
        # model w/ musicianship interaction term
        thresh_mod.reson_musician = update(lmm.rand_eff.NAremoved, .~. + 
                                                   reson_out*musicianship)
        
        # compare model fit
        mod_compare.fixed_musician <-  anova(lmm.ran_eff.NAremoved, thresh_mod.reson_musician)
        print(mod_compare.fixed_musician)
        
        if(mod_compare.fixed_musician$`p-value`[2] < params$crit_pval) {
                cat("\nSignificantly better fit with musician fixed effect included\n")
                lmm.rand_fixed1 <- thresh_mod.reson_musician
                data <- thresh.musNAremoved
#                 reson_out <- reson_out.musNAremoved
#                 stim_complexity <- thresh.musNAremoved$stim_complexity
        } else {
                cat("\nNo significant difference in fit with musician fixed effect included\n")
                lmm.rand_fixed1 <- lmm.rand_eff
                data <- data
        }
        
#         #############################
#         # check for effect of yrs mus training
#         thresh_mod.yrs_training <- update(thresh_mod.reson, .~. + reson_out*years_inst_training)
#         #compare model fit
#         print(anova(thresh_mod.reson.NAremoved, thresh_mod.yrs_training))
#         cat("/nNo significant difference in fit with years_inst_training included/n")
        
        ############################
        # check for stimulus complexity fixed effects
        thresh_mod.stim_complexity <- update(lmm.rand_fixed1, .~. + reson_out*stim_complexity)
        
        # compare model fit
        mod_compare.fixed_stimComplexity <- anova(lmm.rand_fixed1, thresh_mod.stim_complexity)
        print(mod_compare.fixed_stimComplexity)
        
        if(mod_compare.fixed_stimComplexity$`p-value`[2] < params$crit_pval) {
                cat("\nSignificantly better fit with stim complexity fixed effect included\n")
                lmm.rand_fixed2 <- thresh_mod.stim_complexity
        } else {
                cat("\nNo significant difference in fit with stim complexity fixed effect included\n")
                lmm.rand_fixed2 <- lmm.rand_fixed1
        }
        
        ############################
        # check for isochronous task performance fixed effects
        
        # first need to remove observations from previous model where we have no isoch observations
        thresh.isoNAremoved = subset(data, !(mean_iso_thresh %in% NaN))
        
#         reson_out.isoNAremoved <- thresh.isoNAremoved$reson_out
#         stim_complexity.isoNAremoved <- thresh.isoNAremoved$stim_complexity
#         subject_id.isoNAremoved <- thresh.isoNAremoved$subject_id
#         iso.isoNAremoved <- thresh.isoNAremoved$mean_iso_thresh
        if(params$exp_name == "attmap_altstim_intensitydec"){
                dv.isoNAremoved = thresh.isoNAremoved$abs_val_thresh
        } else {
                dv.isoNAremoved - thresh.isoNAremoved$mean_pdf
        }
        # respecify previous model w/ this data set
        lmm.rand_fixed2.NAremoved <- update(lmm.rand_fixed2, dv.isoNAremoved ~ ., 
                                            data=thresh.isoNAremoved)
        
        # model w/ mean iso performance fixed effect
        thresh_mod.iso <- update(lmm.rand_fixed2.NAremoved, .~. + reson_out*mean_iso_thresh)
        
        # compare model fit
        mod_compare.fixed_iso <- anova(lmm.rand_fixed2.NAremoved, thresh_mod.iso)
        print(mod_compare.fixed_iso)
        
        if(mod_compare.fixed_iso$`p-value`[2] < params$crit_pval) {
                cat("\nSignificantly better fit with mean isochronous performance fixed effect included\n")
                lmm.rand_fixed3 <- thresh_mod.iso
                data <- thresh.isoNAremoved
        } else {
                cat("\nNo significant difference in fit with mean isochronous performance fixed effect included\n")
                lmm.rand_fixed3 <- lmm.rand_fixed2
                data <- data
        }
        
        ############################
        # check for BAT-Tempo threshold fixed effect
        
        # first need to remove observations from previous model where we have no isoch observations
        thresh.BAT_tempo_NAremoved <- subset(data, !(bat_tempo_thresh %in% NaN))
        if(params$exp_name == "attmap_altstim_intensitydec"){
                dv.BAT_tempo_NAremoved <- thresh.BAT_tempo_NAremoved$abs_val_thresh
        } else {
                dv.BAT_tempo_NAremoved <- thresh.BAT_tempo_NAremoved$mean_pdf
        }
        
        # respecify previous model w/ this data set
        lmm.rand_fixed3.NAremoved <- update(lmm.rand_fixed3, dv.BAT_tempo_NAremoved ~ ., 
                                            data=thresh.BAT_tempo_NAremoved)
        
        # model w/ mean iso performance fixed effect
        thresh_mod.bat_tempo <- update(lmm.rand_fixed3.NAremoved, .~. + reson_out*bat_tempo_thresh)
        
        # compare model fit
        mod_compare.bat_tempo <- anova(lmm.rand_fixed3.NAremoved, thresh_mod.bat_tempo)
        print(mod_compare.bat_tempo)
        
        if(mod_compare.bat_tempo$`p-value`[2] < params$crit_pval) {
                cat("\nSignificantly better fit with BAT-Tempo fixed effect included\n")
                lmm.rand_fixed4 <- thresh_mod.bat_tempo
                data <- thresh.BAT_tempo_NAremoved
        } else {
                cat("\nNo significant difference in fit with BAT-Tempo fixed effect included\n")
                lmm.rand_fixed4 <- lmm.rand_fixed3
                data <- data
        }
        
        ############################
        # check for BAT-Phase threshold fixed effects
        
        # first need to remove observations from previous model where we have no isoch observations
        thresh.BAT_phase_NAremoved <- subset(data, !(bat_phase_thresh %in% NaN))
        if(params$exp_name == "attmap_altstim_intensitydec"){
                dv.BAT_phase_NAremoved <- thresh.BAT_phase_NAremoved$abs_val_thresh
        } else {
                dv.BAT_phase_NAremoved <- thresh.BAT_phase_NAremoved$mean_pdf
        }
        
        # respecify previous model w/ this data set
        lmm.rand_fixed4.NAremoved <- update(lmm.rand_fixed4, dv.BAT_phase_NAremoved ~ ., 
                                            data=thresh.BAT_phase_NAremoved)
        
        # model w/ mean iso performance fixed effect
        thresh_mod.bat_phase <- update(lmm.rand_fixed4.NAremoved, .~. + reson_out*bat_phase_thresh)
        
        # compare model fit
        mod_compare.bat_phase <- anova(lmm.rand_fixed4.NAremoved, thresh_mod.bat_phase)
        print(mod_compare.bat_phase)
        
        if(mod_compare.bat_phase$`p-value`[2] < params$crit_pval) {
                cat("\nSignificantly better fit with BAT-Tempo fixed effect included\n")
                lmm.rand_fixed5 <- thresh_mod.bat_phase
                data <- thresh.BAT_phase_NAremoved
        } else {
                cat("\nNo significant difference in fit with BAT-Tempo fixed effect included\n")
                lmm.rand_fixed5 <- lmm.rand_fixed4
                data <- data
        }
        
        # print final model
        final_model <- update(lmm.rand_fixed5, method="REML")
        cat("\n\n#####################\nFINAL MODEL\n\n")
        print(summary(final_model))
        
        # output data
        sink()
        
        return(final_model)
        
}