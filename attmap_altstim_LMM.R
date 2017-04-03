# Builds and prints linear mixed-effects model for attmap_altstim experiments 
# (attmap_altstim_inc & attmap_altstim_intensitydec).
# 
# Called from attmap_altstim_analyses.R 
# 
# 06 Nov 2016 - BKH

attmap_altstim_LMM <- function(thresh.data, params) {
        
        # Load mixed effects package
        require(nlme)
        require(dplyr)
        
        # set output file
        sink(file.path(params$out_fpath,params$out_fname))
        
        # get data from only Blocks 1 & 4 in separate tables. We will use this 
        # in testing BAT results -- i.e. whether any difference in effects btw 
        # beginning & end of experiment
        thresh.data.blck1 <- filter(thresh.data, run_num==1)
        thresh.data.blck4 <- filter(thresh.data, run_num==4)
        
        # remove iteration limit from lme
        ctrl <- lmeControl(opt='optim')
        
        # specify the DV based on which experiment we are looking at
        if (params$exp_name == "attmap_altstim_intensitydec") {
                thresh.data$dv <- thresh.data$abs_val_thresh
                thresh.data.blck1$dv <- thresh.data.blck1$abs_val_thresh
                thresh.data.blck4$dv <- thresh.data.blck4$abs_val_thresh
                thresh.data$iso <- thresh.data$abs_val_meanIso
                thresh.data.blck1$iso <- thresh.data.blck1$abs_val_meanIso
                thresh.data.blck4$iso <- thresh.data.blck4$abs_val_meanIso
                
                ##################
                # specify model w/ reson predictor and random intercept, and random slope for subjects
                lmm.simp <- lme(dv ~ 1, control=ctrl, random = ~ reson_out|subject_id, 
                                data=thresh.data, na.action=na.exclude, method="REML")
                
                cat('\n\n##########\nMODEL1\n')
                lmm.mod1 <- update(lmm.simp, . ~ reson_out*musicianship + reson_out*stim_complexity + 
                                           reson_out*abs_val_meanIso + bat_tempo_thresh + 
                                           bat_phase_thresh)
                print(summary(lmm.mod1))
                
                # repeate above model on BLOCK 1
                cat('\n\n##########\nMODEL1 - BLOCK 1\n')
                lmm.mod1.blck1 <- update(lmm.mod1, data=thresh.data.blck1)
                summary(lmm.mod1.blck1)
                
                # repeate above model on BLOCK 1
                cat('\n\n##########\nMODEL1 - BLOCK 4\n')
                lmm.mod1.blck4 <- update(lmm.mod1.blck1, data=thresh.data.blck4)
                summary(lmm.mod1.blck4)
                
                cat('\n\n##########\nMODEL2: BAT removed\n')
                lmm.mod2 <- update(lmm.mod1, . ~ reson_out*musicianship + reson_out*stim_complexity +
                                           reson_out*abs_val_meanIso)
                print(summary(lmm.mod2))
                
                cat('\n\n##########\nMODEL3: sMESS - Structure\n')
                lmm.mod3 <- update(lmm.mod2, fixed = . ~ . + reson_out*structure)
                cat('\nsMESS structure not significant\n')
                print(summary(lmm.mod3))
                
                cat('\n\n##########\nMODEL4: BIAS\n')
                lmm.mod4 <- update(lmm.mod2, fixed = . ~ . + reson_out*mean_vividness)
                cat('\nBIAS not significant\n')
                print(summary(lmm.mod4))
                
                cat('\n\n##########\nMODEL5: RUN# (raw)\n')
                lmm.mod5 <- update(lmm.mod2, fixed = . ~ . + run_num)
                cat('\nRun# not significant\n')
                print(summary(lmm.mod5))
                
                # print final model
                cat('\n\n##########\nFINAL MODEL FIT WITH REML\n')
                final_model <- update(lmm.mod5, method="REML")
                print(summary(final_model))
                
        } else {  
                thresh.data$dv <- thresh.data$mean_pdf
                thresh.data$iso <- thresh.data$mean_iso_thresh
                
                ##################
                # specify model w/ reson predictor and random intercept, and random slope for subjects
                lmm.simp <- lme(dv ~ 1, control=ctrl, random = ~ reson_out|subject_id, 
                                data=thresh.data, na.action=na.exclude, method="REML")
                
                cat('\n\n##########\nMODEL1\n')
                lmm.mod1 <- update(lmm.simp, . ~ reson_out*musicianship + reson_out*stim_complexity + 
                                           reson_out*mean_iso_thresh + bat_tempo_thresh + 
                                           bat_phase_thresh)
                print(summary(lmm.mod1))
                
                # repeate above model on BLOCK 1
                cat('\n\n##########\nMODEL1 - BLOCK 1\n')
                lmm.mod1.blck1 <- update(lmm.mod1, data=thresh.data.blck1, mean_pdf ~ .)
                summary(lmm.mod1.blck1)
                
                # repeate above model on BLOCK 1
                cat('\n\n##########\nMODEL1 - BLOCK 4\n')
                lmm.mod1.blck4 <- update(lmm.mod1.blck1, data=thresh.data.blck4)
                summary(lmm.mod1.blck4)
                
                cat('\n\n##########\nMODEL2\n')
                lmm.mod2 <- update(lmm.mod1, . ~ reson_out*stim_complexity + reson_out*musicianship + 
                                           reson_out*mean_iso_thresh)
                print(summary(lmm.mod2))
                
                cat('\n\n##########\nMODEL3: ISO removed\n')
                lmm.mod3 <- update(lmm.mod2, . ~ reson_out*stim_complexity + reson_out*musicianship)
                print(summary(lmm.mod3))
                
                cat('\n\n##########\nMODEL4: sMESS - Structure\n')
                lmm.mod4 <- update(lmm.mod3, fixed = . ~ . + reson_out*structure)
                cat('\nsMESS structure not significant\n')
                print(summary(lmm.mod4))
                
                cat('\n\n##########\nMODEL5: BIAS\n')
                lmm.mod5 <- update(lmm.mod3, fixed = . ~ . + reson_out*mean_vividness)
                cat('\nsMESS structure not significant\n')
                print(summary(lmm.mod5))
                
                cat('\n\n##########\nMODEL6: RUN# (raw)\n')
                lmm.mod6 <- update(lmm.mod3, fixed = . ~ . + run_num)
                cat('\nRun# not significant\n')
                print(summary(lmm.mod6))
                
                cat('\n\n##########\nMODEL7: RUN# (log10)\n')
                lmm.mod7 <- update(lmm.mod3, fixed = . ~ . + log10(run_num))
                cat('\nlog10(run) not significant\n')
                print(summary(lmm.mod7))
                
                
                # print final model
                cat('\n\n##########\nFINAL MODEL FIT WITH REML\n')
                final_model <- update(lmm.mod3, method="REML")
                print(summary(final_model))
        }
        
        
        # write data to file
        sink()
        
        lmm.mod <- list()
        lmm.mod$simple_mod <- lmm.simp
        lmm.mod$final_mod <- final_model
        
        # output list with simple & final model
        return(lmm.mod)
}