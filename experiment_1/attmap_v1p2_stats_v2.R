attmap_v1p2_stats_v2 <- function(thresh.data, params) {

        # Preliminary statistical analysis for attmap intensity deviance threshold data (attmap_v1p2)
        # 23 May 2015 BH - started script
        # 16 Dec 2015 BH - reorganized code into a function to adapt to attmap_v1p2_analyses.R 
        #                       analysis job stack; added txt output of results
        
        # Load mixed effects package
        require(nlme)
        
        # set output file
        sink(file.path(params$out_fpath,params$out_fname))
        
        # optimize convergence iteration limit
        ctrl <- lmeControl(opt='optim')
        
        cat("\nEvaluating fixed effects of . . .\n\n")
        
        # LMM w/ random effects of subjects' slopes and intercepts
        thresh_mod.simp <- lme(mean_pdf ~ 1, data = thresh.data, random = ~ reson_out|subject_id,
                               na.action=na.exclude, method="REML")
        
        # simple model w/ fixed effect of interest: resonator 
        cat("\n################\nResonator output\n")
        thresh_mod1 <- lme(mean_pdf ~ reson_out, control=ctrl,
                                        data = thresh.data, random = ~ reson_out|subject_id,
                                        na.action=na.exclude, method="REML")
        print(summary(thresh_mod1))
        
        # check sMESS "structure" factor fixed effect
        cat("\n################\nsMESS structure factor not significant (not by itself nor interaction)\n")
        thresh_mod3 <- lme(mean_pdf ~ reson_out*structure, 
                                        data = thresh.data, random = ~ reson_out|subject_id, 
                                        na.action=na.exclude, method="ML")
        print(summary(thresh_mod3))
        
        cat("\n#############\n(checked STOMP factors. none significant)\n")
        
        cat("\n#############\nBIAS\n")
        thresh_mod4 <- update(thresh_mod1, fixed = . ~ reson_out + mean_vividness)
        print(summary(thresh_mod4))
        cat("\nMean vividness ratings not significant\n")
        
        
        # check block# as predictor
        cat("\n################\nRUN# (remove musicianship so we have full data set)\n")
        cat("Raw Runs\n")
        thresh_mod5 <- update(thresh_mod1, fixed = . ~ reson_out + run_num, method="ML")
        print(summary(thresh_mod5))
        
        cat("Log10(Block Number)")
        thresh_mod5_runLog <- update(thresh_mod1, fixed = . ~ reson_out + log10(run_num),
                                     method="ML")
        print(summary(thresh_mod5_runLog))
        
        cat("\nlog10(block number) significantly better fit\n")
        print(anova(thresh_mod5, thresh_mod5_runLog))
        
        cat("\n######################\nFINAL MODEL\n")
        final_model <- update(thresh_mod5_runLog, method="REML")
        print(summary(final_model))
        
        sink()
        
        lmm.mod <- list()
        lmm.mod$simple_mod <- thresh_mod.simp
        lmm.mod$final_mod <- final_model
        
        return(lmm.mod)
        
}