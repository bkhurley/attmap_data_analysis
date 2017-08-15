# Preliminary statistical analysis for attmap intensity decrement threshold data (attmap_intensitydec)
# Modeled after attmap_v1p2_stats.R. Called by attmap_intensitydec_analyses.R analysis stack.
# 07 DEC 2015 BH - created
# 16 DEC 2015 BH - organized into a function to conform to attmap_intensitydec_analyses.R job stack

attmap_intensitydec_stats_v2 <- function(thresh.data, params) {
        
        # Load mixed effects package
        require(nlme);
        
        # set output file
        sink(file.path(params$out_fpath,params$out_fname))
        
        cat("/nEvaluating fixed effects of . . ./n/n")
        
        ctrl <- lmeControl(opt='optim')
        
        thresh_mod.simp <- lme(abs_mean_pdf ~ 1, data = thresh.data, 
                              random = ~reson_out|subject_id,
                              control=ctrl,
                              na.action=na.exclude, method="ML")
        
        # check for resonator effect
        cat("\n################\nResonator\n")
        thresh_mod1 <- update(thresh_mod.simp, fixed= . ~ reson_out)
        print(summary(thresh_mod1))
        
        cat("\n################\nsMESS structure factor not significant (not by itself nor interaction)\n")
        thresh_mod3 <- update(thresh_mod1,fixed = . ~ reson_out*structure)
        print(summary(thresh_mod3))
        
        # thresh_mod1 = update(thresh_mod1, method="ML")
        # thresh_mod3 = update(thresh_mod3, method="ML")
        # 
        # cat("\nNo significnat added model fit w/ sMESS structure\n")
        # print(anova(thresh_mod1, thresh_mod3))
        
        cat("\n#############\nBIAS\n")
        thresh_mod4 <- update(thresh_mod1, fixed = . ~ . + reson_out*mean_vividness)
        print(summary(thresh_mod4))
        cat("\nMean vividness ratings not significant\n")
        
        # thresh_mod4 = update(thresh_mod4, method = "ML")
        # cat("\nNo significnat added model fit w/ BIAS\n")
        # print(anova(thresh_mod1, thresh_mod4))
        
        # check block# as predictor
        cat("\n################\nRUN#\n")
        cat("Raw Runs\n")
        thresh_mod5 <- update(thresh_mod1, fixed = . ~ . + run_num)
        print(summary(thresh_mod5))
        
        cat("Log Runs")
        thresh_mod5_runLog <- update(thresh_mod1, fixed = . ~ . + log10(run_num))
        print(summary(thresh_mod5_runLog))
        
        thresh_mod5 = update(thresh_mod5, method = "ML")
        thresh_mod5_runLog = update(thresh_mod5_runLog, method = "ML")
        cat("\nlog10(run_num) significantly better fit\n")
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