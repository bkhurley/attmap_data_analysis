attmap_v1p2_clean_data <- function(thresh.data, params) {
        # transforms subject names to integer IDs (factor)
        # detects and removes duplicate observations
        
        # anonmyize sub ids by transforming sub name to integer
        thresh.data <- thresh.data %>% 
                mutate(subject_id=factor(subject_id, labels=1:length(levels(thresh.data$subject_id))))
        
        # the following table shows situations in which n > 1 for a 
        # subject-stim-probe combo. these are duplicate observations
        thresh.data %>% 
                group_by(subject_id, stim_name, probe_time) %>% 
                filter(length(probe_time) > 1) %>%
                summarise(n = length(probe_time))
        
        # find where the duplicates exist and remove them from data set
        duplicate_idx <- thresh.data %>% 
                select(subject_id, stim_name, probe_time) %>% 
                duplicated
        thresh.data <- thresh.data[!(duplicate_idx),]
        
        return(thresh.data)
}