#!/usr/bin/Rscript   
getwd()
setwd('~/git/covid_analysis/')
getwd()
############################################################
# TASK: basic plots

# useful links:
# http://www.sthda.com/english/wiki/ggplot2-dot-plot-quick-start-guide-r-software-and-data-visualization
############################################################
# source data
source("read_data.R")
############################################################
#=========================
# output: paired_analysis
#=========================
stats_time_paired = paste0(outdir_stats, "stats_paired_v3.csv")

############################################################
# data assignment for stats
wf = wf_data
lf = lf_data
########################################################################
# Pairwise stats by timepoint: wilcoxon paired analysis with correction
########################################################################
# with adjustment: fdr and BH are identical
my_adjust_method = "BH"
stats_by_timepoint = compare_means(value~timepoint, group.by = "mediator"
                                   , data = lf
                                   , paired = TRUE
                                   , p.adjust.method = my_adjust_method)


# check: satisfied!!!!
wilcox.test(wf$sESelectin_ngmL_t1, wf$sESelectin_ngmL_t2, paired = T)
wilcox.test(wf$sRAGE_pgmL_t1, wf$sRAGE_pgmL_t2, paired = T)

# delete unnecessary column
stats_by_timepoint = subset(stats_by_timepoint, select = -c(.y.))

# reflect stats method correctly
stats_by_timepoint$method
stats_by_timepoint$method = gsub("Wilcoxon", "Wilcoxon_paired", stats_by_timepoint$method)
stats_by_timepoint$method

# replace "." in colnames with "_"
colnames(stats_by_timepoint)
#names(stats_by_timepoint) = gsub("\.", "_", names(stats_by_timepoint)) # weird!!!!

colnames(stats_by_timepoint) = c("mediator"
                                 ,"group1"
                                 ,"group2"
                                 ,"p"
                                 ,"p_adj"
                                 ,"p_format"
                                 ,"p_signif"
                                 ,"method" ) 

colnames(stats_by_timepoint)

# add an extra column for padjust_signif
stats_by_timepoint$padjust_signif = round(stats_by_timepoint$p_adj, digits = 2)

# add appropriate symbols for padjust_signif
#stats_by_timepoint = stats_by_timepoint %>%
#  mutate(padjust_signif = case_when(padjust_signif == 0.05 ~ "."
#                                    , padjust_signif <0.05 ~ '*'
#                                    , padjust_signif <=0.01 ~ '**'
#                                    , padjust_signif <=0.001 ~ '***'
#                                    , padjust_signif <=0.0001 ~ '****'
#                                    , TRUE ~ 'ns'))
                                    
stats_by_timepoint = dplyr::mutate(stats_by_timepoint, padjust_signif = case_when(padjust_signif == 0.05 ~ "."
                                    , padjust_signif <=0.0001 ~ '****'
                                    , padjust_signif <=0.001 ~ '***'
                                    , padjust_signif <=0.01 ~ '**'
                                    , padjust_signif <0.05 ~ '*'
                                    , TRUE ~ 'ns'))

# reorder columns
print("preparing to reorder columns...")
colnames(stats_by_timepoint)
my_col_order2 = c("mediator"
                  , "group1"
                  , "group2"
                  , "method"
                  , "p"
                  , "p_format"
                  , "p_signif"
                  , "p_adj"
                  , "padjust_signif")

if( length(my_col_order2) == ncol(stats_by_timepoint) && isin(my_col_order2, colnames(stats_by_timepoint)) ){
  print("PASS: Reordering columns...")
  stats_by_timepoint_f = stats_by_timepoint[, my_col_order2]
  print("Successful: column reordering")
  print("formatted df called:'stats_by_timepoint_f'")
  cat('\nformatted df has the following dimensions\n')
  print(dim(stats_by_timepoint_f ))
} else{
  cat(paste0("FAIL:Cannot reorder columns, length mismatch"
             , "\nExpected column order for: ", ncol(stats_by_timepoint)
             , "\nGot:", length(my_col_order2)))
  quit()
}    

#******************
# write output file
#******************
cat("Paired stats by timepoint will be:", stats_time_paired)
write.csv(stats_by_timepoint_f, stats_time_paired, row.names = FALSE)
