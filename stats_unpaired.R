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

# clear unwanted variables
rm(my_data)

############################################################
#============================
# Output: unpaired analysis
#============================
stats_time_unpaired = paste0(outdir_stats, "stats_unpaired_v3.csv")

############################################################
# data assignment for stats
wf = wf_data
lf = lf_data

########################################################################
# Unpaired stats at each timepoint b/w groups: wilcoxon UNpaired analysis with correction
#######################################################################
# with adjustment: fdr and BH are identical
my_adjust_method = "BH"

#==============
# unpaired: t1
#==============
lf_t1 = lf[lf$timepoint == "t1",]
stats_un_t1 = compare_means(value~outcomes, group.by = "mediator"
                                   , data = lf_t1 
                                   , paired = FALSE
                                   , p.adjust.method = my_adjust_method)

stats_un_t1$timepoint = "t1"

stats_un_t1 = as.data.frame(stats_un_t1)
class(stats_un_t1)

# check: satisfied!!!!
wilcox.test(wf$sESelectin_ngmL_t1[wf$outcomes == 0], wf$sESelectin_ngmL_t1[wf$outcomes == 1]
            , paired = FALSE)

wilcox.test(wf$PF_units_t1[wf$outcomes==0], wf$PF_units_t1[wf$outcomes == 1]
            , paired = FALSE)

#==============
# unpaired: t2
#==============
lf_t2 = lf[lf$timepoint == "t2",]
stats_un_t2 = compare_means(value~outcomes, group.by = "mediator"
                            , data = lf_t2 
                            , paired = FALSE
                            , p.adjust.method = my_adjust_method)

stats_un_t2$timepoint = "t2"

stats_un_t2 = as.data.frame(stats_un_t2)
class(stats_un_t2)

# check: satisfied!!!!
wilcox.test(wf$sESelectin_ngmL_t2[wf$outcomes == 0], wf$sESelectin_ngmL_t2[wf$outcomes == 1]
            , paired = FALSE)

wilcox.test(wf$PF_units_t2[wf$outcomes==0], wf$PF_units_t2[wf$outcomes == 1]
            , paired = FALSE)

#==============
# unpaired: t3
#==============
lf_t3 = lf[lf$timepoint == "t3",]
stats_un_t3 = compare_means(value~outcomes, group.by = "mediator"
                            , data = lf_t3 
                            , paired = FALSE
                            , p.adjust.method = my_adjust_method)

stats_un_t3$timepoint = "t3"

stats_un_t3 = as.data.frame(stats_un_t3)
class(stats_un_t3)

# check: satisfied!!!!
wilcox.test(wf$sESelectin_ngmL_t3[wf$outcomes == 0], wf$sESelectin_ngmL_t3[wf$outcomes == 1]
            , paired = FALSE)

wilcox.test(wf$PF_units_t3[wf$outcomes==0], wf$PF_units_t3[wf$outcomes == 1]
            , paired = FALSE)


#==============
# Rbind these dfs
#==============
str(stats_un_t1);str(stats_un_t2); str(stats_un_t3)

n_dfs = 3

if ( all.equal(nrow(stats_un_t1), nrow(stats_un_t2), nrow(stats_un_t3)) && 
     all.equal(ncol(stats_un_t1), ncol(stats_un_t2), ncol(stats_un_t3)) ) {
  expected_rows = nrow(stats_un_t1) * n_dfs
  expected_cols = ncol(stats_un_t1)
  print("PASS: expected_rows and cols variables generated for downstream sanity checks")
}else{
  cat("FAIL: dfs have different no. of rows and cols"
      , "\nCheck harcoded value of n_dfs"
      , "\nexpected_rows and cols could not be generated")
  quit()
}


if ( all.equal(colnames(stats_un_t1), colnames(stats_un_t2), colnames(stats_un_t3)) ){
  print("PASS: colnames match. Rbind the 3 dfs...")
  combined_unpaired_stats = rbind(stats_un_t1, stats_un_t2, stats_un_t3)
} else{
  cat("FAIL: cannot combined dfs. Colnames don't match!")
  quit()
}


if ( nrow(combined_unpaired_stats) == expected_rows && ncol(combined_unpaired_stats) == expected_cols ){
  cat("PASS: combined_df has expected dimension"
      , "\nNo. of rows in combined_df:", nrow(combined_unpaired_stats)
      , "\nNo. of cols in combined_df:", ncol(combined_unpaired_stats) )
}else{
  cat("FAIL: combined_df dimension mismatch")
  quit()
}

#===============================================================
# formatting df
# delete unnecessary column
combined_unpaired_stats = subset(combined_unpaired_stats, select = -c(.y.))

# reflect stats method correctly
combined_unpaired_stats$method
combined_unpaired_stats$method = gsub("Wilcoxon", "Wilcoxon_unpaired", combined_unpaired_stats$method)
combined_unpaired_stats$method

# replace "." in colnames with "_"
colnames(combined_unpaired_stats)
#names(combined_unpaired_stats) = gsub("\.", "_", names(combined_unpaired_stats)) # weird!!!!

colnames(combined_unpaired_stats) = c("mediator"
                                      ,"group1"
                                      ,"group2"
                                      ,"p"
                                      ,"p_adj"
                                      ,"p_format"
                                      ,"p_signif"
                                      ,"method"
                                      , "timepoint") 

colnames(combined_unpaired_stats)

# add an extra column for padjust_signif
combined_unpaired_stats$padjust_signif = round(combined_unpaired_stats$p_adj, digits = 2)

# add appropriate symbols for padjust_signif
#combined_unpaired_stats = combined_unpaired_stats %>%
#  mutate(padjust_signif = case_when(padjust_signif == 0.05 ~ "."
#                                    , padjust_signif <0.05 ~ '*'
#                                    , padjust_signif <=0.01 ~ '**'
#                                    , padjust_signif <=0.001 ~ '***'
#                                    , padjust_signif <=0.0001 ~ '****'
#                                    , TRUE ~ 'ns'))

combined_unpaired_stats  = dplyr::mutate(combined_unpaired_stats, padjust_signif = case_when(padjust_signif == 0.05 ~ "."
                                    , padjust_signif <=0.0001 ~ '****'
                                    , padjust_signif <=0.001 ~ '***'
                                    , padjust_signif <=0.01 ~ '**'
                                    , padjust_signif <0.05 ~ '*'
                                    , TRUE ~ 'ns'))


# reorder columns
print("preparing to reorder columns...")
colnames(combined_unpaired_stats)
my_col_order2 = c("mediator"
                  , "timepoint"
                  , "group1"
                  , "group2"
                  , "method"
                  , "p"
                  , "p_format"
                  , "p_signif"
                  , "p_adj"
                  , "padjust_signif")

if( length(my_col_order2) == ncol(combined_unpaired_stats) && isin(my_col_order2, colnames(combined_unpaired_stats)) ){
  print("PASS: Reordering columns...")
  combined_unpaired_stats_f = combined_unpaired_stats[, my_col_order2]
  print("Successful: column reordering")
  print("formatted df called:'combined_unpaired_stats_f'")
  cat('\nformatted df has the following dimensions\n')
  print(dim(combined_unpaired_stats_f ))
} else{
  cat(paste0("FAIL:Cannot reorder columns, length mismatch"
             , "\nExpected column order for: ", ncol(combined_unpaired_stats)
             , "\nGot:", length(my_col_order2)))
  quit()
}    

#******************
# write output file
#******************
cat("UNpaired stats for groups will be:", stats_time_unpaired)
write.csv(combined_unpaired_stats_f, stats_time_unpaired, row.names = FALSE)

