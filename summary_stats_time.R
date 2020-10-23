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
#===============================
# output: summary stats by time
#===============================
summary_stats_timepoint_combined = paste0(outdir_stats, "summary_stats_timepoint_v3.csv")

############################################################
# data assignment for stats
wf = wf_data
lf = lf_data
########################################################################
#=======================================================
# summary stats by timepoint and outcome: each mediator
#=======================================================
#******************************
# all major summary stats: by time and outcome
#******************************
gp_stats = groupedstats::grouped_summary(
  data = lf,
  grouping.vars = c(mediator, timepoint),
  measures = value,
  measures.type = "numeric")

# convert to df
gp_stats_df = as.data.frame(gp_stats)
orig_ncols = ncol(gp_stats_df)

print("Dropping skim cols...")
ncols_drop = 2
gp_stats_df  = subset(gp_stats_df , select = -c(skim_type, skim_variable))

if(ncol(gp_stats_df) == orig_ncols - ncols_drop){
  
  print('PASS: cols dropped successfully and dimemsions matched')
} else{
  print("FAIL: dim mismatch. check harcoded value of ncol_drop")
}

class(gp_stats_df); str(gp_stats_df)

# convert to factor to be consistent with to allow intersecting cols for merge
gp_stats_df$mediator = as.character(gp_stats_df$mediator)
gp_stats_df$timepoint = as.character(gp_stats_df$timepoint)

class(gp_stats_df); str(gp_stats_df)

#===============================================================

#******************************
# summary stats not covered 
# by groupedstats
#******************************
gp_gmean = lf %>%
  group_by(mediator, timepoint) %>%
  dplyr::summarise(total_subjects = n()
                   , mean = mean(value, na.rm = T)
                   , sd = sd(value, na.rm = T)
                   , mean_CI = qwraps2::frmtci(mean_ci(value, na_rm =T))
                   , gmean_SD = gmean_sd(value, na_rm = T, denote_sd = "paren")
                   , gmean_CI = qwraps2::frmtci(gmeanCI(value, na.rm = T)) 
                   , median =  median(value, na.rm = T)
                   , median_iqr = median_iqr(value, na_rm = T)
                   , skew = skewness(value, na.rm = T)
                   , kurtosis = kurtosis(value, na.rm = T)
  )

class(gp_gmean); str(gp_gmean)

#***************
# convert to df
#***************
gp_gmean_df = as.data.frame(gp_gmean)
class(gp_gmean_df); str(gp_gmean_df)

#***************
# format cols displaying na_rm info, this is covered by complete column
#***************
gp_gmean_df$gmean_SD <- gsub("[0-9]+; ", "", gp_gmean_df$gmean_SD)
gp_gmean_df$median_iqr <- gsub("[0-9]+; ", "", gp_gmean_df$median_iqr)

############################################################

#*******************
# merge the two dfs
#*******************
common_cols = intersect(names(gp_gmean_df), names(gp_stats_df))
print(common_cols)

# sanity check
for (i in common_cols){
  #print(i)
  if( all.equal(gp_gmean_df[i], gp_stats_df[i]) ){
    print('PASS')
    expected_rows = nlevels(factor(gp_gmean_df$mediator)) * nlevels(factor(gp_gmean_df$timepoint))
  } else{
    print('something went wrong!')
  }
}

# combining dfs
if (nrow(gp_gmean_df) && nrow(gp_stats_df) == expected_rows){
  expected_cols = ncol(gp_gmean_df) + ncol(gp_stats_df) - length(common_cols)
  print('combining dfs...')
  
  # combined_df of stats
  combined_stats_df = merge(gp_stats_df, gp_gmean_df, by = common_cols, all = T )
  
  cat(paste0('nrows combined df: ', dim(combined_stats_df)[1])) 
  cat(paste0('\nncols combined df: ', dim(combined_stats_df)[2])) 
}

# make complete_to_percentage
combined_stats_df$complete_cases_percent = round(combined_stats_df$complete*100, digits = 2)
head(combined_stats_df$complete_cases_percent)

# rearrange cols
orig_colnames = colnames(combined_stats_df)
orig_colnames

# rearrange columns
my_col_order = c("mediator" ,"timepoint" 
                 , "n", "total_subjects" ,"missing"   
                 , "complete", "complete_cases_percent"
                 , "mean", "mean.conf.low" ,"mean.conf.high", "mean_CI"
                 , "sd", "std.error"
                 , "min" ,"median","p25","p75"   
                 , "median_iqr","max", "gmean_SD"     
                 , "gmean_CI","skew","kurtosis")

length(my_col_order)

if( (length(orig_colnames) == length(my_col_order)) && all(my_col_order %in% orig_colnames) ) {
  cat("PASS: ncols match and colnames BOTH match for reordering"
      , "\nReordering columns...")
  # combined_df reordered
  combined_stats_df_formatted =  combined_stats_df[, my_col_order]
  print("Successful: column reordering")
  print("formatted df called:'combined_stats_df_formatted'")
  cat('\nformatted df has the following dimensions\n')
  print(dim(combined_stats_df_formatted ))
} else{
  cat(paste0("FAIL:Cannot reorder columns, length mismatch"
             , "\nExpected column order for: ", ncol(combined_stats_df)
             , "\nGot:", length(my_col_order)))
  quit()
}    

# rename cols
orig_colnames_f = colnames(combined_stats_df_formatted)
orig_colnames_f

# rename cols
new_colnames = c("mediator" ,"timepoint" 
                 , "n", "n_total_subjects" ,"n_missing"   
                 , "complete", "complete_cases_percent"
                 , "mean","mean_ci_low" ,"mean_ci_high", "mean_ci"
                 , "sd", "std_error"
                 , "min" ,"median","p25","p75"   
                 , "median_iqr","max", "gmean_SD"     
                 , "gmean_CI","skew","kurtosis")



if(length(new_colnames) == length(orig_colnames_f)){
  print("PASS: renaming columns...")
  colnames(combined_stats_df_formatted) = new_colnames 
  print("Columns renamed successfully")
}


# delete redundant cols
#combined_stats_df_formatted = subset(combined_stats_df_formatted, select = -c(mean_ci, median_iqr))

#cols_to_remove = c("complete", "mean_ci", "median_iqr")
combined_stats_df_f = subset(combined_stats_df_formatted, select = -c(complete, mean_ci, median_iqr) )

############################################################

#****************
# write file out
#*****************
cat("Summary stats by timepoint will be:", summary_stats_timepoint_combined)
write.csv(combined_stats_df_f, summary_stats_timepoint_combined, row.names = FALSE)

############################################################

