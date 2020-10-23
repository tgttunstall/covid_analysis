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
#===================================
# output: KS test at each timepoint
#===================================
ks_each_time = paste0(outdir_stats, "KS_test_by_time_v3.csv")

############################################################
# data assignment for stats
wf = wf_data
lf = lf_data
#========================================================
sum(is.na(lf$value))
lf$log10_value = log10(lf$value)

#lf = lf[!is.na(lf$value),]
#sum(is.na(lf$value))
#str(lf$value)

########################################################################
#==============
# timepoint: t1
#==============
lf_t1 = lf[lf$timepoint=="t1",]

table(lf_t1$outcomes)

lf_death_t1 = lf_t1[lf_t1$outcomes == 0,]
lf_recovered_t1 = lf_t1[lf_t1$outcomes == 1,]

#=======================================================================
# individual mediator: t1
ks.test(lf_death_t1$value[lf_death_t1$mediator == "PF"]
, lf_recovered_t1$value[lf_recovered_t1$mediator == "PF"])

ks.test(lf_death_t1$value[lf_death_t1$mediator == "sRAGE"]
        , lf_recovered_t1$value[lf_recovered_t1$mediator == "sRAGE"])

#=======================================================================
# loop: t1
ks_df_t1 = data.frame()
mediator_names = levels(factor(lf$mediator))

for (i in mediator_names) {
  df_t1 = data.frame(mediator = NA, test_name = NA, ks_statistic = NA, ks_pvalue = NA)
  #print(i)
  ks.test(lf_death_t1$value[lf_death_t1$mediator == i]
          ,  lf_recovered_t1$value[lf_recovered_t1$mediator == i])
  
  med = i
  ks_method = ks.test(lf_death_t1$value[lf_death_t1$mediator == i]
                      ,  lf_recovered_t1$value[lf_recovered_t1$mediator == i])$method
  
  ks_statistic = ks.test(lf_death_t1$value[lf_death_t1$mediator == i]
                         ,  lf_recovered_t1$value[lf_recovered_t1$mediator == i])$statistic
  
  ks_pval = ks.test(lf_death_t1$value[lf_death_t1$mediator == i]
                    ,  lf_recovered_t1$value[lf_recovered_t1$mediator == i])$p.value
  
  #print(c(med, ks_method, ks_statistic[[1]], ks_pval))
  
  df_t1$mediator = med
  df_t1$test_name = ks_method 
  df_t1$ks_statistic = ks_statistic[[1]]
  df_t1$ks_pvalue = ks_pval
  
  print(df)
  ks_df_t1 = rbind(ks_df_t1, df_t1)
}

# formatting
ks_df_t1$group = "0 vs 1"
ks_df_t1$timepoint = "t1"

str(ks_df_t1)
ks_df_t1$pvalue_signif = ks_df_t1$ks_pvalue
str(ks_df_t1$pvalue_signif)

ks_df_t1 = dplyr::mutate(ks_df_t1
                         , pvalue_signif = case_when(pvalue_signif == 0.05 ~ "."
                                                     , pvalue_signif <=0.0001 ~ '****'
                                                     , pvalue_signif <=0.001 ~ '***'
                                                     , pvalue_signif <=0.01 ~ '**'
                                                     , pvalue_signif <0.05 ~ '*'
                                                     , TRUE ~ 'ns'))

##########################################################################
#==============
# timepoint: t2
#==============
lf_t2 = lf[lf$timepoint=="t2",]

table(lf_t2$outcomes)

lf_death_t2 = lf_t2[lf_t2$outcomes == 0,]
lf_recovered_t2 = lf_t2[lf_t2$outcomes == 1,]
#=======================================================================
# individual mediator: t2
ks.test(lf_death_t2$value[lf_death_t2$mediator == "PF"]
        , lf_recovered_t2$value[lf_recovered_t2$mediator == "PF"])

ks.test(lf_death_t2$value[lf_death_t2$mediator == "sRAGE"]
        , lf_recovered_t2$value[lf_recovered_t2$mediator == "sRAGE"])

#=======================================================================
# loop: t2
ks_df_t2 = data.frame()
mediator_names = levels(factor(lf$mediator))

for (i in mediator_names) {
  df_t2 = data.frame(mediator = NA, test_name = NA, ks_statistic = NA, ks_pvalue = NA)
  #print(i)
  ks.test(lf_death_t2$value[lf_death_t2$mediator == i]
          ,  lf_recovered_t2$value[lf_recovered_t2$mediator == i])
  
  med = i
  ks_method = ks.test(lf_death_t2$value[lf_death_t2$mediator == i]
                      ,  lf_recovered_t2$value[lf_recovered_t2$mediator == i])$method
  
  ks_statistic = ks.test(lf_death_t2$value[lf_death_t2$mediator == i]
                         ,  lf_recovered_t2$value[lf_recovered_t2$mediator == i])$statistic
  
  ks_pval = ks.test(lf_death_t2$value[lf_death_t2$mediator == i]
                    ,  lf_recovered_t2$value[lf_recovered_t2$mediator == i])$p.value
  
  #print(c(med, ks_method, ks_statistic[[1]], ks_pval))
  
  df_t2$mediator = med
  df_t2$test_name = ks_method 
  df_t2$ks_statistic = ks_statistic[[1]]
  df_t2$ks_pvalue = ks_pval
  
  print(df)
  ks_df_t2 = rbind(ks_df_t2, df_t2)
}

# formatting
ks_df_t2$group = "0 vs 1"
ks_df_t2$timepoint = "t2"

str(ks_df_t2)
ks_df_t2$pvalue_signif = ks_df_t2$ks_pvalue
str(ks_df_t2$pvalue_signif)

ks_df_t2 = dplyr::mutate(ks_df_t2
                         , pvalue_signif = case_when(pvalue_signif == 0.05 ~ "."
                                                     , pvalue_signif <=0.0001 ~ '****'
                                                     , pvalue_signif <=0.001 ~ '***'
                                                     , pvalue_signif <=0.01 ~ '**'
                                                     , pvalue_signif <0.05 ~ '*'
                                                     , TRUE ~ 'ns'))
#=======================================================================
#==============
# timepoint: t3
#==============
lf_t3 = lf[lf$timepoint=="t3",]

table(lf_t3$outcomes)

lf_death_t3 = lf_t3[lf_t3$outcomes == 0,]
lf_recovered_t3 = lf_t3[lf_t3$outcomes == 1,]
#=======================================================================
# individual mediator: t3
ks.test(lf_death_t3$value[lf_death_t3$mediator == "PF"]
        , lf_recovered_t3$value[lf_recovered_t3$mediator == "PF"])

ks.test(lf_death_t3$value[lf_death_t3$mediator == "sRAGE"]
        , lf_recovered_t3$value[lf_recovered_t3$mediator == "sRAGE"])

#=======================================================================
# loop: t3
ks_df_t3 = data.frame()
mediator_names = levels(factor(lf$mediator))

for (i in mediator_names) {
  df_t3 = data.frame(mediator = NA, test_name = NA, ks_statistic = NA, ks_pvalue = NA)
  #print(i)
  ks.test(lf_death_t3$value[lf_death_t3$mediator == i]
          ,  lf_recovered_t3$value[lf_recovered_t3$mediator == i])
  
  med = i
  ks_method = ks.test(lf_death_t3$value[lf_death_t3$mediator == i]
                      ,  lf_recovered_t3$value[lf_recovered_t3$mediator == i])$method
  
  ks_statistic = ks.test(lf_death_t3$value[lf_death_t3$mediator == i]
                         ,  lf_recovered_t3$value[lf_recovered_t3$mediator == i])$statistic
  
  ks_pval = ks.test(lf_death_t3$value[lf_death_t3$mediator == i]
                    ,  lf_recovered_t3$value[lf_recovered_t3$mediator == i])$p.value
  
  #print(c(med, ks_method, ks_statistic[[1]], ks_pval))
  
  df_t3$mediator = med
  df_t3$test_name = ks_method 
  df_t3$ks_statistic = ks_statistic[[1]]
  df_t3$ks_pvalue = ks_pval
  
  print(df)
  ks_df_t3 = rbind(ks_df_t3, df_t3)
}

# formatting
ks_df_t3$group = "0 vs 1"
ks_df_t3$timepoint = "t3"

str(ks_df_t3)
ks_df_t3$pvalue_signif = ks_df_t3$ks_pvalue
str(ks_df_t3$pvalue_signif)

ks_df_t3 = dplyr::mutate(ks_df_t3
                         , pvalue_signif = case_when(pvalue_signif == 0.05 ~ "."
                                                     , pvalue_signif <=0.0001 ~ '****'
                                                     , pvalue_signif <=0.001 ~ '***'
                                                     , pvalue_signif <=0.01 ~ '**'
                                                     , pvalue_signif <0.05 ~ '*'
                                                     , TRUE ~ 'ns'))

##########################################################################
# combine all three ks_dfs
n_dfs = 3
if ( all.equal(nrow(ks_df_t1), nrow(ks_df_t2), nrow(ks_df_t3)) && 
     all.equal(ncol(ks_df_t1), ncol(ks_df_t2), ncol(ks_df_t3)) ){
  cat("\nPASS: Calculating expected rows and cols for sanity checks on combined_dfs")
  expected_rows = nrow(ks_df_t1) * n_dfs
  expected_cols = ncol(ks_df_t1)
  ks_df_combined = rbind(ks_df_t1, ks_df_t2, ks_df_t3)
 if ( nrow(ks_df_combined) == expected_rows && ncol(ks_df_combined) == expected_cols ){
   cat("\nPASS: combined df successfully created"
       , "\nnrow combined_df:", nrow(ks_df_combined)
       , "\nncol combined_df:", ncol(ks_df_combined))
   }
  else{
      cat("\nFAIL: Dim mismatch"
        , "\nExpected rows:", expected_rows
        , "\nGot:", nrow(ks_df_combined)
        , "\nExpected cols:", expected_cols
        , "\nGot:", ncol(ks_df_combined))
        }
  }else{
  cat("\nFAIL: Could not generate expected_rows and/or expected_cols"
      , "\nCheck hardcoded value of n_dfs")
  }  

#=======================================================================
#******************
# write output file: KS test
#******************
cat("Output of KS test by time will be in:", ks_each_time)
write.csv(ks_df_combined, ks_each_time , row.names = FALSE)
