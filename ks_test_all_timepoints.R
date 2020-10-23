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
# output: KS test for cumulated time
#===================================
ks_all_timepoints = paste0(outdir_stats, "KS_test_all_timepoints_v3.csv")

############################################################
# data assignment for stats
wf = wf_data
lf = lf_data
#========================================================
# data assignment for stats
lf
sum(is.na(lf$value))

#lf = lf[!is.na(lf$value),]
#sum(is.na(lf$value))
#str(lf$value)

#========================================================
# ks test to analyse dist (all timepoints)
table(lf$outcomes, lf$mediator)
str(lf$outcomes)

table(lf$outcomes); str(lf$outcomes)

lf_death = lf[lf$outcomes==0,]
lf_recovered = lf[lf$outcomes==1,]

#=======================================================================
# for one
ks_pf = ks.test(lf_death$value[lf_death$mediator == "PF"]
                ,  lf_recovered$value[lf_recovered$mediator == "PF"]); ks_pf

ks_srage = ks.test(lf_death$value[lf_death$mediator == "sRAGE"]
                ,  lf_recovered$value[lf_recovered$mediator == "sRAGE"]); ks_srage

#=======================================================================
# loop
ks_df = data.frame()
mediator_names = levels(factor(lf$mediator))

for (i in mediator_names) {
  df = data.frame(mediator = NA, test_name = NA, ks_statistic = NA, ks_pvalue = NA)
  #df = data.frame()
  
  #print(i)
  ks.test(lf_death$value[lf_death$mediator == i]
                ,  lf_recovered$value[lf_recovered$mediator == i])
  
  med = i
  ks_method = ks.test(lf_death$value[lf_death$mediator == i]
                      ,  lf_recovered$value[lf_recovered$mediator == i])$method
  
  ks_statistic = ks.test(lf_death$value[lf_death$mediator == i]
               ,  lf_recovered$value[lf_recovered$mediator == i])$statistic
  
  ks_pval = ks.test(lf_death$value[lf_death$mediator == i]
                         ,  lf_recovered$value[lf_recovered$mediator == i])$p.value
  
  #print(c(med, ks_method, ks_statistic[[1]], ks_pval))
  
  df$mediator = med
  df$test_name = ks_method 
  df$ks_statistic = ks_statistic[[1]]
  df$ks_pvalue = ks_pval
  
  print(df)
  ks_df = rbind(ks_df, df)
}

#=======================================================================
# formatting
#=======================================================================
ks_df$group = "0 vs 1"
ks_df$timepoint = "t1,t2,t3"

str(ks_df)
ks_df$pvalue_signif = ks_df$ks_pvalue
str(ks_df$pvalue_signif)

# adding pvalue_signif
ks_df = dplyr::mutate(ks_df,pvalue_signif = case_when(pvalue_signif == 0.05 ~ "."
                                    , pvalue_signif <=0.0001 ~ '****'
                                    , pvalue_signif <=0.001 ~ '***'
                                    , pvalue_signif <=0.01 ~ '**'
                                    , pvalue_signif <0.05 ~ '*'
                                    , TRUE ~ 'ns'))


#******************
# write output file: KS test
#******************
cat("Output of KS test all timeoints by group will be in:", ks_all_timepoints)
write.csv(ks_df, ks_all_timepoints, row.names = FALSE)

