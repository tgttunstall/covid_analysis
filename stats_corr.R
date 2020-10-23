#!/usr/bin/Rscript   
getwd()
setwd('~/git/covid_analysis/')
getwd()
############################################################
# TASK: Pairwise correaltion and P-values: T1
############################################################
# source data
source("read_data.R")

# clear unwanted variables
rm(my_data)
############################################################
#=============
# Output
#=============
# corr analysis
my_spearman_corr = paste0(outdir_stats, "corr_spearman_t1_v3.csv")
my_pvals = paste0(outdir_stats, "corr_pvalues_t1_v3.csv")
############################################################
# data assignment for stats
wf = wf_data
lf = lf_data
########################################################################
#           Pairwise correaltions and analyses
########################################################################

head(wf$outcomes)
wf <- wf[order(wf$outcomes),]

# subset t1 data
wf_t1 = cbind(wf[c(1:3)],  wf %>% dplyr:: select(grep("_t1", names(wf))))
colnames(wf_t1)

# assign nice column names
my_colnames = c("id"
                , "studygroup"
                , "outcomes"
                #, "idcol"
                , "Angiopoietin2"
                , "PF"       
                , "PSelectin"     
                , "sESelectin"  
                , "sICAM1"
                , "sRAGE"     
                , "sVCAM1")

if (length(my_colnames) == length(my_colnames)){
  print("PASS: Reassigning column names...")
  colnames(wf_t1) = my_colnames 
} else{
  cat(paste0("FAIL:Cannot assign column names, length mismatch"
             , "\nExpected length: ", length(orig_colanmes)
             , "\nGot:", length(my_colnames)))
  quit()
}    

print('formatted colnames are:'); print(colnames(wf_t1))

# inspect
class(wf_t1); str(wf_t1)

# Compute a matrix of correlation p-values
start_med = which(colnames(wf_t1) == "Angiopoietin2")
end_med = which(colnames(wf_t1) == "sVCAM1")

#---------------
# Data for corr analysis
#---------------
corr_M = wf_t1[start_med:end_med]

my_corr1 = cor(corr_M
              , method = "spearman"
              , use =  "pairwise.complete.obs")

my_corr = psych::corr.test(corr_M
                           , method = "spearman"
                           , use =  "pairwise.complete.obs")$r

my_pval =  psych::corr.test(corr_M
                            , method = "spearman"
                            , adjust = "none"
                            , use =  "pairwise.complete.obs")$p

#******************
# write output file: correlations and pvalues
#******************
write.csv(my_corr, my_spearman_corr, row.names = TRUE)
write.csv(my_pval, my_pvals, row.names = TRUE)
