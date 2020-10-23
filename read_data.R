#!/usr/bin/Rscript   
getwd()
setwd('~/git/covid_analysis/')
getwd()
############################################################
# TASK:  Read data to serve as input for downstream analyses
############################################################
# load libraries, packages and local imports

source("Header_TT.R")
#==========================================================
datadir = "~/git/covid_data/"
outdir = "~/git/covid_data/output/"
outdir_plots = "~/git/covid_data/output/plots/"
#outdir_plots_send = "~/git/covid_data/output/plots/to_send"
outdir_stats = "~/git/covid_data/output/stats/"

#==========================================================
# input data files
infile_covid = paste0(datadir, "/covid19_v3.csv")

infile_icu_wf = paste0(datadir, "/icu_covid_wf_v3.csv")
infile_icu_lf = paste0(datadir, "/icu_covid_lf_v3.csv")

infile_icu_wf_loess = paste0(datadir, "/icu_covid_wf_v3_loess.csv")
infile_icu_lf_loess = paste0(datadir, "/icu_covid_lf_v3_loess.csv")

#%%========================================================
# read files
my_data = read.csv(infile_covid, stringsAsFactors = F) 

wf_data = read.csv(infile_icu_wf, stringsAsFactors = F) 
dim(wf_data)

lf_data = read.csv(infile_icu_lf, stringsAsFactors = F) 
dim(lf_data)

wf_data_loess = read.csv(infile_icu_wf_loess, stringsAsFactors = F) 
dim(wf_data_loess)

lf_data_loess = read.csv(infile_icu_lf_loess, stringsAsFactors = F) 
dim(lf_data_loess)
#%%========================================================
