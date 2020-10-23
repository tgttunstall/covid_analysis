#!/usr/bin/Rscript   
getwd()
setwd('~/git/covid_analysis/')
getwd()
############################################################
# TASK: checking stats from manuscript
############################################################
# source data
source("read_data.R")

#%%========================================================
# read file
lf_data = read.csv(infile_icu_lf , stringsAsFactors = F) 
dim(lf_data)

wf_data = read.csv(infile_icu_wf , stringsAsFactors = F) 
dim(wf_data)

lf_t1 = lf_data[lf_data$timepoint == "t1",]

lf_death_t1 = lf_t1[lf_t1$outcomes == 0,]
lf_recovered_t1 = lf_t1[lf_t1$outcomes == 1,]


wf_death = wf_data[wf_data$outcomes == 0,]
wf_recovered = wf_data[wf_data$outcomes == 1,]
kruskal.test(wf_death$Angiopoietin2_pgmL_t1, wf_death$Angiopoietin2_pgmL_t2, wf_death$Angiopoietin2_pgmL_t3)

friedman.test(wf_death$Angiopoietin2_pgmL_t1, wf_recovered$Angiopoietin2_pgmL_t1)



wilcox.test(wf_data$sRAGE_pgmL_t1, wf_data$sRAGE_pgmL_t3)

# checks
# sRAGE
wilcox.test(wf$sRAGE_pgmL_t1, wf$sRAGE_pgmL_t3, paired = T) # 0.00039

ks.test(wf_death$sRAGE_pgmL_t1, wf_death$sRAGE_pgmL_t2, wf_death$sRAGE_pgmL_t3)
ks.test(wf_death$sRAGE_pgmL_t1, wf_death$sRAGE_pgmL_t3)
wilcox.test(wf_death$sRAGE_pgmL_t1, wf_death$sRAGE_pgmL_t3, paired = T)
summary(wf_death$sRAGE_pgmL_t1)


ks.test(lf_death_t1$value[lf_death_t1$mediator == "sRAGE"]
        , lf_recovered_t1$value[lf_recovered_t1$mediator == "sRAGE"])

#t1
median(lf_t1$value[lf_t1$mediator == "sRAGE"])
summary(lf_t1$value[lf_t1$mediator == "sRAGE"])

# t2
lf_t2 = lf_data[lf_data$timepoint == "t2",]
median(lf_t2$value[lf_t2$mediator == "sRAGE"], na.rm = T)
summary(lf_t2$value[lf_t2$mediator == "sRAGE"], na.rm = T)

#t3
lf_t3 = lf_data[lf_data$timepoint == "t3",]
median(lf_t3$value[lf_t3$mediator == "sRAGE"], na.rm = T)
summary(lf_t3$value[lf_t3$mediator == "sRAGE"], na.rm = T)

# for trend
lf_death = lf_data[lf_data$outcomes == 0,]
lf_recovered = lf_data[lf_data$outcomes == 1,]


ks.test(lf_death$value[lf_death$mediator == "sRAGE"]
        , lf_recovered$value[lf_recovered$mediator == "sRAGE"])
