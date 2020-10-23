#!/usr/bin/Rscript   
getwd()
setwd('~/git/covid_analysis/')
getwd()
############################################################
# TASK: Loess plots for days into hospital till T1
############################################################

#==========================================================
#=============
# input:source data
#=============
source("read_data.R")

# clear unwanted variables
rm(my_data, wf_data, lf_data)

#=============
# Output
#=============
output_plots_loess_hosp = paste0(outdir_plots, "output_plots_v3_loess_hosp.pdf")

#%%========================================================
#=====================
# data for plots
#=====================
# remove PF
table(lf_data_loess$mediator)
lf_loess = lf_data_loess[lf_data_loess$mediator!= "PF",]
table(lf_loess$mediator)

#%=======================================================
hosp_days_min = min(lf_loess$days_from_hospitalization_t1); hosp_days_min 
hosp_days_max = max(lf_loess$days_from_hospitalization_t1);hosp_days_max

my_xscale_hosp = seq(hosp_days_min , hosp_days_max, 5)
my_xscale_hosp

#####################################################################
#                        95% CI: t1_data
#                        days_from_hospitalization_t1
#####################################################################

lf_loess_t1 = lf_loess[lf_loess$timepoint == "t1",]

# Output plots as one pdf
cat("Output plots will be in:", output_plots_loess_hosp)
pdf(output_plots_loess_hosp, width = 15, height = 8)

#-----------
# linear
#-----------
p1_hosp = ggplot(lf_loess_t1, aes(x = days_from_hospitalization_t1
               , y = value
               , colour = factor(outcomes))) + 
  #geom_point() + 
  geom_smooth(method = "loess", size = 1.5, na.rm = T) +
  facet_wrap(~mediator, nrow = 2, scales = "free_y")+
  labs(title = "Hospital days into t1: Linear scale and 95% CI"
       , x = "Days from hospitalisation to t1"
       , y = "T1 Levels")+
  scale_x_continuous(breaks = my_xscale_hosp, limits = c(hosp_days_min , hosp_days_max))+
  scale_colour_discrete(name = "Patient outcome"
                        , labels = c("Death", "Recovered"))+
  theme(axis.text.x = element_text(size = 13)
        , axis.text.y = element_text(size = 13
                                     , angle = 0
                                     , hjust = 1
                                     , vjust = 0)
        , axis.title.x = element_text(size = 13)
        , axis.title.y = element_text(size = 13)
        , plot.title = element_text(size = 15, hjust = 0.5)
        , strip.text.x = element_text(size = 13, colour = "black")
        , legend.title = element_text(color = "black", size = 13)
        , legend.text = element_text(size = 13)
        , legend.position = "right"
        , legend.direction = "vertical") 

p1_hosp

#-----------
# log
#-----------
p2_hosp = ggplot(lf_loess_t1, aes(x = days_from_hospitalization_t1
                  , y = value
                  , colour = factor(outcomes))) + 
  scale_y_log10()+
  #geom_point() + 
  geom_smooth(method = "loess", size = 1.5, na.rm = T) +
  facet_wrap(~mediator, nrow = 2, scales = "free_y")+
  labs(title = "Hospital days into t1: Log scale and 95% CI"
       , x = "Days from hospitalisation to t1"
       , y = "T1 Levels (Log10)")+
  
  scale_x_continuous(breaks = my_xscale_hosp, limits = c(hosp_days_min , hosp_days_max))+
  
  scale_colour_discrete(name = "Patient outcome"
                        , labels = c("Death", "Recovered"))+
  theme(axis.text.x = element_text(size = 13)
        , axis.text.y = element_text(size = 13
                                     , angle = 0
                                     , hjust = 1
                                     , vjust = 0)
        , axis.title.x = element_text(size = 13)
        , axis.title.y = element_text(size = 13)
        , plot.title = element_text(size = 15, hjust = 0.5)
        , strip.text.x = element_text(size = 13, colour = "black")
        , legend.title = element_text(color = "black", size = 13)
        , legend.text = element_text(size = 13)
        , legend.position = "right"
        , legend.direction = "vertical") 
p2_hosp

#####################################################################
#                               50% CI: t1_data
#                        days_from_hospitalization_t1
#####################################################################

#-----------
# linear
#-----------
p3_hosp = ggplot(lf_loess_t1, aes(x = days_from_hospitalization_t1
                       , y = value
                       , colour = factor(outcomes))) + 
  #geom_point() + 
  geom_smooth(method = "loess", size = 1.5, na.rm = T, level = 0.50) +
  facet_wrap(~mediator, nrow = 2, scales = "free_y")+
  labs(title = "Hospital days into t1: Linear scale and 50% CI"
       , x = "Days from hospitalisation to t1"
       , y = "T1 Levels") +
  scale_x_continuous(breaks = my_xscale_hosp, limits = c(hosp_days_min , hosp_days_max))+
  scale_colour_discrete(name = "Patient outcome"
                        , labels = c("Death", "Recovered"))+
  theme(axis.text.x = element_text(size = 13)
        , axis.text.y = element_text(size = 13
                                     , angle = 0
                                     , hjust = 1
                                     , vjust = 0)
        , axis.title.x = element_text(size = 13)
        , axis.title.y = element_text(size = 13)
        , plot.title = element_text(size = 15, hjust = 0.5)
        , strip.text.x = element_text(size = 13, colour = "black")
        , legend.title = element_text(color = "black", size = 13)
        , legend.text = element_text(size = 13)
        , legend.position = "right"
        , legend.direction = "vertical") 

p3_hosp

#-----------
# log
#-----------
p4_hosp = ggplot(lf_loess_t1, aes(x = days_from_hospitalization_t1
                       , y = value
                       , colour = factor(outcomes))) + 
  scale_y_log10()+
  #geom_point() + 
  geom_smooth(method = "loess", size = 1.5, na.rm = T, level = 0.50) +
  facet_wrap(~mediator, nrow = 2, scales = "free_y")+
  labs(title = "Hospital days into t1: Log scale and 50% CI"
       , x = "Days from hospitalisation to t1"
       , y = "T1 Levels (Log10)")+
  
  scale_x_continuous(breaks = my_xscale_hosp, limits = c(hosp_days_min , hosp_days_max))+
  
  scale_colour_discrete(name = "Patient outcome"
                        , labels = c("Death", "Recovered"))+
  theme(axis.text.x = element_text(size = 13)
        , axis.text.y = element_text(size = 13
                                     , angle = 0
                                     , hjust = 1
                                     , vjust = 0)
        , axis.title.x = element_text(size = 13)
        , axis.title.y = element_text(size = 13)
        , plot.title = element_text(size = 15, hjust = 0.5)
        , strip.text.x = element_text(size = 13, colour = "black")
        , legend.title = element_text(color = "black", size = 13)
        , legend.text = element_text(size = 13)
        , legend.position = "right"
        , legend.direction = "vertical") 
p4_hosp
dev.off()
#####################################################################
#                        95% CI: Combined data *** only if required
#                        days_from_hospitalization_t1
#####################################################################

#-----------
# linear
#-----------

#p1_all_hosp = ggplot(lf_loess, aes(x = days_from_hospitalization_t1
#                       , y = value
#                       , colour = factor(outcomes))) + 
#  #geom_point() + 
#  geom_smooth(method = "loess", size = 1.5, na.rm = T, level = 0.90) +
#  facet_wrap(~mediator, nrow = 2, scales = "free_y")+
#  labs(title = "Hospital days into t1: linear scale and 95% CI"
#       , x = "Days from hospitalisation to t1"
#       , y = "Combined Levels")+
  
#  scale_x_continuous(breaks = my_xscale_hosp, limits = c(hosp_days_min , hosp_days_max))+
  
#  scale_colour_discrete(name = "Patient outcome"
#                        , labels = c("Death", "Recovered"))

#p1_all_hosp

#-----------
# log
#-----------
#p2_all_hosp  = ggplot(lf_loess, aes(x = days_from_hospitalization_t1
#                       , y = value
#                       , colour = factor(outcomes))) + 
#  scale_y_log10()+
#  #geom_point() + 
#  geom_smooth(method = "loess", size = 1.5, na.rm = T) +
#  facet_wrap(~mediator, nrow = 2, scales = "free_y")+
#  labs(title = "Hospital days into t1: Log scale and 95% CI"
#       , x = "Days from hospitalisation to t1"
#       , y = "Combined Levels (Log10)")+
#  scale_x_continuous(breaks = my_xscale_hosp, limits = c(hosp_days_min , hosp_days_max))+
  
#  scale_colour_discrete(name = "Patient outcome"
#                        , labels = c("Death", "Recovered"))
#p2_all_hosp 
