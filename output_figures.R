#!/usr/bin/Rscript   
getwd()
setwd('~/git/covid_analysis/')
getwd()
############################################################
# TASK: ggridges at T1 only
# selected figure for manuscript
############################################################
# source data
source("read_data.R")

# clear unwanted variables
rm(my_data)
############################################################
#=============
# Output
#=============
output_dist_t1 = paste0(outdir_plots, "output_dist_t1_v3.svg")
output_dist_t1_f = paste0(outdir_plots, "output_dist_t1_v3_f.svg")

#%%========================================================
# read file
lf_data = read.csv(infile_icu_lf , stringsAsFactors = F) 
dim(lf_data)

#=====================
# data for plots: LF
#=====================
# omit PF
table(lf_data$mediator)
expected_rows = nrow(lf_data) - table(lf_data$mediator)[["PF"]]; expected_rows
expected_cols = length(lf_data); expected_cols

lf = lf_data[lf_data$mediator!="PF",]
table(lf$mediator)

if ( nrow(lf) == expected_rows && length(lf) == expected_cols ){
  cat ("PASS: dimensions match in subsetted data"
       , "\nnrows:", nrow(lf)
       , "\nncols:", length(lf))
}else{
  cat ("FAIL: dimensions mismatch. Check expected rows and cols calculations"
       , "\nExpected rows:", expected_rows
       , "\nGot:", nrow(lf)
       , "\nExpected cols:", expected_cols
       , "\nGot:", length(lf))
}

dim(lf); str(lf)

levels(factor(lf$mediator)); table(lf$mediator)

#---------------------
# nice mediator names
#---------------------
nice_mediator_names1 = c(Angiopoietin2 = "ANG-2"
                        , PSelectin = "sP-Selectin"
                        , sESelectin = "sE-Selectin"
                        , sICAM1="sICAM-1"
                        , sRAGE= "sRAGE"
                        , sVCAM1="sVCAM-1")

# assign nice mediator names 
lf$mediator_names1 <- as.character(nice_mediator_names1[lf$mediator])

#---------------------
# nice names without leading 's'
#---------------------
nice_mediator_names2 = c(Angiopoietin2 = "ANG-2"
                        , PSelectin = "P-Selectin"
                        , sESelectin = "E-Selectin"
                        , sICAM1="ICAM-1"
                        , sRAGE= "RAGE"
                        , sVCAM1="VCAM-1")

# assign nice mediator names without leading 's' 
lf$mediator_names2 <- as.character(nice_mediator_names2[lf$mediator])

dim(lf); str(lf)
levels(factor(lf$mediator)); table(lf$mediator)

#-------------------------------
# generate order for mediators
#------------------------------
mediator_order = c("Angiopoietin2" = 2
                        , "PSelectin" = 5
                        , "sESelectin" = 6
                        , "sICAM1" = 3
                        , "sRAGE" = 1
                        , "sVCAM1" = 4)

# reorder mediators as required
lf$order_mediators <- mediator_order[lf$mediator]
table(lf$order_mediators)

# reorder the df according to order_mediators: not needed as ggridges ignores this!
#table(lf$mediator)
#lf = lf[order(lf$order_mediators),]
#table(lf$mediator)

lf$outcome_category = as.factor(lf$outcomes)
levels(lf$outcome_category); str(lf$outcome_category)

lf$mediator_names1 = as.factor(lf$mediator_names1)
str(lf$mediator_names1); levels(lf$mediator_names1); table(lf$mediator_names1)

lf$mediator_names2 = as.factor(lf$mediator_names2)
str(lf$mediator_names2); levels(lf$mediator_names2); table(lf$mediator_names2)

#%%========================================================
# count na in mediator col
if (table(is.na(lf$mediator)) == nrow(lf)){
  print("PASS: No NAs detected, lf data is good for plotting")
}else{
  print("FAIL: NAs detected in mediator columna. Check source formatting for lf data")
  #lf <- lf[!is.na(lf$mediator),]
  #head(lf); str(lf)
  quit()
}

#====================================
# Output plot: Distribution at T1
#====================================
lf_t1 = dplyr::filter(lf, timepoint == "t1")
my_title_dist_t1 = "Distributions of mediators at T1"
#x_axis_text = expression("Log"[10])
x_axis_text = expression(Log[10]~(Levels))

#-----------------
# ggrides: at T1
#-----------------

cat("Output plots will be in:", output_dist_t1)
svg(output_dist_t1, width = 15, height = 15)

p_dist_t1 = ggplot(lf_t1, aes(x = log10(value)
                         #, y = mediator))+
                         , y = fct_reorder(mediator_names1, order_mediators, .fun = sum))) +
  
  geom_density_ridges(aes(fill = factor(outcomes))
                      , alpha = 0.6)  + 

theme(axis.text.x = element_text(size = 22)
      , axis.text.y = element_text(size = 22
                                   , angle = 0
                                   , hjust = 1
                                   , vjust = 0)
      , axis.title.x = element_text(size = 22)
      , axis.title.y = element_text(size = 22)
      #, plot.title = element_text(size = 15, hjust = 0.5)
      , strip.text.x = element_text(size = 22, colour = "black")
      #, legend.title = element_text(color = "black", size = 22)
      , legend.title = element_blank()
      , legend.text = element_text(size = 25)
      #, legend.position = "right"
      , legend.position = c(0.8, 0.95)
      , legend.direction = "vertical") +
  labs(title = ""
       #, x = "Log10 (Levels)"
       , x = x_axis_text
       , y = "") +
  #scale_fill_discrete(name = "Patient outcome", labels = c("Died", "Recovered")) 
  scale_fill_discrete(name = "Patient outcome", labels = c("Non-survivors", "Survivors")) +
  guides(fill = guide_legend(reverse = T))
  
p_dist_t1
dev.off()

# test
#foo = p_dist_t1 + guides(fill = guide_legend(reverse = T))
#foo
###################################################################################
#-----------------
# ggrides: at T1, version 2
# without 's' in mediator names
#-----------------

cat("Output plots will be in:", output_dist_t1_f)
svg(output_dist_t1_f, width = 15, height = 15)

p_dist_t1_v3 = ggplot(lf_t1, aes(x = log10(value)
                              #, y = mediator))+
                              , y = fct_reorder(mediator_names2, order_mediators, .fun = sum))) +
  
  geom_density_ridges(aes(fill = factor(outcomes))
                      , alpha = 0.6)  + 
  
  theme(axis.text.x = element_text(size = 22)
        , axis.text.y = element_text(size = 22
                                     , angle = 0
                                     , hjust = 1
                                     , vjust = 0)
        , axis.title.x = element_text(size = 22)
        , axis.title.y = element_text(size = 22)
        #, plot.title = element_text(size = 15, hjust = 0.5)
        , strip.text.x = element_text(size = 22, colour = "black")
        #, legend.title = element_text(color = "black", size = 22)
        , legend.title = element_blank()
        , legend.text = element_text(size = 25)
        #, legend.position = "right"
        , legend.position = c(0.8, 0.95)
        , legend.direction = "vertical") +
  labs(title = ""
       #, x = "Log10 (Levels)"
       , x = x_axis_text
       , y = "") +
  scale_fill_discrete(name = "Patient outcome", labels = c("Non-survivors", "Survivors")) +
  guides(fill = guide_legend(reverse = T))
p_dist_t1_v3

dev.off()
###################################################################################
