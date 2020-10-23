#!/usr/bin/Rscript   
getwd()
setwd('~/git/covid_analysis/')
getwd()
############################################################
# TASK: basic plots
# boxplots
# spaghetti plots
# ggridges
# pairs.panel
# heatmap

# useful links:
# http://www.sthda.com/english/wiki/ggplot2-dot-plot-quick-start-guide-r-software-and-data-visualization
############################################################
# source data
source("read_data.R")

# clear unwanted variables
rm(my_data)
############################################################
#===========================
# Output: different types 
# of plots, correlation
# and heatmaps
#=============================
output_plots = paste0(outdir_plots, "output_plots.pdf")
corr_and_hmap = paste0(outdir_plots, "corr_hmap.pdf")

#%%========================================================
# read file
# data assignment for plots
wf = wf_data
lf = lf_data

#=====================
# data for plots: LF
#=====================
dim(lf); str(lf)
levels(factor(lf$mediator)); table(lf$mediator)

nice_mediator_names = c(Angiopoietin2 = "Angiopoietin2"
                        , PF = "PF"
                        #, PSELECTIN = "PSelectin"
                        , PSelectin = "PSelectin"
                        , sESelectin = "sESelectin"
                        , sICAM1="sICAM1"
                        , sRAGE= "sRAGE"
                        , sVCAM1="sVCAM1")
# assign nice names 
lf$mediator <- as.character(nice_mediator_names[lf$mediator])

dim(lf); str(lf)
levels(factor(lf$mediator)); table(lf$mediator)

lf$outcome_category = as.factor(lf$outcomes)
levels(lf$outcome_category)

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

# FIXME : stats on log 10 value
# ===========
# stats 
# ===========
# with adjustment: also gives unadjusted P-values (BH and fdr are the same)
my_stat = compare_means(value~timepoint, group.by = "mediator"
                        , data = lf,  paired = TRUE
                        , p.adjust.method = "BH")
                        #, p.adjust.method = "bonferroni")

# !! check: satsified!!
wilcox.test(wf$sESelectin_ngmL_t1, wf$sESelectin_ngmL_t2, paired = T)
wilcox.test(wf$sRAGE_pgmL_t1, wf$sRAGE_pgmL_t2, paired = T)
wilcox.test(wf$sVCAM1_ngmL_t1, wf$sVCAM1_ngmL_t3, paired = T)

my_comparisons <- list( c("t1", "t2"), c("t2", "t3"), c("t1", "t3") )

# FIXME
#my_stat_minmax = compare_means(minmax_score~timepoint, group.by = "mediator", data = lf, paired = TRUE)

#lf$timepoint <- as.factor(lf$timepoint)
#lf$mediator <- as.factor(lf$mediator)
#lf$outcomes <- as.factor(lf$outcomes)

# if you really wanted to custom colour category
# currently unused
my_outcome_cols =  c("0" = "red", "1" = "green") 

#====================================
# Output plots as one pdf
cat("Output plots will be in:", output_plots)
pdf(output_plots, width=15, height=12)

# ====================================
# 1) Boxplot with facet wrap:
# x = time
# y = Linear (Levels)
# coloured: outcome
# ====================================
y_value = "value"
my_title1 = "Boxplots of mediators over time: linear scale"

p1 = ggplot(lf, aes(x = timepoint
              , y     = eval(parse(text=y_value)) ))  + 
  #geom_text(aes(label=id)) +
  facet_wrap(~ mediator, nrow = 2, scales = "free_y") + 
  geom_boxplot(fill = "white", outlier.colour = NA
              # position = position_dodge(width = 0.9)
              , width = 0.5) +
  geom_point(position = position_jitterdodge(dodge.width=0.01)
             , aes(colour = factor(outcomes))) +
  theme(axis.text.x = element_text(size = 15)
        , axis.text.y = element_text(size = 15
                                     , angle = 0
                                     , hjust = 1
                                     , vjust = 0)
        , axis.title.x = element_text(size = 15)
        , axis.title.y = element_text(size = 15)
        , plot.title = element_text(size = 20, hjust = 0.5)
        , strip.text.x = element_text(size = 15, colour = "black")
        , legend.title = element_text(color = "black", size = 20)
        , legend.text = element_text(size = 15)
        , legend.direction = "vertical") +
  labs(title = my_title1
       , x = ""
       , y = "Levels") +
  scale_colour_discrete(name = "Patient outcome", labels = c("Death", "Recovered"))+
  #scale_colour_manual(values = my_outcome_cols)
  stat_compare_means(comparisons = my_comparisons
                     , method = "wilcox.test"
                     , paired = TRUE
                     , label = "p.format") 

shift_legend2(p1)

# ====================================
# 2) Boxplot with facet wrap:
# x = time
# y = Log(Levels)
# coloured: outcome
# ====================================
#y_value = "log10(value)" # shows log transformed scale
y_value = "value"
my_title2 = "Boxplots of mediators over time: log scale"

p2 = ggplot(lf, aes(x = timepoint
                  , y = eval(parse(text=y_value)) ))  + 
  #geom_text(aes(label = id)) +
  facet_wrap(~ mediator, nrow = 2, scales = "free_y") + 
  scale_y_log10()+
  geom_boxplot(fill = "white", outlier.colour = NA 
               # position = position_dodge(width = 0.9)
               , width = 0.5) +
  geom_point(position = position_jitterdodge(dodge.width=0.01)
             , aes(colour = factor(outcomes))) +
  theme(axis.text.x = element_text(size = 15)
        , axis.text.y = element_text(size = 15
                                     , angle = 0
                                     , hjust = 1
                                     , vjust = 0)
        , axis.title.x = element_text(size = 15)
        , axis.title.y = element_text(size = 15)
        , plot.title = element_text(size = 20, hjust = 0.5)
        , strip.text.x = element_text(size = 15, colour = "black")
        , legend.title = element_text(color = "black", size = 20)
        , legend.text = element_text(size = 15)
        , legend.direction = "vertical") +
  labs(title = my_title2
       , x = ""
       , y = "Levels (Log10)")+
  scale_colour_discrete(name = "Patient outcome"
                        , labels = c("Death", "Recovered")) +
  stat_compare_means(comparisons = my_comparisons
                     , method = "wilcox.test"
                     , paired = TRUE
                     , label = "p.format") 

shift_legend2(p2)

#========================
# outlier plot: log
#========================
# create a separate df
lf2 = lf
lf2$value2 = lf2$value
lf2$value2 = replace_na(lf2$value2,0)

# outlier detection
is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}

lf2 = lf2 %>% group_by(timepoint, mediator) %>%
  mutate(is_outlier = ifelse(is_outlier(value2), value2
                             , as.numeric(NA) ))
lf2$outlier_id=lf2$id

lf2$outlier_id[which(is.na(lf2$is_outlier))] <- as.numeric(NA)

#********************
# labelled outliers
#********************
my_outlier_title = "Boxplot with top outliers labelled"
p_outlier = ggplot(lf2, aes(x = timepoint
                            , y = value))  + 
  facet_wrap(~ mediator, nrow = 2, scales = "free_y") + 
  scale_y_log10() + 
  geom_boxplot(fill = "white", outlier.colour = NA 
               , width = 0.5) +
  geom_point(position = position_jitterdodge(dodge.width=0.01)
             , aes(colour = factor(outcomes))) +
  theme(axis.text.x = element_text(size = 15)
        , axis.text.y = element_text(size = 15
                                     , angle = 0
                                     , hjust = 1
                                     , vjust = 0)
        , axis.title.x = element_text(size = 15)
        , axis.title.y = element_text(size = 15)
        , plot.title = element_text(size = 20, hjust = 0.5)
        , strip.text.x = element_text(size = 15, colour = "black")
        , legend.title = element_text(color = "black", size = 15)
        , legend.text = element_text(size = 15)
        , legend.direction = "vertical") +
  labs(title = my_outlier_title
       , x = ""
       , y = "Levels (Log10)")+
  scale_colour_discrete(name = "Patient outcome"
                        , labels = c("Death", "Recovered")) +
  geom_text_repel(aes(label = outlier_id), na.rm = TRUE)

shift_legend2(p_outlier)

# ====================================
# 3) Spaghetti plot with facet wrap:
# x = time
# y = Linear(Levels)
# coloured: outcome
# ====================================
y_value = "value"
my_title3 = "Spaghetti plot of mediators over time: linear scale"

p3 =  ggplot(lf, aes(x = timepoint, 
                 y = eval(parse(text=y_value)), 
                 group = id,
                 colour = factor(outcomes))) +
    #geom_text(aes(label=id))+
    facet_wrap(~mediator, scales = "free") + 
    #ylab("Levels") +
    geom_line(alpha = 0.8) + 
    geom_beeswarm(alpha = 0.7,
               size = 1.5) +
    
    theme(axis.text.x = element_text(size = 13)
          , axis.text.y = element_text(size = 13
                                       , angle = 0
                                       , hjust = 1
                                       , vjust = 0)
          , axis.title.x = element_text(size = 13)
          , axis.title.y = element_text(size = 13)
          , plot.title = element_text(size = 20, hjust = 0.5)
          , strip.text.x = element_text(size = 13, colour = "black")
          , legend.title = element_text(color = "black", size = 20)
          , legend.text = element_text(size = 15)
          , legend.direction = "vertical") +
    labs(title = my_title3
         , x = ""
         , y = "Levels")+
    scale_colour_discrete(name = "Patient outcome"
                          , labels = c("Death", "Recovered")) +
    stat_compare_means(comparisons = my_comparisons
                       , method = "wilcox.test"
                       , paired = TRUE
                       , label = "p.format") 
shift_legend2(p3)

# ====================================
# Spaghetti plot with facet wrap:
# x = time
# y = Log(Levels)
# coloured: outcome
# ====================================
#y_value = "log10(value)"
y_value = "value"
my_title4 = "Spaghetti plot of mediators over time: log scale"

p4 =  ggplot(lf, aes(x = timepoint, 
                     y = eval(parse(text=y_value)), 
                     group = id,
                     colour = factor(outcomes))) +
  #geom_text(aes(label=id))+
  facet_wrap(~mediator, scales = "free") + 
  #ylab("Levels") +
  scale_y_log10()+
  geom_line(alpha = 0.8) + 
  geom_beeswarm(alpha = 0.7,
                size = 1.5) +
  
  theme(axis.text.x = element_text(size = 13)
        , axis.text.y = element_text(size = 13
                                     , angle = 0
                                     , hjust = 1
                                     , vjust = 0)
        , axis.title.x = element_text(size = 13)
        , axis.title.y = element_text(size = 13)
        , plot.title = element_text(size = 20, hjust = 0.5)
        , strip.text.x = element_text(size = 15, colour = "black")
        , legend.title = element_text(color = "black", size = 20)
        , legend.text = element_text(size = 15)
        #, legend.position = "right"
        #, legend.position = c(1, 0.2)
        , legend.direction = "vertical") +
  labs(title = my_title4
       , x = ""
       , y = "Levels (Log10)")+
  scale_colour_discrete(name = "Patient outcome"
                        , labels = c("Death", "Recovered")) +
  stat_compare_means(comparisons = my_comparisons
                     , method = "wilcox.test"
                     , paired = TRUE
                     , label = "p.format") 
shift_legend2(p4)

#===================================================
# ggridges
#===================================================
#*******************************
#5) ggrides: all timeoints
#*******************************

my_title5 = "Distributions of mediators: all timepoints"

p5 = ggplot(lf, aes(x = log10(value) , y = mediator)) +
  geom_density_ridges(aes(fill = factor(outcomes))
                      , alpha = 0.6) +
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
        , legend.direction = "vertical") +
  labs(title = my_title5
       , x = "Log10(Levels)"
       , y = "") +
  scale_fill_discrete(name = "Patient outcome", labels = c("Death", "Recovered")) 

p5
#==================================
# 6) ggrdiges facet
# x = levels (Log)
# y = timepoints
# colour = outcomes
#==================================

my_title6 = "Distributions of mediators facet by time: Log"

p6 = ggplot(lf, aes(x = log10(value) , y = mediator)) +
  facet_wrap (~timepoint) +
  geom_density_ridges(aes(fill = factor(outcomes))
                      , alpha = 0.6) +
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
        , legend.direction = "vertical") +
  labs(title = my_title6
       , x = "Log10(Levels)"
       , y = "") +
  scale_fill_discrete(name = "Patient outcome", labels = c("Death", "Recovered")) 

p6

#*******************************
#7) ggrides: at T1
#*******************************
data_t1 = dplyr::filter(lf, timepoint == "t1")

my_title7 = "Distributions of mediators at T1"

p7 = ggplot(data_t1, aes(x = log10(value) , y = mediator)) +
  geom_density_ridges(aes(fill = factor(outcomes))
                      ,alpha = 0.6)  + 

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
      , legend.direction = "vertical") +
  labs(title = my_title7
       , x = "Log10(Levels)"
       , y = "") +
  scale_fill_discrete(name = "Patient outcome", labels = c("Death", "Recovered")) 
p7

#*******************************
#8) ggrides: at T2
#*******************************
data_t2 = dplyr::filter(lf, timepoint == "t2")

my_title8 = "Distributions of mediators at T2"

p8 = ggplot(data_t2, aes(x = log10(value) , y = mediator)) +
  geom_density_ridges(aes(fill = factor(outcomes))
                      , alpha = 0.6)  + 
  
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
        , legend.direction = "vertical") +
  labs(title = my_title8
       , x = "Log10(Levels)"
       , y = "") +
  scale_fill_discrete(name = "Patient outcome", labels = c("Death", "Recovered")) 
p8

#*******************************
#9) ggrides: at T3
#*******************************
data_t3 = dplyr::filter(lf, timepoint == "t3")

my_title9 = "Distributions of mediators at T3"

p9 = ggplot(data_t3, aes(x = log10(value) , y = mediator)) +
  geom_density_ridges(aes(fill = factor(outcomes))
                      , alpha = 0.6)  + 
  
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
        , legend.direction = "vertical") +
  labs(title = my_title9 
       , x = "Log10(Levels)"
       , y = "") +
  scale_fill_discrete(name = "Patient outcome", labels = c("Death", "Recovered")) 
p9

#==================================
# 10) ggrdiges facet
# x = levels (Linear)
# y = timepoints
# colour = outcomes
#==================================
my_title10 = "Distributions of mediators: linear"

p10 = ggplot(lf, aes(x = value , y = timepoint)) +
  geom_density_ridges(aes(fill = factor(outcomes))
                      , alpha = 0.6) +
  facet_wrap(~mediator, nrow = 2, scales = "free") +
theme(axis.text.x = element_text(size = 13)
      , axis.text.y = element_text(size = 13
                                   , angle = 0
                                   , hjust = 1
                                   , vjust = 0)
      , axis.title.x = element_text(size = 13)
      , axis.title.y = element_text(size = 13)
      , plot.title = element_text(size = 20, hjust = 0.5)
      , strip.text.x = element_text(size = 15, colour = "black")
      , legend.title = element_text(color = "black", size = 20)
      , legend.text = element_text(size = 15)
      #, legend.position = "right"
      , legend.direction = "vertical") +
  labs(title = my_title10
       , x = "Levels"
       , y = "") +
  scale_fill_discrete(name = "Patient outcome", labels = c("Death", "Recovered"))

shift_legend2(p10)

#==================================
# 11) ggrdiges facet
# x = levels (Log)
# y = timepoints
# colour = outcomes
#==================================
my_title11 = "Distributions of mediators: Log "

p11 = ggplot(lf, aes(x = value , y = timepoint)) +
  scale_x_log10()+
  geom_density_ridges(aes(fill = factor(outcomes))
                      , alpha = 0.6) +
  facet_wrap(~mediator, nrow = 2, scales = "free") +
  theme(axis.text.x = element_text(size = 13)
        , axis.text.y = element_text(size = 13
                                     , angle = 0
                                     , hjust = 1
                                     , vjust = 0)
        , axis.title.x = element_text(size = 13)
        , axis.title.y = element_text(size = 13)
        , plot.title = element_text(size = 20, hjust = 0.5)
        , strip.text.x = element_text(size = 15, colour = "black")
        , legend.title = element_text(color = "black", size = 20)
        , legend.text = element_text(size = 15)
        #, legend.position = "right"
        , legend.direction = "vertical") +
  labs(title = my_title11
       , x = "Levels(Log)"
       , y = "") +
  scale_fill_discrete(name = "Patient outcome", labels = c("Death", "Recovered"))

shift_legend2(p11)

dev.off()
########################################################################


########################################################################
#                           Plots with wf data
#                       clustering k means good example: TO DO?
########################################################################

cat("Plots will be in:", corr_and_hmap)
pdf(corr_and_hmap , width = 15, height = 12)

# =======================
# 5) Correlation plot at T1
#0.40 to 0.69	A moderate correlation
#0.70 to 0.89	A strong correlation
#0.90 to 1.00	A very strong correlation
# =======================
#library(ggcorrplot)
wf = wf_data
wf <- wf[order(wf$outcomes),]

# add colour based on outcomes column to match colour grouping with ggplots
my_red = "#F8766D"
my_green = "#00BA38" 
my_blue = "#619CFF"

str(wf$outcomes)

wf$idcol <- mapvalues(wf$outcomes
                      , from = c(0,1)
                      , to = c(my_red, my_blue))  

n = which(colnames(wf) == "idcol"); print(n)

# subset t1 data
wf_t1 = cbind(wf[c(1:3,n)],  wf %>% dplyr:: select(grep("_t1", names(wf))))

colnames(wf_t1)

# assign nice column names
my_colnames = c("id"
                , "studygroup"
                , "outcomes"
                , "idcol"
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
outcomes_col = which(colnames(wf_t1) == "outcomes")

#=========== 
# Data for corrrelations
#===========

### THIS PLOT CORR
# Data: But this is not needed as this is inside the function
corr_M = wf_t1[start_med:end_med]
corr_data = wf_t1[c(outcomes_col, start_med:end_med)]

#===========
# Corrplot with R and P values
#===========
my_corr = psych::corr.test(corr_M
                           , method = "spearman"
                           , use =  "pairwise.complete.obs")$r

my_pval =  psych::corr.test(corr_M
                            , method = "spearman"
                            , adjust = "none"
                            , use =  "pairwise.complete.obs")$p

# check: satisfied!
psych::corr.test(wf_t1$sICAM1, wf_t1$PSelectin
                 , method = "spearman"
                 , use =  "pairwise.complete.obs")$r
psych::corr.test(wf_t1$sICAM1, wf_t1$PSelectin
                 , method = "spearman"
                 , use =  "pairwise.complete.obs")$p

#----------------
# ggcorrplot: Not so good!
#----------------
#ggcorr_plot = ggcorrplot(my_corr
#                         , p.mat = my_pval
#                         #, method = "circle"
#                         #, insig = "blank"
#                         , hc.order = TRUE
#                         , outline.col = "black"
#                         , lab = T
#                         , lab_size = 6
#                         #, ggtheme = ggplot2::theme_gray
#                         , colors = c("#6D9EC1", "white", "#E46726")
#                         , title = "Spearman correlations: T1\n(X: not significant)")    


#----------------
# paris.panel
#----------------

corr_plot2 = pairs.panels(corr_data[2:length(corr_data)]
                          , method = "spearman" # correlation method
                          , hist.col = "grey" ##00AFBB
                          , density = TRUE  # show density plots
                          , ellipses = F # show correlation ellipses
                          , stars = TRUE
                          , rug = F
                          , breaks = "Sturges"
                          , show.points = T
                          , bg = c(my_red, my_blue)[unclass(factor(corr_data$outcomes))]
                          , pch = 21
                          #, alpha = .05
                          #, points(pch = 19, col = c("#f8766d", "#00bfc4"))
                          , cex =2
                          , cex.axis = 1.4
                          , cex.labels = 2
                          , cex.cor = 1
                          , smooth = F
                          , main = "Pairwise Spearman correlations with signifcance: T1"
)

corr_plot2

# ===============
# Heatmaps of mediators at T1
# ===============
library("RColorBrewer")
library(gplots)

#my_palette <- colorRampPalette(brewer.pal(10, "RdYlBu"))(n=50)
#my_palette <- colorRampPalette(c("red", "white", "blue"))(n = 256)
my_palette <- cm.colors(n = 256)
my_palette <- cm.colors(n = 50)
#my_palette <- topo.colors(n=256)

# data for heatmap
wf_t1_m = as.matrix(wf_t1[start_med:end_med])
colnames(wf_t1_m) 

#hmap_colnames = c("Angiopoietin2"
#,"PF"       
#,"PSelectin"     
#,"sESelectin"  
#,"sICAM1"
#,"sRAGE"     
#,"sVCAM1")

#colnames(wf_t1_m) = hmap_colnames
rownames(wf_t1_m) = wf_t1$id

my_red = "#F8766D"
my_green = "#00BA38" 
my_blue = "#619CFF"

# ===============
# Heatmaps of mediators at T1
# log scaled values
# NO subject ordering
# ===============

hmap_data = "log10(wf_t1_m)"
my_title_hmap1 = "Heirarchical clustering of mediators at T1: Log scaled values"

heatmap.2(eval(parse(text = hmap_data))
          , na.rm = T
          , scale = "column"
          , Rowv = F
          , Colv = T
          , dendrogram = "column"
          , col =  my_palette
          , trace = "none"
          , density.info = "density"
          #, RowSideColors = c(rep(my_red, 11), rep(my_green, 11), rep(my_blue, 9))
          , RowSideColors = c(rep(my_red, 11), rep(my_blue, 20)) #version 3
          , keysize = 0.9
          #, key.xlab = "Z-score"
          , cexRow = 1.75
          , cexCol = 1.75
          , sepcolor = "black"
          , main = ""
          , srtCol = 20)
par(lend = 1)
legend("topright"      
       , legend = c("Death", "Recovered")
       , col = c(my_red, my_blue)
       , lty= 1       
       , lwd = 10)
title(main = my_title_hmap1, cex.main = 1.3)

# ===============
# Heatmaps of mediators at T1
# log scaled values
# no subject ordering
# ===============
#hmap_data = "wf_t1_m"
hmap_data = "log10(wf_t1_m)"
my_title_hmap2 = "Heirarchical clustering mediators and subjects at T1: Log scaled values"

heatmap.2(eval(parse(text = hmap_data))
          , scale = "column"
          , Rowv = T
          , Colv = T
          , dendrogram = "both"
          , col =  my_palette
          , trace = "none"
          , density.info = "density"
          #, RowSideColors = c(rep(my_red, 11), rep(my_green, 11), rep(my_blue, 9))
          , RowSideColors = c(rep(my_red, 11), rep(my_blue, 20)) #version 3
          , keysize = 0.8
          , key.xlab = "Z-score"
          , cexRow = 1.75
          , cexCol = 1.75
          , sepcolor = "black"
          , main = ""
          , srtCol = 20)
par(lend = 1)
legend("topright"      
       , legend = c("Death", "Recovered")
       , col = c(my_red, my_blue)
       , lty= 1       
       , lwd = 10)
title(main = my_title_hmap2, cex.main = 1.3)

dev.off()
