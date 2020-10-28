#!/usr/bin/Rscript   
getwd()
setwd('~/git/covid_analysis/')
getwd()
############################################################
# TASK: boxplots at T1
# FIXME: currently not rendering, problem with NAs for stats?
############################################################
# source data
source("read_data.R")

# clear unwanted variables
rm(my_data)
############################################################
#=============
# Output: boxplot at T1 for all mediators
# currently ununsed
#=============
output_boxplot_t1 = paste0(outdir, "boxplot_t1_v3.pdf")

#%%========================================================
# read file
# data assignment for plots
wf = wf_data
lf = lf_data

#=====================
# data for plots: LF@T1
#=====================
table(lf$timepoint)

lf_t1 = lf[lf$timepoint == "t1",]

dim(lf_t1); str(lf_t1)
levels(factor(lf_t1$mediator)); table(lf_t1$mediator)

lf_t1$outcome_category = as.factor(lf_t1$outcomes)
levels(lf_t1$outcome_category)

lf_t1$outcomes  = as.factor(lf_t1$outcomes)
str(lf_t1$outcomes)

#%%========================================================
# count na in mediator col
if (table(is.na(lf_t1$mediator)) == nrow(lf_t1)){
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
my_stat = compare_means(value~outcomes, group.by = "mediator"
                        , data = lf_t1
                        , paired = FALSE
                        , p.adjust.method = "BH")
                        #, p.adjust.method = "bonferroni")

# !! check: satsified!!
wf_death = wf_data[wf_data$outcomes == 0,]; table(wf_death$outcomes)
wf_recovered = wf_data[wf_data$outcomes == 1,]; table(wf_recovered$outcomes)

wilcox.test(wf_death$Angiopoietin2_pgmL_t1, wf_recovered$Angiopoietin2_pgmL_t1, paired = F)
wilcox.test(wf_death$sICAM1_ngmL_t1, wf_recovered$sICAM1_ngmL_t1, paired = F)

my_comparisons <- list( c(0, 1) )
my_comparisons <- list( c("0", "1") )

#====================================
# Output plots as one pdf
cat("Output plots will be in:", output_boxplot_t1)
pdf(output_plots, width=15, height=12)

# ====================================
# 1) Boxplot with facet wrap:
# x = time
# y = Linear (Levels)
# coloured: outcome
# ====================================
my_comparisons <- list( c("t1", "t2"), c("t2", "t3"), c("t1", "t3") )

y_value = "value"
my_title1 = "Boxplots of mediators at t1: linear scale"

p1 = ggplot(lf_t1, aes(x = factor(outcomes), y = eval(parse(text=y_value)) ))  + 
  facet_wrap(~ mediator, nrow = 2, scales = "free_y") + 
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
        , legend.title = element_text(color = "black", size = 20)
        , legend.text = element_text(size = 15)
        , legend.direction = "vertical") +
  labs(title = my_title1
       , x = ""
       , y = "Levels") +
  scale_colour_discrete(name = "Patient outcome", labels = c("Non-survivors", "Survivors"))+
  #guides(fill = guide_legend(reverse = T))
  #scale_colour_manual(values = my_outcome_cols)
  stat_compare_means(comparisons = my_comparisons
                     , method = "wilcox.test"
                     , paired = F
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
my_title2 = "Boxplots of mediators at t1: log scale"

p2 = ggplot(lf_t1, aes(x = factor(outcomes), y = eval(parse(text=y_value)) ))  + 
  facet_wrap(~ mediator, nrow = 2, scales = "free_y") + 
  scale_y_log10()+
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
        , legend.title = element_text(color = "black", size = 20)
        , legend.text = element_text(size = 15)
        , legend.direction = "vertical") +
  labs(title = my_title2
       , x = ""
       , y = "Levels (Log10)") +
  scale_colour_discrete(name = "Patient outcome", labels = c("Non-survivors", "Survivors"))+
  #scale_colour_manual(values = my_outcome_cols)
  stat_compare_means(comparisons = my_comparisons
                     , method = "wilcox.test"
                     , paired = F
                     , label = "p.format") 

shift_legend2(p2)
dev.off()
