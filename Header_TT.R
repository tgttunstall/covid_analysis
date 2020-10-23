#!/usr/bin/Rscript  
#if (!require(tidyverse)){
#install.packages("tidyverse")
#}

#install.packages("tidyverse")
library(tidyverse)

#install.packages("plyr")
library(plyr)

#install.packages("psych")
library(psych)

#install.packages("Hmisc")
library(Hmisc)

#install.packages("rstatix")
library(rstatix)

#install.packages("GGally")
library(GGally)

#install.packages("ggpubr")
library(ggpubr)

#install.packages("standardize")
library(standardize)

#install.packages("quantable")
library(quantable)

#install.packages("DescTools")
library(DescTools)

#install.packages("scales")
library(scales)

#install.packages("arsenal")
library(arsenal)

#install.packages("ggcorrplot")
library(ggcorrplot)

library("RColorBrewer")
library(gplots)
#install.packages("png")
library(png)

library(corrplot)

library(corrgram)

library(ggrepel)
#install.packages("ggbeeswarm")
library(ggbeeswarm)

#install.packages("ggridges")
library(ggridges)

#install.packages("summarytools")
#library(summarytools)

#install.packages("groupedstats")
library(groupedstats)

#install.packages("moments")
library(moments)

#install.packages("prob")
library(prob) # python type isin function

#install.packages("cobalt")
#library(cobalt) # var.names

library(magrittr)

library(qwraps2)
#options(qwraps2_markup = "markdown")


#if (!requireNamespace("BiocManager", quietly = TRUE))
#  install.packages("BiocManager")
#BiocManager::install("ComplexHeatmap")

#library(ComplexHeatmap)

########################################################################
#                       My functions
########################################################################

my_kurtosis <- function(x) {  
  m4 <- mean((x - mean(x))^4) 
  kurtosis <- m4/(sd(x)^4) - 3  
  kurtosis
}

my_skewness <-  function(x) {
  m3 <- mean((x - mean(x))^3)
  skewness <- m3/(sd(x)^3)
  skewness
}
########################################################################
#                       My local imports
########################################################################
source("legend_adjustment.R")
