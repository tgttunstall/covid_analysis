#!/usr/bin/Rscript   
getwd()
setwd('~/git/covid_analysis/')
getwd()
############################################################
# TASK: data cleaning and extraction: for loess plots
############################################################
# source data
source("read_data.R")

# clear unwanted variables
rm(lf_data, wf_data)

#==================================
# output: formatted and clean data: loess
#==================================
outfile_icu_wf = paste0(datadir,"/icu_covid_wf_v3_loess.csv")
outfile_icu_lf = paste0(datadir,"/icu_covid_lf_v3_loess.csv")
outfile_colnames = paste0(datadir, "/colnames_check_v3_loess.csv")
#%%========================================================

colnames_loess = as.data.frame(colnames(my_data))

# some numerical cols are characters, change these
#my_data <- as.data.frame(sapply(my_data, function(x) as.numeric(as.character((x)))))
#str(my_data)

# colnames
all_cols_loess = as.data.frame(colnames(my_data))
#write.csv(all_cols_loess, "colnames_loess.csv")

#==================================
# select the mediators to analyse
#==================================
cols_to_select = c("id",                                         
                "sRAGEpgmLt1",                           
                "sICAM1ngmLt1",                          
                "PSELECTINngmLt1",                       
                "sVCAM1ngmLt1",                          
                "Angiopoietin2pgmLt1",                   
                "sESelectinngmLt1",                      
                "sESelectinngmLt2",                      
                "Angiopoietin2pgmLt2",                   
                "sRAGEpgmLt2",                           
                "sICAM1ngmLt2",                          
                "PSELECTINngmLt2",                       
                "sVCAM1ngmLt2",                          
                "sESelectinngmLt3",                      
                "Angiopoietin2pgmLt3",                   
                "sRAGEpgmLt3",                           
                "sICAM1ngmLt3",                          
                "PSELECTINngmLt3",                       
                "sVCAM1ngmLt3",  
                "days_from_symptons_onset_t1",
                "days_from_hospitalization_t1",
                "outcomes0death1recovery2other",       
                "PF_t1",                                   
                "PF_t2" ,                                     
                "PF_t3" ,                                     
                "studygroup0coorteA1coorteB2coorteC")

# check if these columns to select are present in the data
cols_to_select%in%colnames(my_data)
all(cols_to_select%in%colnames(my_data))

table(my_data$studygroup0coorteA1coorteB2coorteC)

# subset
my_df = my_data[,cols_to_select]
dim(my_df) 

# some numerical cols are characters, change these
str(my_df)
my_df <- as.data.frame(sapply(my_df, function(x) as.numeric(as.character((x)))))
str(my_df)

# add column name subject_id with "S" prefix to id or simply add it to id
my_df$id = paste0("S", my_df$id)

# assign nicer colnames
original_colnames = colnames(my_df)
orig_cols = as.data.frame(colnames(my_df))

my_colnames = c("id",                                         
                   "sRAGE_pgmL_t1",                           
                   "sICAM1_ngmL_t1",                          
                   "PSelectin_ngmL_t1",                       
                   "sVCAM1_ngmL_t1",                          
                   "Angiopoietin2_pgmL_t1",                   
                   "sESelectin_ngmL_t1",                      
                   "sESelectin_ngmL_t2",                      
                   "Angiopoietin2_pgmL_t2",                   
                   "sRAGE_pgmL_t2",                           
                   "sICAM1_ngmL_t2",                          
                   "PSelectin_ngmL_t2",                       
                   "sVCAM1_ngmL_t2",                          
                   "sESelectin_ngmL_t3",                      
                   "Angiopoietin2_pgmL_t3",                   
                   "sRAGE_pgmL_t3",                           
                   "sICAM1_ngmL_t3",                          
                   "PSelectin_ngmL_t3",                       
                   "sVCAM1_ngmL_t3",
                   "days_from_symptons_onset_t1",
                   "days_from_hospitalization_t1",
                   "outcomes",       
                   "PF_units_t1",                                   
                   "PF_units_t2" ,                                     
                   "PF_units_t3" ,                                     
                   "studygroup")


if (length(original_colnames) == length(my_colnames) && all(cols_to_select%in%colnames(my_data))){
  print("PASS: length of colnames match. Assigning clean colnames")
  colnames(my_df) = my_colnames
  revised_colnames = colnames(my_df)
  colnames_check = as.data.frame(cbind(original_colnames, revised_colnames))
  
} else{
  cat(paste0("FAIL:length mismatch when assigning colnames"
             , "\nExpected length of colnames: ", length(original_colnames)
             , "\nGot: ", length(my_colnames)))
  quit()
}

print(colnames(my_df))
n_patients = length(unique(my_df$id))
print(paste0("Total no. of patients:", n_patients))

table(my_df$studygroup)
table(my_df$outcomes)
table(my_df$studygroup, my_df$outcomes)

#%% subset only icu patients
my_df_icu = my_df[my_df$studygroup == 0,] 
n_icupatients = length(unique(my_df_icu$id))

cat(paste0("Total no. of rows in original df:", nrow(my_df)
           , "\nTotal no. of unique patients:",n_patients 
           , "\nTotal no. of ICU patients:", n_icupatients))

table(my_df_icu$studygroup, my_df_icu$outcomes)
tab1 = table(my_df_icu$studygroup, my_df_icu$outcomes)
cat(paste0("no. of icu patients who"
           , "\ndied:", tab1[1], " ~ ", tab1[1]/n_icupatients*100, "%"
           , "\nrecovered:", tab1[2]," ~ ", tab1[2]/n_icupatients*100, "%"))
           #, "\nother:", tab1[3], " ~ ", tab1[3]/n_icupatients*100, "%"))

table(my_df$studygroup)
#=====================================================
#=========
# lf data
#=========
pivot_cols = c("id"
               , "studygroup"
               , "days_from_symptons_onset_t1"
               , "days_from_hospitalization_t1"
               , "outcomes")

expected_rows_lf = nrow(my_df_icu) * (length(my_df_icu) - length(pivot_cols)) 

# using regex: 
df_lf = my_df_icu %>% 
  tidyr::pivot_longer(-all_of(pivot_cols), names_to = c("mediator", "units", "timepoint"), 
                      names_pattern = "(.*)_(.*)_(.*)", 
                      values_to = "value")

if ((nrow(df_lf) == expected_rows_lf) & (sum(table(is.na(df_lf$mediator))) == expected_rows_lf)) {
  cat(paste0("PASS: long format data has correct no. of rows and NA in mediator:"
        , "\nNo. of rows: ", nrow(df_lf)
        , "\nNo. of cols: ", ncol(df_lf)))
} else{
  cat(paste0("FAIL:long format data has unexpected no. of rows or NAs in mediator"
             , "\nExpected no. of rows: ", expected_rows_lf
             , "\nGot: ", nrow(df_lf)
             , "\ncheck expected rows calculation!"))
  quit()
}

class(df_lf) # hmmm
str(df_lf)

class(as.data.frame(df_lf))
str(as.data.frame(df_lf))

# COMMENT: slight difference in class and structure b/w the outout from pivot and when you convert to df
# I will use the df as I am familiar with it!
lf_df = as.data.frame(df_lf)
class(lf_df)
str(lf_df)

# sort by mediator and timepoint
lf_df = lf_df[order(lf_df$mediator, lf_df$timepoint),]

table(is.na(lf_df$mediator))

#=========
# wf data
#=========
# icu data is your wf data
# sort icu data by columnames 
auto_col_order = order(names(my_df_icu))
#my_col_order = c(1,25, 24, 20, 6, 9, 15, 21, 22, 23,  4, 12, 18,  7,  8, 14,  3, 11, 17,  2, 10, 16, 5, 13, 19)

my_col_order = c(1, 26, 20, 21, 22
                 , 23, 24, 25
                 , 6, 9, 15
                 , 2, 10, 16
                 , 4, 12, 18
                 , 5, 13, 19
                 , 7, 8, 14
                 , 3, 11, 17)


if(length(auto_col_order) == length(my_col_order)){
  print("PASS: column order successfully generated. Reordering column in wf data")
  wf_df = my_df_icu[, my_col_order]
} else{
  cat(paste0("FAIL:length mismatch of column orders"
             , "\nExpected column order for: ", length(auto_col_order)
             , "\nGot:", length(my_col_order)))
  quit()
}          
#all.equal(my_df_icu, wf_df)
#=========================================================== 
#%% write icu files

# lf_data
write.csv(lf_df, outfile_icu_lf, row.names = F)
cat(paste0("Finsihed wrting lf data:"
    , "\nNo. of rows: ", nrow(lf_df)
    , "\nNo. of cols: ", ncol(lf_df)))

# column names to check
write.csv(colnames_check, outfile_colnames, row.names = F)
cat(paste0("Finsihed wrting colnames original and revised:"
    , "\nNo. of rows: ", nrow(colnames_check)
    , "\nNo. of cols: ", ncol(colnames_check)))

# wf_data: only original
write.csv(wf_df, outfile_icu_wf, row.names = F)
cat(paste0("\nFinsihed wrting wf data:"
           , "\nNo. of rows: ", nrow(wf_df)
           , "\nNo. of cols: ", ncol(wf_df)))

# COMMENT: wf_data for scaled values not written out!
#=======================================================
# end of script
