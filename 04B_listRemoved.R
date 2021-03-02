# Title: 04B_listRemoved.R
# Author: Christiana Ade
# Date: 2/28/2021
# Purpose: Determine which points you removed from analysis
# make a list and remove or flag those same locations 
# **Requires** 
# 1)  THIS WAS ACTUALLY RUN IN THE P03_MUI folder
####################################################################################
## require packages
require(tidyverse)
projDir <- "Z:/Projects/DSC_Delta/Task1/P03_newMui"
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####             START USER DEFINED VARIABLES       #### 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# The following variables should be defined by the user each time.
#### INPUT FILES ####
fLOld<- list.files(paste0(projDir, "/Output/CSV/02_editExtract"), pattern = "classAdd_20210201.*csv", full.names = T)
fLNew <- paste0(projDir,"/Output/CSV/04_editSpec/editSpectra_20210204.csv")
#### OUTPUT FILES ####
outFile <- paste0(projDir, "/Output/CSV/04_editSpec/roiRemoved_R1_0228.csv")
#### USER DEFINED FUNCTIONS ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####             END USER DEFINED VARIABLES        ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####             START SCRIPT                      ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# read in the file that you created after you removed some bad spectra and flagged them 
mDatNew <- read_csv(fLNew) %>%
  filter(!rDate == "20190425") %>%
  mutate(rYr = str_extract(rDate,"(2016|2017|2018|2019)")) %>%
  mutate(Class6_ID = paste0(Class6,"_",rYr, "_", ras_col,"_",ras_row))
  
# read in the old file 
mDatOld <- map_dfr(fLOld, read_csv) %>% 
  filter(!rDate == "20190425") %>%
  mutate(rYr = str_extract(rDate,"(2016|2017|2018|2019)")) %>%
  mutate(Class6_ID = paste0(Class6,"_",rYr, "_", ras_col,"_",ras_row))
# removed 275 spectrum

### Determine which ones were deleted 
# create a delete column and then also copy over cadeFil column(in the next round of edits, 
# you should just add delete)
# for 2017 only returns 254 spectra, so either there are some dupilcates orr??
mDel <- mDatOld %>%
  filter(!Class6_ID %in% mDatNew$Class6_ID) %>%
  mutate(filCade = "delete") %>%
  dplyr::select(Class6_ID, filCade)
# you will have a lot less unique entries than rows for this because most of the points are dups/trips 

mFlag <- mDatNew %>% 
  filter(!is.na(filCade)) %>%
  dplyr::select(Class6_ID, filCade)

# createa unique ID based on class 6 and the row and col
mCom <- bind_rows(mFlag, mDel)

write_csv(mCom, outFile)
