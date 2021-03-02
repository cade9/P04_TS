# Title: 02_editExtract.R
# Author: Christiana Ade
# Date: 3/1/2021
# Purpose: Edit the extracted raster points to include the different classifications we are trying
# **Requires** 
# 1) NOTE CREATED class key by editing this by hand and the table 
# tibble(Species_1 = unique(sort(allDat$Species_1)))
# write_csv(m,"./Data/CSV/02_ClassPartKey.csv")
#
# EDITS MADE
# 1) Filtering for duplicates 
# 2) remove unesc. columns
# 3) remove points that Mui labeled as unsuitable by year and season
####################################################################################
## require packages
require(raster)
require(tidyverse)
require(stringr)
require(lubridate)
if(Sys.info()["sysname"] == "Windows"){
  setwd("Z:/Projects/DSC_Delta/Task1/P04_TS") } else {
    setwd("/beanstore/Projects/DSC_Delta/Task1/P04_TS")
  }
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####             START USER DEFINED VARIABLES       #### 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# The following variables should be defined by the user each time.
#### INPUT FILES ####
# 1) list of the extracted spectral CSVs

sL <- list.files("./Output/CSV/00_extractSpectra_feb28", full.names = T )

# made updates to the classKey file 
# FLT-cade and kept NPV-FLT
ckN <- "./Data/CSV/02_ClassPartKey_feb28.csv"


#### OUTPUT FILES ####
# 1) Out Directory
outDir <- "./Output/CSV/02_editExtract_feb28"
#### USER DEFINED FUNCTIONS ####
# remove duplicate location function
cleanDup <- function(df) {
  # determine if there is more than 1 row in it 
  numRow <- nrow(df)
  if (numRow > 1) {
    # determine if the classes are the same from the species column 
    # going to use class2 to determine this because my training data only have labels, FLT, SAV, Water, and EMR
    # if they have the same class
    if (length(unique(df$Class2 == 1))) {
      # then keep the column with more information
      # determine na per row
      rowNA <- rowSums(is.na(df))
      # if number of NAs is the same 
      if(length(unique(rowNA)) == 1) {
        datSource <- unique(df$shpSource)
        # check if they have different datasources
        # if they have the same shpSource
        if(length(datSource) == 1) {
          # then just pick the first row
          df2 <- df %>% slice(1)
          # else they have diffrerent data sources
        } else {
          # then you should pick the data source from a hierchy
          #mui_edit > cstars_edit > shrutiAdd_edit > cade > cstar_edit_randomSamp
          # this actually really doesn't matter at this point 
          # just pick the first entry again
          df2 <- df %>% slice(1)
        }
      } else {
        # find the index of the minimum and slice the dataframe by that row number
        rowNA_min <- which.min(rowNA)
        df2 <- df %>% slice(rowNA_min)
      }
    # else they have different classes and we want to remove the point
    } else {
      # remove the entry 
      df2 <- df %>% mutate(deleteEntry = "yes")
    }
  } else {
    df2 <- df
  }
return(df2)
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####             END USER DEFINED VARIABLES        ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
select_if(~sum(!is.na(.)) > 0)

# columns to remove 
colRe <- c("GPS_Date", "GPS_Time","Lifeform",	"Class",	"ORIG_FID",	"Area",	"ORIG_FID_1", "Area_1",
           "randno","Rake_Spe11",	"Rake_Spe12", "Photo_Link", "yearcomm", "Photos", 
           "Team", "ID", "Horz_Prec", "Northing", "Easting", "Datafile",
           "Unfilt_Pos", "Filt_Pos","GNSS_Lengt")



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####             START SCRIPT                      ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# read in class key
classKey <- read_csv(ckN)


for (fn in sL) {
  # determine the year of the file 
  yrCol <- paste0("X_", str_extract(fn, "(?!_)\\d{8}(?=_)"))
  
  # read in extracted data and group by datasource
  mDat_sub <- read_csv(fn, guess_max = 3800) %>%
    # remove unnes columns
    select(-colRe) %>%
    group_by(shpSource) %>%
    nest()
  
  #### PART 1: Filter out points that Mui indicated should not be kept ####
  # determine which index belongs to mui_edit
  mIndex <- which(mDat_sub$shpSource == "mui_edit")
  
  ## Changed March 1 2021
  # should have a sub based on the season and the year 
  # so would remove those from 2018 that Mui said were not a good fit...
  # WHY ARE THESE TRUE AND FALSE NOW? ugh
  
    mDat_sub$data[[mIndex]] <- mDat_sub$data[[mIndex]] %>%  
    filter(X_20190425  == 1 & X_20191002  == 1)
  
  ## ungroup, add in class key, and remove spectra that have NA in the species column
  mDat_ungroup <- mDat_sub %>%
    unnest(cols = c(data)) %>%
    ungroup() %>%
    # change the naming of Water in species one even thoug  the class key accounts for it
    mutate(Species_1 = str_replace_all(Species_1, c("water" = "Water"))) %>%
    left_join(classKey) %>%
    filter(!is.na(Species_1)) %>%
    # remove yearcomm column
    dplyr::select(-yearcomm) %>%
    # add spot id for part 2
    #### PART 2: Filter out duplicate locations #####
    mutate(spotID = paste0(rDate, "_", ras_row, "_", ras_col)) %>%
    group_by(spotID) %>%
    nest() %>%
    mutate(data = map(.f = cleanDup, .x = data)) %>%
    unnest(cols = c(data)) %>%
    ungroup() 

  # file name 
  bn <- str_extract(fn,"(?=[^\\\\|/]*$)(.)+(?=\\.)")
  # write file 
  eDate <- str_replace_all(Sys.Date(),  "-","")
  write_csv(mDat_ungroup, paste0(outDir, "/",bn,"_classAdd_",eDate,".csv"))
  print(bn)
}


#####~! TODO DELETE THIS
#### for testing of the cleanDup function
# test <- mDat_ungroup %>%
#   group_by(spotID) %>%
#   nest() %>%
#   mutate(rowCount = unlist(map(data, ~nrow(.x)))) 
# 
# 
# test1 <- test %>%
#   filter(rowCount > 1) %>% 
#   unnest(cols = c(data))
# 
# 
#   test3 <- mDat_ungroup %>%
#     group_by(spotID) %>%
#     nest() %>%
#     mutate(rowCount = unlist(map(data, ~nrow(.x)))) %>%
#     filter(rowCount > 1 ) %>%
#     mutate(naCount =  map(data, ~rowSums(is.na(.x))))
# 
# 
# df <- test[92,]
# dfIn <- test$data[[92]]
# df_L <- test[c(92:93,2852),]
#   fTest <- df_L %>%
# mutate(data = map(.f = cleanDup, .x = data))
