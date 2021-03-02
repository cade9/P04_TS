# Title: 00_ExtractS21819.R
# Status:
# Author: Christiana Ade
# Date: 2/8/2021
# Modified: 
# Purpose: Extract spectra from .dat files for L8
# Z:\Projects\DSC_Delta\Task1\P04_TS\Output\Vector\00_editTrainTest
# was copied from the P03_muiNew group on 2/8/2021
# and script was edited from 01_ExtractS2A.R from mui 
# **Requires** 
# 1)  
####################################################################################
## require packages
require(raster)
require(rgdal)
library(tidyverse)
library(stringr)
library(lubridate)
library(rgeos)
require(maptools)
require(sp)
require(spdplyr)
require(tools)
require(magrittr)
require(tictoc)
# Determine OS
if(Sys.info()["sysname"] == "Windows"){
  pDir <- ("Z:/") } else {
    pDir <- "/beanstore"
  }
# project directory
projDir <- getwd()
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####             START USER DEFINED VARIABLES       #### 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# The following variables should be defined by the user each time.
#### INPUT FILES ####
# 1) List of Shapefiles mui new shapefiles that I edited
shpL <- list.files(paste0(projDir,"/Output/Vector/00_editTrainTest"), recursive = T,
                   full.names = T, pattern = "_editV2.shp$")
shpL
# 2) RastFile
rL <- list.files(paste0(pDir,"/Data/Raster/Sentinel2/S2_Level2_10mStack"), pattern = "BOA_.*dat$", full.names = T, recursive = T)
rL <- str_subset(rL,"(2017|2018|2019).*10SFH")
#### OUTPUT FILES ####
# 1) Name of the output directory for the spectra
outDir <- "/Output/CSV/00_extractSpectra_feb28"
#### USER DEFINED FUNCTIONS ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####             END USER DEFINED VARIABLES        ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# create dictionary of matching files 
# these are going to be different now 
shpL19 <- shpL[c(2:3,5:8,10:13,22,23)] ## need to consider that there are both fall and spring shpfiles
shpL18 <- shpL[c(8,9,10:13, 18:21, 23)]
shpL17 <- shpL[c(8,4,10:13,14:17, 23)] # should you extract the 2019 FLT or whater?

shpDic <- vector(mode = "list", length = length(rL))
names(shpDic) <- rL

for(i in 1:length(rL)){
  #determine name of list
  elmName <- names(shpDic)[[i]]
  if (str_detect(elmName, "2018") == T){
    shpDic[[i]] <- shpL18
    
  } else if (str_detect(elmName, "2017") == T) {
    shpDic[[i]] <- shpL17
    
  } else if (str_detect(elmName, "2019") == T) {
    shpDic[[i]] <- shpL19
    
  }
}
shpDic
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####             START SCRIPT                      ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

print("the script is running")

for (ras in names(shpDic)){
  # tic()
  # read in raster #
  S2 <- raster::stack(ras)
  # change names #
  names(S2) <- c( "BlueAersol","Blue","Green","Red","VR1","VR2","VR3","NIR","WaterVapor","SWIR1","SWIR2")
  # rasN
  rasFileN <- str_extract(ras,  "(?=[^\\\\|/]*$)(.)+(?=\\.)")
  
  shp_ex = NULL
  
  ## Extract information with Shapefiles ##
  for(shp in shpDic[[ras]]){
    # read in shape file #
    roi <- readOGR(shp, verbose = F)
    # extract name of the shapefile #
    file <- str_extract(shp,"(?=[^\\\\|/]*$)(.)+(?=\\.)")
    # very specfic to our data need to edit 1/31/2021
    if (file == "upland_editV2"){
      colnames(roi@data) <- c("X_20191002", "X_20190425", "X_20181007", "X_20171101", "X_20161012", "Species_1",
                              "CommentsMui" ,"shpSource", "shpDate", "pointID" )
    }
    ### extract information ### 
    # different for line vs. point
    if(class(roi) == "SpatialPointsDataFrame"){
      myExtract <- raster::extract(S2, roi, sp =T, cellnumbers = T)
      myExtractDf  <- myExtract@data %>%
        mutate(ras_row = rowFromCell(S2,cells),
               ras_col = colFromCell(S2,cells),
               rDate = str_extract(rasFileN,"20[0-9]{6}"),
               rRes = res(S2)[1],
               rasN = rasFileN,
               shpN = shp,
               shpFileN = file,
               unqId = paste0(Species_1, "_",ras_row, "_", ras_col,"_",rDate)) %>%
        as_tibble() %>%
        mutate_if(is.factor, as.character)
      ### Add Information to Shapefile Extract for that Raster ###
      shp_ex <- shp_ex %>%
        bind_rows(myExtractDf)
      
      
    } else {
      
      myExtract <- raster::extract(S2, roi, df =T, cellnumbers = T) #when you convert from sp to df it adds coord columns
      
      myExtractDf  <- myExtract %>%
        mutate_if(is.factor, as.character) %>%
        rename("cells" = "cell") %>%
        left_join(.,roi@data, by = c("ID" = "sLineID")) %>%
        mutate(ras_row = rowFromCell(S2,cells),
               ras_col = colFromCell(S2,cells),
               rDate = str_extract(rasFileN,"20[0-9]{6}"),
               rRes = res(S2)[1],
               rasN = rasFileN,
               shpN = shp,
               shpFileN = file,
               unqId = paste0(Species_1, "_",ras_row, "_", ras_col,"_",rDate))  %>%
        as_tibble() %>%
        mutate_if(is.factor, as.character) %>%
        # change the ID name back to sLineID?
        rename( "pointID" = "ID")
      
      ### Add Information to Shapefile Extract for that Raster ###
      shp_ex <- shp_ex %>%
        bind_rows(myExtractDf)
    }
  }
  ## write CSV of all data
  bn <- str_extract(ras, "(?=[^\\\\|/]*$)(.)+(?=\\.)")
  # date of extract
  eDate <- str_replace_all(Sys.Date(),  "-","")
  write_csv(shp_ex, paste0(projDir,outDir,"/",bn, "_","ext_",eDate,".csv"))
  rm(S2)
  removeTmpFiles(h = 0)
  print(paste(ras, "is complete"))
}
