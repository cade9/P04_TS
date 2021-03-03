# Title: 05_addVI.R
# Author: Christiana Ade
# Date: 3/3/2021
# Purpose: Add vegetation indexes to the extracted points
####################################################################################
## require packages
require(tidyverse)
require(stringr)
# Determine OS
if(Sys.info()["sysname"] == "Windows"){
  pDir <- ("Z:/") } else {
    pDir <- "/beanstore/"
  }
# project directory
projDir <- getwd()
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####             START USER DEFINED VARIABLES       #### 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# The following variables should be defined by the user each time.
#### INPUT FILES ####
fL <- list.files(paste0(projDir,"/Output/CSV/03_caFil"), full.names = T)
#### OUTPUT FILES ####
outDir <- paste0(projDir, "/Output/CSV/05_addVI")
#### USER DEFINED FUNCTIONS ####
source(paste0(projDir,"/Functions/VegIndex.R"))
applyIndex <- function(dfN, outDir) {
  varsS2 <- c("BlueAersol", "Blue","Green","Red","VR1","VR2","VR3","NIR","SWIR1","SWIR2")
  df2 <- read_csv(dfN) %>%
    #filter(!Class1 == "OTHER") %>%
    filter(!is.na(Blue)) %>%
    # adjust to units of reflectance %
    mutate_at(varsS2, .funs = function(x) {return(x/10000)}) %>%
    # add all veg indexes 
    mutate(ndvi = ndvi(NIR,Red),
           ndavi = ndavi(NIR,Blue),
           wavi = wavi(NIR, Blue),
           savi = savi(NIR, Red),
           rendvi1 = rendvi1(NIR, VR2),
           rendvi2 = rendvi2(NIR, VR3),
           ndwi = ndwi(Green, NIR),
           ndmi = ndmi(NIR, SWIR1),
           mndwi = mndwi(Green, SWIR1),
           #cari = cari(VR1, Red, Green),
           mtci = mtci(VR1, VR2,Red))
  fn <- str_extract(dfN, "(?=[^\\\\|/]*$)(.)+(?=\\.)")
  write_csv(df2, paste0(outDir,"/",fn, "_calcVI.csv") )
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####             END USER DEFINED VARIABLES        ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####             START SCRIPT                      ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# lapply to write out new 
lapply(fL, applyIndex, outDir = outDir)




# should select points that MUI said were the same in all years because then


