# Title: 06_VI_rSamp.R
# Status:
# Author: Christiana Ade
# Date: 1/5/2020
# Modified: 
# Purpose: Add vegetation indexes to the extracted points and do a random sample 
# or should you do a random sample on the model that gives you the best results 
# **Requires** 
# 1)  
####################################################################################
## require packages
require(tidyverse)
require(stringr)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####             START USER DEFINED VARIABLES       #### 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# The following variables should be defined by the user each time.
#### INPUT FILES ####
fL <- list.files("./Output/CSV/04_editSpec", full.names = T)
#### OUTPUT FILES ####
outDir <- "./Output/CSV/05_addVI"
#### USER DEFINED FUNCTIONS ####
source("./Functions/VegIndex.R")
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


