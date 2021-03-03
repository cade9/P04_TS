# Title: rasterVItest_forEach.R
# Status:
# Author: Christiana Ade
# Date: 02/05/2021
# Modified: 
# Purpose: Calculate the VI rasters
# **Requires** 
# 1)  
####################################################################################
## require packages
require(raster)
require(rgdal)
require(stringr)
require(tictoc)
require(readr)
require(doParallel)
require(foreach)
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
# 1) Reflectance Rasters
rL <- list.files(paste0(pDir,"Data/Raster/Sentinel2/S2_Level2_10mStack"), pattern = "BOA_.*dat$", full.names = T, recursive = T)

#2 ) detect cores
useCores <- 5 #detectCores()*3/4
#Register CoreCluster
cl <- makeCluster(useCores)
registerDoParallel(cl)

# header changes
# hdrRep <- read_lines(paste0(projDir, "/Data/CSV/09_headNameUpdate.txt"), skip = 1)
#### OUTPUT FILES ####
outDir <- paste0(projDir,"/Output/Raster/testVI_v2")
#### USER DEFINED FUNCTIONS ####
source(paste0(projDir,"/Functions/VegIndex.R"))
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####             END USER DEFINED VARIABLES        ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####             START SCRIPT                      ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#### ~! TODO if the file is already there
foreach(i=1:length(rL)) %dopar% {
  require(raster)
  require(rgdal)
  require(tictoc)
  require(stringr)
  ras <-rL[i]
  tic(paste("processing", ras))
 
  S2 <- raster::stack(ras)
  S2_div <- S2/10000
  # remove from memory
  rm(S2)
  # change names #
  #### TODO check counts of pixels greater than 1 and remove them 
  
  names(S2_div) <- c( "BlueAersol","Blue","Green","Red","VR1","VR2","VR3","NIR","WaterVapor","SWIR1","SWIR2")

  ras_NDVI <- ndvi(NIR = S2_div[[8]], Red = S2_div[[4]])
  ras_ndavi <- ndavi(NIR = S2_div[[8]], Blue = S2_div[[2]])
  ras_wavi <- wavi(NIR = S2_div[[8]], Blue = S2_div[[2]])
  ras_savi <- savi(NIR = S2_div[[8]], Red = S2_div[[4]])
  ras_evi <- evi(NIR = S2_div[[8]], Red = S2_div[[4]], Blue = S2_div[[2]])
  ras_rendvi1 <- rendvi1(NIR = S2_div[[8]], VR2 = S2_div[[6]])
  ras_rendvi2 <- rendvi2(NIR = S2_div[[8]], VR3 = S2_div[[7]])
  ras_ndwi <- ndwi(Green = S2_div[[3]], NIR = S2_div[[8]])
  ras_ndmi <- ndmi(NIR = S2_div[[8]], SWIR = S2_div[[10]])
  ras_mndwi <- mndwi(Green = S2_div[[3]], SWIR = S2_div[[10]])

  # stack raster
  rStack <- addLayer(S2_div,ras_NDVI,ras_ndavi,ras_wavi,ras_savi,ras_evi,ras_rendvi1,ras_rendvi2,ras_ndwi,ras_ndmi,ras_mndwi)

  # write out raster
  bn <- str_extract(ras, "(?=[^\\\\|/]*$)(.)+(?=\\.)")
  outN <- paste0(outDir,"/",bn,"_VIStack")
  writeRaster(rStack, outN, format = "ENVI", datatype = "FLT8S") # make sure it is 12 rather than data type 4
  ###########################################################
  # edit raster based on previous header
  #### TODO #### add header changer
  # ~! for the tests, you just edited the header manually 
  # hr_orig <- "Z:\\Projects\\DSC_Delta\\Task1\\P03_newMui\\Output\\Raster\\testVI\\S2B2A_20171101_113_10SFH_BOA_10_VIStack.hdr"
  # m <- read_lines(hr_orig)
  # bandPos <- which(str_detect(m,"band names"))
  # bandEnd <- bandPos + 12
  # m[c(bandPos:length(m))] <- hdrRep
  ###########################################################
  # remove raster
  rm(S2_div,ras_savi,ras_mndwi,ras_ndmi, ras_ndwi, ras_rendvi2, ras_rendvi1, ras_evi, ras_wavi,ras_ndavi, ras_NDVI,rStack)
  removeTmpFiles(h = 0)
  print(paste(ras, "is complete"))
  toc(log = T)
  
}



# Error in rgdal::putRasterData(x@file@transient, vv, band = i, offset = off) : 
#   
#   GDAL Error 3: Failed to write scanline 0 to file.


