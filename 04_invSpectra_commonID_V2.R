# Title: 04_InvSpectra_commonID.R
# Author: Christiana Ade
# Date: 2/4/2021
# Purpose: Extract common spectra for 2018, 2019 fall and spring 
# **Requires** 
# 1)  
####################################################################################
## require packages
require(raster)
require(tidyverse)
require(stringr)
require(rgdal)
require(ggplot2)
require(ggthemes)
require(lubridate)
require(readxl)
require(patchwork)
library(gridExtra)
library(grid)
library(lattice)
require(RColorBrewer)

# set project directory
if(Sys.info()["sysname"] == "Windows"){
  setwd("Z:/Projects/DSC_Delta/Task1/P04_TS") } else {
    setwd("/beanstore/Projects/DSC_Delta/Task1/P04_TS") }

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####             START USER DEFINED VARIABLES       #### 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# The following variables should be defined by the user each time.
#### INPUT FILES ####
sL <- list.files("./Output/CSV/02_editExtract", pattern = ".csv$", full.names = T)
#### OUTPUT FILES ####
outDir <- "./Output/Figures/04_commonID"
#### USER DEFINED FUNCTIONS ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####             END USER DEFINED VARIABLES        ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Sentinel-2 variables 
varsS2 <- c("BlueAersol", "Blue","Green","Red","VR1","VR2","VR3","NIR","SWIR1","SWIR2")
s2Key <- tibble(bands= varsS2,
                wavelength = c(443, 492.7,559.8,664.6,704.1,740.5,782.8,832,1613.7,2202.4))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####             START SCRIPT                      ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##################### PART 1: Determine most common point id's #####################################
# read in those three files to see how much we would even want to keep
# 20181007, 20190425, 20191002
mDat <- map_dfr(str_subset(sL, "20181007|20190425|20191002"), read_csv)  %>%
  # add in a 1 for all the files that are not related to muis
  mutate_at(vars(X_20191002, X_20181007, X_20171101, X_20190425), ~replace_na(., 1))  %>%
  # filter out spectra with na values
  filter(!is.na(Blue)) %>%
  # filter for only files we want 
  # filter(shpSource %in% c("mui_edit","cade","cstar_edit","shrutiAddLine_edit")) %>%
  filter(shpSource %in% c("mui_edit","cade","cstar_edit")) %>%
  # remove columns that have all NAs
  select_if(~sum(!is.na(.)) > 0) %>%
  # add a uniqueID
  mutate(fID = as.character(1:nrow(.))) %>%
  # filter for points only Mui decided were suitable in all years 
  filter(X_20191002 != 0  & X_20181007 !=0 & X_20190425!=0) %>%
  # remove other spectra
  filter(!Class3 %in% c("REMOVE","OTHER"))  %>%
  # change the way cade FLT is labeled %>%
  mutate(Class6 = ifelse(Species_1 == "FLT", "FLT-CADE", Class6)) %>%
  # create new grouping column for each year 
  mutate(rYr = str_extract(rDate,"(2016|2017|2018|2019|2020)")) %>%
  # # add id to filter
  mutate(Class6_ID = paste0(Class6,"_",rYr, "_", ras_col,"_",ras_row)) 

# Althougth in the future it might be helpful to see how the different SAV's look 

### TODO ####
# use cells to filter out points that are too close together 
# different filters 
# all --> 4 shpSource --> 3 shpSource (no shruti line)
# 6825 --> 6629 --> Line 5033
# View(mDat2 %>% group_by(rDate,Class3) %>% summarise(count1 = n()))

##################### PART 2: Create color table #####################################
# bad dates from 2019
bad19 <- c("20190420", "20190430", "20190629","20190729", "20190828", "20190525", "20190624", "20191111", "20191231")

# create unqDat table 
unqCol <- str_extract(sL, "(2016|2017|2018|2019|2020)\\d{4}") %>% 
  as.numeric() %>% 
  sort() %>%
  tibble(rDate = .) %>%
  # filter out bad 2019 dates with cloud cover
  filter(!rDate %in% bad19) %>%
  # determine what season something is from
  mutate(season1 = case_when(
    month(ymd(rDate)) %in% c(12,1,2) ~ "winter",
    month(ymd(rDate)) %in% c(3,4,5) ~ "spring",
    month(ymd(rDate)) %in% c(6,7,8) ~ "summer",
    month(ymd(rDate)) %in% c(9,10,11) ~ "fall")) %>%
  # colors accordingly
  # winter is brown, spring orange, summer is green, fall is purple 
  mutate(color1 = case_when(
    season1 %in% c("winter") ~ "#d95f02",
    season1 %in% c("spring") ~ "#e7298a",
    season1 %in% c("summer") ~ "#1b9e77", 
    season1 %in% c("fall") ~ "#7570b3")) # purple
#1b9e77 - green
#d95f02 - orange
#7570b3 - purple
#e7298a - pink 

##################### PART 3: Filtering #####################################
# Filter based on the unique ID's listed above and 
# added 2/28 the ones you deleted in your match-update investigation
# updated on 2/28 including year in the Class6_ID 
# read in C comments
#### TODO - change to work on beanstore
cCom <- read_csv("Z:/Projects/DSC_Delta/Task1/P03_newMui/Output/CSV/04_editSpec/roiRemoved_R1_0228.csv") %>%
  arrange(Class6_ID) %>% 
  distinct() # with this is 1697 (after removing the Spring dates because we really didn't look atthoes )

# Now where are the duplicates coming from because oh wait we have a 2019 spring and a 2019 fall
# unique IDs to filter based on class 6 
filID <- unique(mDat$Class6_ID)

# read in all data
# nrow( no filters 269690)
allDat <- map_dfr(sL,read_csv) %>%
  # create new grouping column for each year 
  mutate(rYr = str_extract(rDate,"(2016|2017|2018|2019|2020)")) %>%
  # change the way you label the FLT-Cade 
  # you are only doing this because you might select it for the other classifications besides class 6
  mutate(Class6 = ifelse(Species_1 == "FLT", "FLT-CADE", Class6)) %>%
  # # add id to filter
  mutate(Class6_ID = paste0(Class6,"_",rYr, "_", ras_col,"_",ras_row))

# after the first filter its 143492
# what is the class breakdown? do we have enough 
# shit after the 
y <- allDat %>%
  # filter by the common IDs
  filter(Class6_ID %in% filID) %>%
  # add cade comments 
  left_join(cCom) %>%
  # delete ones with delete in the filCade column 
  filter(!filCade == "delete")

#### ALSO CHECK THAT ONE ENTRY YOU REMOVED 

# 20210208 why the hell is this still a spotID 
# edit data table 
mDat <- allDat %>% 
  # select only required columns 
  dplyr::select(., c(varsS2,"Class6", "shpSource", "rDate","Class6_ID")) %>%
  # arrange R dates in correct numerical order 
  arrange(rDate) %>% 
  # filter out bad 2019 dates with cloud cover
  filter(!rDate %in% bad19) %>%
  # divde the ref spectra by 10,000
  mutate_at(varsS2, .funs = function(x) {return(x/10000)})  %>%
  # join with the color table created above 
  left_join(unqCol) %>%
  gather( key = 'bands', value = 'ref', -c(Class6,rDate,rDate, shpSource,rYr, Class6_ID,season1, color1)) %>%
  left_join(s2Key) %>%
  # sort R date 
  group_by(Class6_ID,rYr) %>%
  # convert to character
  mutate(rDate = as.character(rDate)) 

##################### PART 4:plotting  #####################################

z<- m %>%
  do( plots = ggplot(data = .,aes(y= ref, x = wavelength, col = rDate)) +
        geom_line(size=1) +
        scale_y_continuous(breaks = seq(-0.1, 0.6, 0.10)) +
        ggtitle(.$Class6_ID) +
        geom_vline( aes(xintercept = wavelength), col = "grey60") +
        geom_text(data = ., mapping = aes(label = bands, y = -0.1),angle = 60, hjust = 0) +
        scale_colour_manual(values = .$color1) +
        theme_classic())

z2 <- m %>%
  do( plots = ggplot(data = .,aes(y= ref, x = wavelength, col = rDate)) +
        geom_line(size=1) +
        scale_y_continuous(breaks = seq(-0.1, 0.6, 0.10)) +
        ggtitle(.$Class6_ID) +
        geom_vline( aes(xintercept = wavelength), col = "grey60") +
        geom_text(data = ., mapping = aes(label = bands, y = -0.1),angle = 60, hjust = 0) +
        theme_classic())


for (i in 1:nrow(z)){
  #chart$plots[1][[1]]
  myPlot <- z$plots[[i]] + z2$plots[[i]]
  
  # filename
  bn <- paste0(z$Class6_ID[i], "_Yr",z$rYr[i] )
  myFile <- paste0(outDir,"/",bn,".png")
  
  ggsave(myPlot,filename=myFile,width=13.66,height=7.05,limitsize = FALSE) 
}












