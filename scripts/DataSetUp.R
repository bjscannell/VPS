# -------------------------------------------------------------------------

# This code produces fully set up dataframe with filtered detections from 
# VUE and IDed tags in the otn metadata tag format 

# remember detection timestamps = time
# transmitter = full_id
# -------------------------------------------------------------------------


library(readxl)
library(janitor)
library(dplyr)
library(ggplot2)
library(sf)
library(ctmm)


source("https://raw.githubusercontent.com/bjscannell/lab_code/master/load_vps_csvs.R")

# Load in the detections (youll have to go back and change this function
# when you include more artificial reefs)

dets <- load_vps_files()  %>% 
  clean_names() %>% 
  filter(!grepl("^A69-1601", full_id)) 


# Read in reference data -------------------------------------------------------
# NYSDEC tagged fish
NYSDEC_fish <- read_csv("tags/NYSDEC_otn_metadata_tagging.csv") %>% clean_names()

# Orsted Southfork fish
Southfork_fish <- read_csv("tags/Southfork_otn_metadata_tagging.csv") %>% clean_names() 

# Landscape Lab fish
LL_fish <- read_csv("tags/LLab_otn_metadata_tagging.csv") %>% clean_names() 

# Orsted Sunrise fish
Sunrise_fish <- read_csv("tags/Sunrise_otn_metadata_tagging.csv") %>% clean_names() 

# Non Peterson Lab tags
Other_fish <- read_csv("tags/NonPeterson_otn_metadata_tagging.csv") %>% clean_names() 

tags <- rbind(NYSDEC_fish, Southfork_fish, Sunrise_fish, LL_fish, Other_fish)

# Combine dataframes ------------------------------------------------------


dets_full <- dets %>% 
  left_join(tags, by = c("full_id" = "transmitter"), multiple = "all") %>% 
  select(3,4,5,6,7,8,9,10,11,12,13, 19,20, 25, 26, 29,32,35,43,44,45,46,47,50,54,55,56,57,76) # fuck this

