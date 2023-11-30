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
  filter(!grepl("^A69-1601", transmitter)) 


# Read in reference data -------------------------------------------------------
# NYSDEC tagged fish
NYSDEC_fish <- read_csv("tags/NYSDEC_otn_metadata_tagging.csv") %>% clean_names()

# Orsted Southfork fish
Southfork_fish <- read_csv("tags/Southfork_otn_metadata_tagging.csv") %>% clean_names() 

# Landscape Lab fish
LL_fish <- read_csv("tags/LLab_otn_metadata_tagging.csv") %>% clean_names() 

# Orsted Sunrise fish


# Non Peterson Lab tags
Other_fish <- read_csv("tags/NonPeterson_otn_metadata_tagging.csv") %>% clean_names() 

tags <- rbind(NYSDEC_fish, Southfork_fish, LL_fish, Other_fish)

# Combine dataframes ------------------------------------------------------


dets_full <- dets %>% 
  left_join(tags, by = "transmitter", multiple = "all") %>% 
  select(1,2,3,5,6,7,8,9,10,12,14,20,27,28,30,32,33,34,37,38,39,40,41,42,,43,45,49,50,51,53,71,72) # fuck this