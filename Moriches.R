library(readxl)
library(janitor)
library(ggplot2)

source("https://raw.githubusercontent.com/bjscannell/lab_code/master/load_vps_csvs.R")

# Load in the detections (youll have to go back and change this function
# when you include more artificial reefs)

dets <- load_vps_files()

# Read in tag data

sf <- read_csv("tags/southfork_fish.csv") %>% clean_names()
dec <-  read_csv("tags/NYSDEC_otn_metadata_tagging.csv") %>% clean_names()
internal <- read_excel("tags/Receiever Internal Tag IDs.xlsx") %>% clean_names()
rec_id <- internal$transmit_id


fish <- dets %>% clean_names() %>% 
  filter(!grepl("1601", full_id, fixed = TRUE)) %>% 
  left_join(sf, by = c("full_id" = "transmitter")) %>% 
  left_join(dec, by = c("full_id" = "transmitter")) %>% 
  filter(species == "Black Sea Bass") %>% 
  rename(individual.local.identifier = full_id, 
         timestamp = time,
         location.long = longitude,
         location.lat = latitude) %>% dplyr::select(individual.local.identifier, timestamp, location.lat, location.long)

ggplot(fish, aes(x = timestamp, y = individual.local.identifier)) + geom_point(size = 0.5, shape = 16)

fish.telem <- as.telemetry(fish,timeformat="auto",timezone="UTC",projection="+proj=utm +zone=18 +datum=WGS84 +units=m +no_defs +type=crs",datum="WGS84",
             dt.hot=NA,timeout=Inf,na.rm="row",mark.rm=FALSE,keep=FALSE,drop=FALSE)



library(ggplot2)
leo <- fish %>% filter(individual.local.identifier == "A69-1604-7684")
ggplot(leo, aes(x = location.long, y = location.lat, color = individual.local.identifier)) + 
  geom_point(size = 0.5, shape = 16) + theme_bw() + coord_cartesian()

library(ctmm)
fish.telem <- as.telemetry(leo,timeformat="auto",timezone="UTC",projection="+proj=utm +zone=18 +datum=WGS84 +units=m +no_defs +type=crs",datum="WGS84",
                           dt.hot=NA,timeout=Inf,na.rm="row",mark.rm=FALSE,keep=FALSE,drop=FALSE)

plot(fish.telem,  error=FALSE)


SVF <- variogram(fish.telem$`A69-1604-7684`)
level <-  c(0.5,0.95)
xlim <- c(0,12 %#% "hour") # 0-12 hour window
plot(SVF,xlim=xlim,level=level)


plot(SVF,fraction=0.65,level=level)

GUESS <- ctmm.guess(fish.telem$`A69-1604-7684`,interactive=FALSE)


# fit a bunch of models, tell me what models are being fit, return all models, and use all but one CPU core
FITS <- ctmm.select(fish.telem$`A69-1604-7684`,GUESS,trace=3,verbose=TRUE,cores=-1)


summary(FITS)

save(FITS, file = "FITS.rda")
load("FITS.rda")

RES <- residuals(fish.telem$`A69-1604-7684`, FITS[[1]])
plot(RES)

plot(SVF, CTMM = FITS[[1]],
     units = TRUE, fraction = 0.5, level = c(0.95, 0.50), 
     col = "black", col.CTMM = "red")



AKDE <-akde(fish.telem$`A69-1604-7684`, FITS[[1]])
wAKDE <- akde(fish.telem$`A69-1604-7684`,FITS[[1]],weights=TRUE)

summary(AKDE)

plot(fish.telem$`A69-1604-7680`, UD = AKDE, level=0.95, error = F)


# calculate one extent for all UDs
EXT <- extent(list(AKDE,wAKDE),level=0.95)


plot(fish.telem$`A69-1604-7680`,UD=AKDE,xlim=EXT$x,ylim=EXT$y)
title(expression("OUF AKDE"["C"]))
plot(fish.telem$`A69-1604-7680`,UD=wAKDE,xlim=EXT$x,ylim=EXT$y)
title(expression("weighted OUF AKDE"["C"]))