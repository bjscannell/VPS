library(readxl)
library(janitor)
library(ggplot2)
library(sf)
library(ctmm)
library(scattermore)
library(ggspatial)


source("https://raw.githubusercontent.com/bjscannell/lab_code/master/load_vps_csvs.R")

# Load in the detections (youll have to go back and change this function
# when you include more artificial reefs)


fish <- dets_full %>% filter(common_name_e == "Black Sea Bass") %>% 
  rename(individual.local.identifier = full_id, 
         timestamp = time,
         location.long = longitude,
         location.lat = latitude) %>% dplyr::select(individual.local.identifier, timestamp, location.lat, location.long)

ggplot(fish, aes(x = timestamp, y = individual.local.identifier)) + geom_scattermore(size = 0.5, shape = 16)

fish.telem <- as.telemetry(fish,timeformat="auto",timezone="UTC",projection="+proj=utm +zone=18 +datum=WGS84 +units=m +no_defs +type=crs",datum="WGS84",
             dt.hot=NA,timeout=Inf,na.rm="row",mark.rm=FALSE,keep=FALSE,drop=FALSE)



library(ggplot2)
leo <- fish %>% filter(individual.local.identifier == "A69-9007-8321")
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
#wAKDE <- akde(fish.telem$`A69-1604-7684`,FITS[[1]],weights=TRUE)

summary(AKDE)

plot(fish.telem$`A69-1604-7684`, 
     UD = AKDE,
     level=0.95, error = F, 
     DF = "CDF",
     pch = 1, col = rgb(red = 0.7, green = 0.7, blue = 0.7, alpha = .1))



# Attempt to pull out the AKDE countours
UD.poly <-  SpatialPolygonsDataFrame.UD(AKDE)
leo.coords <- SpatialPoints.telemetry(fish.telem)


sf_object <- st_as_sf(UD.poly)
sf_leo <- st_as_sf(leo.coords)

#"+proj=utm +zone=18 +datum=WGS84 +units=m +no_defs"

# Get structure points
StructurePoints <- read_csv("StructurePoints.csv") %>%  clean_names()
sf_str <- st_as_sf(StructurePoints, coords = c("longitude", "latitude"), 
                   crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") %>% 
  st_transform(32618)

str.in <-c(2,5,6,7,8,10,15,20,21)

# Create ggplot plot
p <- ggplot() +
  geom_sf(data = sf_leo, alpha = 0.1, shape = 16, color = "azure4") +
  geom_sf(data = sf_object, fill = rgb(red = 0.7, green = 0.7, blue = 0.7, alpha = .1), color = "#510400") +
  ggsflabel::geom_sf_label_repel(data = sf_str %>% filter(id %in% str.in), 
                      aes(label = stringr::str_wrap(material, 20)),
                      label.size = NA, 
                      alpha = 0.6, 
                      fontface = 'bold', color = 'black',
                      box.padding = 0.7, point.padding = 0.5) +
  geom_sf(data = sf_str, aes(color = type), size = 2.5) +
  annotation_scale(location = "bl", width_hint = 0.1) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.01, "in"),
                         style = north_arrow_fancy_orienteering) +
  scale_color_manual(values = c("#ccab5e", "#8a5ecc", "#cc5ea0", "#a0cc5e")) +
  coord_sf(xlim = c( st_bbox(sf_object)[["xmin"]]-20,  st_bbox(sf_object)[["xmax"]]+12), 
           ylim = c( st_bbox(sf_object)[["ymin"]]-40,  st_bbox(sf_object)[["ymax"]]+12)) +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white",
                                        color = "white"),
        plot.background = element_rect(fill = "white",
                                       color = "white")) +
  labs(title="Home Range of Leo",
       x ="Longitude", y = "Latitude") +
  guides(color=guide_legend(title="Material Type")) 

ggsave(filename = "leo_plot.png", p, dpi = 360, width = 10, height = 8, units = "in")

