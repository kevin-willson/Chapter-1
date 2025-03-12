# This script is to parameterize a distance-only model that will be used as a comparative model in the paper

library(readxl)
library(dplyr)
library(bimixt)
library(car)
library(MASS)
library(tidyr)
library(data.table)
library(xlsx)
library(forecast)
library(lmtest)
library(MuMIn)
library(caret)
library(broom)
library(rstatix)
library(gridExtra)
library(DescTools)
library(ggplot2)
library(ggpubr)
library(statmod)

setwd("D:/Dryad_submission/Data/")
seedling_distance <- read_excel("Transect_Data_5-25-21.xlsx", sheet = 1)
Plotdata_HA <- read_excel("Plot_data.xlsx", sheet = 1)
seedling_distance$FPT <- paste(seedling_distance$Fire, "-", seedling_distance$Patch, "-", seedling_distance$Transect)
seedling_distance <- seedling_distance[!is.na(seedling_distance$Vertical_Distance),]

## Get equal densities across all transects
x <- 1
additional_shelly_1_1 <- seedling_distance[FALSE,]
d <- seedling_distance[FALSE,]

while(x <= length(seedling_distance$Fire)){
  if(seedling_distance$Fire[x] == "Shelly" & seedling_distance$Patch[x] == 1 & seedling_distance$Transect[x] == 1){
    d <- rbind(seedling_distance[x,],seedling_distance[x,],seedling_distance[x,],seedling_distance[x,],seedling_distance[x,],
               seedling_distance[x,],seedling_distance[x,],seedling_distance[x,],seedling_distance[x,])
    additional_shelly_1_1 <- rbind(additional_shelly_1_1, d)
    x <- x + 1
  }
  else{
    x <- x + 1
  }
}

seedling_distance <- rbind(seedling_distance, additional_shelly_1_1)


## Convert data to densities
x <- 1
seedling_distance$Distance_grouping <- NA
while(x <= length(seedling_distance$Fire)){
  if(seedling_distance$Vertical_Distance[x] <= 10){
    seedling_distance$Distance_grouping[x] <- 5
    x <- x+1
  }
  else if(seedling_distance$Vertical_Distance[x] > 10 & seedling_distance$Vertical_Distance[x] <= 20){
    seedling_distance$Distance_grouping[x] <- 15
    x <- x+1
  }
  else if(seedling_distance$Vertical_Distance[x] > 20 & seedling_distance$Vertical_Distance[x] <= 30){
    seedling_distance$Distance_grouping[x] <- 25
    x <- x+1
  }
  else if(seedling_distance$Vertical_Distance[x] > 30 & seedling_distance$Vertical_Distance[x] <= 40){
    seedling_distance$Distance_grouping[x] <- 35
    x <- x+1
  }
  else if(seedling_distance$Vertical_Distance[x] > 40 & seedling_distance$Vertical_Distance[x] <= 50){
    seedling_distance$Distance_grouping[x] <- 45
    x <- x+1
  }
  else if(seedling_distance$Vertical_Distance[x] > 50 & seedling_distance$Vertical_Distance[x] <= 60){
    seedling_distance$Distance_grouping[x] <- 55
    x <- x+1
  }
  else if(seedling_distance$Vertical_Distance[x] > 60 & seedling_distance$Vertical_Distance[x] <= 70){
    seedling_distance$Distance_grouping[x] <- 65
    x <- x+1
  }
  else if(seedling_distance$Vertical_Distance[x] > 70 & seedling_distance$Vertical_Distance[x] <= 80){
    seedling_distance$Distance_grouping[x] <- 75
    x <- x+1
  }
  else if(seedling_distance$Vertical_Distance[x] > 80 & seedling_distance$Vertical_Distance[x] <= 90){
    seedling_distance$Distance_grouping[x] <- 85
    x <- x+1
  }
  else if(seedling_distance$Vertical_Distance[x] > 90 & seedling_distance$Vertical_Distance[x] <= 100){
    seedling_distance$Distance_grouping[x] <- 95
    x <- x+1
  }
  else if(seedling_distance$Vertical_Distance[x] > 100 & seedling_distance$Vertical_Distance[x] <= 110){
    seedling_distance$Distance_grouping[x] <- 105
    x <- x+1
  }
  else if(seedling_distance$Vertical_Distance[x] > 110 & seedling_distance$Vertical_Distance[x] <= 120){
    seedling_distance$Distance_grouping[x] <- 115
    x <- x+1
  }
  else if(seedling_distance$Vertical_Distance[x] > 120 & seedling_distance$Vertical_Distance[x] <= 130){
    seedling_distance$Distance_grouping[x] <- 125
    x <- x+1
  }
  else if(seedling_distance$Vertical_Distance[x] > 130 & seedling_distance$Vertical_Distance[x] <= 140){
    seedling_distance$Distance_grouping[x] <- 135
    x <- x+1
  }
  else if(seedling_distance$Vertical_Distance[x] > 140 & seedling_distance$Vertical_Distance[x] <= 150){
    seedling_distance$Distance_grouping[x] <- 145
    x <- x+1
  }
  else if(seedling_distance$Vertical_Distance[x] > 150 & seedling_distance$Vertical_Distance[x] <= 160){
    seedling_distance$Distance_grouping[x] <- 155
    x <- x+1
  }
}

## Group regen by transect and distance to count the number of each
seed_density_distance <- seedling_distance %>% 
  group_by(Fire, Patch, Transect, Distance_grouping) %>% 
  tally()

## Convert density to ha (100 sq m to 10,000 sq m)
seed_density_distance$Density_ha <- seed_density_distance$n*100

## Bind the plot data with it in preparation for analysis
merging <- Plotdata_HA[, c(1,2,3, 11, 10, 6, 12, 13, 4, 5, 9, 8)]

seed_density_distance <- merge(seed_density_distance, merging, by = c("Fire", "Patch", "Transect"), all.x = T)

seed_density_distance <- seed_density_distance[!is.na(seed_density_distance$AvgHeight),]

colnames(seed_density_distance)[8] <- "Aspect"
colnames(seed_density_distance)[7] <- "Slope_Angle"
colnames(seed_density_distance)[9] <- "Tree_Height"
colnames(seed_density_distance)[10] <- "Prevailing_Wind"
colnames(seed_density_distance)[11] <- "Cont_Prevailing_Wind"
colnames(seed_density_distance)[13] <- "PIPODensity_25"
colnames(seed_density_distance)[14] <- "Combined_slope"

seed_density_distance$Prevailing_Wind <- ifelse(seed_density_distance$Cont_Prevailing_Wind>0, "Yes", "No")
seed_density_distance$Cont_Prevailing_Wind_0 <- ifelse(seed_density_distance$Cont_Prevailing_Wind>0, seed_density_distance$Cont_Prevailing_Wind,0)

## Remove transects that had nearby overstory trees at the far end of the transect
seed_density_distance <- seed_density_distance[!(seed_density_distance$Fire == "Shelly" & seed_density_distance$Patch == "NB" & seed_density_distance$Transect == "2"),]
seed_density_distance <- seed_density_distance[!(seed_density_distance$Fire == "Shelly" & seed_density_distance$Patch == "1" & seed_density_distance$Transect == "2"),]
seed_density_distance <- seed_density_distance[!(seed_density_distance$Fire == "Glass" & seed_density_distance$Patch == "2" & seed_density_distance$Transect == "7"),]
seed_density_distance <- seed_density_distance[!(seed_density_distance$Fire == "Shelly" & seed_density_distance$Patch == "NB" & seed_density_distance$Transect == "1"),]
seed_density_distance <- seed_density_distance[!(seed_density_distance$Fire == "Divide" & seed_density_distance$Patch == "6" & seed_density_distance$Transect == "10"),]

## Specify the distance only logistic model
bionomal_data <- read.csv("final_model_df.csv")

bionomal_data$binom <- ifelse(bionomal_data$Seedling_density > 0,1,0)

distance_binom <- glm(binom ~ log(Distance), family = binomial(), data=bionomal_data,  na.action = "na.fail" )

summary(distance_binom)

AIC(distance_binom)

## Specify the distance only linear model 
distance <- glm(log(Density_ha) ~ Distance_grouping, data=seed_density_distance,  na.action = "na.fail" )

## Check the regression stats
summary(distance)

## This part converts the dataframe into a line shapefile and determines the number of PIPO in between the burn scar and the specific pipo pixel

### import libraries
library(tibble)
library(rgeos)
library(rgdal)
library(raster)
library(sf)
library(sfheaders)
library(foreign)
library(doParallel)
library(foreach)

cdata_parallel <- read.csv("test_lines_df_12-24-21.csv")

### Lots of this code was kept as is because removing some parts caused it not to work 
cdata_parallel <- cdata_parallel[, !names(cdata_parallel) %in% c("X")]

cdata_parallel$id <- row.names(cdata_parallel) 

cdata_parallel$angle <- (atan2((cdata_parallel$PIPO.X-cdata_parallel$High.Severity.X),(cdata_parallel$PIPO.Y-cdata_parallel$High.Severity.Y))*180/3.14159265) +180

### I need to calculate location and slope for 160 meters from the intact forest in the direction of the burn pixel because of how I calculated it for the
### model. First, lets get the location for 160 meters at that angle from intact forest
cdata_parallel$PIPO.X.160 <- sin(cdata_parallel$angle*0.01745329)*160 + cdata_parallel$PIPO.X
cdata_parallel$PIPO.Y.160 <- cos(cdata_parallel$angle*0.01745329)*160 + cdata_parallel$PIPO.Y

PIPO_end_dem <- cdata_parallel[,c(1,2,11,13,14)]

xy <- PIPO_end_dem[,c(4,5)]

spdf <- SpatialPointsDataFrame(coords = xy, data = PIPO_end_dem,
                               proj4string = CRS("+proj=utm +zone=13 +datum=WGS84 +units=m +no_defs" ))

DEM <- raster("All_Fires_DEM_UTM.tif")

pipo_dems_160_1 <- as.data.frame(extract(DEM, spdf[1:1000000,]))
colnames(pipo_dems_160_1) <- "Elev_160"
pipo_dems_160_2 <- as.data.frame(extract(DEM, spdf[1000001:2000000,]))
colnames(pipo_dems_160_2) <- "Elev_160"
pipo_dems_160_3 <- as.data.frame(extract(DEM, spdf[2000001:3000000,]))
colnames(pipo_dems_160_3) <- "Elev_160"
pipo_dems_160_4 <- as.data.frame(extract(DEM, spdf[3000001:4000000,]))
colnames(pipo_dems_160_4) <- "Elev_160"
pipo_dems_160_5 <- as.data.frame(extract(DEM, spdf[4000001:5000000,]))
colnames(pipo_dems_160_5) <- "Elev_160"
pipo_dems_160_6 <- as.data.frame(extract(DEM, spdf[5000001:6000000,]))
colnames(pipo_dems_160_6) <- "Elev_160"
pipo_dems_160_7 <- as.data.frame(extract(DEM, spdf[6000001:7000000,]))
colnames(pipo_dems_160_7) <- "Elev_160"
pipo_dems_160_8 <- as.data.frame(extract(DEM, spdf[7000001:8000000,]))
colnames(pipo_dems_160_8) <- "Elev_160"
pipo_dems_160_9 <- as.data.frame(extract(DEM, spdf[8000001:9000000,]))
colnames(pipo_dems_160_9) <- "Elev_160"
pipo_dems_160_10 <- as.data.frame(extract(DEM, spdf[9000001:10000000,]))
colnames(pipo_dems_160_10) <- "Elev_160"
pipo_dems_160_11 <- as.data.frame(extract(DEM, spdf[10000001:11000000,]))
colnames(pipo_dems_160_11) <- "Elev_160"
pipo_dems_160_12 <- as.data.frame(extract(DEM, spdf[11000001:12000000,]))
colnames(pipo_dems_160_12) <- "Elev_160"
pipo_dems_160_13 <- as.data.frame(extract(DEM, spdf[12000001:12645807,]))
colnames(pipo_dems_160_13) <- "Elev_160"

pipo_dems_160 <- rbind(pipo_dems_160_1, pipo_dems_160_2, pipo_dems_160_3, pipo_dems_160_4, pipo_dems_160_5, pipo_dems_160_6, pipo_dems_160_7, 
                       pipo_dems_160_8, pipo_dems_160_9, pipo_dems_160_10, pipo_dems_160_11, pipo_dems_160_12, pipo_dems_160_13)

cdata_parallel$PIPO.160.DEM.value <- pipo_dems_160$Elev_160

### THIS IS CORRECT
cdata_parallel$slope <- (atan((cdata_parallel$PIPO.160.DEM.value - cdata_parallel$PIPO.DEM.value)/160))*180/3.14159265

cdata_parallel$bin <- NA

cdata_parallel$bin <- ifelse(cdata_parallel$Distance <= 10 & cdata_parallel$Distance > 0,5, cdata_parallel$bin)
cdata_parallel$bin <- ifelse(cdata_parallel$Distance <= 20 & cdata_parallel$Distance > 10,15, cdata_parallel$bin)
cdata_parallel$bin <- ifelse(cdata_parallel$Distance <= 30 & cdata_parallel$Distance > 20,25, cdata_parallel$bin)
cdata_parallel$bin <- ifelse(cdata_parallel$Distance <= 40 & cdata_parallel$Distance > 30,35, cdata_parallel$bin)
cdata_parallel$bin <- ifelse(cdata_parallel$Distance <= 50 & cdata_parallel$Distance > 40,45, cdata_parallel$bin)
cdata_parallel$bin <- ifelse(cdata_parallel$Distance <= 60 & cdata_parallel$Distance > 50,55, cdata_parallel$bin)
cdata_parallel$bin <- ifelse(cdata_parallel$Distance <= 70 & cdata_parallel$Distance > 60,65, cdata_parallel$bin)
cdata_parallel$bin <- ifelse(cdata_parallel$Distance <= 80 & cdata_parallel$Distance > 70,75, cdata_parallel$bin)
cdata_parallel$bin <- ifelse(cdata_parallel$Distance <= 90 & cdata_parallel$Distance > 80,85, cdata_parallel$bin)
cdata_parallel$bin <- ifelse(cdata_parallel$Distance <= 100 & cdata_parallel$Distance > 90,95, cdata_parallel$bin)
cdata_parallel$bin <- ifelse(cdata_parallel$Distance <= 110 & cdata_parallel$Distance > 100,105, cdata_parallel$bin)
cdata_parallel$bin <- ifelse(cdata_parallel$Distance <= 120 & cdata_parallel$Distance > 110,115, cdata_parallel$bin)
cdata_parallel$bin <- ifelse(cdata_parallel$Distance <= 130 & cdata_parallel$Distance > 120,125, cdata_parallel$bin)
cdata_parallel$bin <- ifelse(cdata_parallel$Distance <= 140 & cdata_parallel$Distance > 130,135, cdata_parallel$bin)
cdata_parallel$bin <- ifelse(cdata_parallel$Distance <= 150 & cdata_parallel$Distance > 140,145, cdata_parallel$bin)
cdata_parallel$bin <- ifelse(cdata_parallel$Distance <= 160 & cdata_parallel$Distance > 150,155, cdata_parallel$bin)


# Create the line shapefile
## To use `sfheaders` the data needs to be in long form
cdata_parallel <- as.data.table(cdata_parallel)

dt1 <- cdata_parallel[, .(id, lon = High.Severity.X, lat = High.Severity.Y)]
dt2 <- cdata_parallel[, .(id, lon = PIPO.X, lat = PIPO.Y)]

## Add on a 'sequence' variable so we know which one comes first
dt1[, seq := 1L ]
dt2[, seq := 2L ]

## put back together
dt <- rbindlist(list(dt1, dt2), use.names = TRUE)
setorder(dt, id, seq)

sf_lines <- sf_linestring(
  obj = dt
  , x = "lon"
  , y = "lat"
  , linestring_id = "id",
  list_columns = "geometry"
)

## Convert this datatable to a spatialdatafrme
sf_lines <- merge(sf_lines, cdata_parallel, by = "id")

### This converts the datatable to a spatial dataframe 
sf_lines <- as(sf_lines, "Spatial")

crs(sf_lines) <- "+proj=utm +zone=13 +datum=WGS84 +units=m +no_defs "

## Calculate wind influence
### Import fire footprints
fire_footprints <- readOGR("All_Fires_Shapefile.shp")
fire_footprints <- spTransform(fire_footprints, CRS("+proj=utm +zone=13 +datum=WGS84 +units=m +no_defs "))

### Find which lines occur in which fire footprints to calculate influence of wind 
sf_lines$Fire_footprint <- over(sf_lines, fire_footprints[,"Fire_Name"])$Fire_Name

### Calculate the direction of the transect relative to wind direction 
sf_lines$wind <- NA

### Name of Torres fire is listed as Q-Ball here 
sf_lines$wind <- ifelse(sf_lines$Fire_footprint == "DIVIDE" | sf_lines$Fire_footprint == "BONNER", cos((sf_lines$angle-100)*0.01745329), sf_lines$wind)
sf_lines$wind <- ifelse(sf_lines$Fire_footprint == "GLASS" | sf_lines$Fire_footprint == "SHELLY" | sf_lines$Fire_footprint == "Q-BALL",
                        cos((sf_lines$angle-15)*0.01745329), sf_lines$wind)

### Zero out the wind values that are below 0 (part of the variable value for the final model equation)
sf_lines$wind_0 <- ifelse(sf_lines$wind <= 0, 0, sf_lines$wind)

## Get aspect value for seed density calculations
sf_lines$aspect_value <- NA

sf_lines$aspect_value <- ifelse(sf_lines$Aspect > 315 | sf_lines$Aspect <= 45, 0.817343, sf_lines$aspect_value)
sf_lines$aspect_value <- ifelse(sf_lines$Aspect > 45 & sf_lines$Aspect <= 135, 0, sf_lines$aspect_value)
sf_lines$aspect_value <- ifelse(sf_lines$Aspect > 135 & sf_lines$Aspect <= 225, 0.126749, sf_lines$aspect_value)
sf_lines$aspect_value <- ifelse(sf_lines$Aspect > 225 & sf_lines$Aspect <= 315, 0.063284, sf_lines$aspect_value)

## Need to calculate the likelihood of presence/absence of regeneration
### If I use a threshold of 0.4 for reasons listed below, the distance cutoff will be a bin distance of 55, so anything beyond 55 m from the burned pixel will not have a high enough likelihood of causing regeneration to have a presence. 
### This is an interesting cut off because that 50 m distance from chambers et al. 2016 indicates a similar threshold for likelihood of good regeneration in ponderosa pine 
sf_lines$seedling_presence <- exp(0.69133 + (-0.26882* log(sf_lines$bin)))/(1+ exp(0.69133 + (-0.26882* log(sf_lines$bin))))

## Calculate seed density based on these values
# sf_lines$seedling_density <- exp(5.788377 + (-0.004818* sf_lines$bin)) ## If you just delete variables, but I think this is wrong
sf_lines$seedling_density <- exp(6.070404 + (-0.004791* sf_lines$bin))

## This would incorporate the hurdle, which I will need to test/assess. I didn't do it here cause I figured it's better to have the whole dataframe of most likely pixels, but I wanted to keep this explanation for using 0.4 here since its the clearest I've written it yet
### if the logistic model estimates a value > 0.4, then I set the value to 1, but 0 if not. Then multiply that value by the density value
#### I decided upon a value of 0.4 because that is the threshold to have favorable odds that regeneration would occur given the 40% chance of regeneration occurring within the 736 binds (288 bins had regeneration, so 40% frequency). I felt comfortable converting 40% frequency to a 40% chance because this value is very similar to the frequency found in Rother and Veblen 2016 and Haffey et al. 2018 for ponderosa pine within 150 m of intact forest.
# sf_lines$binomial_value <- ifelse(sf_lines$seedling_presence >= 0.4, 1, 0)
# 
# sf_lines$regen_density <- sf_lines$seedling_density * sf_lines$binomial_value

## Sum the values for each box
sf_lines_sf <- st_as_sf(sf_lines)

## I need to remove the pairs where slope is >30 because my empirical data doesn't have anything to support that
sf_lines_sf <- sf_lines_sf[sf_lines_sf$slope <= 30 & sf_lines_sf$slope >= -30,]

row.names(sf_lines_sf) <- NULL

# sf_lines_sf_final <- sf_lines_sf %>% 
#   group_by(High.Severity.Number) %>% 
#   summarise(total_seedling_density = max(seedling_density))


sf_lines_sf_2 <- data.frame(sf_lines_sf[,c("High.Severity.Number","seedling_presence")])[,-3]

sf_lines_sf_2 <- sf_lines_sf_2[order(sf_lines_sf_2$High.Severity.Number),]

rownames(sf_lines_sf_2) <- NULL

## Run this in parallel because the group_by took foreverrrrr
### This will also take forever, but less forever
HS_raster_polygon <- readOGR("HS_polygons_10m_12-7-21.shp")
HS_raster_polygon <- spTransform(HS_raster_polygon, CRS("+proj=utm +zone=13 +datum=WGS84 +units=m +no_defs "))

cl <- makeCluster(detectCores() - 15) 

registerDoParallel(cl)

### This takes about 4.5 hours 
system.time(total_seedling_density_2 <- foreach(i = 1:length(unique(sf_lines_sf_2$High.Severity.Number)), .combine = "rbind") %dopar% {
  cbind.data.frame("High.Severity.Number" = unique(sf_lines_sf_2$High.Severity.Number)[i],
                   "seedling_presence" = sort(sf_lines_sf_2[sf_lines_sf_2$High.Severity.Number %in% unique(sf_lines_sf_2$High.Severity.Number)[i],"seedling_presence"], decreasing =T)[1])
})

for_merging <-sf_lines_sf

for_merging$geometry <- NULL

sf_lines_sf_final <- merge(total_seedling_density_2, for_merging, all.x = TRUE, by = c("High.Severity.Number", "seedling_presence"))

sf_lines_sf_final <- unique(sf_lines_sf_final)

sf_lines_sf_final <- na.omit(sf_lines_sf_final)

sf_lines_sf_final <- sf_lines_sf_final[!duplicated(sf_lines_sf_final$High.Severity.Number),]

sf_lines_sf_final <- st_as_sf(sf_lines_sf_final, coords=c("High.Severity.X", "High.Severity.Y"))

sf_lines_sf_final <- as(sf_lines_sf_final, "Spatial")

crs(sf_lines_sf_final) <- "+proj=utm +zone=13 +datum=WGS84 +units=m +no_defs "

stopCluster(cl)

writeOGR(sf_lines_sf_final, dsn = '.', layer = 'seedling_presence_density_dist_1-6-24', driver = "ESRI Shapefile")


