# This code creates estimated regeneration density maps of high-severity patches in the five sampled fires that were
# used for analysis in this manuscript

## Import libraries and set working directory
library(raster)
library(rgdal)
library(gdalUtils)
library(foreign)
library(rgeos)
setwd("D:/Dryad_submission/Data/")

# Part 1 - Prepare/create all the spatial dataframes needed for this analysis
## Create 10 x 10 m raster pixels of the high severity (HS) shapefile
HS_polygon <- readOGR("high_severity_spots_shapefile_12-7-21.shp")

fire_footprints <- readOGR("All_Fires_Shapefile.shp")

### reproject the polygons into a useable format
HS_polygon <- spTransform(HS_polygon, CRS("+proj=utm +zone=13 +datum=WGS84 +units=m +no_defs "))
fire_footprints <- spTransform(fire_footprints, CRS("+proj=utm +zone=13 +datum=WGS84 +units=m +no_defs "))

start <- raster()

extent(start) <- extent(HS_polygon)

res(start) <- 10

HS_raster <- rasterize(HS_polygon, start)

rm(start)

### Change the pixel values to 1 to keep everything simple 
HS_raster <- reclassify(HS_raster, c(0, 1410, 1))

## Convert raster pixels into 10 x 10 m polygons
HS_raster_polygon <- rasterToPolygons(HS_raster)

rm(HS_raster)

### Save this polygon shapefile
writeOGR(HS_raster_polygon, dsn = '.', layer = 'HS_polygons_10m_12-21-21', driver = "ESRI Shapefile", overwrite_layer = TRUE)

## Create 30x30 m polygon shapefile of just the PIPO pixels from Gila TEUI data 
veg <- readOGR("GilaTEUI.shp")
veg <- spTransform(veg, CRS("+proj=utm +zone=13 +datum=WGS84 +units=m +no_defs "))

### subset the veg data that falls within the fire footprints 
veg_firefootprints <- veg[fire_footprints,]

rm(veg)

### have to separately import the attributes table and combine it with the veg cover
pondo <- veg_firefootprints[veg_firefootprints@data$MAP_UNIT_S == 005 | veg_firefootprints@data$MAP_UNIT_S == 007 | veg_firefootprints@data$MAP_UNIT_S == 012 | veg_firefootprints@data$MAP_UNIT_S == 023 |
               veg_firefootprints@data$MAP_UNIT_S == 024 |veg_firefootprints@data$MAP_UNIT_S == 028 |veg_firefootprints@data$MAP_UNIT_S == 036 |veg_firefootprints@data$MAP_UNIT_S == 063 |
               veg_firefootprints@data$MAP_UNIT_S == 065 |veg_firefootprints@data$MAP_UNIT_S == 079 |veg_firefootprints@data$MAP_UNIT_S == 087 |veg_firefootprints@data$MAP_UNIT_S == 102 |
               veg_firefootprints@data$MAP_UNIT_S == 105 |veg_firefootprints@data$MAP_UNIT_S == 108 |veg_firefootprints@data$MAP_UNIT_S == 112 |veg_firefootprints@data$MAP_UNIT_S == 113 |
               veg_firefootprints@data$MAP_UNIT_S == 117 |veg_firefootprints@data$MAP_UNIT_S == 131 |veg_firefootprints@data$MAP_UNIT_S == 151 |veg_firefootprints@data$MAP_UNIT_S == 174 |
               veg_firefootprints@data$MAP_UNIT_S == 186 |veg_firefootprints@data$MAP_UNIT_S == 188 |veg_firefootprints@data$MAP_UNIT_S == 189 |veg_firefootprints@data$MAP_UNIT_S == 274 |
               veg_firefootprints@data$MAP_UNIT_S == 278 |veg_firefootprints@data$MAP_UNIT_S == 311 |veg_firefootprints@data$MAP_UNIT_S == 312 |veg_firefootprints@data$MAP_UNIT_S == 313 |
               veg_firefootprints@data$MAP_UNIT_S == 501 | veg_firefootprints@data$MAP_UNIT_S == 510 | veg_firefootprints@data$MAP_UNIT_S == 520 | veg_firefootprints@data$MAP_UNIT_S == 524 |
               veg_firefootprints@data$MAP_UNIT_S == 527 |veg_firefootprints@data$MAP_UNIT_S == 532 |veg_firefootprints@data$MAP_UNIT_S == 533 |veg_firefootprints@data$MAP_UNIT_S == 536 |
               veg_firefootprints@data$MAP_UNIT_S == 541 |veg_firefootprints@data$MAP_UNIT_S == 545 |veg_firefootprints@data$MAP_UNIT_S == 546 |veg_firefootprints@data$MAP_UNIT_S == 551 |
               veg_firefootprints@data$MAP_UNIT_S == 552 |veg_firefootprints@data$MAP_UNIT_S == 553 |veg_firefootprints@data$MAP_UNIT_S == 554 |veg_firefootprints@data$MAP_UNIT_S == 555 |
               veg_firefootprints@data$MAP_UNIT_S == 556 |veg_firefootprints@data$MAP_UNIT_S == 558 |veg_firefootprints@data$MAP_UNIT_S == 561 |veg_firefootprints@data$MAP_UNIT_S == 562 |
               veg_firefootprints@data$MAP_UNIT_S == 568 |veg_firefootprints@data$MAP_UNIT_S == 569 |veg_firefootprints@data$MAP_UNIT_S == 572 |veg_firefootprints@data$MAP_UNIT_S == 573 |
               veg_firefootprints@data$MAP_UNIT_S == 574 |veg_firefootprints@data$MAP_UNIT_S == 575 |veg_firefootprints@data$MAP_UNIT_S == 576 |veg_firefootprints@data$MAP_UNIT_S == 579 |
               veg_firefootprints@data$MAP_UNIT_S == 580 | veg_firefootprints@data$MAP_UNIT_S == 582 | veg_firefootprints@data$MAP_UNIT_S == 583 | veg_firefootprints@data$MAP_UNIT_S == 584 |
               veg_firefootprints@data$MAP_UNIT_S == 585 |veg_firefootprints@data$MAP_UNIT_S == 586 |veg_firefootprints@data$MAP_UNIT_S == 591 |veg_firefootprints@data$MAP_UNIT_S == 592 |
               veg_firefootprints@data$MAP_UNIT_S == 596 |veg_firefootprints@data$MAP_UNIT_S == 605 |veg_firefootprints@data$MAP_UNIT_S == 609 |veg_firefootprints@data$MAP_UNIT_S == 610 |
               veg_firefootprints@data$MAP_UNIT_S == 618 |veg_firefootprints@data$MAP_UNIT_S == 629 |veg_firefootprints@data$MAP_UNIT_S == 633 |veg_firefootprints@data$MAP_UNIT_S == 636 |
               veg_firefootprints@data$MAP_UNIT_S == 657 |veg_firefootprints@data$MAP_UNIT_S == 661 |veg_firefootprints@data$MAP_UNIT_S == 662 |veg_firefootprints@data$MAP_UNIT_S == 665 |
               veg_firefootprints@data$MAP_UNIT_S == 668 |veg_firefootprints@data$MAP_UNIT_S == 671 |veg_firefootprints@data$MAP_UNIT_S == 677 |veg_firefootprints@data$MAP_UNIT_S == 678 |
               veg_firefootprints@data$MAP_UNIT_S == 680 |veg_firefootprints@data$MAP_UNIT_S == 681 |veg_firefootprints@data$MAP_UNIT_S == 685 |veg_firefootprints@data$MAP_UNIT_S == 687 |
               veg_firefootprints@data$MAP_UNIT_S == 691 |veg_firefootprints@data$MAP_UNIT_S == 693 |veg_firefootprints@data$MAP_UNIT_S == 694 |veg_firefootprints@data$MAP_UNIT_S == 695 |
               veg_firefootprints@data$MAP_UNIT_S == 696 |veg_firefootprints@data$MAP_UNIT_S == 698,]

### Create a 30x30 raster to convert the shapefile into 30x30 meter polygons 

start4 <- raster()

extent(start4) <- extent(pondo)

res(start4) <- 30

PIPO_raster <- rasterize(pondo, start4)

rm(start4)

rm(pondo)

PIPO_raster <- reclassify(PIPO_raster, c(0, 1000000000000, 5))

## Convert raster pixels into 30 x 30 m polygons
PIPO_raster_polygon <- rasterToPolygons(PIPO_raster)

gc()

writeOGR(PIPO_raster_polygon, dsn = '.', layer = 'PIPO_polygons_30m', driver = "ESRI Shapefile", overwrite_layer = TRUE)

rm(PIPO_raster)
gc()

## Select the PIPO polygons that are not in high severity fire areas and create a new shapefile of just those
### Just to keep in mind, the non_hs_pipo shapefile is a SpatialPolygon and NOT a SpatialPolygonDataFrame
non_hs_pipo <- gDifference(PIPO_raster_polygon, HS_raster_polygon)

rm(HS_raster_polygon)

#### to save a SpatialPolygon
# shapefile(non_hs_pipo, "test_30.shp")

### To convert into a SpatialPolygonDataFrame
non_hs_pipo_id <- sapply(slot(non_hs_pipo, "polygons"), function(x) slot(x, "ID"))

non_hs_pipo.df <- data.frame( ID=1:length(non_hs_pipo), row.names = non_hs_pipo_id)

non_hs_pipo <- SpatialPolygonsDataFrame(non_hs_pipo, non_hs_pipo.df)

#### I had set up check points to close R and start from other points to reduce memory useage, some lines are vestigial code 
#### from that 
# non_hs_pipo <- readOGR('non_HS_PIPO_polygons_30m_test.shp')

### This new polygon is seen as a multipart with no breaks, so had to get it back to the 30 meter grid
start_1 <- raster()

extent(start_1) <- extent(non_hs_pipo)

res(start_1) <- 30

non_hs_pipo_raster <- rasterize(non_hs_pipo, start_1)

### There was a strange problem with extra lines of pixels being created, so had to remove those through raster calculations and masks
non_hs_pipo_raster <- reclassify(non_hs_pipo_raster, c(0, 2, 5))

non_hs_pipo_raster[is.na(non_hs_pipo_raster[])] <- 0

extra_lines <- mask(non_hs_pipo_raster, HS_polygon)

rm(HS_polygon)

gc()

extra_lines_reclass <- reclassify(extra_lines, c(0, 5, 4))

extra_lines_reclass[is.na(extra_lines_reclass[])] <- 0

non_hs_pipo_nolines <- non_hs_pipo_raster - extra_lines_reclass

non_hs_pipo_nolines <- as.data.frame(non_hs_pipo_nolines, xy = TRUE, na.rm = T)

non_hs_pipo_nolines <- non_hs_pipo_nolines[non_hs_pipo_nolines$layer == 5 ,]

coordinates(non_hs_pipo_nolines) <- ~x+y

non_hs_pipo_nolines <- rasterFromXYZ(non_hs_pipo_nolines, res = c(30,30), crs = "+proj=utm +zone=13 +datum=WGS84 +units=m +no_defs ", digits = 5)

PIPO_firefootprint <- readOGR("pondo_firefootprint.shp")

non_hs_pipo_nolines <- mask(non_hs_pipo_nolines, PIPO_firefootprint)

rm(PIPO_firefootprint)

non_hs_pipo <- rasterToPolygons(non_hs_pipo_nolines)

## Save outputs as a check point 
writeOGR(non_hs_pipo, dsn = '.', layer = 'non_HS_PIPO_polygons_30m_12-21-21', driver = "ESRI Shapefile", overwrite_layer = TRUE)
gc()

# Part 2 - Determine all of the PIPO pixels that are within 160 m of each burned pixel. 

## Import libraries
library(tibble)

## These are the packages for running in parallel. The snow package also has to be installed (though not loaded)
library(doParallel)
library(foreach)

non_hs_pipo <- readOGR("non_HS_PIPO_polygons_30m_12-21-21.shp")
HS_raster_polygon <- readOGR("HS_polygons_10m.shp")

non_hs_pipo_points <- gCentroid(non_hs_pipo, byid = T)


## Written as a (parallel) loop that takes each burned pixel, makes a 160 meter buffer, extracts all PIPO polygons within that 
## range,  and gets distance and angle from the PIPO pixels
DEM <- raster("All_Fires_DEM_UTM.tif")

aspect <- raster("All_Fires_aspect_UTM.tif")

pipo_distance_angle_dem <- data.frame(matrix(ncol = 10, nrow = 0))

colnames(pipo_distance_angle_dem) <- c("High Severity Number", "PIPO Number", "High Severity X", "High Severity Y", "PIPO X", "PIPO Y", "Distance", "HS DEM value", "PIPO DEM value", "Aspect")

cl <- makeCluster(detectCores(logical = F) - 2) 

registerDoParallel(cl)

gc()

clusterExport(cl, ls())

### Needed to import libraries to each core
clusterEvalQ(cl, library(tibble))
clusterEvalQ(cl, library(rgeos))
clusterEvalQ(cl, library(rgdal))
clusterEvalQ(cl, library(raster))

### The next step, as is, took 40 hours to run in parallel with 18 cores
system.time(cdata_parallel <- foreach(i = 1:length(HS_raster_polygon), .combine=rbind) %dopar%{  
  polygon <- HS_raster_polygon[i,]
  polygon <- gCentroid(polygon)
  buffered_poly <- buffer(polygon, width = 165, dissolve = T)
  pipo_in_buffer <- non_hs_pipo_points[buffered_poly,] ## called subsetting, not clipping
  #pipo_in_buffer <- gIntersection(non_hs_pipo, buffered_poly, byid = T) => use this if I want to subset by polygons that fall within the circle
  #pipo_in_buffer <- non_hs_pipo[which(gContains(buffered_poly, non_hs_pipo, byid = T)),] => use this if I want to only subset polygons completely within the buffer
  
  polygon_extent <- extent(polygon)
  
  pipo_in_buffer_df <- as.data.frame(pipo_in_buffer, xy= T)
  pipo_in_buffer_df <- rownames_to_column(pipo_in_buffer_df)
  
  if(dim(pipo_in_buffer_df)[1] > 0){
    
    pipo_dems <- extract(DEM, pipo_in_buffer)
    
    HS_dem <- extract(DEM, polygon)
    
    HS_aspect <- extract(aspect, polygon)
    
    HS_dem_value <- mean(HS_dem[[1]])
    
    distances <- gDistance(polygon, pipo_in_buffer, byid = T) - 5 #made 5 meters shorter because I wanted to the edge of the pixel, not center (reasonable estimate)
    
    loop_df <- data.frame(matrix(ncol = 10, nrow = length(pipo_in_buffer_df$rowname)))
    
    colnames(loop_df) <- c("High Severity Number", "PIPO Number", "High Severity X", "High Severity Y", "PIPO X", "PIPO Y", "Distance", "HS DEM value", "PIPO DEM value", "Aspect")
    
    loop_df$`PIPO Number` <- pipo_in_buffer_df$rowname
    loop_df$`High Severity X` <- polygon_extent[1]
    loop_df$`High Severity Y` <- polygon_extent[3]
    loop_df$`PIPO X` <- pipo_in_buffer_df$x
    loop_df$`PIPO Y` <- pipo_in_buffer_df$y
    loop_df$Distance <- distances
    loop_df$`HS DEM value` <- HS_dem_value
    loop_df$`PIPO DEM value` <- pipo_dems
    loop_df$`High Severity Number` <- i
    loop_df$Aspect <- HS_aspect
  }
  else {
    loop_df <- data.frame(matrix(ncol = 9, nrow = 0))
  } 
  return(loop_df)
  
}

)

stopCluster(cl)

## Save outputs as a check point
write.csv(cdata_parallel, "test_lines_df_12-24-21.csv")

# Part three - This part converts the cdata_parallel dataframe into a line shapefile and determines regen density based on the
# topographic, wind, and distance relationship between the burned pixel and each PIPO pixel within 160 m of it 

library(sf)
library(sfheaders)
library(data.table)
library(dplyr)

cdata_parallel$id <- row.names(cdata_parallel) 

## Calculate the 
cdata_parallel$angle <- (atan2((cdata_parallel$PIPO.X-cdata_parallel$High.Severity.X),(cdata_parallel$PIPO.Y-cdata_parallel$High.Severity.Y))*180/3.14159265) +180

cdata_parallel$slope <- (atan((cdata_parallel$HS.DEM.value - cdata_parallel$PIPO.DEM.value)/cdata_parallel$Distance))*180/3.14159265

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

## Calculate prevailing wind influence
### Find which lines occur in which fire footprints to calculate influence of wind 
sf_lines$Fire_footprint <- over(sf_lines, fire_footprints[,"Fire_Name"])$Fire_Name

### Calculate the direction of the transect relative to wind direction 
sf_lines$wind <- NA

sf_lines$wind <- ifelse(sf_lines$Fire_footprint == "DIVIDE" | sf_lines$Fire_footprint == "BONNER", cos((sf_lines$angle-100)*0.01745329), sf_lines$wind)
sf_lines$wind <- ifelse(sf_lines$Fire_footprint == "GLASS" | sf_lines$Fire_footprint == "SHELLY" | sf_lines$Fire_footprint == "Q-BALL",
                        cos((sf_lines$angle-15)*0.01745329), sf_lines$wind)

### Zero out the wind values that are below 0 (part of the variable value for the final model)
sf_lines$wind_0 <- ifelse(sf_lines$wind <= 0, 0, sf_lines$wind)

## Get aspect value for seed density calculations
### Used coefficient values from model 5 because I didn't have continuous ponderosa pine overstory density
sf_lines$aspect_value <- NA

sf_lines$aspect_value <- ifelse(sf_lines$Aspect > 315 | sf_lines$Aspect <= 45, 0.8197, sf_lines$aspect_value)
sf_lines$aspect_value <- ifelse(sf_lines$Aspect > 45 & sf_lines$Aspect <= 135, 0, sf_lines$aspect_value)
sf_lines$aspect_value <- ifelse(sf_lines$Aspect > 135 & sf_lines$Aspect <= 225, -0.2383, sf_lines$aspect_value)
sf_lines$aspect_value <- ifelse(sf_lines$Aspect > 225 & sf_lines$Aspect <= 315, -0.2236, sf_lines$aspect_value)

## Calculate seed density based on these values
sf_lines$seedling_density <- exp(5.9241 + (-0.0190* sf_lines$slope) + (3.6670 * sf_lines$wind_0) + (-4.1574 * sf_lines$wind_0*sf_lines$wind_0) + 
                                   (-0.0051* sf_lines$bin) + sf_lines$aspect_value)

### Also calculate these one by one for each variable for future comparisons of each 
sf_lines$wind_value <- 3.6670 * sf_lines$wind_0
sf_lines$wind_value_sq <- -4.1574 * sf_lines$wind_0*sf_lines$wind_0
sf_lines$wind_total <- sf_lines$wind_value_sq + sf_lines$wind_value
sf_lines$slope_value <- -0.0190* sf_lines$slope
sf_lines$distance_value <- -0.0051* sf_lines$bin
sf_lines$intercept_value <- 5.9241 

## Sum the values for each box
sf_lines_sf <- st_as_sf(sf_lines)

sf_lines_sf_2 <- data.frame(sf_lines_sf[,c("High.Severity.Number","seedling_density")])[,-3]

sf_lines_sf_2 <- sf_lines_sf_2[order(sf_lines_sf_2$High.Severity.Number),]

rownames(sf_lines_sf_2) <- NULL

## Run this in parallel because the group_by took foreverrrrr
### This will also take forever, but less forever
HS_raster_polygon <- readOGR("HS_polygons_10m.shp")
HS_raster_polygon <- spTransform(HS_raster_polygon, CRS("+proj=utm +zone=13 +datum=WGS84 +units=m +no_defs "))

cl <- makeCluster(detectCores(logical = F) - 2) 

registerDoParallel(cl)



### This takes almost exactly 3 hours with 18 cores
system.time(total_seedling_density_2 <- foreach(i = 1:length(unique(sf_lines_sf_2$High.Severity.Number)), .combine = "rbind") %dopar% {
  cbind.data.frame("High.Severity.Number" = unique(sf_lines_sf_2$High.Severity.Number)[i],
                   "seedling_density" = sort(sf_lines_sf_2[sf_lines_sf_2$High.Severity.Number %in% unique(sf_lines_sf_2$High.Severity.Number)[i],"seedling_density"], decreasing =T)[1])
})

stopCluster(cl)

for_merging <-sf_lines_sf

for_merging$geometry <- NULL

sf_lines_sf_final <- merge(total_seedling_density_2, for_merging, all.x = TRUE, by = c("High.Severity.Number", "seedling_density"))

sf_lines_sf_final <- unique(sf_lines_sf_final)

sf_lines_sf_final <- st_as_sf(sf_lines_sf_final, coords=c("High.Severity.X", "High.Severity.Y"))

sf_lines_sf_final <- as(sf_lines_sf_final, "Spatial")

crs(sf_lines_sf_final) <- "+proj=utm +zone=13 +datum=WGS84 +units=m +no_defs "

writeOGR(sf_lines_sf_final, dsn = '.', layer = 'seedling_density_2-24-22', driver = "ESRI Shapefile")


