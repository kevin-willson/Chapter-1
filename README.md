# Code for "Abiotic factors modify ponderosa pine regeneration outcomes after high-severity fire"

Publisher link: https://link.springer.com/article/10.1007/s10021-024-00911-2

Read-only link: https://rdcu.be/dHXJb

Data associated with the project is described below and can be downloaded from Dryad: Willson, Kevin; Hurteau, Matthew (2024). Abiotic factors modify ponderosa pine regeneration outcomes after high-severity fire [Dataset]. Dryad. https://doi.org/10.5061/dryad.2547d7wxh

Created by Kevin G. Willson

Code:

1 - Model Selection.Rmd = Code for model parameterization and selection

2 - Model Extrapolation.R = Code for extrapolating final model estimates across 257 1-ha patches

3 - Distance only model parameterization and extrapolation.R = Code for parameterizing and extrapolating distance-only model estimates across 257 1-ha patches

4a - Figures Script.R = Code to create figures 2 and 3 in the manuscript

4b - Tables and misc results section stats code.R = Code to gather data for all tables and miscellaneous statistics referenced in the results section 

Data:

final_model_df.csv - This dataframe contains regeneration density data calculated by binning transect data into 10x10 m bins. This dataframe was used to parameterize candidate logistic and general linear models for model selection. Columns are as follows: "Fire" = Fire footprint where bin occurred, "Patch" = the patch within the fire footprint that the bin occurred, "Transect" = the transect number within the patch/fire footprint that the bin occurred in, "Distance" = the distance of the bin from the start of the transect at the edge of intact forest (in meters), "n" = number of regenerated ponderosa pine individuals occurred within the bin, "Seedling_density" = the density of regeneration within each bin (= n*100), "Combined_slope" = hillslope paired with slope position of the transect relative to the intact forest with negative values indictating transects going downhill from intact forest and positive values indicating transects going uphill of intact forest, "Aspect" = aspect of the transect associated with that bin, "Slope_Angle" = hillslope of the transect associated with that bin, "Tree_Height" = Average height of 2-3 measured trees within the nested plot next to the transect of the bin (meters), "Prevailing_Wind" = a binary yes/no of whether the transect was downwind (Yes) or upwind (No) of intact forest, "Cont_Prevailing_Wind" = continuous variable of the transect direction from intact forest relative to the prevailing wind with areas upwind of intact forest marked as 0 because of improved model performance, "NestedDensity" = the density of trees in the nested plot paired with the transect (trees/ha), "Ponderosa_overstory_density" = the density of ponderosa pine trees larger than 25 cm dbh in the nested plot paired with the transect (trees/ha), "Elevation" = elevation of plot location (m). 

patch_characteristics_df.csv - This dataframe contains the size and varying shape metrics of all 1,597 high-severity patches that occurred within the five fire footprints (257 of which were > 1 ha). This dataframe was used to analyze the effects of patch characteristics on core area and regeneration patterns. Extraction was conducted in R using the landscapemetrics package. Columns are as follows: "Id" = patch id to match with other dataframes, "Shape_Leng" = perimeter distance of the patch (meters), "Shape_Area" = the area of the patch (meters^2), "frac" = a standardized measure of patch complexity ranging from 1-2, "shape" = the ratio between the actual perimeter of the patch and the hypothetical minimum perimeter of the patch that is >= 1, "circle" = the ratio between the patch area and the smallest circumscribing circle of the patch < 1, "para" = the unstandardized ratio of perimeter distance to patch area, "cai" = core area metric that is the percentage of a patch that is core area, 
"core" = total core area in the patch (ha), SDC = stand-replacing decay coefficient that measures the decay of the proportion of a patch within a certain distance to the patch edge as quantified by Collins et al. (2017)

seedling_density_df_2-24-22.csv - This dataframe contains the estimated regeneration density from the abiotic factors model and various abiotic parameters of all 366,630 pixels that occurred within the 257 1-ha patches. This dataframe was used to quantify the effect of patch characteristic and abiotic factors on expected dispersal and regeneration patterns. Columns are as follows: "Hgh_S_N" = the number associated with each pixel, "sdlng_d" = regeneration density of the pixel (individuals/ha), "id" = a number designated by arcmap for each pixel, "PIPO_Nm" = the ponderosa pine forest pixel number that was used to quantify distance, slope, and wind values for the burned pixel, "Distanc" = distance from the pixel to the ponderosa pine forest pixel (meters), "HS_DEM_" = the elevation of the burned pixel taken from the 10m DEM raster (meters), "PIPO_DE" = the elevation of the associated ponderosa pine pixel taken from the 10m DEM raster (meters), "Aspect" = the aspect of the burned pixel (degrees), "angle" = the direction of the burned pixel from the associated ponderosa pine forest pixel, "slope" = slope from ponderosa pine forest pixel to burned pixel calculated using the change in DEM elevation over the distance between the two, "bin" = the binned value of the "Distanc" column value (i.e. if the "Distanc" column value was 78.60, then it was binned into the 70-80m bin which was given the half point value of 75), "Fr_ftpr" = the fire footprint where the pixel occurred, "wind" = the prevailing wind value calculated based on the angle between the burned pixel and ponderosa pine forest pixel relative to the prevailing wind direction, "wind_0" = the "wind" value with negative values reset to 0 because of improved model performance, "patch_no" = the patch number that the pixel occurred in, "patch_size" = the size of the patch within which the pixel occurred (square meters), "patch_length" = the perimeter distance of the patch within wich the pixel occurred. 

seedling_density_fullmodel_distancemodel_1-8-24.csv - This dataframe contains the estimated regeneration density from the distance-only and abiotic factors models and various abiotic parameters of all 366,630 pixels that occurred within the 257 1-ha patches. This dataframe was used to construct Figure 3, which quantified how the effect of abiotic factors changed as distance increased. Columns are as follows: "H_S_N" = the number associated with each pixel, "seedling_presence_windslope" = the likelihood of regeneration present calculated using the logistic model from the abiotic factors model, "id" = a number designated by arcmap for each pixel, "PIPO_N" = the ponderosa pine forest pixel number that was used to quantify distance, slope, and wind values for the burned pixel, "Dstnc" = distance from the pixel to the ponderosa pine forest pixel (meters), "HS_DE" = the elevation of the burned pixel taken from the 10m DEM raster (meters), "PIPO_D" = the elevation of the associated ponderosa pine pixel taken from the 10m DEM raster (meters), "Aspct" = the aspect of the burned pixel (degrees), "angle" = the direction of the burned pixel from the associated ponderosa pine forest pixel, "slope" = slope from ponderosa pine forest pixel to burned pixel calculated using the change in DEM elevation over the distance between the two, "bin" = the binned value of the "Dstnc" column value (i.e. if the "Dstnc" column value was 78.60, then it was binned into the 70-80m bin which was given the centroid point value of 75), "Fr_ft" = the fire footprint where the pixel occurred, "wind" = the prevailing wind value calculated based on the angle between the burned pixel and ponderosa pine forest pixel relative to the prevailing wind direction, "wnd_0" = the "wind" value with negative values reset to 0 because of improved model performance, "seedling_density_windslope" = regeneration density of the pixel from the ABIOTIC FACTORS model (individuals/ha), "seedling_presence_distance" = the likelihood of regeneration present calculated using the logistic model from the distance-only model, "seedling_density_distance" = regeneration density of the pixel from the DISTANCE-ONLY model (individuals/ha),       

test_lines_df_12-24-21.csv - This dataframe contains all burn pixel-ponderosa pine forest pixel combinations that were within 160 m of one another. This dataframe was created as part of the 2 - Model Extrapolation script and was used in the 4 - Distance only model parameterization and extrapolation script to reduce time spent finding all ponderosa pine forest pixels within 160 m of each burned pixel (since it was the same baseline). Columns are as follows: "High Severity Number" = the unique number associated with each pixel, "PIPO Number" = the ponderosa pine forest pixel number associated with the burned pixel, "High Severity X" = the UTM X-coordinate for the burned pixel, "High Severity Y" = the UTM Y-coordinate for the burned pixel, "PIPO X" = the UTM X-coordinate for the ponderosa pine forest pixel, "PIPO Y" = the UTM Y-coordinate for the ponderosa pine forest pixel, "Distance" = the distance between the burned pixel and ponderosa pine forest pixel (meters), "HS DEM value" = the elevation of the burned pixel taken from the 10m DEM raster (meters), "PIPO DEM value" = the elevation of the associated ponderosa pine pixel taken from the 10m DEM raster (meters), Aspect = the aspect of the burned pixel (degrees)

Plot_data.xlsx - This dataframe contains the plot and transect-level data used to attached plot/transect data to the binned regeneration density data. Columns are as follows: "Fire" = Fire footprint where the transect occurred, "Patch" = the patch within the fire footprint that the transect occurred, "Transect" = the transect number within the patch/fire footprint, "NestedDensity" = the density of trees in the nested plot paired with the transect (trees/ha), "OverstoryPIPODensity_25" = the density of ponderosa pine trees larger than 25 cm dbh in the nested plot paired with the transect (trees/ha), "AvgHeight" = Average height of 2-3 measured trees within the nested plot next to the transect (meters), "Elevation" = elevation at the start of the transect (meters), "Slope_position" = the slope position of the transect relative to intact forest with "Below" indicating transects running downhill of intact forest and "Above" indicating transects running uphill of intact forest, "Slope" = hillslope combined with slope position of the transect relative to the intact forest with negative values indictating transects going downhill from intact forest and positive values indicating transects going uphill of intact forest, "CardDirec" = average aspect of the transect grouped by the four cardinal directions, "absSlope" = hillslope of the transect, "Direction" = a binary yes/no of whether the transect was downwind (Yes) or upwind (No) of intact forest, "Cont_Wind_direc" = continuous variable of the transect direction from intact forest relative to the prevailing wind.

Transect_Data_5-25-21.xlsx - This dataframe contains the locations of all measured regeneration that occurred within transects. This dataframe was used to calculate regeneration density that was the response variable of the GLMs. Columns are as follows: "Fire" = Fire footprint where the transect occurred, "Patch" = the patch within the fire footprint that the transect occurred, "Transect" = the transect number within the patch/fire footprint, "Regen_code" = the number given for each individual recorded in a transect with "T" in front indicating that the individual was cored, "Vertical_Distance" = the distance the individual occurred from the start of the transect, "Horizontal_Distance" = the distance the individual occurred from the transect centerline, "DBH" = diameter at breast height of the individual with no value indicating the individual was shorter than 1.37 m. 

Spatial Data:
All_Fires_aspect_UTM.tif - 10 meter aspect raster across all five fire footprints in UTM coordinates
All_Fires_DEM_UTM.tif - 10 meter elevation raster across all five fire footprints in UTM coordinates
All_Fires_Shapefile.shp - polygon shapefile of fire boundaries for all five fire footprints
GilaTEUI.shp - polygon shapefile of Terrestruak Ecological Unit Inventory vegetation types across the Gila national forest
high_severity_spots_shapefile_12-7-21.shp - polygon shapefile of all high-severity patches in the five fire footprints
high_severity_spots_shapefile_12-7-21_raster.tif - 10x10m raster of all high-severity pixels in the five fire footprints
HS_polygons_10m.shp - 10x10m polygon shapefile of all high-severity pixels in the five fire footprints
non_HS_PIPO_polygons_30m_12-21-21.shp - 30x30m polygon shapefile of intact forest pixels that contained ponderosa pine trees
pondo_firefootprint.shp - polygon shapefile of intact forest area that contained ponderosa pine trees
