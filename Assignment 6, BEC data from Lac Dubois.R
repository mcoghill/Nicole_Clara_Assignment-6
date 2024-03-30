## Assignment 7, BEC data

# b.	Download and analyze all of the BEC data that falls within the Lac Du Bois 
# Grasslands Protected Area (you can find this layer in the “bcmaps” package)
# install package (if not done already)
install.packages("bcmaps")

#load libraries
library(bcmaps)  # to get the boundary/shape for LDB
library(mapview)
library(bcdata)
library(tidyverse)
library(terra)
library(ggplot2)

# To explore all available layers in the BCmaps package
View(available_layers())

# Get (and name) the bec layer
All_BEC_zones<-bec()

# !! to view all BC BEC zones, but loads a lot of data so slows down the computer!! 
# mapview(All_BEC_zones)

# Check CRS of the BEC zone layers, 3005 confirmed
crs(All_BEC_zones)

# Confirm URL for: BC Parks, Ecological Reserves, and Protected Areas
BCParks_ProtectedArea_search<-bcdc_search("BC Parks, Ecological Reserves, and Protected Areas") 
# To view:Peer through the returned list object, we notice that the "id" field 
# contains the unique ID required for downloading this dataset. 
View(BCParks_ProtectedArea_search)

# Load Lac Du Bois layer
LacDuBois <- bcdc_query_geodata("1130248f-f1a3-4956-8b2e-38d29d3e4af7", crs = 3005) |> 
  filter(PROTECTED_LANDS_NAME == "LAC DU BOIS GRASSLANDS PROTECTED AREA") |> 
  collect()      # collect function actually downloads it

# to view LacDuBois layer
mapview(LacDuBois)

# Create the LDB geometry (AOI), convert the sf (simple feature) object to an 
# sfc (simple feature collection) object
LDB_geometry_sfc <- st_geometry(LacDuBois)


# Perform the intersection (clip), want to intersect All BEC zones with the LDB
# geometry (area of interest)
BEC_clip <- st_intersection(All_BEC_zones, LDB_geometry_sfc)

# view features
BEC_clip

# to view LDB
plot(BEC_clip)

#########
## 4/4 ##
#########

### Q1- Calculate the total area of each of the resulting features in hectares
# shows hectares in new (last column)
BEC_clip_hectares <- BEC_clip %>% mutate(Feature_area_hectares = FEATURE_AREA_SQM/10000)

#########
## 1/2 ##
#########

## use the st_area() function to calculate the area of each of the features - the
## FEATURE_AREA_SQM column contains areas of the entire polygon extending beyond
## what was clipped.

### Q2- Create a bar plot where the “MAP_LABEL” column is along the X-axis, and the 
# area is along the Y-axis. Display each bar using different colors.

#Bar plot
ggplot(BEC_clip_hectares, aes(x= MAP_LABEL, y= Feature_area_hectares)) + 
  geom_col()+
  labs(title = "Bar plot of Site variance (Map labels)", 
       x= "Site variance", y = "Area (hectares)")

#########
## 4/5 ##
#########

## Each of the bars should be a different color.

### Q3 Extract the mean elevation of each of the features 
# (you will need to pull in the DEM from the “cded_terra” function)
# Pulling the elavation data within area of interest
Site_dem <- cded_terra(BEC_clip_hectares) # cded_terra from the BCmap package
Site_dem
#### EPSG is different: 4269 (NAD83)

# Perform initial projection to BC Albers
# project function ask for dem as well as the projection
Site_dem_albers <- project(Site_dem, "epsg:3005")
res(Site_dem_albers)  # resolution is 17.06, round up to 18m

# Decrease resolution to 15m by resampling. Must create a 15m grid to resample to
# ext is the extent, resample at resolution of 18m
resamp_grid <- rast(ext(Site_dem_albers), res = 18, crs = "epsg:3005")
# resample
Site_dem_albers <- resample(Site_dem_albers, resamp_grid)
plot(Site_dem_albers) # is faster then mapview because we are starting to have a lot of data

# This is a box, so not the same shape as area of interest, so have to mask 
Site_dem_mask <- mask(Site_dem_albers, vect(BEC_clip_hectares))

# To write this TIF file to disk; it will save in the same folder as this script
writeRaster(Site_dem_mask, "LDB_DEM.tif", overwrite = TRUE)

# Plot the DEM and color the NA areas in grey to show that the masking worked:
plot(Site_dem_mask, colNA = "grey")
# seems to have worked!

# extracting the elevation for the aoi (elevation was a raster layer so now
# extracting it as a dataframe)
BEC_zones_elevation <- terra::extract(Site_dem_mask, BEC_clip_hectares,
  fun = mean, na.rm = TRUE)

# see the mean elevation values
BEC_zones_elevation

# To add a  another column to the data frame to include the mean elevation
BEC_zones_LDB<-BEC_clip_hectares %>% 
  mutate("Mean elevation"= BEC_zones_elevation)

#########
## 4/4 ##
#########

## There is a more concise way of doing this, but this works as well.


### Q4 Create a mapview of the BEC vector layer, coloring the polygons by their subzone label.


# Color by column names with zcol function


mapview(BEC_clip_hectares, zcol = "SUBZONE")

#########
## 2/2 ##
#########


## Part 2 total:

###########
## 14/17 ##
###########
