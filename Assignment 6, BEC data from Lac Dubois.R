## Assignment 7, BEC data

# b.	Download and analyze all of the BEC data that falls within the Lac Du Bois 
# Grasslands Protected Area (you can find this layer in the “bcmaps” package)
# install package (if not done already)
install.packages("bcmaps")

#load libraries
library(bcmaps)
library(mapview)
library(bcdata)

# To explore all available layers
View(available_layers())

# Get (and name) the bec layer
All_BEC_zones<-bec()

# !! to view all BC BEC zones, but loads a lot of data so slows down the computer!! 
mapview(All_BEC_zones)

# Confirm URL for: BC Parks, Ecological Reserves, and Protected Areas
BCParks_ProtectedArea_search<-bcdc_search("BC Parks, Ecological Reserves, and Protected Areas") 
# View:Peer through the returned list object, we notice that the "id" field 
# contains the unique ID required for downloading this dataset. 
View(BCParks_ProtectedArea_search)

# Load Lac Du Bois layer
LacDuBois <- bcdc_query_geodata("1130248f-f1a3-4956-8b2e-38d29d3e4af7", crs = 3005) |> 
  filter(PROTECTED_LANDS_NAME == "LAC DU BOIS GRASSLANDS PROTECTED AREA") |> 
  collect()

# to view LacDuBois layer
mapview(LacDuBois)



# bbox or these codes?




# Use the BC Data Catalogue to get the Sechelt LU TEM (terrestrial ecosystem mapping)
# project boundary:
tem <- bcdc_search("tem project boundaries", res_format = "wms")
tem_id <- grep("-tem-project-boundaries", tem)   
# grep to match patern
site_aoi <- bcdc_query_geodata(tem[[tem_id]]$id, crs = 3005) %>% 
  filter(PROJECT_NAME == "Sechelt LU TEM") %>% 
  collect()

# Repeat the collection of greenspaces with our new AOI:
greenspaces_aoi <- bcdc_query_geodata(gs_search[[1]]$id, crs = 3005) %>% 
  filter(INTERSECTS(site_aoi)) %>%   # filter out all greenspaces that intersect with site of interest (aoi)
  collect()


# Calculate the total area of each of the resulting features in hectares.
# Create a bar plot where the “MAP_LABEL” column is along the X-axis, and the area is along the Y-axis. Display each bar using different colors.
# Extract the mean elevation of each of the features (you will need to pull in the DEM from the “cded_terra” function)
# Create a mapview of the BEC vector layer, coloring the polygons by their subzone label.

