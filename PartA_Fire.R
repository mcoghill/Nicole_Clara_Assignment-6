install.packages("tidyverse")
install.packages("readxl")
install.packages("sf")
install.packages("terra")
install.packages("mapview")
install.packages("bcdata")
install.packages("bcmaps")

library(tidyverse)
library(readxl)
library(sf)
library(terra)
library(mapview)
library(bcdata)
library(bcmaps)


ldb <- bcdc_query_geodata("1130248f-f1a3-4956-8b2e-38d29d3e4af7", crs = 3005) |> 
  filter(PROTECTED_LANDS_NAME == "LAC DU BOIS GRASSLANDS PROTECTED AREA") |> 
  collect()

fire_search <- bcdc_search("fire incident historical")
View(fire_search)

bcdc_describe_feature("https://catalogue.data.gov.bc.ca/dataset/e2dadc60-292f-4d98-b42b-56ca9e4fe694")

####option 1
fire_incidents <- bcdc_query_geodata("e2dadc60-292f-4d98-b42b-56ca9e4fe694")

fire_incidents_ldb <- fire_incidents %>%
  filter(INTERSECTS(ldb)) %>%
  collect()

mapview(fire_incidents_ldb)

##or option 2 - both have 279 observations/points with 21 variables

fire_search <- bcdc_search("fire incident locations")
View(fire_search)

fires_aoi <- bcdc_query_geodata(fire_search[[1]]$id, crs = 3005) %>% 
  filter(INTERSECTS(ldb)) %>% 
  collect()

mapview(fires_aoi)

#########
## 4/4 ##
#########

fires_summary <- fire_incidents_ldb %>%
  group_by(FIRE_YEAR) %>%
  summarize(fires_count = n())

#########
## 2/2 ##
#########

cause_summary <- fire_incidents_ldb %>%
  group_by(FIRE_CAUSE) %>%
  summarize(fires_count = n())

#########
## 2/2 ##
#########

##FIRE_CAUSE   fires_count
##Lightning    63
##Person       118
##Unknown      98

mapview(fire_incidents_ldb, zcol = "FIRE_CAUSE")

#########
## 2/2 ##
#########

mean_fires_per_year <- fire_incidents_ldb %>%
  group_by(FIRE_YEAR) %>%
  summarize(mean_fires_per_year = n())

mean_fires_by_cause <- fire_incidents_ldb %>%
  group_by(FIRE_CAUSE, FIRE_YEAR) %>%
  summarize(fires_per_year = n()) # %>%
  # group_by(FIRE_CAUSE) %>%
  # summarize(mean_fires_per_year = mean(fires_per_year))

ggplot(mean_fires_by_cause, aes(x = FIRE_CAUSE, y = fires_per_year, fill = FIRE_CAUSE)) +
  geom_boxplot() +
  labs(x = "Fire Cause", y = "Mean Fires per Year") +
  theme_minimal()

##a bar chart may display the data better:
# ggplot(cause_summary, aes(x = FIRE_CAUSE, y = fires_count, fill = FIRE_CAUSE)) +
#   geom_bar(stat = "identity") +
#   labs(x = "Fire Cause", y = "Fires Count") +
#   scale_fill_discrete(name = "Fire Cause")

#########
## 4/5 ##
#########

## You were so close with the summary - mean_fires_by_cause just needed the 
## first three lines, the other two were not needed. Great job!


## Part 1 total:

###########
## 14/15 ##
###########
