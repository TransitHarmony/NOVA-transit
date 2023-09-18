# How the pandemic changed transit service in Northern Virginia -----

## Set up workspace

library(tidyverse)
library(tidytransit)
library(here)
library(censusapi)
library(tidycensus)
library(sf)
library(mapview)
library(units) 
library(toolingtransit)

rm(list = ls()) ### clear environment
#Load variables that I've already run and saved in the toolingtrasit/data folder
#Census Data 
CensusData <- st_read("data/CensusData.shp")
# Employment data interpolated to Census block groups 
JobsbyBG <- st_read("data/JobsbyBG.shp")
# 2019 stop data for all agencies (including WMATA, excluding LCT)
NovaStops_2019 <- st_read("data/NovaStops_2019.shp")
# 2022stop data for all agencies (including WMATA, excluding LCT)
NovaStops_2022 <- st_read("data/NovaStops_2022.shp")

# Census Data -------------------------------------------------------------
# Start with setting up census geographies and gathering census data
# I've created a function in tooling transit that gathers the census data 
# and creates the variables 


NOVA_Counties <-  c("Arlington", 
                    "Fairfax County", 
                    "Fairfax city", 
                    "Loudoun", 
                    "Alexandria City",
                    "Falls Church City")

#create census data for the whole region 
CensusData <- countycensus(NOVA_Counties)



# Service data

## GTFS File paths
#make sure to run this so you can access the right folder for GTFS
GTFS_path <- file.path ("Z:", ### File path where data is located
                        "NVTC General",
                        "Projects and Programs",
                        "Transit Resource Center (TRC)",
                        "Data",
                        "GTFS")


### ADD EMPLOYMENT DATA
Jobs <- st_read("Z:/NVTC General/Projects and Programs/Transit Resource Center (TRC)/Data/MWCOG Forecasts/DRAFT_COG_Cooperative_Forecast_9.2/DRAFT_COG_Cooperative_Forecast_9.2.shp") %>% 
  st_transform(crs = 4326) %>% st_intersection(Nova) %>% select(EMP2015, EMP2020, EMP2025)

#interpolate the employement data to census block groups 
JobsbyBG <- st_interpolate_aw(Jobs, CensusData, extensive = T)
st_write(JobsbyBG, "data/JobsbyBG.shp")
JobsbyBG <- st_read("data/JobsbyBG.shp")

#explore sum of employment by county for 2020
JobsCnty <- Jobs %>% filter(STATE_FIPS == 51) %>% group_by(NAME) %>% summarize(emp20sum = sum(EMP2020))


### 2019 All Bus Stops in Nova
# stoploc is a function in tooling transit, also pasted here for convenience 
# this function pulls the GTFS data and crates a stop shape file for each agency
# this only works for non-WMATA stops. WE will do WMATA separately
stoploc <- function(gtfszip, agency) {
  ### All gtfs data is located in this file path
  GTFS_path <- file.path ("Z:",
                          "NVTC General",
                          "Projects and Programs",
                          "Transit Resource Center (TRC)",
                          "Data",
                          "GTFS")
  
  #establish gtfs data
  CountyPath <- file.path(GTFS_path, gtfszip)
  CountyGTFS <- read_gtfs(CountyPath)
  #create spatial file for stops and select only necessary variables
  CountyStopLoc <- stops_as_sf(CountyGTFS$stops) %>%
    dplyr::select(stop_id) %>%
    mutate("Agency" = agency)
  return(CountyStopLoc)
}


## 2019 all stops 
NovaStops_2019 <- rbind(
  stoploc("2019-10_Arlington.zip", "ART"),
  stoploc("2020-12_CUE.zip", "CUE"),
  stoploc("2019-10_DASH.zip", "DASH"),
  stoploc("2019-09_Fairfax_Connector.zip", "FFX")
  # stoploc("2019-12_Loudoun.zip", "LCT") #remove loudoun bc the 2019 gtfs data is not complete 
)

#Create a simple shapefile of Nova to clip the WMATA stops to 
Nova <- CensusData %>% select(geometry) %>% st_union
Nova <- st_read("data/Nova.shp")

### WMATA Stops (Virginia Only) 2019
WMATA2019GTFS <- file.path(GTFS_path, "2019-10_WMATA.zip") %>% read_gtfs(.)

#need to remove metrorail stations from 2019 wmata gtfs data 
#need to join stop and route data ,since route-id shows if its a rail line
#then intersect with just nova, then finally, pull the stop_ids associated the rail lines
metrostops19 <- left_join(WMATA2019GTFS$stop_times, WMATA2019GTFS$stops) %>% 
  select(trip_id, stop_id, stop_lat, stop_lon) %>% 
  full_join(., WMATA2019GTFS$trips) %>% 
  select(route_id, stop_id, stop_lat, stop_lon) %>%
  stops_as_sf(.) %>% st_intersection(., Nova)
metrostop_ids <- metrostops19 %>% filter(route_id %in% c("BLUE", "ORANGE", "SILVER", "YELLOW")) %>% unique() %>% pull(stop_id)


#### Make a spatial file for stops, only use va stops, add column for agency name
WMATA2019StopLoc <- stops_as_sf(WMATA2019GTFS$stops) %>% 
  st_intersection(., Nova) %>% 
  #remove metorail stations 
  filter(!stop_id %in% metrostop_ids) %>% 
  select(stop_id) %>%
  mutate("Agency" = "WMATA")


#bind all stops into one shapefile
NovaStops_2019 <- rbind(WMATA2019StopLoc,
                        NovaStops_2019)

NovaStops_2019 <- unite(NovaStops_2019, stop_id, Agency, col = "newstop_id")
st_write(NovaStops_2019, "data/NovaStops_2019.shp", append = F)



#2022 all stops 

NovaStops_2022 <- rbind(
  stoploc("2022-04_Arlington.zip", "ART"),
  stoploc("2022-03_CUE.zip", "CUE"),
  stoploc("2022-04_DASH.zip", "DASH"),
  stoploc("2022-03_Fairfax_Connector.zip", "FFX")
  #stoploc("2022-03_Loudoun.zip", "LCT")
)
#do not need to remove metrorail station stop ids in 2022 data bc they are not included in the GTFS data
WMATA2022GTFS <- file.path(GTFS_path, "2022-04_WMATA.zip") %>% read_gtfs(.)
#### Make a spatial file for stops, only use va stops, add column for agency name
WMATA2022StopLoc <- stops_as_sf(WMATA2022GTFS$stops) %>%
  st_intersection(., Nova) %>% 
  select(stop_id) %>%
  mutate("Agency" = "WMATA")


#join the all stop data 
NovaStops_2022 <- rbind(WMATA2022StopLoc,
                        NovaStops_2022)

#create new stop id
NovaStops_2022 <- unite(NovaStops_2022, stop_id, Agency, col = "newstop_id")
st_write(NovaStops_2022, "data/NovaStops_2022.shp", append = F)


### Interpolate census data to buffered stop data 

#function to find access to transit for each county
access2transit <- function(county, allstops) {
  #get census data
  Census <- countycensus(county) %>% st_transform(crs = 4326)
  #create county shape
  CountyShape <- st_union(Census) %>% st_transform(crs = 4326)
  #pull just the stops within a 1/4 mile radius of county
  BuffJoin <- st_intersection(allstops,
                              st_buffer(CountyShape, dist = 400)) %>%
    #buffer those stops and join them with union
    st_buffer(dist = 400) %>%
    st_union() %>% st_make_valid()
  #join employment data with demographic data # THIS IS ON PAUSE, WILL COME BACK TO THIS
  #CensusJobs <- st_join(Census, JobsbyBG)
  #interpolate census data to these stops
  TransitCensus <- st_interpolate_aw(
    Census,
    BuffJoin,
    extensive = T
  )
  return(TransitCensus)
}



##Arlington 
ARL2019Transit_Census <- access2transit("Arlington", NovaStops_2019)
ARL2022Transit_Census <- access2transit("Arlington", NovaStops_2022)
#save without geometry
st_write(ARL2019Transit_Census %>% st_drop_geometry(), "data/transitcensus.csv", append = F)
st_write(ARL2022Transit_Census %>% st_drop_geometry(), "data/transitcensus.csv", append = T)

##Alexandria 
ALX2019Transit_Census <- access2transit("Alexandria City", NovaStops_2019)
ALX2022Transit_Census <- access2transit("Alexandria City", NovaStops_2022)
#save w/o geometry
st_write(ALX2019Transit_Census %>% st_drop_geometry(), "data/transitcensus.csv", append = T)
st_write(ALX2022Transit_Census %>% st_drop_geometry(), "data/transitcensus.csv", append = T)

#City of Falls Church 
FallsChurch2019Transit_Census <- access2transit("Falls Church City", NovaStops_2019)
FallsChurch2022Transit_Census <- access2transit("Falls Church City", NovaStops_2022)
#save w/o geometry
st_write(FallsChurch2019Transit_Census %>% st_drop_geometry(), "data/transitcensus.csv", append = T)
st_write(FallsChurch2022Transit_Census %>% st_drop_geometry(), "data/transitcensus.csv", append = T)

## City of Fairfax
CityFFX2019Transit_Census <- access2transit("Fairfax city", NovaStops_2019)
CityFFX2022Transit_Census <- access2transit("Fairfax city", NovaStops_2022)
# save w/o geometry
st_write(CityFFX2019Transit_Census %>% st_drop_geometry(), "data/transitcensus.csv", append = T)
st_write(CityFFX2022Transit_Census %>% st_drop_geometry(), "data/transitcensus.csv", append = T)

## Fairfax County
FFX2019Transit_Census <- access2transit("Fairfax County", NovaStops_2019)
FFX2022Transit_Census <- access2transit("Fairfax County", NovaStops_2022)
#save w/o geometry
st_write(FFX2019Transit_Census %>% st_drop_geometry(), "data/transitcensus.csv", append = T)
st_write(FFX2022Transit_Census %>% st_drop_geometry(), "data/transitcensus.csv", append = T)


#add loudoun for 2022 only 
Lou_Stops_2022 <- stoploc("2022-03_Loudoun.zip", "LCT") %>% unite(stop_id, Agency, col = "newstop_id")
NovaStops_Lou_2022 <- rbind(
  NovaStops_2022, 
  Lou_Stops_2022
)
Lou2022Transit_Census <- access2transit("Loudoun", NovaStops_Lou_2022)
st_write(Lou2022Transit_Census %>% st_drop_geometry(), "data/transitcensus.csv", append = T)




