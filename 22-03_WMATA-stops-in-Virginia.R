library(tidyverse)
library(tidytransit)
library(here)
library(censusapi)
library(tidycensus)
library(sp)
library(sf)
library(tmap)
library(mapview)
library(units) 

rm(list = ls()) ### clear environment

# Census Data -------------------------------------------------------------


# Start with setting up census geographies and gathering census data

## Reload census API

readRenviron("~/.Renviron")

NOVA_Counties <-  c("Arlington", 
                    "Fairfax County", 
                    "Fairfax city", 
                    "Loudoun", 
                    "Alexandria City",
                    "Falls Church City")

ACS_variables <- load_variables(2020,"acs5", cache = TRUE)
View(ACS_variables)

CensusData <- get_acs(
  geography = "block group",
  year = 2020,
  variables = c(TotalPopulation = "B01003_001",
                TotalCommuters = "B08301_001",
                DriveAlone = "B08301_003",
                PublicTransport = "B08301_010",
                HHCount = "B25044_001",
                PovertyRatioTotal = "C17002_001",
                PovertyOver200Pct = "C17002_008",
                HHOwn0Veh = "B25044_003",
                HHRent0Veh = "B25044_010",
                WhitePop = "B02001_002"),
  state = "VA",
  county = NOVA_Counties,
  geometry = TRUE,
  output = "wide"
)

#### Change CensusData reference to match bus stop shapefiles
CensusData <-  st_transform(CensusData, crs = st_crs(4326))


mapview(CensusData)

NOVA <- st_union(CensusData)

mapview(NOVA)

## GTFS File paths
GTFS_path <- file.path ("Z:", ### File path where data is located
                        "NVTC General",
                        "Projects and Programs",
                        "Transit Resource Center (TRC)",
                        "Data",
                        "GTFS")

WMATA2022Path <- file.path(GTFS_path, "2022-04_WMATA.zip")
WMATA2022GTFS <- read_gtfs(WMATA2022Path)

### Make a spatial file for stops
WMATA2022StopLoc <- stops_as_sf(WMATA2022GTFS$stops) 

## Identify only WMATA stops in Virginia
WMATA_VA <- st_intersection(WMATA2022StopLoc, NOVA)
