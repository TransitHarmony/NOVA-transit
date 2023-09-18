# WMATA rail line distance in Virginia -----

## Set up workspace

library(tidyverse)
library(here)
library(censusapi)
library(tidytransit)
library(tidycensus)
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

CensusData <- get_acs(
  geography = "county",
  year = 2020,
  variables = c(TotalPopulation = "B01003_001"),
  state = "VA",
  county = NOVA_Counties,
  geometry = TRUE,
  output = "wide"
)

## Combine Northern Virginia jurisdictions into one polygon
NOVA_polygon <- st_union(CensusData) %>% st_transform(crs=4326)

mapview(NOVA_polygon)

mapview(NOVA_polygon)+ mapview(WMATA_VA_Rail)


# GTFS File paths
GTFS_path <- file.path ("Z:", ### File path where data is located
                        "NVTC General",
                        "Projects and Programs",
                        "Transit Resource Center (TRC)",
                        "Data",
                        "GTFS")

Rail_routes <- c("BLUE", "SILVER", "YELLOW", "ORANGE")


##2019 GTFS data=====
WMATA2019Path <- file.path(GTFS_path, "2019-10_WMATA.zip")
WMATA2019GTFS <- read_gtfs(WMATA2019Path)

WMATA2019GTFS <- gtfs_as_sf(WMATA2019GTFS)


## List of unique shape_ids that may touch Virginia
Trip_list <- WMATA2019GTFS$trips %>%
  filter(route_id %in%
           Rail_routes) %>%
  group_by(shape_id) %>%
  summarize(max_trip = max(trip_id)) %>%
  select(max_trip) %>%
  as.list()


## Filter for rail trips with shape_ids that may touch Virginia
WMATA_VA_Rail <- get_trip_geometry(WMATA2019GTFS, Trip_list$max_trip)

## Spatially clip rail trip shapes to get Virginia miles
VA_rail <- st_intersection(WMATA_VA_Rail, NOVA_polygon)


### Calculate length (default is metres)
VA_rail$length <- st_length(VA_rail)

### Change units to miles
VA_rail$length <- set_units(VA_rail$length, mi)

VA_rail_table <- st_drop_geometry(VA_rail)

mapview(VA_rail) + mapview(NOVA_polygon)

write.csv(VA_rail_table, here("Data_output", "VA_rail_table2019.csv"), row.names = FALSE)



### QA/QC

#### 8 OR/SL/BL/YL shape_ids don't appear to have Virginia miles
#### Check trips using these shape_ids to ensure this is accurate

No_Virginia <- c("76633106913",
                 "77463187276",
                 "78043105834",
                 "78623186376",
                 "81013105061",
                 "81023105061",
                 "83293102761",
                 "83303102761")

QAQC <- get_trip_geometry(WMATA2019GTFS, No_Virginia)

mapview(QAQC)

#### all 8 shape_ids are in MD and DC only; list of 128 is accurate
#### Eight


QAQC_two <- get_trip_geometry(WMATA2019GTFS, "25773106940")

mapview(QAQC_two)





##2022 June GTFS data=====
WMATA202206Path <- file.path(GTFS_path, "2022-06_Metrorail.zip")
WMATA202206GTFS <- read_gtfs(WMATA202206Path)

WMATA202206GTFS <- gtfs_as_sf(WMATA202206GTFS)


## List of unique shape_ids that may touch Virginia
Trip_list <- WMATA202206GTFS$trips %>%
  filter(route_id %in%
           Rail_routes) %>%
  group_by(shape_id) %>%
  summarize(max_trip = max(trip_id)) %>%
  select(max_trip) %>%
  as.list()


## Filter for rail trips with shape_ids that may touch Virginia
Metrorail_VA_202206 <- get_trip_geometry(WMATA202206GTFS, Trip_list$max_trip)

## Spatially clip rail trip shapes to get Virginia miles
VA_rail202206 <- st_intersection(Metrorail_VA_202206, NOVA_polygon)


### Calculate length (default is metres)
VA_rail202206$length <- st_length(VA_rail202206)

### Change units to miles
VA_rail202206$length <- set_units(VA_rail202206$length, mi)

VA_rail202206table <- st_drop_geometry(VA_rail202206)

mapview(VA_rail202206)

write.csv(VA_rail202206table, here("Data_output", "VA_rail_table202206.csv"), row.names = FALSE)






##2022 September GTFS data=====
WMATA202209Path <- file.path(GTFS_path, "2022-09_Metrorail.zip")
WMATA202209GTFS <- read_gtfs(WMATA202209Path)

WMATA202209GTFS <- gtfs_as_sf(WMATA202209GTFS)


## List of unique shape_ids that may touch Virginia
Trip_list <- WMATA202209GTFS$trips %>%
  filter(route_id %in%
           Rail_routes) %>%
  group_by(shape_id) %>%
  summarize(max_trip = max(trip_id)) %>%
  select(max_trip) %>%
  as.list()


## Filter for rail trips with shape_ids that may touch Virginia
Metrorail_VA_202209 <- get_trip_geometry(WMATA202209GTFS, Trip_list$max_trip)

## Spatially clip rail trip shapes to get Virginia miles
VA_rail202209 <- st_intersection(Metrorail_VA_202209, NOVA_polygon)


### Calculate length (default is metres)
VA_rail202209$length <- st_length(VA_rail202209)

### Change units to miles
VA_rail202209$length <- set_units(VA_rail202209$length, mi)

VA_rail202209table <- st_drop_geometry(VA_rail202209)

mapview(VA_rail202209)+ mapview(VA_rail202206)

write.csv(VA_rail202209table, here("Data_output", "VA_rail_table202209.csv"), row.names = FALSE)



##2022 November GTFS data=====
WMATA202211Path <- file.path(GTFS_path, "2022-11_Metrorail.zip")
WMATA202211GTFS <- read_gtfs(WMATA202211Path)

WMATA202211GTFS <- gtfs_as_sf(WMATA202211GTFS)


## List of unique shape_ids that may touch Virginia
Trip_list <- WMATA202211GTFS$trips %>%
  filter(route_id %in%
           Rail_routes) %>%
  group_by(shape_id) %>%
  summarize(max_trip = max(trip_id)) %>%
  select(max_trip) %>%
  as.list()


## Filter for rail trips with shape_ids that may touch Virginia
Metrorail_VA_202211 <- get_trip_geometry(WMATA202211GTFS, Trip_list$max_trip)

## Spatially clip rail trip shapes to get Virginia miles
VA_rail202211 <- st_intersection(Metrorail_VA_202211, NOVA_polygon)


### Calculate length (default is metres)
VA_rail202211$length <- st_length(VA_rail202211)

### Change units to miles
VA_rail202211$length <- set_units(VA_rail202211$length, mi)

VA_rail202211table <- st_drop_geometry(VA_rail202211)

mapview(VA_rail202211)

write.csv(VA_rail202211table, here("Data_output", "VA_rail_table202211.csv"), row.names = FALSE)


##2023 February GTFS data=====
WMATA202302Path <- file.path(GTFS_path, "2023-02_Metrorail.zip")
WMATA202302GTFS <- read_gtfs(WMATA202302Path)

WMATA202302GTFS <- gtfs_as_sf(WMATA202302GTFS)


## List of unique shape_ids that may touch Virginia
Trip_list <- WMATA202302GTFS$trips %>%
  filter(route_id %in%
           Rail_routes) %>%
  group_by(shape_id) %>%
  summarize(max_trip = max(trip_id)) %>%
  select(max_trip) %>%
  as.list()


## Filter for rail trips with shape_ids that may touch Virginia
Metrorail_VA_202202 <- get_trip_geometry(WMATA202302GTFS, Trip_list$max_trip)

## Spatially clip rail trip shapes to get Virginia miles
VA_rail202302 <- st_intersection(Metrorail_VA_202202, NOVA_polygon)


### Calculate length (default is metres)
VA_rail202302$length <- st_length(VA_rail202302)

### Change units to miles
VA_rail202302$length <- set_units(VA_rail202302$length, mi)

VA_rail202302table <- st_drop_geometry(VA_rail202302)

mapview(VA_rail202302)

write.csv(VA_rail202302table, here("Data_output", "VA_rail_table202302.csv"), row.names = FALSE)



##2023 March GTFS data=====
WMATA202303Path <- file.path(GTFS_path, "2023-03_Metrorail.zip")
WMATA202303GTFS <- read_gtfs(WMATA202303Path)

WMATA202303GTFS <- gtfs_as_sf(WMATA202303GTFS)


## List of unique shape_ids that may touch Virginia
Trip_list <- WMATA202303GTFS$trips %>%
  filter(route_id %in%
           Rail_routes) %>%
  group_by(shape_id) %>%
  summarize(max_trip = max(trip_id)) %>%
  select(max_trip) %>%
  as.list()


## Filter for rail trips with shape_ids that may touch Virginia
Metrorail_VA_202303 <- get_trip_geometry(WMATA202303GTFS, Trip_list$max_trip)

## Spatially clip rail trip shapes to get Virginia miles
VA_rail202303 <- st_intersection(Metrorail_VA_202303, NOVA_polygon)


### Calculate length (default is metres)
VA_rail202303$length <- st_length(VA_rail202303)

### Change units to miles
VA_rail202303$length <- set_units(VA_rail202303$length, mi)

VA_rail202303table <- st_drop_geometry(VA_rail202303)

mapview(VA_rail202303)

write.csv(VA_rail202303table, here("Data_output", "VA_rail_table202303.csv"), row.names = FALSE)



## Length of all routes

WMATA202303_all_shapes <- WMATA202303GTFS$shapes %>%
  mutate(distance = st_length(geometry))

mapview(WMATA202303_all_shapes)

WMATA202303_all_shapes$distance <- set_units(WMATA202303_all_shapes$distance, mi)

WMATA202303_all_shapes <- st_drop_geometry(WMATA202303_all_shapes)

write.csv(WMATA202303_all_shapes, here("Data_output", "WMATA202303_all_shapes.csv"), row.names = FALSE)



##2023 May 16 GTFS data=====
WMATA20230516Path <- file.path(GTFS_path, "2023-05-16_Metrorail.zip")
WMATA20230516GTFS <- read_gtfs(WMATA20230516Path)

WMATA20230516GTFS <- gtfs_as_sf(WMATA20230516GTFS)


## List of unique shape_ids that may touch Virginia
Trip_list <- WMATA20230516GTFS$trips %>%
  filter(route_id %in%
           Rail_routes) %>%
  group_by(shape_id) %>%
  summarize(max_trip = max(trip_id)) %>%
  select(max_trip) %>%
  as.list()


## Filter for rail trips with shape_ids that may touch Virginia
Metrorail_VA_20230516 <- get_trip_geometry(WMATA20230516GTFS, Trip_list$max_trip)

## Spatially clip rail trip shapes to get Virginia miles
VA_rail20230516 <- st_intersection(Metrorail_VA_20230516, NOVA_polygon)


### Calculate length (default is metres)
VA_rail20230516$length <- st_length(VA_rail20230516)

### Change units to miles
VA_rail20230516$length <- set_units(VA_rail20230516$length, mi)

VA_rail20230516table <- st_drop_geometry(VA_rail20230516)

mapview(VA_rail20230516)

write.csv(VA_rail20230516table, here("Data_output", "VA_rail_table20230516.csv"), row.names = FALSE)



##2023 May 22 GTFS data=====
WMATA20230522Path <- file.path(GTFS_path, "2023-05-22_Metrorail.zip")
WMATA20230522GTFS <- read_gtfs(WMATA20230522Path)

WMATA20230522GTFS <- gtfs_as_sf(WMATA20230522GTFS)


## List of unique shape_ids that may touch Virginia
Trip_list <- WMATA20230522GTFS$trips %>%
  filter(route_id %in%
           Rail_routes) %>%
  group_by(shape_id) %>%
  summarize(max_trip = max(trip_id)) %>%
  select(max_trip) %>%
  as.list()


## Filter for rail trips with shape_ids that may touch Virginia
Metrorail_VA_20230522 <- get_trip_geometry(WMATA20230522GTFS, Trip_list$max_trip)

## Spatially clip rail trip shapes to get Virginia miles
VA_rail20230522 <- st_intersection(Metrorail_VA_20230522, NOVA_polygon)


### Calculate length (default is metres)
VA_rail20230522$length <- st_length(VA_rail20230522)

### Change units to miles
VA_rail20230522$length <- set_units(VA_rail20230522$length, mi)

VA_rail20230522table <- st_drop_geometry(VA_rail20230522)

mapview(VA_rail20230522) + mapview(VA_rail20230516)

write.csv(VA_rail20230522table, here("Data_output", "VA_rail_table20230522.csv"), row.names = FALSE)



##2023 June 06 GTFS data=====
WMATA20230605Path <- file.path(GTFS_path, "2023-06_Metrorail.zip")
WMATA20230605GTFS <- read_gtfs(WMATA20230605Path)

WMATA20230605GTFS <- gtfs_as_sf(WMATA20230605GTFS)


## List of unique shape_ids that may touch Virginia
Trip_list <- WMATA20230605GTFS$trips %>%
  filter(route_id %in%
           Rail_routes) %>%
  group_by(shape_id) %>%
  summarize(max_trip = max(trip_id)) %>%
  select(max_trip) %>%
  as.list()


## Filter for rail trips with shape_ids that may touch Virginia
Metrorail_VA_20230605 <- get_trip_geometry(WMATA20230605GTFS, Trip_list$max_trip)

## Spatially clip rail trip shapes to get Virginia miles
VA_rail20230605 <- st_intersection(Metrorail_VA_20230605, NOVA_polygon)


### Calculate length (default is metres)
VA_rail20230605$length <- st_length(VA_rail20230605)

### Change units to miles
VA_rail20230605$length <- set_units(VA_rail20230605$length, mi)

VA_rail20230605table <- st_drop_geometry(VA_rail20230605)

mapview(VA_rail20230605)

write.csv(VA_rail20230605table, here("Data_output", "VA_rail_table20230605.csv"), row.names = FALSE)



##2023 June 26 GTFS data=====
WMATA20230626Path <- file.path(GTFS_path, "2023-06-26_Metrorail.zip")
WMATA20230626GTFS <- read_gtfs(WMATA20230626Path)

WMATA20230626GTFS <- gtfs_as_sf(WMATA20230626GTFS)


## List of unique shape_ids that may touch Virginia
Trip_list <- WMATA20230626GTFS$trips %>%
  filter(route_id %in%
           Rail_routes) %>%
  group_by(shape_id) %>%
  summarize(max_trip = max(trip_id)) %>%
  select(max_trip) %>%
  as.list()


## Filter for rail trips with shape_ids that may touch Virginia
Metrorail_VA_20230626 <- get_trip_geometry(WMATA20230626GTFS, Trip_list$max_trip)

## Spatially clip rail trip shapes to get Virginia miles
VA_rail20230626 <- st_intersection(Metrorail_VA_20230626, NOVA_polygon)


### Calculate length (default is metres)
VA_rail20230626$length <- st_length(VA_rail20230626)

### Change units to miles
VA_rail20230626$length <- set_units(VA_rail20230626$length, mi)

VA_rail20230626table <- st_drop_geometry(VA_rail20230626)

mapview(VA_rail20230626)

write.csv(VA_rail20230626table, here("Data_output", "VA_rail_table20230626.csv"), row.names = FALSE)






