# Packages for Data Wrangling
library(tidyverse)
library(here)
library(data.table)

# Packages for Maps
library(sf)
library(leaflet)

# Packages for Database Connections
library(odbc)
library(DBI)


# Census Tract Population -------------------------------------------------
tract.population <- as_tibble(fread(here('data','census_tract__table__population.csv')))
tract.lookup <- as_tibble(fread(here('data','usim_by14_census_tracts_table.csv')))
tract.population <- left_join(tract.population, tract.lookup, by=c("census_tract_id")) %>% 
  select(geoid10, population_2018, population_2050) %>% 
  rename(geoid=geoid10) %>%
  mutate(geoid = as.character(geoid))
rm(tract.lookup)

# Census Tract Equity Geographies -----------------------------------------
census.year <- 2018
server_name <- "AWS-PROD-SQL\\SOCKEYE"
database_name <- "Elmer"

disabled_query <- paste0("SELECT geoid, equity_geog_vs_reg_total FROM Census.disability_equity_geographies(",census.year,",'Tract')")
elderly_query <- paste0("SELECT geoid, equity_geog_vs_reg_total FROM Census.elderly_equity_geographies(",census.year,",'Tract')")
youth_query <- paste0("SELECT geoid, equity_geog_vs_reg_total FROM Census.youth_equity_geographies(",census.year,",'Tract')")
lep_query <- paste0("SELECT geoid, equity_geog_vs_reg_total FROM Census.limited_english_equity_geographies(",census.year,",'Tract')")
poverty_query <- paste0("SELECT geoid, equity_geog_vs_reg_total FROM Census.poverty_equity_geographies(",census.year,",'Tract')")
peopleofcolor_query <- paste0("SELECT geoid, equity_geog_vs_reg_total FROM Census.racial_equity_geographies(",census.year,",'Tract')")

db_con <- dbConnect(odbc::odbc(),
                    driver = "SQL Server",
                    server = server_name,
                    database = database_name,
                    trusted_connection = "yes"
)

tract.disability <- as_tibble(DBI::dbGetQuery(db_con, disabled_query)) %>% rename(disabled = equity_geog_vs_reg_total)
tract.elderly <- as_tibble(DBI::dbGetQuery(db_con, elderly_query)) %>% rename(elderly=equity_geog_vs_reg_total)
tract.limited.english <- as_tibble(DBI::dbGetQuery(db_con, lep_query)) %>% rename(limited_english=equity_geog_vs_reg_total)
tract.poverty <- as_tibble(DBI::dbGetQuery(db_con, poverty_query)) %>% rename(poverty=equity_geog_vs_reg_total)
tract.people.of.color <- as_tibble(DBI::dbGetQuery(db_con, peopleofcolor_query)) %>%  rename(people_of_color=equity_geog_vs_reg_total)
tract.youth <- as_tibble(DBI::dbGetQuery(db_con, youth_query)) %>% rename(youth=equity_geog_vs_reg_total)

odbc::dbDisconnect(db_con)

equity.tracts <- list(tract.population, tract.disability, tract.elderly, tract.limited.english, tract.poverty, tract.people.of.color, tract.youth) %>% 
  reduce(left_join, by = "geoid") %>%
  mutate(across(everything(), ~replace_na(.x, 0)))

rm(tract.population, tract.disability, tract.elderly, tract.limited.english, tract.poverty, tract.people.of.color, tract.youth, db_con)

# Spatial Analysis ------------------------------------------------------------
wgs84 <- 4326
spn <- 2285 

stops.2018 <- as_tibble(fread(here('gtfs','2018','stops.txt'))) %>% 
  select(stop_id,stop_lat,stop_lon)

stops.2050 <- as_tibble(fread(here('gtfs','2050','stops.txt'))) %>% 
  select(stop_id,stop_lat,stop_lon)

tract.layer <- st_read("https://services6.arcgis.com/GWxg6t7KXELn1thE/arcgis/rest/services/tract2010_nowater/FeatureServer/0/query?where=0=0&outFields=*&f=pgeojson") %>% 
  st_transform(spn) %>%
  select(COUNTYFP10,GEOID10) %>%
  rename(county=COUNTYFP10, geoid=GEOID10) %>%
  st_transform(spn)

stops.2018.layer <- st_as_sf(stops.2018, coords = c("stop_lon", "stop_lat"), crs = wgs84) %>% 
  st_transform(spn)

stops.2018.layer <- st_intersection(stops.2018.layer, tract.layer)

stops.2050.layer <- st_as_sf(stops.2050, coords = c("stop_lon", "stop_lat"), crs = wgs84) %>% 
  st_transform(spn)

stops.2050.layer <- st_intersection(stops.2050.layer, tract.layer)

# Transit Service for Stops -----------------------------------------------
brt.constrained <- c('69930699','86386337','059d725b','0e632d9b','1f4e7147','2819732c','2b10424c','2d969405','35d8b755','3ac667df',
                     '3b7a72ab','4551b28a','54daeb98','604f7a45','628fdd6e','64335d82','64b42288','652b10f6','78bfd309','858586eb',
                     '8cd15f5d','8ced6ebb','9383b962','9d2a97f3','9e423688','af734d73','b19647c7','b5fe11d9','b7b2e507','d06fcd19',
                     'd10ce9f2','d5b2b21b','d5cac403','d7a11731','dac342ce','f49ff308',
                     'ct_701','ct_702',
                     'kcm_100512', 'kcm_102548', 'kcm_102576', 'kcm_102581', 'kcm_102615', 'kcm_102619')

brt.unconstrained <- c('8fd32294','927d06ce','a85c881c','b073cfd8','c7599818')

brt.full.plan <- c(brt.constrained,brt.unconstrained)

pof.constrained <- c('838df28c','fe6cc15a','4ddaf1d8','d1a584e2','7c6aed77','f09e3012','97bdbd4c','94f23b4b','213ab30d','bb27165f',
                     'kcm_100336', 'kcm_100337',
                     'kt_Ferry', 'kt_Annapolis', 'kt_Kitsap Fast Ferry')

ferry.constrained <- c('WSF2100','WSF2101','WSF2102', 'WSF2103', 'WSF2104','WSF2105', 'WSF2106','WSF2107',
                       'PCF_Anderson_Ketron', 'PCF_Steilacoom_Ketron', 'PCF_Steilacoom-Anderson')

srt.constrained <- c('04663955', '6d393241', '9fda70a6',
                     'kcm_100340', 'kcm_102638')

monorail.constrained <- c('8e776757')

crt.constrained <- c('d0c48cae', 'acaa2738',
                     'st_SNDR_EV','st_SNDR_TL','st_AMTK',
                     'st_a-AMTK', 'st_a-SNDR_EV', 'st_a-SNDR_TL')

lrt.constrained <- c('961fa992','edb59098','a100f6d5','d41e3912','c5ad7b63',
                     'kcm_100479', 'st_TLINK', 'st_100479', 'st_a-TLINK')

# 2018 Transit Routes -----------------------------------------------------
gtfs.2018.url <- here('gtfs//2018//')

routes.2018 <- as_tibble(fread(here(gtfs.2018.url,'fall-2018-routes-summary.csv'))) %>%
  mutate(typology = case_when(
    route_id %in% srt.constrained ~ "Streetcar",
    route_id %in% lrt.constrained ~ "Light Rail",
    !(route_id %in% srt.constrained | route_id %in% lrt.constrained) ~ typology)) %>%
  select(route_id, typology)

trips.2018 <- as_tibble(fread(here(gtfs.2018.url,'trips.txt'))) %>% select(trip_id, route_id)
stoptimes.2018 <- as_tibble(fread(here(gtfs.2018.url,'stop_times.txt'))) %>% select(trip_id, stop_id)

trips.2018 <- left_join(trips.2018, routes.2018, by=c("route_id"))
stoptimes.2018 <- left_join(stoptimes.2018, trips.2018, by=c("trip_id"))

transit.stops.2018 <- stoptimes.2018 %>% 
  select(stop_id, typology) %>% 
  mutate(trips=1) %>%
  group_by(stop_id, typology) %>%
  summarize(trips = sum(trips))

gtfs.2050.url <- here('gtfs//2050//')

routes.2050 <- as_tibble(fread(here(gtfs.2050.url,'fall-2050-routes-summary.csv'))) 
