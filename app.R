# Packages for Dashboard
library(shiny)
library(shinydashboard)

# Packages for Data Wrangling
library(tidyverse)
library(here)
library(data.table)

# Packages for Maps
library(sf)
library(leaflet)

# Inputs ------------------------------------------------------------

min.transit.density <- 15
min.hct.density <- 40

hct.order <- c("BRT - Programmed", "BRT - Unprogrammed",
               "Auto Ferry", "Passenger Only Ferry",
               "Light Rail", "Streetcar", "Monorail",
               "Commuter Rail",
               "Bus")

wgs84 <- 4326
spn <- 2285 

brt.constrained <- c('69930699','86386337','059d725b','0e632d9b','1f4e7147','2819732c','2b10424c','2d969405','35d8b755','3ac667df',
                     '3b7a72ab','4551b28a','54daeb98','604f7a45','628fdd6e','64335d82','64b42288','652b10f6','78bfd309','858586eb',
                     '8cd15f5d','8ced6ebb','9383b962','9d2a97f3','9e423688','af734d73','b19647c7','b5fe11d9','b7b2e507','d06fcd19',
                     'd10ce9f2','d5b2b21b','d5cac403','d7a11731','dac342ce','f49ff308')

brt.unconstrained <- c('8fd32294','927d06ce','a85c881c','b073cfd8','c7599818')

brt.full.plan <- c(brt.constrained,brt.unconstrained)

pof.constrained <- c('838df28c','fe6cc15a','4ddaf1d8','d1a584e2','7c6aed77','f09e3012','97bdbd4c','94f23b4b','213ab30d','bb27165f')

ferry.constrained <- c('WSF2100','WSF2101','WSF2102', 'WSF2103', 'WSF2104','WSF2105', 'WSF2106','WSF2107',
                       'PCF_Anderson_Ketron', 'PCF_Steilacoom_Ketron', 'PCF_Steilacoom-Anderson')

srt.constrained <- c('04663955', '6d393241', '9fda70a6')

monorail.constrained <- c('8e776757')

crt.constrained <- c('d0c48cae', 'acaa2738')

lrt.constrained <- c('961fa992','edb59098','a100f6d5','d41e3912','c5ad7b63')

au.bins <- c(0, 2, 18, 30, 45, 85, 5000)

# 2050 Transit Route Summary ----------------------------------------------------

gtfs.2050.url <- 'Y://2022 RTP//Future Transit Network//newest//2050//model_import//revised_routes_new_stop_ids//merged'

routes <- as_tibble(fread(here(gtfs.2050.url,'routes.txt'))) %>%
    mutate(Typology = case_when(
        route_id %in% brt.constrained ~ "BRT - Programmed",
        route_id %in% brt.unconstrained ~ "BRT - Unprogrammed",
        route_id %in% pof.constrained ~ "Passenger Only Ferry",
        route_short_name %in% ferry.constrained ~ "Auto Ferry",
        route_id %in% srt.constrained ~ "Streetcar",
        route_id %in% crt.constrained ~ "Commuter Rail",
        route_id %in% lrt.constrained ~ "Light Rail",
        route_id %in% monorail.constrained ~ "Monorail")) %>%
    select(route_id, route_short_name, route_long_name, route_type, Typology) %>%
    mutate(Typology = replace_na(Typology, "Bus"))

routes$Typology <- factor(routes$Typology, levels=hct.order)

brt.constrained.routes <- routes %>% filter(route_id%in%brt.constrained) %>% select(route_short_name) %>% pull() %>% length()
brt.unconstrained.routes <- routes %>% filter(route_id%in%brt.unconstrained) %>% select(route_short_name) %>% pull() %>% length()
brt.routes <- routes %>% filter(route_id%in%brt.full.plan) %>% select(route_short_name) %>% pull() %>% length()

pof.routes <- routes %>% filter(route_id%in%pof.constrained) %>% select(route_short_name) %>% pull() %>% length()

ferry.routes <- routes %>% filter(route_short_name%in%ferry.constrained) %>% select(route_short_name) %>% distinct() %>% pull() %>% length()

srt.routes <- routes %>% filter(route_id%in%srt.constrained) %>% select(route_short_name) %>% pull() %>% length()

monorail.routes <- routes %>% filter(route_id%in%monorail.constrained) %>% select(route_short_name) %>% pull() %>% length()

crt.routes <- routes %>% filter(route_id%in%crt.constrained) %>% select(route_short_name) %>% pull() %>% length()

lrt.routes <- routes %>% filter(route_id%in%lrt.constrained) %>% select(route_short_name) %>% pull() %>% length()

bus.routes <- routes %>% filter(Typology=="Bus") %>% select(route_short_name) %>% pull() %>% length()

# Shapefiles --------------------------------------------------------------

transit.routes.lyr <- st_read(here('data', 'directions_2050.shp')) %>% 
    st_transform(wgs84) %>%
    select(line_id,line_name) %>%
    distinct(line_id,line_name,.keep_all = TRUE)

transit.routes.lyr <- left_join(transit.routes.lyr, routes, by=c("line_id"="route_id"))

hct.routes.lyr <- transit.routes.lyr %>% filter(!(Typology=="Bus"))
bus.routes.lyr <- transit.routes.lyr %>% filter(Typology=="Bus")

psrc.taz <- st_read(here('data', 'taz_data.shp')) %>% st_transform(wgs84)

transit.supportive.zones <- psrc.taz %>% select(taz, AUDen50) %>% 
    st_transform(spn) %>%
    filter(AUDen50>=min.transit.density) %>%
    mutate(`Transit Supportive Density`="Yes") %>%
    select(`Transit Supportive Density`) %>%
    st_union() %>%
    st_transform(wgs84) %>%
    st_sf() %>%
    mutate(`Transit Supportive Density`="Yes")

hct.supportive.zones <- psrc.taz %>% select(taz, AUDen50) %>% 
    st_transform(spn) %>%
    filter(AUDen50>=min.hct.density) %>%
    mutate(`High Capacity Transit Supportive Density`="Yes") %>%
    select(`High Capacity Transit Supportive Density`) %>%
    st_union() %>%
    st_transform(wgs84) %>%
    st_sf() %>%
    mutate(`High Capacity Transit Supportive Density`="Yes")
    

# PSRC Colors -------------------------------------------------------------

transit.pal <- colorFactor(
    palette = c("#91268F", "#C388C2", "#73CFCB", "#00A7A0", "#F05A28", "#F4835E", "#F7A489", "#8CC63E", "#4C4C4C"),
    levels = c("BRT - Programmed", "BRT - Unprogrammed", "Auto Ferry", "Passenger Only Ferry", "Light Rail", "Streetcar", "Monorail", "Commuter Rail",  "Bus" ))

# Functions ---------------------------------------------------------------

create.map <- function() {
    
    m <- leaflet() %>% 
        addProviderTiles(providers$CartoDB.Positron) %>%
        
        addLayersControl(baseGroups = c("Base Map"),
                         overlayGroups = c("High Capacity Transit Density",
                                           "High Capacity Transit Routes",
                                           "Local & Express Bus Density",
                                           "Local & Express Bus Routes"),
                         options = layersControlOptions(collapsed = FALSE)) %>%
        
        addEasyButton(easyButton(
            icon="fa-globe", title="Region",
            onClick=JS("function(btn, map){map.setView([47.615,-122.257],8); }"))) %>%
        
        addEasyButton(easyButton(
            icon="fa-anchor", title="Bremerton",
            onClick=JS("function(btn, map){  map.setView([47.565,-122.654],10); }"))) %>%
        
        addEasyButton(easyButton(
            icon="fa-amazon", title="Cross-Lake",
            onClick=JS("function(btn, map){  map.setView([47.615,-122.257],10); }"))) %>%
        
        addEasyButton(easyButton(
            icon="fa-plane", title="Everett",
            onClick=JS("function(btn, map){  map.setView([47.975,-122.196],10); }"))) %>%
        
        addEasyButton(easyButton(
            icon="fa-glass", title="Tacoma",
            onClick=JS("function(btn, map){  map.setView([47.252,-122.442],10); }"))) %>%
        
        setView(lat=47.615, lng=-122.257, zoom=9) %>%
        
        addPolygons(data=hct.supportive.zones,
                    fillOpacity = 0.5,
                    fillColor = "blue",
                    opacity = 0,
                    weight = 0,
                    color = "blue",
                    dashArray = "1",
                    group = "High Capacity Transit Density") %>%
        
        addPolylines(data = hct.routes.lyr,
                     color = ~transit.pal(Typology),
                     weight = 3,
                     fillColor = ~transit.pal(Typology),
                     group = "High Capacity Transit Routes") %>%
        
        addPolygons(data=transit.supportive.zones,
                    fillOpacity = 0.5,
                    fillColor = "cornflowerblue",
                    opacity = 0,
                    weight = 0,
                    color = "cornflowerblue",
                    dashArray = "1",
                    group = "Local & Express Bus Density") %>%
        
        addPolylines(data = bus.routes.lyr,
                     color = ~transit.pal(Typology),
                     weight = 1,
                     fillColor = ~transit.pal(Typology),
                     group = "Local & Express Bus Routes") %>%
    
        addLegend(pal = transit.pal,
              values = hct.routes.lyr$Typology,
              group = "High Capacity Transit Routes",
              position = "bottomleft",
              title = "High Capacity Transit Mode") %>%
        
        addLegend(colors=c("#4C4C4C"),
                  labels=c("Bus"),
                  group = "Local & Express Bus Routes",
                  position = "bottomleft",
                  title = "Local & Express Bus") %>%
        
        addLegend(colors=c("blue"),
                  labels=c("Yes"),
                  group = "High Capacity Transit Density",
                  position = "bottomright",
                  title = "More than 40 Activity Units per Acre") %>%
        
        addLegend(colors=c("cornflowerblue"),
                  labels=c("Yes"),
                  group = "Local & Express Bus Density",
                  position = "bottomright",
                  title = "More than 15 Activity Units per Acre") %>%
        
        hideGroup("High Capacity Transit Routes") %>%
        hideGroup("Local & Express Bus Routes")

    return(m)
    
}

# User Interface for Dashboard --------------------------------------------
ui <- dashboardPage(skin = "black", title = "2022-2050 RTP",
    dashboardHeader(title = "2022-2050 Regional Transporation Plan Transit Dashboard", titleWidth = '50%'),
    
    dashboardSidebar(disable = TRUE),
    
    dashboardBody(
        tabItem(tabName = "2050 Transit Network",

                    fluidRow(column(width = 4,
                               infoBoxOutput("brtBox", width = NULL),
                               infoBoxOutput("lrtBox", width = NULL),
                               infoBoxOutput("crtBox", width = NULL),
                               infoBoxOutput("ferryBox", width = NULL),
                               infoBoxOutput("busBox", width = NULL)),
                             column(width = 8, box(title="Transit Routes and Density", width = NULL, leafletOutput("taz50map", width="100%", height=600))))
            ) # end of 2050 tabpanel
        ) # end of tabbox
)               

# Server functions for Dashboard ------------------------------------------

server <- function(input, output) {
    
    output$brtBox <- renderInfoBox({
        
        infoBox(
            "Bus Rapid Transit", HTML(paste0(brt.constrained.routes, " Programmed Routes", br(), brt.unconstrained.routes, " Unprogrammed Routes")), icon = icon("bus"),
            color = "purple", fill=TRUE
        )
    })

    output$lrtBox <- renderInfoBox({
        
        infoBox(
            "Light Rail", HTML(paste0(lrt.routes, " Light Rail Routes" , br(), srt.routes, " Streetcar Routes", br(), monorail.routes, " Monorail Route")), icon = icon("subway"),
            color = "orange", fill=TRUE
        )
    })
    
    output$crtBox <- renderInfoBox({
        
        infoBox(
            "Heavy Rail", paste0(crt.routes, " Commuter Rail Routes"), icon = icon("train"),
            color = "green", fill=TRUE
        )
    })

    output$ferryBox <- renderInfoBox({
        
        infoBox(
            "Ferries", HTML(paste0(pof.routes, " Passenger-Only Routes", br(), ferry.routes, " Auto Ferry Routes")), icon = icon("ship"),
            color = "blue", fill=TRUE
        )
    })
    
    output$busBox <- renderInfoBox({
        
        infoBox(
            "Bus", paste0(bus.routes, " Local and Express Routes"), icon = icon("bus"),
            color = "black", fill=TRUE
        )
    })
    

    output$taz50map <- renderLeaflet({create.map()})
    
    
}

shinyApp(ui, server)
