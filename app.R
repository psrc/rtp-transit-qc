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

hct.order <- c("BRT", "BRT - Unprogrammed",
               "Ferry", "Passenger Only Ferry",
               "Light Rail", "Streetcar", "Monorail",
               "Commuter Rail",
               "Very Frequent Route", "Frequent Route",
               "Moderate Frequency Route", "Low Frequency Route")

bus.modes <- c("Very Frequent Route", "Frequent Route",
               "Moderate Frequency Route", "Low Frequency Route")

wgs84 <- 4326
spn <- 2285 

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

au.bins <- c(0, 2, 18, 30, 45, 85, 5000)

# 2018 Transit Route Summary ----------------------------------------------------
gtfs.2018.url <- here('gtfs//2018//')

routes.2018 <- as_tibble(fread(here(gtfs.2018.url,'fall-2018-routes-summary.csv'))) %>%
    mutate(typology = case_when(
        route_id %in% srt.constrained ~ "Streetcar",
        route_id %in% lrt.constrained ~ "Light Rail",
        !(route_id %in% srt.constrained | route_id %in% lrt.constrained) ~ typology))

routes.2018$typology <- factor(routes.2018$typology, levels=hct.order)

brt.constrained.routes.2018 <- routes.2018 %>% filter(route_id%in%brt.constrained) %>% select(route_short_name) %>% pull() %>% length()
brt.unconstrained.routes.2018 <- routes.2018 %>% filter(route_id%in%brt.unconstrained) %>% select(route_short_name) %>% pull() %>% length()
brt.routes.2018 <- routes.2018 %>% filter(route_id%in%brt.full.plan) %>% select(route_short_name) %>% pull() %>% length()
pof.routes.2018 <- routes.2018 %>% filter(route_id%in%pof.constrained) %>% select(route_short_name) %>% pull() %>% length()
ferry.routes.2018 <- routes.2018 %>% filter(route_short_name%in%ferry.constrained) %>% select(route_short_name) %>% distinct() %>% pull() %>% length()
srt.routes.2018 <- routes.2018 %>% filter(route_id%in%srt.constrained) %>% select(route_short_name) %>% pull() %>% length()
monorail.routes.2018 <- routes.2018 %>% filter(route_id%in%monorail.constrained) %>% select(route_short_name) %>% pull() %>% length()
crt.routes.2018 <- routes.2018 %>% filter(route_id%in%crt.constrained) %>% select(route_short_name) %>% pull() %>% length()
lrt.routes.2018 <- routes.2018 %>% filter(route_id%in%lrt.constrained) %>% select(route_short_name) %>% pull() %>% length()

freq.routes.2018 <- routes.2018 %>% filter(typology=="Very Frequent Route" | typology=="Frequent Route") %>% select(route_short_name) %>% pull() %>% length()
modfreq.routes.2018 <- routes.2018 %>% filter(typology=="Moderate Frequency Route") %>% select(route_short_name) %>% pull() %>% length()
lowfreq.routes.2018 <- routes.2018 %>% filter(typology=="Low Frequency Route") %>% select(route_short_name) %>% pull() %>% length()
bus.routes.2018 <- freq.routes.2018+modfreq.routes.2018+lowfreq.routes.2018

# 2050 Transit Route Summary ----------------------------------------------------
gtfs.2050.url <- here('gtfs//2050//')

routes.2050 <- as_tibble(fread(here(gtfs.2050.url,'fall-2050-routes-summary.csv'))) 
routes.2050$typology <- factor(routes.2050$typology, levels=hct.order)

brt.constrained.routes.2050 <- routes.2050 %>% filter(route_id%in%brt.constrained) %>% select(route_short_name) %>% pull() %>% length()
brt.unconstrained.routes.2050 <- routes.2050 %>% filter(route_id%in%brt.unconstrained) %>% select(route_short_name) %>% pull() %>% length()
brt.routes.2050 <- routes.2050 %>% filter(route_id%in%brt.full.plan) %>% select(route_short_name) %>% pull() %>% length()
pof.routes.2050 <- routes.2050 %>% filter(route_id%in%pof.constrained) %>% select(route_short_name) %>% pull() %>% length()
ferry.routes.2050 <- routes.2050 %>% filter(route_short_name%in%ferry.constrained) %>% select(route_short_name) %>% distinct() %>% pull() %>% length()
srt.routes.2050 <- routes.2050 %>% filter(route_id%in%srt.constrained) %>% select(route_short_name) %>% pull() %>% length()
monorail.routes.2050 <- routes.2050 %>% filter(route_id%in%monorail.constrained) %>% select(route_short_name) %>% pull() %>% length()
crt.routes.2050 <- routes.2050 %>% filter(route_id%in%crt.constrained) %>% select(route_short_name) %>% pull() %>% length()
lrt.routes.2050 <- routes.2050 %>% filter(route_id%in%lrt.constrained) %>% select(route_short_name) %>% pull() %>% length()

freq.routes.2050 <- routes.2050 %>% filter(typology=="Very Frequent Route" | typology=="Frequent Route") %>% select(route_short_name) %>% pull() %>% length()
modfreq.routes.2050 <- routes.2050 %>% filter(typology=="Moderate Frequency Route") %>% select(route_short_name) %>% pull() %>% length()
lowfreq.routes.2050 <- routes.2050 %>% filter(typology=="Low Frequency Route") %>% select(route_short_name) %>% pull() %>% length()
bus.routes.2050 <- freq.routes.2050+modfreq.routes.2050+lowfreq.routes.2050

psrc.taz <- st_read(here('data', 'taz_data.shp')) %>% st_transform(wgs84)

# Shapefiles 2018 --------------------------------------------------------------
transit.routes.lyr.2018 <- st_read(here('data', 'fall-2018-routes.shp')) %>% 
    st_transform(wgs84) %>%
    select(route_d,rt_lng_) %>%
    rename(line_id=route_d, line_name=rt_lng_) %>%
    distinct(line_id,line_name,.keep_all = TRUE)

transit.routes.lyr.2018 <- left_join(transit.routes.lyr.2018, routes.2018, by=c("line_id"="route_id"))

hct.routes.lyr.2018 <- transit.routes.lyr.2018 %>% filter(!(typology%in%bus.modes)) %>% mutate(agency_id=as.character(agency_id))
bus.routes.lyr.2018 <- transit.routes.lyr.2018 %>% filter(typology%in%bus.modes)

transit.supportive.zones.2018 <- psrc.taz %>% select(taz, AUDen18) %>% 
    st_transform(spn) %>%
    filter(AUDen18>=min.transit.density) %>%
    st_transform(wgs84) %>%
    mutate(AUDen18 = round(AUDen18,0)) %>%
    rename(`Activity Units` = AUDen18)

hct.supportive.zones.2018 <- psrc.taz %>% select(taz, AUDen18) %>% 
    st_transform(spn) %>%
    filter(AUDen18>=min.hct.density) %>%
    st_transform(wgs84) %>%
    mutate(AUDen18 = round(AUDen18,0)) %>%
    rename(`Activity Units` = AUDen18)

# Shapefiles 2050 ---------------------------------------------------------
transit.routes.lyr.2050 <- st_read(here('data', 'directions_2050.shp')) %>% 
    st_transform(wgs84) %>%
    select(line_id,line_name) %>%
    distinct(line_id,line_name,.keep_all = TRUE)

transit.routes.lyr.2050 <- left_join(transit.routes.lyr.2050, routes.2050, by=c("line_id"="route_id"))

hct.routes.lyr.2050 <- transit.routes.lyr.2050 %>% filter(!(typology%in%bus.modes)) %>% mutate(agency_id=as.character(agency_id))
bus.routes.lyr.2050 <- transit.routes.lyr.2050 %>% filter(typology%in%bus.modes)

transit.supportive.zones.2050 <- psrc.taz %>% select(taz, AUDen50) %>% 
    st_transform(spn) %>%
    filter(AUDen50>=min.transit.density) %>%
    st_transform(wgs84) %>%
    mutate(AUDen50 = round(AUDen50,0)) %>%
    rename(`Activity Units` = AUDen50)

hct.supportive.zones.2050 <- psrc.taz %>% select(taz, AUDen50) %>% 
    st_transform(spn) %>%
    filter(AUDen50>=min.hct.density) %>%
    st_transform(wgs84) %>%
    mutate(AUDen50 = round(AUDen50,0)) %>%
    rename(`Activity Units` = AUDen50)

ferry.routes.lyr <- hct.routes.lyr.2050 %>% filter(typology=="Ferry")
hct.routes.lyr.2018 <- bind_rows(hct.routes.lyr.2018, ferry.routes.lyr)

# PSRC Colors -------------------------------------------------------------
transit.pal <- colorFactor(
    palette = c("#91268F", "#C388C2", "#73CFCB", "#00A7A0", "#F05A28", "#F4835E", "#F7A489", "#8CC63E", "#F05A28", "#8CC63E", "#00A7A0", "#4C4C4C"),
    levels = c("BRT", "BRT - Unprogrammed", "Ferry", "Passenger Only Ferry", "Light Rail", "Streetcar", "Monorail", "Commuter Rail",  "Very Frequent Route", "Frequent Route", "Moderate Frequency Route", "Low Frequency Route" ))

# Functions ---------------------------------------------------------------
create.map <- function(hct.zones, trn.zones, hct.lyr, trn.lyr) {
    
    hct.labels <- paste0("Activity Units per Acre: ", prettyNum(round(hct.zones$`Activity Units`, 0), big.mark = ",")) %>% lapply(htmltools::HTML)
    nonhct.labels <- paste0("Activity Units per Acre: ", prettyNum(round(trn.zones$`Activity Units`, 0), big.mark = ",")) %>% lapply(htmltools::HTML)
    
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
        
        addPolygons(data=hct.zones,
                    fillOpacity = 0.5,
                    fillColor = "blue",
                    opacity = 0,
                    weight = 0,
                    color = "blue",
                    dashArray = "1",
                    highlight = highlightOptions(
                        weight =5,
                        color = "76787A",
                        dashArray ="",
                        fillOpacity = 0.7,
                        bringToFront = TRUE),
                    label = hct.labels,
                    labelOptions = labelOptions(
                        style = list("font-weight" = "normal", padding = "3px 8px"),
                        textsize = "15px",
                        direction = "auto"),
                    group = "High Capacity Transit Density") %>%
        
        addPolylines(data = hct.lyr,
                     color = ~transit.pal(typology),
                     weight = 3,
                     fillColor = ~transit.pal(typology),
                     group = "High Capacity Transit Routes") %>%
        
        addPolygons(data=trn.zones,
                    fillOpacity = 0.5,
                    fillColor = "cornflowerblue",
                    opacity = 0,
                    weight = 0,
                    color = "cornflowerblue",
                    dashArray = "1",
                    highlight = highlightOptions(
                        weight =5,
                        color = "76787A",
                        dashArray ="",
                        fillOpacity = 0.7,
                        bringToFront = TRUE),
                    label = nonhct.labels,
                    labelOptions = labelOptions(
                        style = list("font-weight" = "normal", padding = "3px 8px"),
                        textsize = "15px",
                        direction = "auto"),
                    group = "Local & Express Bus Density") %>%
        
        addPolylines(data = trn.lyr,
                     color = ~transit.pal(typology),
                     weight = 2,
                     fillColor = ~transit.pal(typology),
                     group = "Local & Express Bus Routes") %>%
    
        addLegend(pal = transit.pal,
              values = hct.lyr$typology,
              group = "High Capacity Transit Routes",
              position = "bottomleft",
              title = "High Capacity Transit Mode") %>%
        
        addLegend(pal = transit.pal,
                  values = trn.lyr$typology,
                  group = "Local & Express Bus Routes",
                  position = "bottomleft",
                  title = "Local & Express Bus") %>%
        
        addLegend(colors=c("blue"),
                  labels=c("Yes"),
                  group = "High Capacity Transit Density",
                  position = "bottomright",
                  title = "HCT Supportive Density") %>%
        
        addLegend(colors=c("cornflowerblue"),
                  labels=c("Yes"),
                  group = "Local & Express Bus Density",
                  position = "bottomright",
                  title = "Local Transit Supportive Density") %>%
        
        hideGroup("High Capacity Transit Routes") %>%
        hideGroup("Local & Express Bus Routes")

    return(m)
    
}

# User Interface for Dashboard --------------------------------------------
ui <- dashboardPage(skin = "green", title = "2022-2050 RTP",
                    dashboardHeader(title = "2022-2050 RTP Transit", titleWidth = '25%'),
    
    dashboardSidebar(
        sidebarMenu(
            br(),
            menuItem("Transit Routes: 2018", tabName = "transit-2018", icon = icon("bus")),
            menuItem("Transit Routes: 2050", tabName = "transit-2050", icon = icon("bus"))
        )
    ),
    
    dashboardBody(
        tabItems(

            # 2018 Network
            tabItem(tabName = "transit-2018",
                    fluidRow(column(width = 4,
                                    infoBoxOutput("brtBox18", width = NULL),
                                    infoBoxOutput("lrtBox18", width = NULL),
                                    infoBoxOutput("crtBox18", width = NULL),
                                    infoBoxOutput("ferryBox18", width = NULL),
                                    infoBoxOutput("busBox18", width = NULL)),
                             column(width = 8, box(title="Transit Routes and Density: 2018", width = NULL, leafletOutput("taz18map", width="100%", height=600))))
                    
                    
            ),
            
            # 2050 Network
            tabItem(tabName = "transit-2050",
                    fluidRow(column(width = 4,
                                    infoBoxOutput("brtBox50", width = NULL),
                                    infoBoxOutput("lrtBox50", width = NULL),
                                    infoBoxOutput("crtBox50", width = NULL),
                                    infoBoxOutput("ferryBox50", width = NULL),
                                    infoBoxOutput("busBox50", width = NULL)),
                             column(width = 8, box(title="Transit Routes and Density: 2050", width = NULL, leafletOutput("taz50map", width="100%", height=600))))
                    
            )
            
        ) # end of tab items for main body
        
    ) #end of dashboard body
)

#

# Server functions for Dashboard ------------------------------------------

server <- function(input, output) {
    
    output$brtBox18 <- renderInfoBox({
        
        infoBox(
            "Bus Rapid Transit", HTML(paste0(brt.constrained.routes.2018, " Routes")), icon = icon("bus"),
            color = "purple", fill=TRUE
        )
    })
    
    output$brtBox50 <- renderInfoBox({
        
        infoBox(
            "Bus Rapid Transit", HTML(paste0(brt.constrained.routes.2050, " Programmed Routes", br(), brt.unconstrained.routes.2050, " Unprogrammed Routes")), icon = icon("bus"),
            color = "purple", fill=TRUE
        )
    })
    
    output$lrtBox18 <- renderInfoBox({
        
        infoBox(
            "Light Rail", HTML(paste0(lrt.routes.2018, " Light Rail Routes" , br(), srt.routes.2018, " Streetcar Routes", br(), monorail.routes.2050, " Monorail Route")), icon = icon("subway"),
            color = "orange", fill=TRUE
        )
    })
    
    output$lrtBox50 <- renderInfoBox({
        
        infoBox(
            "Light Rail", HTML(paste0(lrt.routes.2050, " Light Rail Routes" , br(), srt.routes.2050, " Streetcar Routes", br(), monorail.routes.2050, " Monorail Route")), icon = icon("subway"),
            color = "orange", fill=TRUE
        )
    })
    
    output$crtBox18 <- renderInfoBox({
        
        infoBox(
            "Heavy Rail", paste0(crt.routes.2018, " Commuter Rail Routes"), icon = icon("train"),
            color = "green", fill=TRUE
        )
    })
    
    output$crtBox50 <- renderInfoBox({
        
        infoBox(
            "Heavy Rail", paste0(crt.routes.2050, " Commuter Rail Routes"), icon = icon("train"),
            color = "green", fill=TRUE
        )
    })
    
    output$ferryBox18 <- renderInfoBox({
        
        infoBox(
            "Ferries", HTML(paste0(pof.routes.2018, " Passenger-Only Routes", br(), ferry.routes.2050, " Ferry Routes")), icon = icon("ship"),
            color = "blue", fill=TRUE
        )
    })
    
    output$ferryBox50 <- renderInfoBox({
        
        infoBox(
            "Ferries", HTML(paste0(pof.routes.2050, " Passenger-Only Routes", br(), ferry.routes.2050, " Ferry Routes")), icon = icon("ship"),
            color = "blue", fill=TRUE
        )
    })
    
    output$busBox18 <- renderInfoBox({
        
        infoBox(
            "Bus", HTML(paste0(freq.routes.2018, " Frequent Bus Routes",
                               br(), modfreq.routes.2018, " Moderately Frequent Bus Routes",
                               br(), lowfreq.routes.2018, " Low Frequency Bus Routes")), icon = icon("bus"),
            color = "black", fill=TRUE
        )
    })
    
    output$busBox50 <- renderInfoBox({
        
        infoBox(
            "Bus", HTML(paste0(freq.routes.2050, " Frequent Bus Routes",
                               br(), modfreq.routes.2050, " Moderately Frequent Bus Routes",
                               br(), lowfreq.routes.2050, " Low Frequency Bus Routes")), icon = icon("bus"),
            color = "black", fill=TRUE
        )
    })
    
    output$taz18map <- renderLeaflet({create.map(hct.zones=hct.supportive.zones.2018, trn.zones=transit.supportive.zones.2018,
                                                 hct.lyr=hct.routes.lyr.2018, trn.lyr=bus.routes.lyr.2018)})
    
    output$taz50map <- renderLeaflet({create.map(hct.zones=hct.supportive.zones.2050, trn.zones=transit.supportive.zones.2050,
                                                 hct.lyr=hct.routes.lyr.2050, trn.lyr=bus.routes.lyr.2050)})
    
}

shinyApp(ui, server)
