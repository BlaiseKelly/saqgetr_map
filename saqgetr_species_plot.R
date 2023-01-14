## script for creating a interactive leaflet plots for species groups showing data available from the saqgetr database

library(leaflet)
library(sf)
library(randomcoloR)
library(saqgetr)
library(tidyverse)
library(htmlwidgets)
library(lubridate)

##define coordinate systems
latlong = "+init=epsg:4326"
rdnew = "+init=epsg:28992"

select <- dplyr::select

## get simple site summary for all sites in saqgetr
site_sums <- get_saq_simple_summaries(summary = "annual_means")

## get all sites from database
sitez <- saqgetr::get_saq_sites()

## get more detailed data
processes <- get_saq_processes()

## combine and convert to sf
site_sums_sf <- sitez %>% 
  left_join(processes, by = 'site') %>% 
  filter(!is.na(latitude)) %>% 
  mutate(variable_long = tolower(variable_long)) %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = latlong)

## generate species groups

## bin black carbon species
bc1 <- site_sums_sf %>%
  filter(variable == "ec_in_pm2.5"|variable == "ec_in_pm10" |variable == "bc")
bc2 <- site_sums_sf %>% 
  filter(grepl(paste(c("black", "smoke", "organic carbon"), collapse = "|"), variable_long))
bc <- rbind(bc1, bc2)

## PM
pm <- site_sums_sf %>% 
  filter(!variable %in% bc$variable | !variable_long %in% bc$variable_long) %>% 
  filter(!grepl(paste(c("in pm"), collapse = "|"), variable_long)) %>% 
  filter(grepl(paste(c("pm10", "pm2.5", "tsp", "spm"), collapse = "|"), variable))

## in PM
in_pm <- site_sums_sf %>% 
  filter(!variable %in% bc$variable | !variable_long %in% bc$variable_long) %>% 
  filter(grepl(paste(c("in pm", "in tsp"), collapse = "|"), variable_long))

## carbon
carbon <- site_sums_sf %>% 
  filter(!variable %in% bc$variable | !variable_long %in% bc$variable_long) %>% 
  filter(grepl("carbon", variable_long))

nitrogen <- filter(site_sums_sf,grepl(paste(c("nitrogen", "ammonia"), collapse = "|"), variable_long))

ozone <- filter(site_sums_sf,grepl("ozone", variable_long))

sulphur <- filter(site_sums_sf,grepl("sulphur", variable_long))

## get full periodic table
PT <- read.csv("https://gist.githubusercontent.com/GoodmanSciences/c2dd862cd38f21b0ad36b8f96b4bf1ee/raw/1d92663004489a5b6926e944c1b3d9ec5c40900e/Periodic%2520Table%2520of%2520Elements.csv")

## group solids metals and other
solid <- PT %>% 
  select(Element, Phase) %>% 
  mutate(Element = tolower(Element)) %>% 
  filter(Phase == "solid")

metals <- site_sums_sf %>% 
  filter(!grepl("pm", variable)) %>% 
  filter(!grepl("in_pm", variable)) %>%  
  filter(!grepl("carbon", variable_long)) %>% 
  filter(!grepl("nitrogen", variable_long)) %>% 
  filter(grepl(paste(solid$Element, collapse = "|"), variable_long))

others <- site_sums_sf %>% 
  filter(!variable %in% bc$variable &
           !variable %in% pm$variable & 
           !variable %in% in_pm$variable & 
           !variable_long %in% carbon$variable_long &
           !variable_long %in% nitrogen$variable_long &
           !variable_long %in% metals$variable_long &
           !variable_long %in% ozone$variable_long &
           !variable_long %in% sulphur$variable_long)

lat <- 51.4
lon <- 7.2

## define categories
cats <- c("bc", "pm", "in_pm", "metals", "sulphur", "ozone", "nitrogen", "others", "carbon")

## set palette for site types
pal_g <- colorFactor("Set3", reverse = FALSE, domain = site_sums_sf$site_type)

##plot on a map

for (c in cats){
 
  ## tryCatch({
  cat <- get(c)

## create base base
m <- leaflet() %>% 
  setView(lon, lat, zoom = 6) %>% 
  addProviderTiles('CartoDB.Positron')
  
## find unique variables
  variable_u <- unique(cat$variable_long)
  
  ## find how many variable colours are needed
  n <- NROW(variable_u)
  ## generate a palette of for all variables
  palette <- distinctColorPalette(n)

## loop through the variables to generate ability to toggle between species
for (u in variable_u){
  
  p <- palette[which(variable_u == u)]
  
  df <- filter(cat, variable_long == u)

m <- m %>% addCircleMarkers(data = df, fillColor = ~pal_g(site_type), color = 'black', weight = 1,
                            opacity = 1.0, fillOpacity = 1.0,
                            popup = paste("site code:", df$site, "<br>",
                                          "site name:", df$site_name, "<br>",
                                          "site type:", df$site_type, "<br>",
                                          "date start:", df$date_start.y, "<br>",
                                          "date end:", df$date_end.y, "<br>",
                                          "data source:", df$data_source.y, "<br>",
                                          "sampling process:", df$sampling_process, "<br>",
                                          "sampling process:", df$sampling_point, "<br>",
                                          "observation count:", df$observation_count.y, "<br>",
                                          "sampling period:", df$period, "<br>"), group = u)

}

m <- m %>% addLegend("bottomleft", pal=pal_g, values=site_sums_sf$site_type, opacity=1, title = "site type",
                       group = "site type")

m <- m %>% addLayersControl(baseGroups = c(variable_u),
                            options = layersControlOptions(collapsed = FALSE))

n <- sprintf('%02d', which(cats == c))

## use html widget saveWidget function to make a standalone html
withr::with_dir('./', saveWidget(m, file = paste0(n, "_", c, ".html")))  

## error catch in case one group gets an error
##  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}
