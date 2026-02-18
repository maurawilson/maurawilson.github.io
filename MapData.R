# Load libraries
library("dplyr")
library("stringr")
library("ggplot2")
library("tidyverse")
library("ggmap")
#install.packages("leaflet")
library("leaflet")
#install.packages("maps")
library("maps")
library("sf")
library("cartogram")
#install.packages("giscoR")
library("giscoR")
library("plotly")
library("htmlwidgets")

#load in data for participants
map_data <- read.csv(
  "C:/Documents/MLIS/CALMA Black Information Futures Symposium Files/BIF_Cities_and_LatLong.csv",
  header = TRUE
)
bif_regions <- read.csv("C:/Documents/MLIS/CALMA Black Information Futures Symposium Files/BIF_regions.csv")
bif_cities <- read.csv(
  "C:/Documents/MLIS/CALMA Black Information Futures Symposium Files/BIF_cities_count.csv"
)


# Map data
gisUSA <- gisco_get_countries(
  year = 2024,
  country = "USA",
  update_cache = FALSE,
  resolution = 01
)
gisCAN <- gisco_get_countries(
  year = 2024,
  country = "CAN",
  update_cache = FALSE,
  resolution = 01
)
gisUK <- gisco_get_countries(
  year = 2024,
  country = "UK",
  update_cache = FALSE,
  resolution = 01
)
gisEur <- gisco_get_nuts(
  year = 2024,
  epsg = 4326,
  country = c("IE", "FR", "ES", "PT"),
  update_cache = FALSE,
  resolution = 01,
  spatialtype = "RG",
  nuts_level = 0
)
states <- map_data("state")
#join states and bif_states
states <- left_join(states, bif_regions, by = c("region"))


#### interactive bubble map - no lat/long lines ####
noLatLong <- ggplot() +
  geom_sf(data = gisUSA,
          fill = 'coral4',
          alpha = 0.4) +
  geom_sf(data = gisCAN,
          fill = 'coral4',
          alpha = 0.9) +
  geom_sf(data = gisEur,
          fill = "coral3",
          alpha = 0.6) +
  geom_sf(data = gisUK,
          fill = 'coral4',
          alpha = 0.7) +
  geom_point(
    data = bif_cities,
    aes(
      x = Long,
      y = Lat,
      city = City,
      size = ParticipantNumber,
      color = ParticipantNumber
    )
  ) +
  xlim(-180, 20) +
  scale_size_continuous(range = c(1, 5)) +
  scale_color_viridis_c(transform = "log") +
  labs(title = "Symposium Participants by City", x = "Longitude", y = "Latitude") +
  theme_void()

ggplotly(noLatLong, tooltip = c("city", "size"))

#### interactive bubble map -  with lat/long lines ####
latlong <- ggplot() +
  geom_sf(data = gisUSA,
          fill = 'coral4',
          alpha = 0.4) +
  geom_sf(data = gisCAN,
          fill = 'coral4',
          alpha = 0.9) +
  geom_sf(data = gisUK,
          fill = 'coral4',
          alpha = 0.7) +
  geom_sf(data = gisEur,
          fill = "coral3",
          alpha = 0.6) +
  geom_point(
    data = bif_cities,
    aes(x = Long, y = Lat, city = City,       size = ParticipantNumber,
      color = ParticipantNumber
    )
  ) +
  xlim(-180, 20) +
  ylim(20, 75) +
  scale_size_continuous(range = c(1, 5)) +
  scale_color_viridis_c(transform = "log") +
  labs(title = "Symposium Participants by City", x = "Longitude", y = "Latitude")

ggplotly(latlong, tooltip = c("city", "size"))


#### interactive map by state ####
# with latitude and longitude lines 
statesCount <- ggplot(
  data = states, 
  aes(
    x=long, 
    y=lat,  
    group=group,
    region = region,
    fill = count
    )
  ) +
  geom_polygon(
    color = 'azure3'
  ) +
  scale_fill_continuous(palette = "oranges") +
  labs(
    title = "Symposium Participants by State", 
    x = "Longitude", 
    y = "Latitude"
  ) +
  coord_fixed(ratio=1.35)
  

ggplotly(statesCount, tooltip = c("region", "count"))


#### Save HTML ####
#save city data without latitude and longitude
saveWidget(
  ggplotly(
    latlong, 
    tooltip = c("city", "size")
    ), 
  file = "C:/Documents/MLIS/CALMA Black Information Futures Symposium Files/BIF_by_city_nolatlong.html"
  )

#save city data with latitude and longitude
saveWidget(
  ggplotly(
    noLatLong, 
    tooltip = c("city", "size")
  ), 
  file = "C:/Documents/MLIS/CALMA Black Information Futures Symposium Files/BIF_cities_count.html"
)

#save US state data with latitude and longitude
saveWidget(
  ggplotly(
    statesCount,
    tooltip = c("region","count")
  ),
  file ="C:/Documents/MLIS/CALMA Black Information Futures Symposium Files/statesCount.html"
)
