# Packages 

require(units)
require(dplyr)
require(ggplot2)
require(move2)
require(rnaturalearth)
require(leaflet)
require(htmlwidgets)

# Set credentials
move2::movebank_store_credentials("etienne_hy", "Miliora_1998!")


#' Download white storck tracking data from the following study: 
#' "Rotics S, Kaatz M, Turjeman SF, Zurell D, Wikelski M, Sapir N, Eggers U, Fiedler W, Jeltsch F, Nathan R. 
#' 2018. Data from: Early arrival at breeding grounds: causes, costs and a trade-off with overwintering 
#' latitude. Movebank Data Repository. https://doi.org/10.5441/001/1.v8d24552"
track <- move2::movebank_download_study("Eastern flyway spring migration of adult white storks (data from Rotics et al. 2018)",
                                 sensor_type_id = "gps",
                                 'license-md5'='59460dc473398b1edfa1abc423b0865d')
# Filter for X individuals
track1 <- track %>%
  dplyr::filter(individual_local_identifier %in% 
                  c(levels(track$individual_local_identifier)[1],
                    levels(track$individual_local_identifier)[2],
                    levels(track$individual_local_identifier)[3]))

# Drop levels 
track1$individual_local_identifier <- droplevels(track1$individual_local_identifier)

track_sf <- sf::st_as_sf(track1)


track_sf <- tidyr::separate(track_sf, col=timestamp, into=c('dt', 'time'), sep=' ', remove = FALSE)
track_sf$dt <- lubridate::as_date(track_sf$dt)

track_sf$year <- lubridate::year(track_sf$dt)

select_r <- by(track_sf, list(id = track_sf$dt), function(x){
  idx <- sample(seq(nrow(x)), size = 1)
  x[idx, ]
})

track_sf_r <- do.call("rbind", select_r)


track_sf_lines <- track_sf_r %>%
  group_by(individual_local_identifier,year) %>%
  arrange(dt) %>%
  summarise(geometry = sf::st_combine(geometry)) %>%
  sf::st_cast("LINESTRING")


pal <- leaflet::colorFactor(
  palette = "Dark2", 
  domain = track_sf_r$individual_local_identifier
)


map_track <- leaflet::leaflet(track_sf_r) %>%
  leaflet::addTiles() %>% 
  leaflet::addProviderTiles("Esri.WorldImagery") %>%
  leaflet::addCircleMarkers(
    radius = 3,
    color = ~pal(individual_local_identifier),
    fillColor = ~pal(individual_local_identifier),
    fillOpacity =1,
    popup = ~paste0(
      "<strong>Bird: </strong>", individual_local_identifier, "<br>",
      "<strong>Date: </strong>", timestamp
    )) %>%
  leaflet::addPolylines(
    data = track_sf_lines,
    color = ~pal(individual_local_identifier),
    weight = 2,
    opacity = 0.8 
  ) %>%
  leaflet::addLegend(
    "bottomright",
    pal = pal,
    values = ~individual_local_identifier,
    title = "White stork ID",
    opacity = 1
  )

# Save leaflet map 
htmlwidgets::saveWidget(map_track, file="figures/map_track.html")

