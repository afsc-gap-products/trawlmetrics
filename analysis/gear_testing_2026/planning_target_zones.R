trawlmetrics::bts_geom |>
  dplyr::filter(SURVEY_DEFINITION_ID == 98, CRUISE == 202401, VESSEL_ID == 162, HAUL %in% c(34,47,19, 20, 21, 23, 24, 33, 49, 50, 51, 55, 58))


library(akgfmaps)
library(terra)
library(tidyterra)

# Catch comparison zone near Amak 

ssl_ntz <- sf::st_read(dsn = "G:/RACE_CHARTS/AI/shapefiles/SSLrookeries/3nm_notransit.shp")

map_layers <- akgfmaps::get_base_layers(select.region = "sebs", set.crs = "EPSG:3338")

flatfish_zone <- 
  dplyr::filter(
    map_layers$survey.grid,
    STATION %in% c("B-09", "A-09", "B-08", "A-08", "C-08", "C-09")
  )

amak_coords <- st_point(c(-1*(163+8/60+38/3600), 55+25/60+6/3600)) |>
  sf::st_sfc() |>
  sf::st_as_sf(crs = "WGS84") |>
  sf::st_transform(crs = "EPSG:3338") |>
  dplyr::mutate(label = "Amak")

amak_buffer <- sf::st_buffer(amak_coords, dist = 100000)

amak_bbox <- sf::st_bbox(amak_buffer)

bathy_raster <- terra::rast(system.file("./extdata/bathymetry.gpkg", package = "akgfmaps"))
bathy_raster <- terra::mask(bathy_raster, amak_bbox)

ggplot() +
  geom_spatraster(
    data = bathy_raster, 
    mapping = aes(fill = Height)
  ) +
  geom_sf(
    data = map_layers$akland, 
    color = NA, 
    fill = "grey80"
  ) +
  geom_sf(
    data = map_layers$survey.grid, 
    fill = NA, 
    color = "black"
  ) +
  geom_sf(
    data = amak_coords, 
    size = rel(0.2)
  ) +
  geom_sf_text(
    data = amak_coords, 
    mapping = aes(label = label), 
    hjust = -0.1
  ) +
  geom_sf(
    data = flatfish_zone, 
    fill = NA, 
    mapping = aes(color = "Flatfish Zone")
  ) +
  geom_sf(
    data = ssl_ntz, 
    alpha = 0.5, 
    mapping = aes(color = "SSL NTZ")
  ) +
  scale_x_continuous(
    limits = c(amak_bbox[['xmin']], amak_bbox[['xmax']]), 
    expand = c(0,0), 
    oob = scales::squish_infinite
  ) +
  scale_y_continuous(
    limits = c(amak_bbox[['ymin']], amak_bbox[['ymax']]), 
    expand = c(0,0), 
    oob = scales::squish_infinite
  ) +
  scale_color_manual(
    name = NULL, 
    values = c("SSL NTZ" = "red", "Flatfish Zone" = "blue")
  ) +
  # scale_fill_manual(name = NULL, values = c("SSL NTZ" = "red")) +
  scale_fill_fermenter(
    name = "Depth (m)", 
    breaks = c(0, 10, 25, 50, 100, 200), 
    direction = 1,
    na.value = NA
  ) +
  theme_bw() +
  theme(axis.title = element_blank())


# Length-frequency


channel <- trawlmetrics::get_connected(schema = "AFSC")

amak_lengths <- RODBC::sqlQuery(
  channel = channel,
  query = "select h.performance, h.haul_type, h.haul, h.vessel, h.cruise, h.bottom_depth, h.stationid as station, tc.common_name,
l.species_code, l.length, l.frequency
from racebase.haul h, racebase.length l, race_data.taxonomic_classification tc
where stationid in ('B-09', 'A-08', 'C-09', 'B-08')
and h.hauljoin = l.hauljoin
and l.species_code between 471 and 39000
and h.cruise > 200500
and tc.species_code = l.species_code;"
  )

select * from racebase.haul where stationid in ('B-09', 'A-08')


select h.performance, h.haul_type, h.haul, h.vessel, h.cruise, h.bottom_depth, tc.common_name,
c.species_code, c.number_fish, c.weight
from racebase.haul h, racebase.catch c, race_data.taxonomic_classification tc
where stationid in ('B-09', 'A-08', 'C-09', 'B-08')
and h.hauljoin = c.hauljoin
and c.species_code between 400 and 39000
and h.cruise > 200500
and tc.species_code = c.species_code;


select h.performance, h.haul_type, h.haul, h.vessel, h.cruise, h.bottom_depth, tc.common_name,
l.species_code, l.length, l.frequency
from racebase.haul h, racebase.length l, race_data.taxonomic_classification tc
where stationid in ('B-09', 'A-08', 'C-09', 'B-08')
and h.hauljoin = l.hauljoin
and l.species_code between 471 and 39000
and h.cruise > 200500
and tc.species_code = l.species_code;

ggplot() +
  geom_bar(
    # data = amak_lengths,
    data = dplyr::filter(amak_lengths, STATION %in% c("B-09", "A-08")),
    mapping = aes(x = LENGTH, y = FREQUENCY, group = LENGTH), 
    stat = "identity") +
facet_wrap(~COMMON_NAME, scales = "free")

