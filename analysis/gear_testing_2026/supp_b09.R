library(sf)
library(trawlmetrics)
library(dplyr)
library(ggplot2)
library(akgfmaps)
library(ggrepel)

load(here::here("data", "07_door_experiment_2023", "spread_height_data.rda"))

load(here::here("data", "07_door_experiment_2023", "bottom_contact_2023.rda"))

hauls$WIRE_LENGTH/hauls$BOTTOM_DEPTH

usable_width_treatments
usable_height_treatments

ggplot() +
  geom_point(
    data = usable_height_treatments,
    mapping = aes(x = HAUL, y = SCOPE_TO_DEPTH, color = factor(treatment))
  )

ggplot() +
  geom_point(
    data = usable_height_treatments,
    mapping = aes(x = BOTTOM_DEPTH, y = SCOPE_TO_DEPTH, color = factor(treatment))
  )

trawl_data_by_haul <- readRDS(file = here::here("data", "trawl_data_by_haul.rds"))

bcs_pings <- readRDS(here::here("data", "bcs_pings.rds")) |>
  dplyr::filter(HAUL %in% c(531:535))

bcs_data_by_haul <- readRDS(here::here("data", "bcs_data_by_haul.rds")) |>
  dplyr::filter(HAUL %in% c(531:535)) |>
  dplyr::left_join(trawl_data_by_haul)

ggplot() +
  geom_point(data = bcs_data_by_haul, 
             mapping = aes(x = SCOPE_RATIO, y = BCS_HEIGHT_CM, color = factor(BRIDLE_CHAIN_WEIGHT))) +
  facet_wrap(~DISTANCE)

ggplot() +
  geom_point(data = bcs_data_by_haul, 
             mapping = aes(x = SCOPE_RATIO, y = DOOR_ROLL_MAX)) +
  facet_wrap(~DISTANCE)

ggplot() +
  geom_point(data = bcs_data_by_haul, 
             mapping = aes(x = DOOR_ROLL_MAX, y = BCS_HEIGHT_CM)) +
  facet_wrap(~DISTANCE)

ggplot() +
  geom_point(data = bcs_data_by_haul, 
             mapping = aes(x = HAUL, y = DOOR_ROLL_MAX)) +
  facet_wrap(~DISTANCE)


haul_towpaths <- sf::st_read(here::here("data", "haul_towpaths.gpkg")) |>
  sf::st_transform(crs = "EPSG:3338") |>
  dplyr::filter(STATIONID == "B-09") |>
  dplyr::rename(geometry = geom)


# 1. Calculate Great-Circle Initial Bearing (in degrees, 0–360)
rad <- pi / 180
lat1 <- hauls$START_LATITUDE * rad
lat2 <- hauls$END_LATITUDE * rad
dlon <- (hauls$END_LONGITUDE - hauls$START_LONGITUDE) * rad

y <- sin(dlon) * cos(lat2)
x <- cos(lat1) * sin(lat2) - sin(lat1) * cos(lat2) * cos(dlon)
hauls$BEARING <- (atan2(y, x) * 180 / pi + 360) %% 360

# 2. Bin into the 16-wind compass rose (22.5° per sector, centered on each direction)
compass_16 <- c(
  "N", "NNE", "NE", "ENE", 
  "E", "ESE", "SE", "SSE", 
  "S", "SSW", "SW", "WSW", 
  "W", "WNW", "NW", "NNW"
)

# Offset by 11.25° so North covers [348.75°, 11.25°)
idx <- (floor((hauls$BEARING + 11.25) / 22.5) %% 16) + 1
hauls$DIRECTION <- compass_16[idx]

# Assign NA if start and end coordinates are identical (zero-length line)
stationary <- (hauls$START_LATITUDE == hauls$END_LATITUDE) & 
  (hauls$START_LONGITUDE == hauls$END_LONGITUDE)
hauls$DIRECTION[stationary] <- NA_character_
hauls$BEARING[stationary] <- NA_real_

# 3. Create LINESTRING geometries from start/end coordinates
coords_mat <- as.matrix(hauls[, c("START_LONGITUDE", "START_LATITUDE", 
                                  "END_LONGITUDE",   "END_LATITUDE")])

lines <- lapply(seq_len(nrow(coords_mat)), function(i) {
  sf::st_linestring(matrix(coords_mat[i, ], ncol = 2, byrow = TRUE))
})

hauls_sf <- sf::st_sf(hauls, geometry = sf::st_sfc(lines, crs = 4326)) |>
  sf::st_transform(crs = "EPSG:3338")

# Combine 2023 and 2026 towpaths
b09_hauls <- dplyr::filter(hauls_sf, STATIONID == "B-09") |>
  dplyr::bind_rows(
    haul_towpaths 
  )

start_coords <- sf::st_coordinates(lwgeom::st_startpoint(b09_hauls))
b09_hauls$start_x <- start_coords[, "X"]
b09_hauls$start_y <- start_coords[, "Y"]

bbox <- sf::st_bbox(b09_hauls)

map_layers <- akgfmaps::get_base_layers(select.region = "sebs", set.crs = "EPSG:3338")

ggplot() +
  geom_sf(
    data = b09_hauls,
    mapping = aes(color = DIRECTION, linetype = factor(floor(CRUISE/100))),
    alpha = 0.7,
    linewidth = 1
  ) +
  geom_sf(data = map_layers$akland, color = NA) +
  scale_linetype(name = "Year") +
  scale_x_continuous(limits = c(bbox["xmin"] - 5000, bbox["xmax"] + 5000)) +
  scale_y_continuous(limits = c(bbox["ymin"] - 20000, bbox["ymax"] + 5000)) +
  scale_color_discrete(name = "Direction") +
  theme_bw() +
  theme(axis.title = element_blank())


ggplot() +
  geom_sf(
    data = b09_hauls,
    mapping = aes(color = DIRECTION, linetype = factor(floor(CRUISE/100)))
  ) +
  ggrepel::geom_text_repel(
    data = b09_hauls,
    mapping = aes(x = start_x, y = start_y, label = HAUL),
    max.overlaps = Inf
  ) +
  geom_sf(data = map_layers$akland, color = NA) +
  facet_wrap(~DIRECTION, ncol = 1) +
  scale_linetype(name = "Year") +
  scale_x_continuous(limits = c(bbox["xmin"], bbox["xmax"])) +
  scale_y_continuous(limits = c(bbox["ymin"], bbox["ymax"])) +
  scale_color_discrete(name = "Direction") +
  theme_bw() +
  theme(axis.title = element_blank())
