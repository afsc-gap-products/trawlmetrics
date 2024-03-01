# remotes::install_github(repo = "afsc-gap-products/postsurvey_hauldata_processing")
# install.packages("plotly", "ggthemes")

library(akgfmaps)
library(trawlmetrics)

map_layers <- akgfmaps::get_base_layers(select.region = "sebs", set.crs = "EPSG:3338")

channel <- trawlmetrics::get_connected(schema = "AFSC")

hauls <- RODBC::sqlQuery(channel = channel,
                         query = "select * from racebase.haul where vessel = 162 and cruise = 202301 and haul_type = 7")

towpaths <- dplyr::bind_rows(
  hauls |>
    dplyr::select(VESSEL, CRUISE, HAUL, STATIONID, BOTTOM_DEPTH, START_LATITUDE, START_LONGITUDE) |>
    sf::st_as_sf(coords =c ("START_LONGITUDE", "START_LATITUDE"), crs = "EPSG:4326"),
  hauls |>
    dplyr::select(VESSEL, CRUISE, HAUL, STATIONID, BOTTOM_DEPTH, END_LATITUDE, END_LONGITUDE) |>
    sf::st_as_sf(coords =c ("END_LONGITUDE", "END_LATITUDE"), crs = "EPSG:4326")
) |>
  dplyr::group_by(VESSEL, CRUISE, HAUL, STATIONID, BOTTOM_DEPTH) |> 
  dplyr::summarize(do_union = FALSE) |> 
  sf::st_transform(crs = "EPSG:3338") |>
  sf::st_cast("LINESTRING")

tow_start <- hauls |>
  dplyr::select(VESSEL, CRUISE, HAUL, STATIONID, BOTTOM_DEPTH, START_LATITUDE, START_LONGITUDE) |>
  sf::st_as_sf(coords =c ("START_LONGITUDE", "START_LATITUDE"), crs = "EPSG:4326") |>
  sf::st_transform(crs = "EPSG:3338")

extended_grid <- sf::st_read(system.file("./extdata/bs_grid_w_corners.shp", package = "akgfmaps")) |>
  sf::st_transform(crs = "EPSG:3338") |>
  dplyr::filter(STATIONID %in% c("B-09", "C-09", "C-10", "B-10", "Z-04"))

z04 <- sf::st_difference(dplyr::filter(extended_grid, STATIONID == "Z-04"),
            dplyr::filter(map_layers$survey.grid, STATIONID == "AZ0504") |>
              dplyr::select(-STATIONID) |>
              sf::st_buffer(15))

project_grid <- extended_grid |>
  dplyr::filter(STATIONID != "Z-04") |> 
  dplyr::bind_rows(z04,
                   map_layers$survey.grid)

tows_by_station <- sf::st_intersection(dplyr::select(towpaths, -STATIONID), 
                                       project_grid)

tow_grid <- dplyr::filter(project_grid, STATIONID %in% unique(tows_by_station$STATIONID))

ggplot() +
  geom_sf(data = tow_grid,
          mapping = aes(color = STATIONID)) +
  geom_sf(data = tows_by_station,
          mapping = aes(color = STATIONID),
          linewidth = 1.5) +
  geom_sf_text(data = sf::st_centroid(tow_grid),
          mapping = aes(color = STATIONID,
                        label = STATIONID)) +
  geom_sf_text(data = tow_start,
               mapping = aes(label = HAUL))


correct_station <- as.data.frame(tows_by_station) |>
  dplyr::select(-geometry) |>
  dplyr::arrange(HAUL) |>
  dplyr::rename(CORRECT_STATIONID = STATIONID) |>
  dplyr::inner_join(dplyr::select(hauls, VESSEL, CRUISE, HAUL, STATIONID)) |>
  dplyr::rename(CURRENT_STATIONID = STATIONID) |>
  dplyr::mutate(MATCHING = CURRENT_STATIONID == CORRECT_STATIONID)


write.csv(x = correct_station, file = "correct_station.csv", row.names = FALSE)

# write.csv(x = correct_station, file = here::here("analysis", "door_experiment", "output", "correct_station.csv"), row.names = FALSE)

