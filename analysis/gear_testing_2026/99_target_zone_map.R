library(akgfmaps)
library(ggthemes)
library(dplyr)

map_layers <- akgfmaps::get_base_layers(select.region = "sebs", set.crs = "EPSG:3338")

prib_zone <- 
  dplyr::filter(
    map_layers$survey.grid, 
    STATION %in% c("E-22", 
                   paste0("F-", 22:25),
                   paste0("G-", 24:26),
                   "H-25", "H-26", "I-25", "I-26", "J-25", "J-26")
  ) |>
  dplyr::mutate(area = "Phase 2: Outer Pribiloff Zone")

prib_zone2 <- dplyr::filter(
  map_layers$survey.grid, 
  STATION %in% c(
    paste0("H-", 18:21),
    paste0("I-", 18:21),
    paste0("J-", 18:21),
    paste0("K-", 18:21)
  )
) |>
  dplyr::mutate(area = "Phase 2: Inner Pribiloff Zone")

dplyr::bind_rows(prib_zone, prib_zone2) |>
sf::st_drop_geometry() |>
  dplyr::select(AREA = area, STATION) |>
  write.csv(file = here::here("analysis", "gear_testing_2026", "catch_comparison_prib_zones.csv"), row.names = FALSE)

kusk_zone <- 
  dplyr::filter(
    map_layers$survey.grid, 
    STATION %in% c(
      paste0("K-0", 4:9),
      paste0("L-0", 4:9),
      paste0("M-0", 4:9),
      paste0("N-0", 4:9)
    ),
  ) |>
  dplyr::mutate(area = "Phase 2: Kuskokwim Zone")

dplyr::bind_rows(prib_zone, prib_zone2, kusk_zone) |>
  sf::st_drop_geometry() |>
  dplyr::select(AREA = area, STATION) |>
  write.csv(file = here::here("analysis", "gear_testing_2026", "2026_catch_comparison_stations.csv"), row.names = FALSE)

gear_testing_zone <- 
  sf::st_point(c(-165.3, 54.5)) |>
  sf::st_sfc(crs = "WGS84") |>
  sf::st_as_sf() |>
  sf::st_set_geometry("geometry") |>
  sf::st_buffer(dist = 70000, nQuadSegs = 1000) |>
  rmapshaper::ms_simplify(keep = 0.3, method = "dp") |>
  smoothr::smooth(method = "ksmooth") |>
  sf::st_transform(crs = "EPSG:3338") |>
  dplyr::mutate(area = "Phase 1: Gear Testing") |>
  dplyr::select(-rmapshaperid)

sf::st_write(
  obj = map_layers$survey.grid,
  dsn = here::here("analysis", "gear_testing_2026", "data", "shelf_survey_grid.shp"),
  append = FALSE
)

sample_zones <- 
  dplyr::bind_rows(kusk_zone, prib_zone, prib_zone2, gear_testing_zone) |>
  dplyr::group_by(area) |>
  summarise(do_union = TRUE)
  
sample_zones |>
  sf::st_make_valid() |>
  sf::st_write(
    dsn = here::here("analysis", "gear_testing_2026", "data", "2026_paired_tow_zones.shp"),
    append = FALSE
  )

ragg::agg_png(filename = here::here("analysis", "gear_testing_2026", "plots", "map_target_zones_2026.png"),
              width = 6, height = 4, units = "in", res = 300)
print(
  ggplot() +
    geom_sf(data = map_layers$survey.grid, fill = NA) +
    geom_sf(data = map_layers$survey.area, fill = NA) +
    geom_sf(data = sample_zones, 
            mapping = aes(fill = area),
            alpha = 0.7) +
    geom_sf(data = map_layers$akland, 
            linewidth = 0.2, 
            fill = "grey80", 
            color = "black") +
    scale_x_continuous(limits = map_layers$plot.boundary$x, 
                       breaks = map_layers$lon.breaks) +
    scale_y_continuous(limits = map_layers$plot.boundary$y, 
                       breaks = map_layers$lat.breaks) +
    scale_fill_colorblind(name = "Area") +
    theme_bw() +
    theme(legend.position = c(0.78, 0.82),
          legend.title = element_blank(),
          legend.text = element_text(size = 6.8))
)
dev.off()

