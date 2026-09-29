# Sample map

library(trawlmetrics)
library(akgfmaps)
library(ggthemes)
library(shadowtext)

channel <- trawlmetrics::get_connected(schema = "AFSC")

locs <- 
  RODBC::sqlQuery(
    query = "select * from racebase.haul where vessel = 134 and cruise = 202601 and haul_type in (7, 23)", 
    channel = channel) |>
  sf::st_as_sf(crs = "WGS84", coords = c("START_LONGITUDE", "START_LATITUDE")) |>
  sf::st_transform(crs = "EPSG:3338") |>
  dplyr::mutate(
    Phase = dplyr::case_when(HAUL_TYPE == 7 ~ "Phase 1: Gear trials",
                             HAUL_TYPE == 23 ~ "Phase 2: Catch comparison")
  )

map_layers <- akgfmaps::get_base_layers(select.region = "sebs", set.crs = "EPSG:3338")

bathy_labels <- data.frame(
  x = c(-163, -165, -168),
  y = c(55.5, 55.8, 55.4),
  lab = c("50 m", "100 m", "200 m")
) |>
  akgfmaps::transform_data_frame_crs(out.crs = "EPSG:3338")


p_sample_map <- 
  ggplot() +
  geom_sf(data = map_layers$bathymetry, color = "grey40", linewidth = 0.2) +
  geom_sf(data = map_layers$akland, color = NA, fill = "grey85") +
  geom_sf(
    data = locs, 
    mapping = aes(color = Phase, shape = Phase), 
    size = rel(3)
    ) +
  geom_shadowtext(
    data = bathy_labels,
    mapping = aes(x = x, y = y, label = lab), color = "grey40", bg.color = "white") +
  scale_x_continuous(limits = map_layers$plot.boundary$x + c(2.5e5, -2e5)) + 
  scale_y_continuous(limits = map_layers$plot.boundary$y + c(-2e5, -5.5e5)) +
  scale_color_tableau() +
  scale_shape(solid = FALSE) +
  theme_bw() +
  theme(legend.position = "inside", 
        legend.position.inside = c(0.75, 0.08),
        legend.title = element_blank(),
        legend.background = element_blank(),
        axis.title = element_blank())

png(filename = here::here("plots", "map_sample_locations.png"), width = 120, height = 120, units = "mm", res = 300)
print(p_sample_map)
dev.off()
