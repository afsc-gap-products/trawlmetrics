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
  x = c(-163.5, -165, -168, -165),
  y = c(55.5, 55.8, 55.4, 56.5),
  lab = c("50 m", "100 m", "200 m", "Eastern Bering Sea")
) |>
  akgfmaps::transform_data_frame_crs(out.crs = "EPSG:3338")


p_sample_map <- 
  ggplot() +
  geom_sf(data = map_layers$bathymetry, color = "grey30", linewidth = 0.2) +
  geom_sf(data = map_layers$akland, color = NA, fill = "grey70") +
  geom_sf(
    data = locs, 
    mapping = aes(color = Phase, shape = Phase), 
    size = rel(2.2)
    ) +
  geom_shadowtext(
    data = bathy_labels,
    mapping = aes(x = x, y = y, label = lab), color = "grey30", bg.color = "white",
    size = 3) +
  scale_x_continuous(limits = map_layers$plot.boundary$x + c(2.5e5, -2e5)) + 
  scale_y_continuous(limits = map_layers$plot.boundary$y + c(-2e5, -5.5e5)) +
  scale_color_tableau() +
  scale_shape(solid = FALSE) +
  theme_bw() +
  theme(legend.position = "inside", 
        legend.position.inside = c(0.75, 0.08),
        legend.title = element_blank(),
        legend.background = element_blank(),
        axis.title = element_blank(),
        plot.margin = unit(c(2, 8, 2, 2), "mm"),
        axis.text = element_text(size = 8),
        legend.text = element_text(size = 8))

png(filename = here::here("plots", "map_sample_locations.png"), width = 120, height = 110, units = "mm", res = 300)
print(p_sample_map)
dev.off()
