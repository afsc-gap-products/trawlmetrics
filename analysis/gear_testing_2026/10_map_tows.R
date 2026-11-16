# Sample map
source("00_functions.R")

loc_points <- sf::st_read(here::here("data", "haul_start_locations.gpkg"))

loc_points <- dplyr::mutate(
  loc_points,
  CODEND = ifelse(HAUL_TYPE == 7, "Open", "Closed")
  
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
    data = loc_points, 
    mapping = aes(shape = CODEND), 
    size = rel(2.2),
    alpha = 0.7,
    color = "#F28E2B"
    ) +
  geom_shadowtext(
    data = bathy_labels,
    mapping = aes(x = x, y = y, label = lab), color = "grey30", bg.color = "white",
    size = 3) +
  scale_x_continuous(limits = map_layers$plot.boundary$x + c(2.5e5, -2e5)) + 
  scale_y_continuous(limits = map_layers$plot.boundary$y + c(-2e5, -5.5e5)) +
  scale_shape_manual(name = "Codend", values = c('Closed' = 19, 'Open' = 21)) +
  theme_bw() +
  theme(legend.position = "inside", 
        legend.position.inside = c(0.84, 0.13),
        legend.title = element_text(size = 9),
        # legend.background = element_blank(),
        axis.title = element_blank(),
        plot.margin = unit(c(2, 8, 2, 2), "mm"),
        axis.text = element_text(size = 8),
        legend.text = element_text(size = 8))

ggsave(
  p_sample_map,
  filename = here::here("plots", "map_sample_locations.png"),
  width = 120, 
  height = 100, 
  units = "mm", 
  dpi = 300
)

