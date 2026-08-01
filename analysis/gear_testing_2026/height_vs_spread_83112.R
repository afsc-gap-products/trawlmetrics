library(akgfmaps)
library(trawlmetrics)
library(ggthemes)
library(cowplot)


base_layers <- 
  akgfmaps::get_base_layers(select.region = "sebs", set.crs = 3338)

ebs_geom <- 
  trawlmetrics::bts_geom |>
  dplyr::filter(SURVEY_DEFINITION_ID == 98, NET_MEASURED == TRUE,  YEAR > 2010) |>
  dplyr::mutate(HEIGHT_TO_SPREAD = NET_WIDTH_M/NET_HEIGHT_M) |>
  dplyr::group_by(STATION) |>
  dplyr::summarise(
    MEAN_DEPTH = mean(DEPTH_M, na.rm = TRUE),
    MEAN_SPREAD = mean(NET_WIDTH_M, na.rm = TRUE),
    MEAN_HEIGHT = mean(NET_HEIGHT_M, na.rm = TRUE),
    MEAN_HTS = mean(HEIGHT_TO_SPREAD, na.rm = TRUE)
  )


ebs_geom <- 
  dplyr::inner_join(base_layers$survey.grid, ebs_geom)


p_ratio <- 
  ggplot() +
  geom_sf(data = base_layers$akland) +
  geom_sf(
    data = ebs_geom,
    mapping = aes(fill = MEAN_HTS)
  ) +
  scale_fill_viridis_c(name = "Height:Spread ratio") +
  scale_x_continuous(breaks = base_layers$lon.breaks, limits = base_layers$plot.boundary$x) +
  scale_y_continuous(breaks = base_layers$lat.breaks, limits = base_layers$plot.boundary$y) +
  ggtitle("Height-to-spread ratio") +
  theme_bw() +
  theme(legend.position = "bottom")

p_spread <- 
  ggplot() +
  geom_sf(data = base_layers$akland) +
  geom_sf(
    data = ebs_geom,
    mapping = aes(fill = MEAN_SPREAD)
  ) +
  scale_fill_viridis_c(name = "Spread (m)", option = "E") +
  scale_x_continuous(breaks = base_layers$lon.breaks, limits = base_layers$plot.boundary$x) +
  scale_y_continuous(breaks = base_layers$lat.breaks, limits = base_layers$plot.boundary$y) +
  ggtitle("Upper Wing-tip Spread") +
  theme_bw() +
  theme(legend.position = "bottom")

p_height <- 
  ggplot() +
  geom_sf(data = base_layers$akland) +
  geom_sf(
    data = ebs_geom,
    mapping = aes(fill = MEAN_HEIGHT)
  ) +
  scale_fill_viridis_c(name = "Height (m)", option = "A") +
  scale_x_continuous(breaks = base_layers$lon.breaks, limits = base_layers$plot.boundary$x) +
  scale_y_continuous(breaks = base_layers$lat.breaks, limits = base_layers$plot.boundary$y) +
  ggtitle("Headrope height") +
  theme_bw() +
  theme(legend.position = "bottom")


cowplot::plot_grid(
  p_spread,
  p_height,
  p_ratio, nrow = 1,
  align = "hv"
)
