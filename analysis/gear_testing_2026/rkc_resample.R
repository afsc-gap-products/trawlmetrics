library(trawlmetrics)
library(dplyr)
library(akgfmaps)


channel <- trawlmetrics::get_connected(schema = "AFSC")

resample <- RODBC::sqlQuery(
  channel = channel,
                            query = "select * from racebase.haul where haul_type = 17"
  ) |>
  dplyr::rename(STATION = STATIONID) |>
  dplyr::mutate(YEAR = floor(CRUISE/100)) |>
  dplyr::select(-STRATUM)


map_layers <- akgfmaps::get_base_layers(select.region = "sebs", set.crs = "EPSG:3338")

resample_sf <- dplyr::inner_join(map_layers$survey.grid, resample)

resample_map <- 
  ggplot() +
  geom_sf(data = map_layers$survey.area, fill = NA) +
  geom_sf(data = resample_sf, mapping = aes(fill = "Resample stations"), linewidth = 0.1) +
  geom_sf(data = map_layers$akland) +
  facet_wrap(~YEAR) +
  scale_x_continuous(limits = map_layers$plot.boundary$x, breaks = map_layers$lon.breaks) +
  scale_y_continuous(limits = map_layers$plot.boundary$y, breaks = map_layers$lat.breaks) +
  theme_bw() +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom"
  )

png(here::here("analysis", "gear_testing_2026", "resample_map.png"), width = 8, height = 6, units = "in", res = 300)
print(resample_map)
dev.off()

# Resample probability based on Bristol Bay mean temperature

rkc_strata <- akgfmaps::get_crab_strata(select.stock = "BBRKC", set.crs = "EPSG:3338")

sea_ice <- read.csv(here::here("analysis", "gear_testing_2026", "april1_sea_ice.csv"))

rkc_temps <- 
  terra::unwrap(coldpool::ebs_bottom_temperature) |>
  terra::mask(touches = TRUE, rkc_strata)

annual_mean <- global(rkc_temps, fun = "mean", na.rm = TRUE)

annual_mean$YEAR <- as.numeric(rownames(annual_mean))

annual_mean <- annual_mean |> 
  dplyr::filter(YEAR >= 1999) |>
  dplyr::rename(MEAN_BT = mean)
  

resample_true <- 
  resample |>
  dplyr::group_by(YEAR) |>
  dplyr::summarise(n = n()) |> 
  dplyr::full_join(
    annual_mean
  ) |>
  dplyr::arrange(YEAR) |>
  dplyr::mutate(resample = !is.na(n))

resample_true <- dplyr::left_join(resample_true, sea_ice)


ggplot() +
  geom_boxplot(data = resample_true, mapping = aes(x = resample, y = mean))

ice_model <- mgcv::gam(
  formula = resample ~ s(poly(ICE_EXTENT, 1), bs = "tp"), 
  family = binomial(), 
  data = resample_true
)

ice_model <- glmmTMB::glmmTMB (
  formula = resample ~ poly(ICE_EXTENT, 1), 
  family = binomial(), 
  data = resample_true
)

sea_ice_pred <- sea_ice
sea_ice_pred$fit <- predict(ice_model, newdata = sea_ice, type = "response")


ggplot(data = sea_ice_pred |>
             dplyr::left_join(resample_true) |>
             dplyr::filter(YEAR > 1998)) +
  geom_path(
    mapping = aes(x = YEAR, y = fit)
  ) +
  geom_point(
    mapping = aes(x = YEAR, y = fit, color = resample),
    size = rel(3)
             ) +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Probability") +
  scale_color_manual(name = "Resample?", values = c("black", "red"), na.value = "grey80") +
  ggtitle("Predicted resample probabilities based on April 1 Bering Sea Ice cover\nfrom Sea Ice Index v4.0") +
  theme_bw() +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18),
        plot.title = element_text(size = 18),
        legend.position = c(0.85, 0.85))

