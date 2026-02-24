library(trawlmetrics)
library(ggthemes)
library(scales)

colors_speed <- c('2.0' = "#0D0887FF",
                  '2.5' = "#6A00A8FF",
                  '3.0' = "#B12A90FF",
                  '3.5' = "#E16462FF",
                  '4.0' = "#FCA636FF",
                  'Field obs.' = "grey",
                  'Field fit' = "black")

flume_2025 <- trawlmetrics::flume_tank |>
  dplyr::filter(!is.na(bridles), 
                pulling_point_elevation_mm < 300,
                additional_floatation_kg == 0, 
                year == 2025,
                is.na(bcs_unit),
                trial > 0,
                catch == "empty",
                bridles != "2-weighted",
                # trawl != "83-112",
                footrope %in% c("PNE", "83-112")) |>
  dplyr::mutate(type = "Flume tank",
                fac_trial = factor(trial),
                type = paste0("Flume ", trawl, ", ", bridles),
                GEAR_NAME = trawl,
                fac_towing_speed = format(towing_speed_kn, nsmall = 1))

theme_presentation <- function() {
  theme_bw()  %+replace%
    theme(axis.title = element_text(size = 14, color = "black"),
          axis.text = element_text(size = 12, color = "black"),
          axis.ticks = element_line(color = "black"),
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 12),
          strip.text = element_text(size = 12))
}

p_ratios_tank_field <- 
  ggplot() +
  geom_point(
    data = dplyr::filter(bts_geom, NET_MEASURED == TRUE, CRUISE >= 200500),
    mapping = aes(x = NET_WIDTH_M, y = NET_WIDTH_M/NET_HEIGHT_M, color = "Field obs."),
    alpha = 0.5,
    shape = 20,
    size = 1
  ) +
  geom_smooth(
    data = dplyr::filter(bts_geom, NET_MEASURED == TRUE, CRUISE >= 200500),
    mapping = aes(x = NET_WIDTH_M, y = NET_WIDTH_M/NET_HEIGHT_M, color = "Field fit"),
  ) +
  geom_point(
    data = flume_2025,
    mapping = aes(x = spread_u_wing_m, y = spread_u_wing_m/opening_headline_m, color = fac_towing_speed), size = 2.5
  ) +
  scale_x_continuous(name = "Upper wing tip spread (m)", limits = c(10, 22)) +
  scale_y_continuous(name = "Spread:Height Ratio", limits = c(0, 15)) +
  scale_color_manual(name = "Towing speed (knots)", values = colors_speed) +
  facet_wrap(~GEAR_NAME, scales = "free_y") +
  theme_bw()

png(filename = here::here("analysis", "flume_tank_2026", "plots", "ratios_field_vs_flume.png"), width = 9, height = 5, units = "in", res = 300)
print(p_ratios_tank_field + theme_presentation())
dev.off()

