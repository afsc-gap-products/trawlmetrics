library(ggplot2)
library(dplyr)
library(trawlmetrics)

flume_data <- trawlmetrics::flume_tank |>
  dplyr::filter(
    trawl == "RACE 78/100",
    footrope == "EBS",
    towing_speed_kn == 3,
    bridles != "standard"
  ) |>
  dplyr::select(
    upper_wing_spread = spread_u_wing_m,
    opening_height = opening_headline_m,
    buoyancy = total_buoyancy_kgf
  ) |>
  dplyr::mutate(trial = "Scale model (flume tank)")

trawl_data_by_haul <- readRDS(file = here::here("data", "trawl_data_by_haul.rds")) |>
  dplyr::filter(GEAR_ON_BOTTOM == "Yes", DOOR_SIZE_M2 == "4.5", CODEND == "Closed") |>
  dplyr::select(
    upper_wing_spread = NET_WIDTH,
    opening_height = NET_HEIGHT,
    buoyancy = FLOAT_BUOYANCY_KGF
  ) |>
  dplyr::mutate(trial = "Full-sized (field)")

combined_data <- dplyr::bind_rows(trawl_data_by_haul, flume_data) |>
  dplyr::mutate(buoyancy = factor(buoyancy, levels = c(333.9, 349.2, 453.2)))

p_flume_v_field <- 
  ggplot(combined_data, aes(x = upper_wing_spread, y = opening_height, color = buoyancy, shape = trial)) +
  geom_point(size = 2.5) +
  scale_shape_manual(name = "Trial", values = c('Full-sized (field)' = 16, 'Scale model (flume tank)' = 17)) +
  scale_color_manual(name = "Total buoyancy (kgf)", values = c('333.9' = "#00204DFF", '349.2' = "#7C7B78FF", '453.2' = "#BCAF6FFF")) +
  scale_x_continuous(name = "Upper wing spread (m)") +
  scale_y_continuous(name = "Opening height (m)") +
  theme_bw() +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(size = 9, face = "bold", hjust = 0),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 9),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 9),
    legend.key.size = unit(4, "mm"),
    panel.grid = element_line(linewidth = 0.25)
  )


ggsave(
  p_flume_v_field,
  filename = here::here("plots", "geom_comparison", "flume_vs_field.png"), 
  width = 120, 
  height = 60,
  units = "mm",
  dpi = 300
  )
