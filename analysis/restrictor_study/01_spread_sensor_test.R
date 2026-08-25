library(readxl)
library(ggplot2)
library(ggthemes)
library(ggpp)

dock_data <- 
  readxl::read_xlsx(
    path = here::here("data", "dock_tests", "dock_measurements_20260326.xlsx")
  )

dock_data_long <-
  dock_data |>
  tidyr::pivot_longer(
    cols = c("obs_wing_m", "obs_door_m")
  ) |>
  dplyr::inner_join(
    data.frame(
      name = c("obs_wing_m", "obs_door_m"),
      abbv = c("Distance Sensor w/ A2S", "Door Sounder Pro NX")
    )
  )


ggplot() +
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  geom_point(
    data = dock_data_long,
    mapping = aes(
      x = distance_m,
      y = value,
      color = abbv,
      shape = abbv
    ),
    size = rel(3)
  ) +
  geom_text_npc(mapping = aes(npcx = 0.02, npcy = 0.95, label = "Dockside test in Lake Washington (March 2026)")) +
  scale_x_continuous(name = "Physical distance (m)") +
  scale_y_continuous(name = "Measured distance (m)") +
  scale_color_tableau(name = "Sensor") +
  scale_shape(name = "Sensor") +
  theme_bw()

mean(dock_data$obs_wing_m-dock_data$distance_m)

