library(readxl)
library(ggplot2)
library(ggthemes)

dock_data <- 
  readxl::read_xlsx(
    path = here::here("analysis", "dock_spreads", "dock_measurements_20260326.xlsx")
  )

dock_data_long <-
  dock_data |>
  tidyr::pivot_longer(
    cols = c("obs_wing_m", "obs_door_m")
  ) |>
  dplyr::inner_join(
    data.frame(
      name = c("obs_wing_m", "obs_door_m"),
      abbv = c("DS Pro", "DS Pro NX")
    )
  )

ggplot() +
  geom_point(
    data = dock_data,
    mapping = aes(
      x = distance_m,
      y = obs_wing_m
    )
  ) +
  geom_abline(intercept = 0, slope = 1, linetype = 2)


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
  scale_x_continuous(name = "Actual distance (m)") +
  scale_y_continuous(name = "Measured distance (m)") +
  scale_color_tableau(name = "Sensor") +
  scale_shape(name = "Sensor") +
  theme_bw()
