source("00_functions.R")

# trawl_data_by_haul <- readRDS(file = here::here("data", "trawl_data_by_haul.rds")) |>
#   dplyr::filter(FOOTROPE_ON_BOTTOM == "Yes", DOOR_SIZE_M2 == "4.5")

trawl_data_by_haul <- readRDS(file = here::here("data", "trawl_data_by_haul.rds")) |>
  dplyr::filter(DOORS_ON_BOTTOM == "Yes")

ggplot() +
  geom_text(
    data = trawl_data_by_haul,
    mapping = aes(x = DOOR_ROLL_MAX, y = DOOR_PITCH_MEAN, label = HAUL, 
                  color = ifelse(HAUL %in% c(531:537), "Hauls 531-537\n(shallow+currents)",
                                 ifelse(HAUL %in% c(538:543), "Hauls 538-543\n(currents)",
                                 "Other"))
    )
  ) + 
  scale_color_manual(name = NULL, 
                     values = c("Other" = "grey20", 
                                "Hauls 531-537\n(shallow+currents)" = "green3", 
                                "Hauls 538-543\n(currents)" = "purple")) +
  scale_x_continuous(name = "Door roll (\u00b0)") +
  scale_y_continuous(name = "Door pitch (\u00b0)") +
  theme_bw()


ggplot() +
  geom_point(
    data = trawl_data_by_haul,
    mapping = aes(x = DOOR_ROLL_MAX, y = DOOR_PITCH_MEAN, color = DOOR_BAIL_POSITION, shape = CODEND)
  ) +
  geom_smooth(
    data = trawl_data_by_haul,
    mapping = aes(x = DOOR_ROLL_MAX, y = DOOR_PITCH_MEAN, color = DOOR_BAIL_POSITION,)
  ) +
  scale_x_continuous(name = "Door roll (\u00b0)") +
  scale_y_continuous(name = "Door pitch (\u00b0)") +
  scale_shape_manual(values = c('Closed' = 16, 'Open' = 21)) +
  scale_color_manual(values = c("red", "blue2")) +
  theme_bw()


ggplot() +
  geom_point(
    data = trawl_data_by_haul,
    mapping = aes(x = BOTTOM_DEPTH, y = DOOR_PITCH_MEAN, color = DOOR_BAIL_POSITION, shape = CODEND, size = DOOR_SETTINGS)
  ) +
  geom_smooth(
    data = trawl_data_by_haul,
    mapping = aes(x = BOTTOM_DEPTH, y = DOOR_PITCH_MEAN, color = DOOR_BAIL_POSITION),
    method = 'gam',
    formula = y ~ s(x, k = 4)
  ) +
  scale_shape_manual(name = "Codend", values = c('Closed' = 16, 'Open' = 21)) +
  scale_color_manual(name = "Bail position", values = c("red", "blue2")) +
  scale_size_discrete(name = "Towing points") +
  scale_x_continuous(name = "Bottom depth (m)") +
  scale_y_continuous(
    name = "Door pitch (\u00B0)", 
    limits = c(-20, 5),
    oob = scales::oob_squish, 
    expand = c(0,0)
  ) +
  facet_grid(paste0(DOOR_SIZE_M2, "-m2")~paste0(DOOR_LEG_SETBACK_IN, "-in")) +
  theme_bw()

ggplot() +
  geom_point(
    data = trawl_data_by_haul,
    mapping = aes(x = BOTTOM_DEPTH, y = DOOR_ROLL_MAX, color = DOOR_BAIL_POSITION, shape = CODEND, size = DOOR_SETTINGS)
  ) +
  geom_smooth(
    data = trawl_data_by_haul,
    mapping = aes(x = BOTTOM_DEPTH, y = DOOR_ROLL_MAX, color = DOOR_BAIL_POSITION),
    method = 'gam',
    formula = y ~ s(x, k = 4)
  ) +
  scale_shape_manual(name = "Codend", values = c('Closed' = 16, 'Open' = 21)) +
  scale_color_manual(name = "Bail position", values = c("red", "blue2")) +
  scale_size_discrete(name = "Towing points") +
  scale_x_continuous(name = "Bottom depth (m)") +
  scale_y_continuous(
    name = "Door roll (\u00B0)", 
    limits = c(-2, 40), 
    oob = scales::oob_squish, 
    expand = c(0,0)
  ) +
  facet_grid(paste0(DOOR_SIZE_M2, "-m2")~paste0(DOOR_LEG_SETBACK_IN, "-in")) +
  theme_bw()


ggplot() +
  geom_boxplot(
    data = trawl_data_by_haul,
    mapping = aes(x = cut(SCOPE_RATIO, c(2.25, 3, 3.5, 4, 5, 6, 9.5)), y = DOOR_ROLL_MAX, 
                  color = ifelse(HAUL %in% c(531:543), "Hauls 531-542 (shallow+currents)", 
                                 "Other"))
  ) + 
  scale_color_manual(name = NULL, values = c("Other" = "black", "Hauls 531-542 (shallow+currents)" = "red")) +
  scale_x_discrete(name = "Scope ratio") +
  scale_y_continuous(name = "Door pitch (\u00b0)") +
  theme_bw()

