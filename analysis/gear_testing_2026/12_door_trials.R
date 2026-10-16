library(trawlmetrics)
library(patchwork)
library(readxl)

# Door trial data ------------

trawl_data_by_haul <- readRDS(here::here("data", "trawl_data_by_haul.rds"))

door_blocks <- readxl::read_xlsx(
  path = here::here("data", "2026_gear_testing_haul_log.xlsx"),
  sheet = "door_blocks"
) |>
  dplyr::select(-DOOR_SIZE_M2)
  
door_trial_data <- trawl_data_by_haul |>
  dplyr::inner_join(door_blocks) |>
  dplyr::arrange(SCOPE_RATIO)

  
table_door_treatment_measurements <- 
  door_trial_data   |>
  dplyr::group_by(BLOCK, DOOR_SIZE_M2) |>
  dplyr::summarise(
    N_TRT = dplyr::n(),
    BOTTOM_DEPTH = mean(BOTTOM_DEPTH),
    DOOR_SPREAD = mean(DOOR_SPREAD, na.rm = TRUE),
    NET_SPREAD = mean(NET_WIDTH, na.rm = TRUE),
    NET_HEIGHT = mean(NET_HEIGHT, na.rm = TRUE),
    DOOR_ROLL_MAX = mean(DOOR_ROLL_MAX, na.rm = TRUE),
    WINCH_TENSION_TOTAL = mean(WINCH_TENSION_TOTAL, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::arrange(BLOCK, DOOR_SIZE_M2) |>
  dplyr::mutate(
    dplyr::across(
      c(BOTTOM_DEPTH, DOOR_SPREAD, NET_SPREAD, NET_HEIGHT, DOOR_ROLL_MAX, WINCH_TENSION_TOTAL),
      ~ dplyr::if_else(is.na(.x), "-", sprintf("%.1f", .x))
    )
  )

write.csv(
  table_door_treatment_measurements,
  here::here("plots", "door_comparison", "door_treatment_table.csv"),
  row.names = FALSE
          )

p_winch_tension <-
  ggplot() +
  geom_point(
    data = door_trial_data,
    mapping = aes(
      x = SCOPE_RATIO,
      y = WINCH_TENSION_TOTAL,
      group = interaction(BLOCK, DOOR_SIZE_M2),
      color = BLOCK,
      shape = DOOR_SIZE_M2
    ),
    size = 1.1
  ) +
  geom_path(
    data = door_trial_data,
    mapping = aes(
      x = SCOPE_RATIO,
      y = WINCH_TENSION_TOTAL,
      group = interaction(BLOCK, DOOR_SIZE_M2),
      color = BLOCK,
      linetype = DOOR_SIZE_M2
    ),
    linewidth = 0.4
  ) +
  scale_color_viridis_d(name = "Block", option = "H", direction = -1) +
  scale_shape(name = "Door size") +
  scale_linetype(name = "Door size") +
  scale_x_continuous(name = "Scope/Depth") +
  scale_y_continuous(name = "Total winch tension (tons)") +
  theme_bw() +
  theme(axis.text = element_text(size = 7),
        axis.title = element_text(size = 8),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 8),
        legend.key.size = unit(4, "mm"))


png(here::here("plots", "door_comparison", "door_treatment_winch_tension.png"), 
    width = 80, height = 60, units = "mm", res = 300)
print(p_winch_tension)
dev.off()


