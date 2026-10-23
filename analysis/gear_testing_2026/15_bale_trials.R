# Compare bridle treatments

library(trawlmetrics)
library(readxl)
library(dplyr)
library(ggthemes)
library(patchwork)

source("00_functions.R")

bale_treatments <- 
  readxl::read_xlsx(path = here::here("data", "2026_gear_testing_haul_log.xlsx"),
                    sheet = "bale_blocks")

trawl_data_by_haul <- readRDS(here::here("data", "trawl_data_by_haul.rds"))

bcs_data_by_haul <- readRDS(here::here("data", "bcs_data_by_haul.rds"))

bcs_13m_height <- readRDS(here::here("data", "bcs_data_by_haul.rds")) |>
  dplyr::filter(
    DISTANCE == 13
  ) |>
  dplyr::group_by(HAUL, WIRE_LENGTH_FM) |>
  dplyr::summarise(
    BCS_13MHEIGHT_CM = mean(BCS_HEIGHT_CM, na.rm = TRUE)
  )

bale_data <- trawl_data_by_haul  |>
  dplyr::inner_join(bale_treatments) |>
  dplyr::filter(WIRE_LENGTH_FM %in% c(75, 100)) |>
  dplyr::inner_join(bcs_13m_height) |>
  dplyr::group_by(WIRE_LENGTH_FM, TOW_DIRECTION, DOOR_BALE_POSITION) |>
  dplyr::summarise(
    NET_WIDTH = mean(NET_WIDTH, na.rm = TRUE),
    NET_HEIGHT = mean(NET_HEIGHT, na.rm = TRUE),
    DOOR_SPREAD = mean(DOOR_SPREAD, na.rm = TRUE),
    DOOR_ROLL_MAX = mean(DOOR_ROLL_MAX, na.rm = TRUE),
    BCS_13MHEIGHT_CM = mean(BCS_13MHEIGHT_CM, na.rm = TRUE),
    BOTTOM_DEPTH = mean(BOTTOM_DEPTH, na.rm = TRUE),
    DOOR_PITCH_MEAN = mean(DOOR_PITCH_MEAN, na.rm = TRUE)
  ) |>
  dplyr::filter(!(WIRE_LENGTH_FM == 100 & TOW_DIRECTION == "SE"))

mean(bale_data$BOTTOM_DEPTH)

dplyr::group_by(
  bale_data,
  TOW_DIRECTION, WIRE_LENGTH_FM, DOOR_BALE_POSITION
) |>
  dplyr::summarise(
    n = n()
  )


# Plot bale data ----

p_door_spread <- plot_gear_block_metric(bale_data, DOOR_BALE_POSITION, DOOR_SPREAD, NULL, "Door\nspread (m)")
p_net_spread  <- plot_gear_block_metric(bale_data, DOOR_BALE_POSITION, NET_WIDTH, NULL, "Upper wing\nspread (m)",  c(15, 20))
p_net_height <- plot_gear_block_metric(bale_data, DOOR_BALE_POSITION, NET_HEIGHT, NULL, "Opening\nheight (m)", c(5, 6))
p_bcs_height <- plot_gear_block_metric(bale_data, DOOR_BALE_POSITION, BCS_13MHEIGHT_CM, NULL, "Footrope 13-m\nelevation (cm)", c(0, 7.62))
p_door_roll  <- plot_gear_block_metric(bale_data, DOOR_BALE_POSITION, DOOR_ROLL_MAX, "Door bale position", "Max\ndoor roll (\u00B0)", 30)
p_door_pitch <- plot_gear_block_metric(bale_data, DOOR_BALE_POSITION, DOOR_PITCH_MEAN, NULL, "Door pitch (\u00B0)")

# Patchwork plot
p_bale_multipanel <- (p_door_spread + p_net_spread + p_net_height + p_bcs_height + p_door_roll + p_door_pitch) + 
  plot_layout(
    nrow = 2, 
    ncol = 3, 
    byrow = TRUE, 
    guides = "collect"
  )

ggsave(
  p_bale_multipanel,
  filename = here::here("plots", "rigging_comparison", "door_bale_settings.png"),
  width = 169, 
  height = 100,
  units = "mm",
  dpi = 300
)
