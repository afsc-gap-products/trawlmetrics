# Review scope table hauls

library(trawlmetrics)
library(akgfmaps)
library(ggthemes)
library(shadowtext)
library(dplyr)
library(ggpp)
library(scales)

# PNE and 83-112 scope tables
scope_tables <- 
  readxl::read_xlsx(path = here::here("data", "shelf_slope_table.xlsx")) |>
  dplyr::mutate(mean_depth_fm = (min_depth_fm+max_depth_fm)/2,
                mean_depth_m = mean_depth_fm * 1.8288,
                scope_to_depth = wire_out_fm/mean_depth_fm) |>
  dplyr::inner_join(data.frame(table = c("GOA/AI", "EBS shelf", "EBS slope"), gear = c("PNE", "83-112", "PNE-S")))

# Load BCS data
bcs_height_summary <- readRDS(file = here::here("output", "bcs_height_summary.rds")) |>
  dplyr::rename(WIRE_LENGTH_FM = scope) |>
  dplyr::rename_with(toupper)

bcs_timeseries <- readRDS(file = here::here("output", "bcs_segments.rds")) |>
  dplyr::filter(!is.na(haul)) |>
  dplyr::rename(WIRE_LENGTH_FM = scope) |>
  dplyr::rename_with(toupper) |>
  dplyr::select(DT, BCS_ID, POSITION, DISTANCE, SIDE, X_G, X_G_ORIGINAL, HEIGHT_FIT, HAUL, WIRE_LENGTH_FM)

# Load trawl measurement timeseries 

trawl_measurements <- readRDS(here::here("output", "trawl_measurements.rds")) |>
  dplyr::rename(WIRE_LENGTH_FM = scope) |>
  dplyr::rename_with(toupper)


# Examine BCS height data
ggplot() +
  geom_hline(yintercept = 0, linetype = 3) +
  geom_path(
    data = bcs_height_summary,
    mapping = aes(x = HAUL, y = MEAN_HEIGHT, group = interaction(HAUL, WIRE_LENGTH_FM)),
    color = "grey30"
  ) +
  geom_point(
    data = bcs_height_summary,
    mapping = aes(x = HAUL, y = MEAN_HEIGHT, color = SIDE),
    size = 0.7
  ) +
  scale_color_manual(name = "Side", values = c('P'="#264EFF", 'C' = "grey60", 'S' = "#D92632")) +
  scale_y_continuous("Distance to bottom (cm)", 
                     limits = c(-2, 40), 
                     oob = scales::squish_infinite) +
  facet_wrap(~DISTANCE) +
  theme_bw()

ggplot() +
  geom_hline(yintercept = 0, linetype = 3) +
  geom_path(
    data = bcs_height_summary,
    mapping = 
      aes(
        x = HAUL, 
        y = MEAN_HEIGHT, 
        group = interaction(HAUL, WIRE_LENGTH_FM), 
        alpha = HAUL > 535 & HAUL < 551),
    color = "grey30"
  ) +
  geom_point(
    data = bcs_height_summary,
    mapping = 
      aes(x = HAUL, y = MEAN_HEIGHT, color = SIDE, 
          alpha = HAUL > 535 & HAUL < 551),
    size = 0.7
  ) +
  scale_color_manual(name = "Side", values = c('P'="#264EFF", 'C' = "grey60", 'S' = "#D92632")) +
  scale_alpha_manual(values = c('TRUE' = 1, 'FALSE' = 0.1)) +
  scale_y_continuous("Distance to bottom (cm)", 
                     limits = c(-2, 40), 
                     oob = scales::squish_infinite) +
  facet_wrap(~DISTANCE) +
  theme_bw()

# Closed codened trawl geometry data ---------------------------------------------------------------
channel <- trawlmetrics::get_connected(schema = "AFSC")

cc_hauls <- 
  RODBC::sqlQuery(
    channel = channel,
    query = "SELECT 
    H.VESSEL,
    H.CRUISE,
    H.HAUL,
    RDH.DOOR_SPREAD, 
    H.BOTTOM_DEPTH,
    H.HAULJOIN,
    H.NET_WIDTH, 
    H.DISTANCE_FISHED, 
    H.NET_HEIGHT, 
    H.DURATION,
    H.WIRE_LENGTH,
    H.START_LONGITUDE,
    H.START_LATITUDE,
    H.START_TIME,
    H.PERFORMANCE
  FROM 
    RACEBASE.HAUL H, 
    RACE_DATA.HAULS RDH,
    RACE_DATA.CRUISES RDC
  WHERE H.CRUISE = 202601 
    AND H.HAUL_TYPE = 23
    AND H.VESSEL = 134
    AND H.HAUL = RDH.HAUL 
    AND RDH.CRUISE_ID = RDC.CRUISE_ID
    AND H.VESSEL = RDC.VESSEL_ID
    AND RDC.CRUISE = H.CRUISE
  "
  ) |>
  dplyr::mutate(
    BOTTOM_DEPTH_FM = BOTTOM_DEPTH / 1.8288,
    WIRE_LENGTH_FM = WIRE_LENGTH / 1.8288
  )

gear_config <- readxl::read_xlsx(path = here::here("data", "2026_gear_config.xlsx")) |>
  dplyr::mutate(
    TOTAL_GEAR_LENGTH_M = (total_bridle_length_ft + door_leg_tail_chain_length_ft + bridle_chain_length_ft)/3.281
  )

names(gear_config) <- toupper(names(gear_config))

gear_config <- gear_config

# Analysis and plot settings
door_roll_breaks <- c(-Inf, -10, 0, 10, 25, Inf)
door_roll_labels <- c("<-10", "-10-0", "0-10", "10-25", ">25")
door_roll_colors <- c("#00204DFF", "#414D6BFF", "#7C7B78FF", "#BCAF6FFF", "red")
names(door_roll_colors) <- door_roll_labels

door_roll_breaks <- c(-Inf, 0, 25, 30, Inf)
door_roll_labels <- c("<0", "0-25", "25-30", ">30")
door_roll_colors <- c("#00204DFF", "#BCAF6FFF", "pink", "red")
names(door_roll_colors) <- door_roll_labels

sdr_breaks <- c(0, 2.5, 3, 3.5, 4, 5, 6, 7, Inf)
sdr_labels <- c("<2.5", "2.5-3", "3.0-3.5", "3.5-4.0", "4-5", "5-6", "6-7", ">7")

depth_breaks <- c(20, 30, 50, 75, 100, 150, 250)
depth_labels <- c("20-30 m", "30-50 m", "50-75 m", "75-100 m", "100-150 m", "150-250 m")

codend_symbols <- c('Open' = 1, 'Closed' = 16)
codend_symbol_size <- c("Closed" = 3.5, "Open" = 1.8)


# Open codend tow data
gear_treatments_45 <- 
  readxl::read_xlsx(
    path = here::here("data", "2026_gear_testing_haul_log.xlsx"),
    sheet = "treatments"
  ) |>
  dplyr::filter(Door_size_m2 == 4.5)

gt_net_data <- 
  readRDS(file = here::here("output", "haul_summary.rds")) |>
  dplyr::select(
    HAUL = haul,
    WIRE_LENGTH_FM = scope,
    NET_WIDTH = MEAN_NET_SPREAD,
    NET_HEIGHT = MEAN_NET_HEIGHT,
    DOOR_SPREAD = MEAN_DOOR_SPREAD
  ) |>
  dplyr::mutate(
    WIRE_LENGTH = WIRE_LENGTH_FM * 1.8288
  )

gt_btd_data <- 
  readRDS(file = here::here("output", "btd_summary.rds")) |>
  dplyr::group_by(scope, haul) |>
  dplyr::slice_max(pass, n = 1) |>
  dplyr::ungroup() |>
  dplyr::select(
    HAUL = haul,
    WIRE_LENGTH_FM = scope,
    BOTTOM_DEPTH_FM
  ) |>
  dplyr::mutate(
    BOTTOM_DEPTH = BOTTOM_DEPTH_FM * 1.8288,
    WIRE_LENGTH = WIRE_LENGTH_FM * 1.8288
  )

gt_door_data <-
  gear_treatments_45 |>
  dplyr::group_by(Scope, Haul) |>
  dplyr::slice_max(Pass, n = 1) |>
  dplyr::ungroup() |>
  dplyr::filter(Event == "EQ") |>
  dplyr::select(
    HAUL = Haul,
    WIRE_LENGTH_FM = Scope,
    WINCH_TENSION_P = Port_Tension,
    WINCH_TENSION_S = Stbd_Tension,
    DOOR_PITCH_P = Port_Door_Pitch,
    DOOR_ROLL_P = Port_Door_Roll,
    DOOR_DEPTH_P = Port_Door_Depth,
    DOOR_PITCH_S = Stbd_Door_Pitch,
    DOOR_ROLL_S = Stbd_Door_Roll,
    DOOR_DEPTH_S = Stbd_Door_Depth
  )

gt_data <- 
  gt_door_data |>
  dplyr::inner_join(gt_btd_data) |>
  dplyr::inner_join(gt_net_data) |>
  dplyr::bind_rows(
    cc_hauls |>
      dplyr::select(HAUL, BOTTOM_DEPTH, WIRE_LENGTH, NET_HEIGHT, NET_WIDTH, DOOR_SPREAD) |>
      unique() |>
      dplyr::inner_join(gt_door_data) |>
      dplyr::mutate(
        WIRE_LENGTH_FM = WIRE_LENGTH/1.8288,
        BOTTOM_DEPTH_FM = BOTTOM_DEPTH/1.8288)
  ) |>
  dplyr::mutate(
    CODEND = ifelse(HAUL >= 600, "Closed", "Open"),
    DOOR_ON_BOTTOM_P = DOOR_DEPTH_P > (BOTTOM_DEPTH - NET_HEIGHT),
    DOOR_ON_BOTTOM_S = DOOR_DEPTH_S > (BOTTOM_DEPTH - NET_HEIGHT),
    DOOR_LT30_P = abs(DOOR_ROLL_P) < 30,
    DOOR_LT30_S = abs(DOOR_ROLL_S) < 30,
    DOORS_GOOD = DOOR_ON_BOTTOM_P & DOOR_ON_BOTTOM_S & DOOR_LT30_P & DOOR_LT30_S) |>
  dplyr::inner_join(
    gear_config
  ) |>
  dplyr::mutate(
    BRIDLE_ANGLE_DEG = 
      trawlmetrics::calc_bridle_angle(
        door_spread_m = DOOR_SPREAD, 
        wing_spread_m = NET_WIDTH,
        total_bridle_length_m = TOTAL_GEAR_LENGTH_M
      ),
    SDR = WIRE_LENGTH/BOTTOM_DEPTH,
    SDR_FAC = 
      cut(SDR, breaks = sdr_breaks, label = sdr_labels),
    BOTTOM_DEPTH_FAC = 
      cut(BOTTOM_DEPTH, breaks = depth_breaks, label = depth_labels),
    MAX_DOOR_ROLL = pmax(abs(DOOR_ROLL_P), abs(DOOR_ROLL_S), na.rm = TRUE),
    TOTAL_WINCH_TENSION = WINCH_TENSION_P+WINCH_TENSION_S
  )

gt_bcs_data <- 
  dplyr::filter(gt_data, HAUL >= 600) |>
  dplyr::select(-WIRE_LENGTH_FM) |>
  dplyr::inner_join(
    bcs_height_summary
  ) |>
  dplyr::bind_rows(
    dplyr::filter(gt_data, HAUL < 600) |>
      dplyr::inner_join(
        bcs_height_summary
      )
  )

# Review data from each haul -----



  

# Plots

depth_range <- range(gt_data$BOTTOM_DEPTH, na.rm = TRUE)
sdr_axis_breaks <- 2:9
max_sdr <- max(gt_data$WIRE_LENGTH/gt_data$BOTTOM_DEPTH, na.rm = TRUE)

# Select hauls after door trials
gt_subset <- 
  dplyr::filter(gt_data, !is.na(SDR_FAC))

gt_bcs_subset <- 
  dplyr::filter(gt_bcs_data, !is.na(SDR_FAC))
  


# SDR facets, bottom depth x-axis
ggplot() +
  geom_hline(yintercept = c(0, 30), linetype = 2) +
  geom_point(
    data = gt_subset, 
    mapping = aes(
      x = BOTTOM_DEPTH, 
      y = MAX_DOOR_ROLL, 
      shape = CODEND, 
      color = factor(BRIDLE_CHAIN_WEIGHT)
    ),
    alpha = 0.7,
    size = 2.2
  ) +
  geom_rug(
    data = gt_subset, 
    mapping = aes(
      x = BOTTOM_DEPTH
    )
  ) +
  scale_color_colorblind(name = "Chain weight (kg)") +
  scale_shape_manual(name = "Codend", values = codend_symbols) +
  scale_x_continuous(name = "Depth (m)") +
  scale_y_continuous(name = "Door roll (\u00B0)", oob = scales::squish, limits = c(-3, 40), expand = c(0, 0)) +
  facet_wrap(~SDR_FAC) +
  theme_bw()

ggplot() +
  geom_hline(yintercept = c(15, 20), linetype = 2) +
  geom_point(
    data = gt_subset, 
    mapping = aes(
      x = BOTTOM_DEPTH, 
      y = NET_WIDTH, 
      shape = CODEND, 
      color = factor(BRIDLE_CHAIN_WEIGHT)
    ),
    alpha = 0.7,
    size = 2.2
  ) +
  geom_rug(
    data = gt_subset, 
    mapping = aes(
      x = BOTTOM_DEPTH
    )
  ) +
  scale_color_colorblind(name = "Chain weight (kg)") +
  scale_size_manual(name = "Codend", values = codend_symbol_size) +
  scale_shape_manual(name = "Codend", values = codend_symbols) +
  scale_x_continuous(name = "Depth (m)") +
  scale_y_continuous(
    name = "Upper wing spread (m)", 
                     oob = scales::squish, 
    limits = c(10, 20.5),
    expand = c(0, 0)) +
  facet_wrap(~SDR_FAC) +
  theme_bw()

ggplot() +
  geom_hline(yintercept = c(5, 6), linetype = 2) +
  geom_point(
    data = gt_subset, 
    mapping = aes(
      x = BOTTOM_DEPTH, 
      y = NET_HEIGHT, 
      shape = CODEND, 
      color = factor(BRIDLE_CHAIN_WEIGHT)
    ),
    alpha = 0.7,
    size = 2.2
  ) +
  geom_rug(
    data = gt_subset, 
    mapping = aes(
      x = BOTTOM_DEPTH
    )
  ) +
  scale_color_colorblind(name = "Chain weight (kg)") +
  scale_shape_manual(name = "Codend", values = codend_symbols) +
  scale_x_continuous(name = "Depth (m)") +
  scale_y_continuous(
    name = "Opening height (m)", 
    oob = scales::squish,
    limits = c(4, 10),
    expand = c(0, 0)) +
  facet_wrap(~SDR_FAC) +
  theme_bw()

ggplot() +
  geom_point(
    data = gt_subset, 
    mapping = aes(
      x = BOTTOM_DEPTH, 
      y = DOOR_SPREAD, 
      shape = CODEND, 
       
      color = factor(BRIDLE_CHAIN_WEIGHT)
    ),
    alpha = 0.7,
    size = 2.2
  ) +
  geom_rug(
    data = gt_subset, 
    mapping = aes(
      x = BOTTOM_DEPTH
    )
  ) +
  scale_color_colorblind(name = "Chain weight (kg)") +
  scale_shape_manual(name = "Codend", values = codend_symbols) +
  scale_x_continuous(name = "Depth (m)") +
  scale_y_continuous(
    name = "Door spread (m)", 
    oob = scales::squish, 
    limits = c(26, 58),
    expand = c(0, 0)) +
  facet_wrap(~SDR_FAC) +
  theme_bw()

ggplot() +
  geom_hline(yintercept = c(18, 21), linetype = 2) +
  geom_point(
    data = gt_subset, 
    mapping = aes(
      x = BOTTOM_DEPTH, 
      y = BRIDLE_ANGLE_DEG, 
      shape = CODEND, 
       
      color = factor(BRIDLE_CHAIN_WEIGHT)
    ),
    alpha = 0.7,
    size = 2.2
  ) +
  geom_rug(
    data = gt_subset, 
    mapping = aes(
      x = BOTTOM_DEPTH
    )
  ) +
  scale_color_colorblind(name = "Chain weight (kg)") +
  scale_shape_manual(name = "Codend", values = codend_symbols) +
  scale_x_continuous(name = "Depth (m)") +
  scale_y_continuous(
    name = "Bridle angle of attack (\u00B0)", 
    oob = scales::squish, 
    limits = c(6, 21.5),
    expand = c(0, 0)) +
  facet_wrap(~SDR_FAC) +
  theme_bw()


# Bottom depth facets, SDR x-axis

ggplot() +
  geom_hline(yintercept = c(0, 30), linetype = 2) +
  geom_point(
    data = gt_subset, 
    mapping = aes(
      x = SDR, 
      y = MAX_DOOR_ROLL, 
      shape = CODEND, 
       
      color = factor(BRIDLE_CHAIN_WEIGHT)
    ),
    alpha = 0.7,
    size = 2.2
  ) +
  scale_color_colorblind(name = "Chain weight (kg)") +
  scale_shape_manual(name = "Codend", values = codend_symbols) +
  scale_x_continuous(name = "Scope/Depth") +
  scale_y_continuous(name = "Door roll (\u00B0)", oob = scales::squish, limits = c(-1, 40), expand = c(0, 0)) +
  facet_wrap(~BOTTOM_DEPTH_FAC, scales = "free") +
  theme_bw()

ggplot() +
  geom_hline(yintercept = c(15, 20), linetype = 2) +
  geom_point(
    data = gt_subset, 
    mapping = aes(
      x = SDR, 
      y = NET_WIDTH, 
      shape = CODEND, 
       
      color = factor(BRIDLE_CHAIN_WEIGHT)
    ),
    alpha = 0.7,
    size = 2.2
  ) +
  scale_color_colorblind(name = "Chain weight (kg)") +
  scale_shape_manual(name = "Codend", values = codend_symbols) +
  scale_x_continuous(name = "Scope/Depth") +
  scale_y_continuous(
    name = "Upper wing spread (m)", 
    oob = scales::squish, 
    limits = c(10, 20.5), 
    expand = c(0, 0)) +
  facet_wrap(~BOTTOM_DEPTH_FAC, scales = "free") +
  theme_bw()

ggplot() +
  geom_point(
    data = gt_subset, 
    mapping = aes(
      x = SDR, 
      y = DOOR_SPREAD, 
      shape = CODEND, 
       
      color = factor(BRIDLE_CHAIN_WEIGHT)
    ),
    alpha = 0.7,
    size = 2.2
  ) +
  scale_color_colorblind(name = "Chain weight (kg)") +
  scale_shape_manual(name = "Codend", values = codend_symbols) +
  scale_x_continuous(name = "Scope/Depth") +
  scale_y_continuous(
    name = "Door spread (m)", 
    oob = scales::squish, 
    limits = c(26, 58),
    expand = c(0, 0)) +
  facet_wrap(~BOTTOM_DEPTH_FAC, scales = "free") +
  theme_bw()

ggplot() +
  geom_hline(yintercept = c(5, 6), linetype = 2) +
  geom_point(
    data = gt_subset, 
    mapping = aes(
      x = SDR, 
      y = NET_HEIGHT, 
      shape = CODEND, 
      color = factor(BRIDLE_CHAIN_WEIGHT)
    ),
    alpha = 0.7,
    size = 2.2
  ) +
  scale_color_colorblind(name = "Chain weight (kg)") +
  scale_shape_manual(name = "Codend", values = codend_symbols) +
  scale_x_continuous(name = "Scope/Depth") +
  scale_y_continuous(
    name = "Opening height (m)", 
    oob = scales::squish,
    limits = c(4, 10),
    expand = c(0, 0)) +
  facet_wrap(~BOTTOM_DEPTH_FAC, scales = "free") +
  theme_bw()

ggplot() +
  geom_hline(yintercept = c(18, 21), linetype = 2) +
  geom_point(
    data = gt_subset, 
    mapping = aes(
      x = SDR, 
      y = BRIDLE_ANGLE_DEG, 
      shape = CODEND, 
      color = factor(BRIDLE_CHAIN_WEIGHT)
    ),
    alpha = 0.7,
    size = 2.2
  ) +
  scale_color_colorblind(name = "Chain weight (kg)") +
  scale_shape_manual(name = "Codend", values = codend_symbols) +
  scale_x_continuous(name = "Scope/Depth") +
  scale_y_continuous(
    name = "Bridle angle of attack (\u00B0)", 
    oob = scales::squish, 
    limits = c(6, 21.5),
    expand = c(0, 0)) +
  facet_wrap(~BOTTOM_DEPTH_FAC, scales = "free") +
  theme_bw()

ggplot() +
  geom_hline(yintercept = c(0, 30), linetype = 2) +
  geom_point(
    data = gt_subset, 
    mapping = aes(
      x = TOTAL_WINCH_TENSION, 
      y = MAX_DOOR_ROLL, 
      shape = CODEND, 
      color = factor(BRIDLE_CHAIN_WEIGHT)
    ),
    alpha = 0.7,
    size = 2.2
  ) +
  scale_color_colorblind(name = "Chain weight (kg)") +
  scale_shape_manual(name = "Codend", values = codend_symbols) +
  scale_x_continuous(name = "Total winch tension (t)") +
  scale_y_continuous(
    name = "Door roll (\u00B0)", 
    oob = scales::squish, 
    limits = c(-1, 40), expand = c(0, 0)
    ) +
  facet_wrap(~BOTTOM_DEPTH_FAC) +
  theme_bw()

ggplot() +
  geom_hline(yintercept = c(0, 30), linetype = 2) +
  geom_point(
    data = gt_subset, 
    mapping = aes(
      x = TOTAL_WINCH_TENSION, 
      y = MAX_DOOR_ROLL, 
      shape = CODEND, 
      color = factor(BRIDLE_CHAIN_WEIGHT)
    ),
    alpha = 0.7,
    size = 2.2
  ) +
  scale_color_colorblind(name = "Chain weight (kg)") +
  scale_shape_manual(name = "Codend", values = codend_symbols) +
  scale_x_continuous(name = "Total winch tension (t)") +
  scale_y_continuous(
    name = "Door roll (\u00B0)", 
    oob = scales::squish, 
    limits = c(-1, 40), expand = c(0, 0)
  ) +
  facet_wrap(~SDR_FAC) +
  theme_bw()

ggplot() +
  geom_hline(yintercept = c(0, 2.54*3), linetype = 2) +
  geom_point(
    data = dplyr::filter(gt_bcs_subset, DISTANCE == 13),
    mapping = aes(
      x = SDR, 
      y = MEAN_HEIGHT, 
      shape = CODEND, 
      color = factor(BRIDLE_CHAIN_WEIGHT)
    ),
    alpha = 0.7,
    size = 2.2
  ) +
  scale_color_colorblind(name = "Chain weight (kg)") +
  scale_shape_manual(name = "Codend", values = codend_symbols) +
  scale_x_continuous(name = "Scope/Depth") +
  scale_y_continuous(
    name = "13-m BCS Height (cm)", 
    oob = scales::squish, 
    limits = c(-1, 40)) +
  facet_wrap(~BOTTOM_DEPTH_FAC, scales = "free") +
  theme_bw()

ggplot() +
  geom_hline(yintercept = c(0, 2.54*3), linetype = 2) +
  geom_point(
    data = dplyr::filter(gt_bcs_subset, DISTANCE == 16),
    mapping = aes(
      x = SDR, 
      y = MEAN_HEIGHT, 
      shape = CODEND, 
      color = factor(BRIDLE_CHAIN_WEIGHT)
    ),
    alpha = 0.7,
    size = 2.2
  ) +
  scale_color_colorblind(name = "Chain weight (kg)") +
  scale_shape_manual(name = "Codend", values = codend_symbols) +
  scale_x_continuous(name = "Scope/Depth") +
  scale_y_continuous(
    name = "16-m BCS Height (cm)", 
    oob = scales::squish, 
    limits = c(-1, 40)) +
  facet_wrap(~BOTTOM_DEPTH_FAC, scales = "free") +
  theme_bw()

ggplot() +
  geom_hline(yintercept = c(0, 2.54*3), linetype = 3) +
  geom_point(
    data = dplyr::filter(gt_bcs_subset, DISTANCE == 8),
    mapping = aes(
      x = SDR, 
      y = MEAN_HEIGHT, 
      shape = CODEND, 
      color = factor(BRIDLE_CHAIN_WEIGHT)
    ),
    alpha = 0.7,
    size = 2.2
  ) +
  scale_color_colorblind(name = "Chain weight (kg)") +
  scale_shape_manual(name = "Codend", values = codend_symbols) +
  scale_x_continuous(name = "Scope/Depth") +
  scale_y_continuous(
    name = "8-m BCS Height (cm)", 
    oob = scales::squish, 
    limits = c(-1, 40)) +
  facet_wrap(~BOTTOM_DEPTH_FAC, scales = "free") +
  theme_bw()

ggplot() +
  geom_hline(yintercept = c(0, 2.54*3), linetype = 2) +
  geom_point(
    data = dplyr::filter(gt_bcs_subset, DISTANCE == 0),
    mapping = aes(
      x = SDR, 
      y = MEAN_HEIGHT, 
      shape = CODEND, 
      color = factor(BRIDLE_CHAIN_WEIGHT)
    ),
    alpha = 0.7,
    size = 2.2
  ) +
  scale_color_colorblind(name = "Chain weight (kg)") +
  # scale_size_manual(name = "Codend", values = codend_symbol_size) +
  scale_shape_manual(name = "Codend", values = codend_symbols) +
  scale_x_continuous(name = "Scope/Depth") +
  scale_y_continuous(
    name = "Center BCS Height (cm)", 
    oob = scales::squish, 
    limits = c(-1, 40)) +
  facet_wrap(~BOTTOM_DEPTH_FAC, scales = "free") +
  theme_bw()

ggplot() +
  geom_hline(yintercept = c(0, 2.54*3), linetype = 2) +
  geom_point(
    data = dplyr::filter(gt_bcs_subset, DISTANCE == 2),
    mapping = aes(
      x = SDR, 
      y = MEAN_HEIGHT, 
      shape = CODEND, 
      color = factor(BRIDLE_CHAIN_WEIGHT)
    ),
    alpha = 0.5
  ) +
  scale_color_colorblind(name = "Chain weight (kg)") +
  # scale_size_manual(name = "Codend", values = codend_symbol_size) +
  scale_shape_manual(name = "Codend", values = codend_symbols) +
  scale_x_continuous(name = "Scope/Depth") +
  scale_y_continuous(
    name = "2-m BCS Height (cm)", 
    oob = scales::squish, 
    limits = c(-1, 40)) +
  facet_wrap(~BOTTOM_DEPTH_FAC, scales = "free") +
  theme_bw()

ggplot() +
  geom_hline(yintercept = c(0, 2.54*3), linetype = 2) +
  geom_point(
    data = dplyr::filter(gt_bcs_subset, DISTANCE == 21),
    mapping = aes(
      x = SDR, 
      y = MEAN_HEIGHT, 
      shape = CODEND, 
      color = factor(BRIDLE_CHAIN_WEIGHT)
    ),
    alpha = 0.7,
    size = 2.2
  ) +
  scale_color_colorblind(name = "Chain weight (kg)") +
  # scale_size_manual(name = "Codend", values = codend_symbol_size) +
  scale_shape_manual(name = "Codend", values = codend_symbols) +
  scale_x_continuous(name = "Scope/Depth") +
  scale_y_continuous(
    name = "21-m BCS Height (cm)", 
    oob = scales::squish, 
    limits = c(-1, 40)) +
  facet_wrap(~BOTTOM_DEPTH_FAC, scales = "free") +
  theme_bw()

ggplot() +
  geom_hline(yintercept = c(0, 2.54*3), linetype = 2) +
  geom_point(
    data = dplyr::filter(gt_bcs_subset, DISTANCE == 29),
    mapping = aes(
      x = SDR, 
      y = MEAN_HEIGHT, 
      shape = CODEND, 
      color = factor(BRIDLE_CHAIN_WEIGHT)
    ),
    alpha = 0.7,
    size = 2.2
  ) +
  scale_color_colorblind(name = "Chain weight (kg)") +
  # scale_size_manual(name = "Codend", values = codend_symbol_size) +
  scale_shape_manual(name = "Codend", values = c(4, 8)) +
  scale_x_continuous(name = "Scope/Depth") +
  scale_y_continuous(
    name = "29-m BCS Height (cm)", 
    oob = scales::squish, 
    limits = c(-1, 40)) +
  facet_wrap(~BOTTOM_DEPTH_FAC, scales = "free") +
  theme_bw()

# No facets


ggplot() +
  geom_path(
    data = scope_tables,
    mapping = aes(x = mean_depth_m, y = scope_to_depth, linetype = gear),
    color = "grey",
    linewidth = 1.2
  ) +
  geom_point(
    data = dplyr::filter(gt_data, !is.na(MAX_DOOR_ROLL)) |>
      dplyr::arrange(MAX_DOOR_ROLL), 
    mapping = aes(
      x = BOTTOM_DEPTH, 
      y = WIRE_LENGTH/BOTTOM_DEPTH, 
      color = cut(MAX_DOOR_ROLL, breaks = door_roll_breaks, labels = door_roll_labels), 
      shape = CODEND,
      size = CODEND),
    alpha = 0.8
  ) + 
  scale_linetype(name = "Scope table/gear") +
  scale_color_manual(name = "Roll (\u00B0)", values = door_roll_colors) +
  scale_x_log10(
    name = "Bottom depth (m)", 
    limits = c(depth_range[1]-2, depth_range[2]), 
    breaks = depth_breaks,
    oob = scales::oob_squish_infinite,
    expand = c(0,0)) +
  scale_y_continuous(
    name = "Scope/Depth", 
    oob = scales::oob_squish_infinite,
    breaks = sdr_axis_breaks,
    limits = c(2, max_sdr)) +
  # scale_size_manual(name = "Codend", values = c("Closed" = 3, "Open" = 1.8)) +
  scale_shape_manual(name = "Codend", values = codend_symbols) +
  theme_bw()

ggplot() +
  geom_path(
    data = scope_tables,
    mapping = aes(x = mean_depth_m, y = scope_to_depth, linetype = gear),
    color = "grey",
    linewidth = 1.2
  ) +
  geom_point(
    data = dplyr::filter(gt_data, !is.na(MAX_DOOR_ROLL)) |>
      dplyr::arrange(MAX_DOOR_ROLL), 
    mapping = 
      aes(
        x = BOTTOM_DEPTH, 
        y = WIRE_LENGTH/BOTTOM_DEPTH, 
        color = cut(MAX_DOOR_ROLL, breaks = door_roll_breaks, labels = door_roll_labels), 
        shape = CODEND),
    alpha = 0.8
  ) + 
  scale_linetype(name = "Scope table/gear") +
  scale_color_manual(name = "Roll (\u00B0)", values = door_roll_colors) +
  scale_x_log10(
    name = "Bottom depth (m)", 
    limits = c(depth_range[1]-2, depth_range[2]), 
    breaks = depth_breaks,
    oob = scales::oob_squish_infinite,
    expand = c(0,0)) +
  scale_y_continuous(
    name = "Scope/Depth", 
    oob = scales::oob_squish_infinite,
    breaks = sdr_axis_breaks,
    limits = c(2, max_sdr)) +
  # scale_size_manual(name = "Codend", values = c("Closed" = 3, "Open" = 1.8)) +
  scale_shape_manual(name = "Codend", values = codend_symbols) +
  theme_bw()



ggplot() +
  geom_hline(yintercept = 0, linetype = 3) +
  geom_path(
    data = bcs_height_summary,
    mapping = aes(x = HAUL, y = MEAN_HEIGHT, group = interaction(HAUL, SCOPE)),
    color = "grey30"
  ) +
  geom_point(
    data = bcs_height_summary,
    mapping = aes(x = HAUL, y = MEAN_HEIGHT, color = SIDE),
    size = 0.7
  ) +
  scale_color_manual(name = "Side", values = c('P'="#264EFF", 'C' = "grey60", 'S' = "#D92632")) +
  scale_y_continuous("Distance to bottom (cm)", 
                     limits = c(-2, 40), 
                     oob = scales::squish_infinite) +
  facet_wrap(~DISTANCE) +
  theme_bw()
