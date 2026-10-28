library(dplyr)
library(readxl)

# Gear configuration spreadsheet
gear_config <- 
  readxl::read_xlsx(path = here::here("data", "2026_gear_config.xlsx")) |>
  dplyr::mutate(
    TOTAL_GEAR_LENGTH_M = (total_bridle_length_ft + door_leg_tail_chain_length_ft + bridle_chain_length_ft)/3.281
  ) |>
  dplyr::rename_with(toupper)|>
  dplyr::mutate(DOOR_SETTINGS =  factor(
    DOOR_SETTINGS, 
    levels = c("B-D", "B-E"), 
    labels = c("30.25\u00B0 (B-D)", "32.60\u00B0 (B-E)"),
  )) |>
  dplyr::mutate(DOOR_SIZE_M2 = factor(DOOR_SIZE_M2, levels = c(4, 4.5), labels = c("4.0", "4.5")),
                BRIDLE_OPTION = factor(BRIDLE_OPTION))


# Qualitative evaluation of footrope and door bottom contact
door_footrope_bc <- 
  readxl::read_xlsx(
    path = here::here("data", "scope_performance.xlsx"),
    sheet = "consensus"
  ) |>
  dplyr::rename(WIRE_LENGTH_FM = scope) |>
  dplyr::rename_with(toupper) |>
  dplyr::mutate(
    GEAR_ON_BOTTOM = ifelse(FOOTROPE_ON_BOTTOM == "Yes" & DOORS_ON_BOTTOM == "Yes", "Yes", "No"),
    GEAR_ON_BOTTOM = ifelse(FOOTROPE_ON_BOTTOM == "Inconclusive" | DOORS_ON_BOTTOM == "Inconclusive", "Inconclusive", GEAR_ON_BOTTOM),
    GEAR_ON_BOTTOM = factor(GEAR_ON_BOTTOM, levels = c("Yes", "Inconclusive", "No"))
  )

# Net and door measurements
trawl_measurement_summary <- 
  readRDS(file = here::here("output", "haul_summary.rds")) |>
  dplyr::group_by(scope, haul) |>
  dplyr::slice_max(pass, n = 1) |>
  dplyr::ungroup() |>
  dplyr::rename_with(toupper) |>
  dplyr::rename(
    WIRE_LENGTH_FM = SCOPE,
    NET_WIDTH = MEAN_NET_SPREAD,
    SD_NET_WIDTH = SD_NET_SPREAD,
    NET_HEIGHT = MEAN_NET_HEIGHT,
    SD_NET_HEIGHT = SD_NET_HEIGHT,
    DOOR_SPREAD = MEAN_DOOR_SPREAD,
    SD_DOOR_SPREAD = SD_DOOR_SPREAD,
    GEAR_DEPTH = BT_DEPTH_M,
    GEAR_DEPTH_FM = BT_DEPTH_FM,
    SCOPE_RATIO = SCOPE_TO_DEPTH) |>
  dplyr::mutate(
    WIRE_LENGTH = WIRE_LENGTH_FM * 1.8288,
    BOTTOM_DEPTH = BOTTOM_DEPTH_FM * 1.8288,
    INV_SCOPE = 1/SCOPE_RATIO,
  ) |>
  dplyr::select(-PASS)


# Winch tension, door depth, door pitch/roll
treatments <- 
  readxl::read_xlsx(
  path = here::here("data", "2026_gear_testing_haul_log.xlsx"),
  sheet = "treatments"
)  |>
  dplyr::group_by(Scope, Haul) |>
  dplyr::slice_max(Pass, n = 1) |>
  dplyr::ungroup() |>
  dplyr::filter(Event == "EQ") |>
  dplyr::rename(
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
  ) |>
  dplyr::mutate(
    WINCH_TENSION_TOTAL = WINCH_TENSION_S + WINCH_TENSION_P,
    DOOR_ROLL_MAX = pmax(abs(DOOR_ROLL_S), abs(DOOR_ROLL_P), na.rm = TRUE),
    DOOR_PITCH_MEAN = rowMeans(dplyr::pick(DOOR_PITCH_P, DOOR_PITCH_S), na.rm = TRUE)
  ) |>
  dplyr::rename_with(toupper) |>
  dplyr::select(-DOOR_SIZE_M2, -PASS, -DATE_AKDT, -TIME_AKDT)


# Bottom contact sensor mean and median height
bcs_heights <- 
  readRDS(file = here::here("output", "bcs_height_summary.rds"))  |>
  dplyr::group_by(scope, haul) |>
  dplyr::slice_max(pass, n = 1) |>
  dplyr::rename_with(toupper) |>
  dplyr::rename(
    WIRE_LENGTH_FM = SCOPE,
    BCS_HEIGHT_CM = MEAN_HEIGHT,
    BCS_HEIGHT_SD_CM = SD_HEIGHT
                ) |>
  dplyr::select(-PASS)

# Effort data from closed codend hauls

closed_effort <- 
  readRDS(here::here("data", "06_catch_data", "catch_records.rds")) |>
  dplyr::filter(VESSEL == 134) |>
  dplyr::group_by(AREA_SWEPT_KM2, HAUL) |>
  dplyr::summarise(TOTAL_CATCH_KG = sum(WEIGHT))

# Combine trawl measurement data into a single data frame
trawl_data_by_haul <- 
  dplyr::inner_join(
  trawl_measurement_summary,
  treatments) |>
  dplyr::inner_join(
    door_footrope_bc
  ) |>
  dplyr::inner_join(
    gear_config
  ) |>
  dplyr::mutate(
    BRIDLE_ANGLE_DEG = trawlmetrics::calc_bridle_angle(
      door_spread_m = DOOR_SPREAD, 
      wing_spread_m = NET_WIDTH, 
      total_bridle_length_m = TOTAL_GEAR_LENGTH_M
    )
  ) |>
  dplyr::left_join(
    closed_effort
  )

bcs_data_by_haul <- 
  dplyr::inner_join(
  bcs_heights,
  treatments) |>
  dplyr::inner_join(
    door_footrope_bc
  ) |>
  dplyr::inner_join(
    gear_config
  )

saveRDS(trawl_data_by_haul, here::here("data", "trawl_data_by_haul.rds"))
saveRDS(bcs_data_by_haul, here::here("data", "bcs_data_by_haul.rds"))


# Bottom contact sensor data

bcs_pings <- 
  readRDS(file = here::here("output", "bcs_segments.rds")) |>
  dplyr::filter(!is.na(scope)) |>
  dplyr::group_by(scope, haul) |>
  dplyr::slice_max(pass, n = 1) |>
  dplyr::ungroup() |>
  dplyr::rename(WIRE_LENGTH_FM = scope) |>
  dplyr::rename_with(toupper) |>
  dplyr::select(DT, BCS_ID, POSITION, DISTANCE, SIDE, X_G, X_G_ORIGINAL, HEIGHT_FIT, HAUL, WIRE_LENGTH_FM) |>
  dplyr::inner_join(door_footrope_bc) |>
  dplyr::left_join(
    gear_config
  )

saveRDS(bcs_pings, here::here("data", "bcs_pings.rds"))

# Load trawl measurement timeseries 

marport_pings <-
  readRDS(here::here("output", "trawl_measurements.rds")) |>
  dplyr::group_by(scope, haul) |>
  dplyr::slice_max(pass, n = 1) |>
  dplyr::ungroup() |>
  dplyr::rename(
    WIRE_LENGTH_FM = scope,
    DOOR_SPREAD = DOOR_SPREAD_M,
    NET_WIDTH = NET_SPREAD_M,
    NET_HEIGHT = NET_HEIGHT_M) |>
  dplyr::rename_with(toupper) |>
  dplyr::inner_join(
    dplyr::select(
      gear_config,
      HAUL,
      TOTAL_GEAR_LENGTH_M
    )
  ) |>
  dplyr::mutate(
    BRIDLE_ANGLE_DEG =
      trawlmetrics::calc_bridle_angle(
        door_spread_m = DOOR_SPREAD,
        wing_spread_m = NET_WIDTH,
        total_bridle_length_m = TOTAL_GEAR_LENGTH_M
      )
  ) |>
  dplyr::inner_join(door_footrope_bc)

saveRDS(marport_pings, here::here("data", "marport_pings.rds"))
