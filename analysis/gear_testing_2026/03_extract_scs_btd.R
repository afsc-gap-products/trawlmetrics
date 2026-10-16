# Extract door spread from SCS xml files
library(xml2)
library(ggthemes)
library(dplyr)
library(ggplot2)
library(glmmTMB)
library(ggrepel)
library(readxl)

source("./functions/extract_and_rename_xml.R")
source("./functions/parse_nmea_xml.R")
source("./functions/isolate_treatments.R")
source("./functions/bridle_angle_wes.R")

scs_zip <- list.files(here::here("data", "04_scs_data"), full.names = TRUE, pattern = ".zip")

vapply(scs_zip, extract_and_rename_xml, FUN.VALUE = character(1))

scs_xml <- list.files(here::here("data", "04_scs_data"), full.names = TRUE, pattern = ".xml")

# Load gear configuration data

trawl_measurements <- 
  lapply(X = scs_xml, FUN = parse_nmea_xml) |>
  do.call(what = dplyr::bind_rows) |> 
  isolate_treatments() |>
  dplyr::filter(!is.na(scope)) |>
  dplyr::select(haul, dt, NET_HEIGHT_M, NET_SPREAD_M, DOOR_SPREAD_M, pass, scope) |>
  dplyr::mutate(
    NET_SPREAD_M = ifelse(NET_HEIGHT_M > 24, NA, NET_SPREAD_M),
    NET_HEIGHT_M = ifelse(NET_HEIGHT_M > 15, NA, NET_HEIGHT_M),
    DOOR_SPREAD_M = ifelse(DOOR_SPREAD_M > 80, NA, DOOR_SPREAD_M),
    NET_SPREAD_M = ifelse(NET_HEIGHT_M < 8, NA, NET_SPREAD_M),
    NET_HEIGHT_M = ifelse(NET_HEIGHT_M < 3, NA, NET_HEIGHT_M),
    DOOR_SPREAD_M = ifelse(DOOR_SPREAD_M < 20, NA, DOOR_SPREAD_M),
  )

trawl_measurement_summary <- 
  trawl_measurements |>
  dplyr::group_by(
    haul, scope
  ) |>
  dplyr::summarise(
    MEAN_NET_SPREAD = mean(NET_SPREAD_M, na.rm = TRUE),
    SD_NET_SPREAD = sd(NET_SPREAD_M, na.rm = TRUE),
    MEAN_NET_HEIGHT = mean(NET_HEIGHT_M, na.rm = TRUE),
    SD_NET_HEIGHT = sd(NET_HEIGHT_M, na.rm = TRUE),
    MEAN_DOOR_SPREAD = mean(DOOR_SPREAD_M, na.rm = TRUE),
    SD_DOOR_SPREAD = sd(DOOR_SPREAD_M, na.rm = TRUE),
    MIN_DT = min(dt),
    MAX_DT = max(dt),
    MEAN_DT = mean(dt)
  ) |>
  dplyr::mutate(
    CV_NET_SPREAD = SD_NET_SPREAD/MEAN_NET_SPREAD,
    CV_NET_HEIGHT = SD_NET_HEIGHT/MEAN_NET_HEIGHT,
    CV_DOOR_SPREAD = SD_DOOR_SPREAD/MEAN_DOOR_SPREAD
  )

# Parse BT and calculate averages for each treatment

btd_path <- list.files(here::here("data", "05_btd_data"), pattern = ".BTD", full.names = TRUE)

btd_data <- 
  lapply(X = btd_path, FUN = read.csv) |>
  do.call(what = dplyr::bind_rows) |>
  dplyr::mutate(dt = as.POSIXct(DATE_TIME, tz = "America/Anchorage", format = "%m/%d/%Y %H:%M:%S")) |>
  dplyr::rename_with(tolower) |>
  isolate_treatments() |>
  dplyr::select(dt, haul, depth, pass, scope)
  
haul_summary <- 
  btd_data |>
  dplyr::filter(!is.na(scope)) |>
  dplyr::group_by(haul, pass, scope) |>
  dplyr::summarise(
    BT_DEPTH_M = mean(depth, na.rm = TRUE),
    BT_DEPTH_FM = BT_DEPTH_M/1.8288
  ) |>
  dplyr::inner_join(
    trawl_measurement_summary
    ) |>
  dplyr::mutate(
    BOTTOM_DEPTH_FM = BT_DEPTH_FM + MEAN_NET_HEIGHT/1.8288,
    SCOPE_TO_DEPTH = scope/BOTTOM_DEPTH_FM 
  )

saveRDS(haul_summary, here::here("output", "haul_summary.rds"))
saveRDS(btd_data, here::here("output", "btd_data.rds"))
saveRDS(trawl_measurements, here::here("output", "trawl_measurements.rds"))
