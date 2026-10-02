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

scope_tables <- 
  read_xlsx(path = here::here("data", "shelf_slope_table.xlsx")) |>
  dplyr::mutate(mean_depth_fm = (min_depth_fm+max_depth_fm)/2,
                scope_to_depth = wire_out_fm/mean_depth_fm) |>
  dplyr::inner_join(data.frame(table = c("GOA/AI", "EBS shelf", "EBS slope"), gear = c("PNE", "83-112", "PNE-S")))

scs_xml <- list.files(here::here("data", "04_scs_data"), full.names = TRUE, pattern = ".xml")

# scs_xml <- scs_xml[50:52]

# Load gear configuration data

trawl_measurements <- 
  lapply(X = scs_xml, FUN = parse_nmea_xml) |>
  do.call(what = dplyr::bind_rows) |> 
  isolate_treatments() |>
  dplyr::filter(!is.na(scope)) |>
  dplyr::select(haul, dt, NET_HEIGHT_M, NET_SPREAD_M, DOOR_SPREAD_M, pass, scope)

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
  dplyr::select(dt, HAUL, DEPTH)

names(btd_data) <- tolower(names(btd_data))

btd_summary <- 
  isolate_treatments(btd_data) |>
  dplyr::filter(!is.na(scope)) |>
  dplyr::group_by(haul, pass, scope) |>
  dplyr::summarise(
    BT_DEPTH_M = mean(depth, na.rm = TRUE),
    BT_DEPTH_FM = BT_DEPTH_M/1.8288
  ) |>
  dplyr::inner_join(
    trawl_measurement_summary |>
      dplyr::select(haul, scope, MEAN_NET_HEIGHT)) |>
  dplyr::mutate(
    BOTTOM_DEPTH_FM = BT_DEPTH_FM + MEAN_NET_HEIGHT/1.8288,
    SCOPE_TO_DEPTH = scope/BOTTOM_DEPTH_FM 
  )

saveRDS(trawl_measurement_summary, here::here("output", "haul_summary.rds"))
saveRDS(btd_summary, here::here("output", "btd_summary.rds"))
saveRDS(trawl_measurements, here::here("trawl_measurements.rds"))
