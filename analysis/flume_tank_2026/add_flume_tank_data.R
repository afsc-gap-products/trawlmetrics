library(trawlmetrics)
library(xlsx)


flume_2026 <- 
  xlsx::read.xlsx(file = here::here("analysis", "flume_tank_2026", "data", "flume_tank_data_2026.xlsx"), sheetName = "All") |>
  # dplyr::filter(nchar(trial) > 0) |>
  dplyr::mutate(
    trial = as.numeric(trial),
    bridles = ifelse(bridles == "Standard", "standard", bridles),
    trawl = paste0(trawl, " (2026)"),
    floats_n = as.character(floats_n),
    year = 2026
  ) |>
  dplyr::rename(
    bridle_l_length_m = bridle_length_m
  )


flume_tank <- 
  trawlmetrics::flume_tank |>
  dplyr::filter(year == 2025) |>
  dplyr::mutate(door_spread_m = door_m) |>
  dplyr::select(-door_m, -bridle_length_m) |>
dplyr::bind_rows(flume_2026)

save(flume_tank, file = here::here("data", "flume_tank.rda"))


flume_tank_rigging  <- 
  xlsx::read.xlsx(file = here::here("analysis", "flume_tank_2026", "data", "flume_tank_data_2026.xlsx"), sheetName = "rig") |>
  dplyr::mutate(
    bridles = ifelse(bridles == "Standard", "standard", bridles),
    trawl = paste0(trawl, " (2026)"),
    year = 2026
  )

save(flume_tank_rigging, file = here::here("data", "flume_tank_rigging.rda"))
