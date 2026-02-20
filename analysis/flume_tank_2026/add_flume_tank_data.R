library(trawlmetrics)
library(xlsx)


flume_2026 <- 
  xlsx::read.xlsx(file = here::here("analysis", "flume_tank_2026", "data", "flume_tank_data_2026.xlsx"), sheetName = "All") |>
  # dplyr::filter(nchar(trial) > 0) |>
  dplyr::mutate(
    trial = as.numeric(trial),
    bridles = ifelse(bridles == "Standard", "standard", bridles),
    floats_n = as.character(floats_n),
    year = 2026
  ) |>
  dplyr::rename(
    bridle_l_length_m = bridle_length_m
  )

flume_tank <- 
  trawlmetrics::flume_tank |>
  dplyr::filter(year == 2025) |>
dplyr::bind_rows(flume_2026)

save(flume_tank, file = here::here("data", "flume_tank.rda"))


flume_tank_rigging  <- 
  xlsx::read.xlsx(file = here::here("analysis", "flume_tank_2026", "data", "flume_tank_data_2026.xlsx"), sheetName = "rig") |>
  dplyr::mutate(
    bridles = ifelse(bridles == "Standard", "standard", bridles),
    trawl = paste0(trawl, " (2026)"),
    year = 2026
  )

flume_tank <- 
  dplyr::bind_rows(
  trawlmetrics::flume_tank |>
    dplyr::filter(trial == 92, year == 2026) |>
    dplyr::mutate(floats_n = "47",
                  total_buoyancy_kgf = 458.3,
                  sweep_length_m = 27.4),
  trawlmetrics::flume_tank |>
    dplyr::filter(!(trial == 92 & year == 2026))
) |>
  dplyr::arrange(year, trial)

# flume_tank <-
# trawlmetrics::flume_tank |>
#   dplyr::select(-total_buoyancy)

save(flume_tank, file = here::here("data", "flume_tank.rda"))

save(flume_tank_rigging, file = here::here("data", "flume_tank_rigging.rda"))


flume_tank <- 
  trawlmetrics::flume_tank |>
  dplyr::inner_join(
    data.frame(trawl = c("83-112", "PNE", "RACE", "RACE (2026)", "PNE (2026)"),
               trawl_name = c("83-112", "PNE", "RACE 88/112", "RACE 78/100", "PNE 78/100"))
  ) |>
  dplyr::mutate(
    trawl = trawl_name 
  ) |>
  dplyr::select(-trawl_name)

save(flume_tank, file = here::here("data", "flume_tank.rda"))

