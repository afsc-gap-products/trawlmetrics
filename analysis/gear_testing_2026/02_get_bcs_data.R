

library(trawlmetrics)
library(readxl)
library(here)
library(lubridate)
library(cowplot)
library(mgcv)
library(dplyr)

source("./functions/parse_bcs_paths.R")
source("./functions/multi_pass_kalman.R")
source("./functions/isolate_treatments.R")


# BCS paths
bcs_paths <- list.files(
  path = here::here("data", "01_bcs_data", "haul_data"), 
  full.names = TRUE, recursive = TRUE, pattern = ".csv"
)

# Load bottom contact sensor height generalized additive models
bcs_gam_2026 <- readRDS(file = here::here("output", "01_bcs_output", "bcs_calibration_gams_2026.rds"))


bcs_data <- 
  lapply(
    X = bcs_paths, 
    bcs_gams = bcs_gam_2026 ,
    FUN = 
      function(x, bcs_gams) {
        
        metadata <- parse_bcs_paths(x)[c("bcs_id", "position")]
        
        bc <- read.csv(x, skip = 1, fileEncoding = "latin1")
        
        if(ncol(bc) > 11) {
          bc <- bc[, 2:8]
          colnames(bc) <- c("dt", "x_g", "y_g", "z_g", "x_tilt", "y_tilt", "z_tilt")
        } else {
          bc <- bc[, 2:5]
          colnames(bc) <- c("dt", "x_g", "y_g", "z_g")
        }
        
        bc <- bc[c("dt", "x_g", "y_g", "z_g")]
        
        bc$bcs_id <- metadata$bcs_id
        bc$position <- metadata$position
        bc$distance <-  as.numeric(gsub("\\D", "", metadata$position))
        bc$side <- gsub("[0-9]", "", metadata$position)
        bc$dt <- as.POSIXct(bc$dt, format = "%m/%d/%y %H:%M:%S", tz = "America/Anchorage")
        
        # Kalman filter on x-axis acceleration
        bc$x_g_original <- bc$x_g
        bc$x_g <- multi_pass_kalman(bc$x_g, n_passes = 1, mode = "lowpass", q = 0.01)
        
        bc$height_fit <- 
          predict(
            object = bcs_gams[[metadata$bcs_id]]$model, 
            newdata = bc
          )
        
        return(bc)
        
      }
    
  )

bcs_data <- 
  bcs_data |>
  do.call(what = dplyr::bind_rows) |>
  dplyr::group_by(dt, bcs_id, position, distance, side) |>
  dplyr::summarise(
    x_g = mean(x_g), 
    y_g = mean(y_g),
    z_g = mean(z_g),
    x_g_original = mean(x_g_original),
    height_fit = mean(height_fit)
  ) |>
  dplyr::ungroup()

# Isolate BCS data by treatment

bcs_segments <- 
  isolate_treatments(
    data_to_split = bcs_data, 
    haul_log_path = here::here("data", "2026_gear_testing_haul_log.xlsx"), 
    buffer_eq_s = 30, 
    buffer_scope_change_s = 10, 
    buffer_hb_s = 10
    )


saveRDS(bcs_segments, file = here::here("output", "bcs_segments.rds"))
