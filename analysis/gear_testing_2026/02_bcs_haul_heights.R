# Analyze bottom contact sensor data for each haul

library(trawlmetrics)
library(readxl)
library(here)
library(lubridate)
library(cowplot)
library(mgcv)

# Load functions
source("./functions/multi_pass_kalman.R")
source("./functions/parse_bcs_paths.R")
source("./functions/split_hauls.R")
source("./functions/isolate_treatments.R")

# Load bottom contact sensor height generalized additive models
bcs_gam_2026 <- readRDS(file = here::here("output", "01_bcs_output", "bcs_calibration_gams_2026.rds"))

# Process data from multi-pass hauls
dir.create(here::here("data", "01_bcs_data", "split_files"), recursive = TRUE, showWarnings = FALSE)

bcs_paths <- 
  list.files(
    path = here::here("data", "01_bcs_data", "combined_files"), 
    recursive = TRUE, pattern = ".csv", full.names = TRUE
  )

bcs_basename <- basename(bcs_paths)

for(ii in 1:length(bcs_paths)) {
  
  print(bcs_paths[ii])
  
  bcs_with_haul <- split_hauls(
    data_to_split = read.csv(
      file = bcs_paths[ii],
      fileEncoding = "latin1",
      skip = 1),
    date_time_field = "Date.Time..GMT.08.00",
    date_time_format = "%m/%d/%y %H:%M:%S",
    date_time_tz = "America/Anchorage",
    haul_log_path = here::here("data", "2026_gear_testing_haul_start_end.xlsx"),
    time_buffer_s = 30
  )
  
  if(nrow(bcs_with_haul) < 1) {
    next
  }
  
  header <- readLines(bcs_paths[ii], 2)[2]
  
  unique_bcs_hauls <- unique(bcs_with_haul$haul)
  
  for(jj in 1:length(unique_bcs_hauls)) {
    
    sel_haul <- bcs_with_haul |>
      dplyr::filter(haul == unique_bcs_hauls[jj]) |>
      dplyr::select(-haul)
    
    new_basename <- sub("^\\d+", replacement = unique_bcs_hauls[jj], x = bcs_basename[ii])
    
    fpath <- here::here("data", "01_bcs_data", "split_files", new_basename)
    
    writeLines(paste0("Plot Title: ", new_basename, " \n"), con = fpath)
    writeLines(header, con = fpath)
    
    write.table(
      x = sel_haul,
      file = fpath,
      append = TRUE,
      sep = ",",
      row.names = FALSE,
      col.names = FALSE,
      quote = FALSE
    )
    
  }
  
}

# Estimate BCS heights

bcs_haul_data <- 
  data.frame(
    path = list.files(path = here::here("data", "01_bcs_data", "haul_data"), full.names = TRUE, recursive = TRUE, pattern = ".csv")
  )

bcs_haul_data <- 
  cbind(
    bcs_haul_data,
    lapply(X = bcs_haul_data$path, FUN = parse_bcs_paths) |>
      do.call(what = rbind)
  )

bcs_bc_data <- vector(mode = "list", length = nrow(bcs_haul_data))

for(ii in 1:nrow(bcs_haul_data)) {
  
  bc <- read.csv(bcs_haul_data$path[ii], skip = 1, fileEncoding = "latin1")
  
  if(ncol(bc) > 11) {
    bc <- bc[, 2:8]
    colnames(bc) <- c("dt", "x_g", "y_g", "z_g", "x_tilt", "y_tilt", "z_tilt")
  } else{
    bc <- bc[, 2:5]
    colnames(bc) <- c("dt", "x_g", "y_g", "z_g")
  }
  
  bc$haul <- bcs_haul_data$haul[ii]
  bc$bcs_id <- bcs_haul_data$bcs_id[ii]
  bc$position <- bcs_haul_data$position[ii]
  bc$distance <-  as.numeric(gsub("\\D", "", bc$position))
  bc$side <- gsub("[0-9]", "", bc$position)
  bc$dt <- as.POSIXct(bc$dt, format = "%m/%d/%y %H:%M:%S", tz = "America/Anchorage")
  
  # Double-pass Kalman filter on x-axis acceleration
  bc$x_g_original <- bc$x_g
  bc$x_g <- multi_pass_kalman(bc$x_g, n_passes = 1, mode = "lowpass", q = 0.01)
  
  
  bc$height_fit <- predict(object = bcs_gam_2026[[bcs_haul_data$bcs_id[ii]]]$model, bc)
  
  bcs_bc_data[[ii]] <- bc
  
}

bcs_heights <- lapply(
  X = bcs_bc_data, 
  FUN = 
    function(x) {
      x[c("dt", "bcs_id", "haul", "position", "distance", "side", "height_fit", "x_g", "x_g_original")]
    }
) |>
  do.call(what = dplyr::bind_rows) |>
  # heights -3 to -5 are changed to -3
  # heights > 40 cm are changed to 40 cm, corresponding to maximum calibration value
  # heights < -5 are removed
  dplyr::mutate(height_fit == ifelse(height_fit <= -3 & height_fit >= -5, -3, height_fit),
                height_fit = ifelse(height_fit > 40, 40, height_fit)) |>
  dplyr::filter(height_fit > -5)


saveRDS(bcs_heights, file = here::here("data", "01_bcs_data", "bcs_heights.rds"))

# Split BCS heights by treatment within a haul

bcs_segments <- 
  isolate_treatments(
    data_to_split = bcs_heights, 
    haul_log_path = here::here("data", "2026_gear_testing_haul_log.xlsx"), 
    buffer_eq_s = 30, buffer_scope_change_s = 10, buffer_hb_s = 10)


bcs_height_summary <- 
  bcs_segments |>
  dplyr::filter(!is.na(scope)) |>
  dplyr::group_by(haul, position, distance, side, pass, scope) |>
  dplyr::summarise(median_height = median(height_fit, na.rm = TRUE),
                   mean_height = mean(height_fit, na.rm = TRUE),
                   sd_height = sd(height_fit, na.rm = TRUE))

unique_haul_scope <- 
  bcs_height_summary |>
  dplyr::ungroup() |>
  dplyr::select(haul, scope) |>
  unique()

saveRDS(bcs_height_summary, file = here::here("output", "bcs_height_summary.rds"))
saveRDS(bcs_segments, file = here::here("output", "bcs_segments.rds"))

