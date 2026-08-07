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
  
  bcs_with_haul <- split_hauls(
    data_to_split <- read.csv(
      file = bcs_paths[ii],
      fileEncoding = "latin1",
      skip = 1),
    date_time_field = "Date.Time..GMT.08.00",
    date_time_format = "%m/%d/%y %H:%M:%S",
    date_time_tz = "America/Anchorage",
    haul_log_path = here::here("data", "2026_gear_testing_haul_start_end.xlsx"),
    time_buffer_s = 30
  )
  
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
  bc$position <- bcs_haul_data$position[ii]
  bc$distance <-  as.numeric(gsub("\\D", "", bc$position))
  bc$side <- gsub("[0-9]", "", bc$position)
  bc$dt <- as.POSIXct(bc$dt, format = "%m/%d/%y %H:%M:%S", tz = "America/Anchorage")
  
  # Double-pass Kalman filter on x-axis acceleration
  bc$x_g_original <- bc$x_g
  bc$x_g <- multi_pass_kalman(bc$x_g, n_passes = 2, mode = "lowpass", q = 0.01)
  
  
  bc$height_fit <- predict(object = bcs_gam_2026[[bcs_haul_data$bcs_id[ii]]]$model, bc)
  
  bcs_bc_data[[ii]] <- bc
  
}

bcs_heights <- lapply(
  X = bcs_bc_data, FUN = 
         function(x) {
           x[c("dt", "haul", "position", "distance", "side", "height_fit", "x_g")]
         }
  ) |>
  do.call(what = dplyr::bind_rows) |>
  dplyr::filter(height_fit > -0.1)


saveRDS(bcs_heights, file = here::here("data", "01_bcs_data", "bcs_heights.rds"))

# Split BCS heights by treatment within a haul

bcs_segments <- isolate_treatments(
  data_to_split = bcs_heights, 
  haul_log_path = here::here("data", "2026_gear_testing_haul_log.xlsx"), 
  buffer_eq_s = 30, buffer_scope_change_s = 10, buffer_hb_s = 10)


height_summary <- 
  bcs_segments |>
  dplyr::filter(!is.na(scope)) |>
  dplyr::group_by(haul, position, distance, side, pass, scope) |>
  dplyr::summarise(median_height = median(height_fit, na.rm = TRUE),
                   mean_height = mean(height_fit, na.rm = TRUE),
                   sd_height = sd(height_fit, na.rm = TRUE))

unique_haul_scope <- 
  height_summary |>
  dplyr::ungroup() |>
  dplyr::select(haul, scope) |>
  unique()

# Plot scope results

dir.create(here::here("plots", "dtb_haul_scope"), recursive = TRUE, showWarnings = FALSE)

for(ii in 1:nrow(unique_haul_scope)) {
  
  sel_hsp <- unique_haul_scope[ii, ]
  
  p_trt <- ggplot() + 
    geom_point(
      data = height_summary |>
        dplyr::inner_join(sel_hsp, by = c("haul", "scope")),
      mapping = aes(
        x = ifelse(side == "P", distance*-1, distance),
        y = median_height)
    ) +
    ggtitle(paste0("Haul: ", sel_hsp$haul, ", Scope: ", sel_hsp$scope, " fm")) +
    geom_vline(xintercept = 0, linetype = 2) +
    scale_x_continuous(name = "Distance from center (m)") +
    scale_y_continuous(name = "Distance off bottom (cm)", limits = c(-0.1, 60), expand = c(0,0)) +
    theme_bw()
  
  png(filename = here::here("plots", "dtb_haul_scope", paste0("dist_to_bottom_", sel_hsp$haul, "_", sel_hsp$scope, ".png")),
      width = 4, height = 4, units = "in", res = 300)
  print(p_trt)
  dev.off()
  
  
}

p_dtb_all <-
  ggplot() +
  geom_point(
    data = height_summary,
    mapping = aes(
      x = ifelse(side == "P", distance*-1, distance),
      y = median_height)
  ) +
  geom_vline(xintercept = 0, linetype = 2) +
  scale_x_continuous(name = "Distance from center (m)") +
  scale_y_continuous(name = "Distance off bottom (cm)", limits = c(-0.1, 60), expand = c(0,0)) +
  theme_bw()

png(filename = here::here("plots", "dtb_haul_scope", paste0("dist_to_bottom_all_hauls.png")),
    width = 4, height = 4, units = "in", res = 300)
print(p_trt)
dev.off()
