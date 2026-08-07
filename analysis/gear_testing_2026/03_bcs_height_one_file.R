# Estimate BCS height for a single haul

# Analyze bottom contact sensor data for each haul

library(trawlmetrics)
library(readxl)
library(here)
library(lubridate)
library(cowplot)
library(mgcv)
library(ggpp)

# Load Kalman filter
source("./functions/multi_pass_kalman.R")
source("./functions/parse_bcs_paths.R")
source("./functions/split_hauls.R")
source("./functions/isolate_treatments.R")

haul_log_path <- here::here("data", "2026_gear_testing_haul_log.xlsx")

# Load BCS generalized additive models
bcs_gam_2026 <- readRDS(file = here::here("output", "01_bcs_output", "bcs_calibration_gams_2026.rds"))

# Specify files
bcs_files <- 
  data.frame(
    path = list.files(path = here::here("data", "01_bcs_data", "haul_data", "Aug 7"), 
                      full.names = TRUE, recursive = TRUE, pattern = ".csv")
  )

bcs_files <- 
  cbind(
    bcs_files,
    lapply(X = bcs_files$path, FUN = parse_bcs_paths) |>
      do.call(what = rbind)
  )

unique_hauls <- unique(bcs_files$haul)

dir.create(here::here("plots", "bcs_dtb"), recursive = TRUE, showWarnings = FALSE)

bcs_all_segments <- vector(mode = "list", length = length(unique_hauls))

for(vv in 1:length(unique_hauls)) {
  
  bcs_haul_data <- dplyr::filter(bcs_files, haul == unique_hauls[vv])
  
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
    X = bcs_bc_data, 
    FUN = 
      function(x) {
        x[c("dt", "haul", "position", "distance", "side", "height_fit", "x_g")]
      }
  ) |>
    do.call(what = dplyr::bind_rows) |>
    dplyr::filter(height_fit > -0.1)
  
  # Split BCS heights by treatment within a haul
  
  bcs_segments <- 
    isolate_treatments(
      data_to_split = bcs_heights, 
      haul_log_path = haul_log_path, 
      buffer_eq_s = 30, buffer_scope_change_s = 10, buffer_hb_s = 10) |>
    dplyr::filter(!is.na(scope))
  
  
  height_summary <- 
    bcs_segments |>
    dplyr::filter(!is.na(scope)) |>
    dplyr::group_by(haul, position, distance, side, pass, scope) |>
    dplyr::summarise(
      median_height = median(height_fit, na.rm = TRUE),
      mean_height = mean(height_fit, na.rm = TRUE),
      sd_height = sd(height_fit, na.rm = TRUE)
    )
  
  p_bcs_median <- 
    ggplot() + 
    geom_point(
      data = height_summary,
      mapping = aes(
        x = ifelse(side == "P", distance*-1, distance),
        y = median_height,
        color = factor(scope)), 
      size = rel(2.5),
      alpha = 0.8
    ) +
    ggtitle(paste0("BCS Height, Haul: ", height_summary$haul[1])) +
    geom_vline(xintercept = 0, linetype = 2) +
    scale_x_continuous(name = "Distance from center (m)") +
    scale_y_continuous(name = "Distance off bottom (cm)", limits = c(-0.1, 60), expand = c(0,0)) +
    scale_color_viridis_d(name = "Scope (fm)", direction = -1) +
    theme_bw()
  
  panel_labels <- 
    height_summary |>
    dplyr::select(haul, distance) |>
    unique()
  
  p_bcs_timeseries <- 
    ggplot() +
    geom_path(
      data = bcs_segments,
      mapping = aes(x = dt, y = height_fit, color = factor(scope), linetype = side),
      linewidth = 1.1
    ) +
    ggpp::geom_text_npc(
      data = panel_labels, 
      mapping = aes(npcx = "left", npcy = "top", label = paste0(distance, " m"))
    ) +
    scale_x_datetime(name = "Date/time (AKDT)") +
    scale_y_continuous(name = "Distance off bottom (cm)", limits = c(-0.1, 60), expand = c(0,0)) +
    scale_color_viridis_d(name = "Scope (fm)", direction = -1) +
    scale_linetype(name = "Side") +
    ggtitle("Time series") +
    facet_wrap(~distance , ncol = 1) +
    theme_bw() +
    theme(strip.text = element_blank(),
          strip.background = element_blank())
  
  p_bcs_panels <-
    cowplot::plot_grid(
      p_bcs_median + theme(legend.position = "none"),
      p_bcs_timeseries,
      ncol = 2,
      rel_widths = c(0.4,0.6)
    )
  
  png(filename = here::here("plots", "bcs_dtb", paste0(panel_labels$haul[1], "_dist_to_bottom", ".png")),
      height = 6, width = 8, units = "in", res = 300)
  print(p_bcs_panels)
  dev.off()
  
}
