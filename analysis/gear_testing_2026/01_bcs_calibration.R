# Develop bottom contact sensor height models for 2026 prototype gear testing experiments

library(trawlmetrics)
library(readxl)
library(here)
library(lubridate)
library(cowplot)
library(mgcv)
library(ggthemes)

# Read calibration event data containing time-referenced height and tilt data

# 1_6 = sensors 1-6
# 7-12 = sensors 7-12

events_1_6 <- 
  readxl::read_xlsx(
    here::here("data", "01_bcs_data", "bcs_calibration.xlsx"),
    sheet = "BCS 1-6",
    skip = 1
  )

colnames(events_1_6) <- c("dt", "height", "angle", "angle2")

events_7_12 <- 
  readxl::read_xlsx(
    here::here("data", "01_bcs_data", "bcs_calibration.xlsx"),
    sheet = "BCS 7-12",
    skip = 1
  )

colnames(events_7_12) <- c("dt", "height", "angle")

events <- 
  dplyr::bind_rows(
    events_1_6,
    events_7_12
  )

# Some angles were measured incorrectly during the first pass; make corrections with second pass values
events$angle[is.na(events$angle)] <- events$angle2[is.na(events$angle)]
events <- events[c("dt", "height", "angle")]

# Date/time corrections
lubridate::year(events$dt) <- 2026
lubridate::month(events$dt) <- 7 
events$dt <- lubridate::force_tz(events$dt, "America/Anchorage")
events$dt <- events$dt + 12*3600

max_dt <- as.POSIXct("2026-07-31 20:48:15", tz = "America/Anchorage")

# Read observations from sensors 1-12
cal_files <- list.files(here::here("data", "01_bcs_data", "calibration_files"), pattern = ".csv", recursive = TRUE, full.names = TRUE)

cal_ids <- gsub("\\D", "", basename(cal_files))

read_cal_file <- function(file_path, cols = 2:8) {
  
  df <- read.csv(file = file_path, skip = 1)[, cols]
  
  colnames(df) <- c("dt", "x_g", "y_g", "z_g", "x_tilt", "y_tilt", "z_tilt")
  
  df$dt <- as.POSIXct(df$dt, format = "%m/%d/%y %H:%M:%S", tz = "America/Anchorage")
  
  return(df)
}

cal_data <- setNames(lapply(cal_files, FUN = read_cal_file), cal_ids)

bcs_gam_2026 <- vector(mode = "list", length = length(cal_data))
names(bcs_gam_2026) <- names(cal_data)

# Match heights with tilt values and fit GAMs

for(jj in 1:length(cal_data)) {
  
  sel_cal <- cal_data[[jj]]
  sel_cal$height <- NA
  sel_cal$angle <- NA
  sel_cal$sensor_id <- names(cal_data)[jj]
  
  for(ii in 1:nrow(events)) {
    
    idx <- sel_cal$dt > events$dt[ii] + 5 & sel_cal$dt < events$dt[ii] + 50 & sel_cal$dt < max_dt 
    
    sel_cal$height[idx] <- events$height[ii]
    
    sel_cal$angle[idx] <- events$angle[ii]
    
  }
  
  # Remove values without heights
  sel_cal <- sel_cal[!is.na(sel_cal$height), ]
  
  cal_gam <- mgcv::gam(height ~ s(x_g, bs = "tp"), data = sel_cal)
  
  fit <- data.frame(x_g = seq(-1,0,0.01))
  fit$fit_height <- predict(cal_gam, newdata = fit)
  fit$se_fit_height <- predict(cal_gam, newdata = fit, se.fit = TRUE)$se.fit
  fit$sensor_id <- names(cal_data)[jj]
  
  p_cal <- 
    cowplot::plot_grid(
      ggplot() +
        geom_point(
          data = sel_cal,
          mapping = aes(x = dt, y = x_tilt)
        ) + ggtitle(paste0("BCS #", names(cal_data)[jj])),
      ggplot() +
        geom_point(
          data = sel_cal,
          mapping = aes(x = dt, y = height)
        ) + ggtitle(paste0("BCS #", names(cal_data)[jj])),
      ggplot() +
        geom_point(
          data = sel_cal,
          mapping = aes(x = x_g, y = x_tilt)
        ),
      ggplot() +
        geom_point(
          data = sel_cal,
          mapping = aes(x = x_g, y = height)
        ) +
        geom_path(
          data = fit,
          mapping = aes(x = x_g, y = fit_height),
          color = "blue"
        ) +
        scale_y_continuous(name = "Height (cm)"),
      nrow = 4
    )
  
  print(p_cal)
  
  bcs_gam_2026[[jj]] <- list(
    model = cal_gam,
    data = sel_cal,
    fit = fit,
    plot = p_cal
  )
  
}

saveRDS(object = bcs_gam_2026, file = here::here("output", "01_bcs_output", "bcs_calibration_gams_2026.rds"))

# Plot all calibration data

bcs_cal_fit <- lapply(bcs_gam_2026, FUN = function(x) x$fit) |>
  do.call(what = rbind) |>
  dplyr::mutate(sensor_id = factor(as.numeric(sensor_id)))

bcs_cal_data <- lapply(bcs_gam_2026, FUN = function(x) x$data) |>
  do.call(what = rbind) |>
  dplyr::mutate(sensor_id = factor(as.numeric(sensor_id)))

p_bcs_fits <- 
  ggplot() +
  geom_ribbon(data = bcs_cal_fit, mapping = aes(x = x_g, ymin = fit_height - 2*se_fit_height, ymax = fit_height + 2*se_fit_height, fill = sensor_id), alpha = 0.2) +
  geom_point(data = bcs_cal_data, mapping = aes(x = x_g, y = height, color = sensor_id), alpha = 0.5, shape = 21) +
  geom_path(data = bcs_cal_fit, mapping = aes(x = x_g, y = fit_height, color = sensor_id)) +
  scale_color_viridis_d(name = "BCS #", option = "A") +
  scale_fill_viridis_d(name = "BCS #", option = "A") +
  scale_x_continuous(name = "X-axis acceleration (g)") +
  scale_y_continuous(name = "Distance to bottom (cm)") +
  theme_bw() +
  theme(legend.position = "inside",
        legend.position.inside = c(0.92, 0.6))

png(here::here("plots", "bcs_gam_fits.png"), width = 169, height = 120, units = "mm", res = 300)
print(p_bcs_fits)
dev.off()


