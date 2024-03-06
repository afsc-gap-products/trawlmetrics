library(trawlmetrics)
library(ggthemes)

get_bcs_data <- function() {
  
  dir.create(path = here::here("analysis", "door_experiment", "plots", "bcs_calibration"))
  
  # Load spread and height data + haul events + treatments
  load(here::here("analysis", "door_experiment", "data", "spread_height_data.rda"))
  
  # Load locations of BCS on trawls, by haul
  bcs_loc <- read.csv(file = here::here("analysis", "door_experiment", "data", "2023_bcs_position_haul.csv")) |>
    tidyr::pivot_longer(cols = paste0("X", 1:5), names_to = "bcs_unit", values_to = "location") |>
    dplyr::mutate(bcs_unit = gsub(pattern = "X", replacement = "", x = bcs_unit))
  
  # Function to read in bottom contact sensor calibration data and fit a linear model between X acceleration and height
  fit_bcs_calibration <- function(bcs_path, cal_path, bcs_unit) {
    
    bcs_dat <- read.csv(file = bcs_path)
    bcs_dat$bcs_unit <- bcs_unit
    bcs_dat$Height <- NA
    bcs_dat$Time <- as.POSIXct(bcs_dat$Time, tz = "America/Anchorage", format = "%H:%M:%S")
    
    cal_dat <- read.csv(file = cal_path)
    cal_dat$Start <- as.POSIXct(cal_dat$Start, tz = "America/Anchorage", format = "%H:%M:%S")
    cal_dat$End <- as.POSIXct(cal_dat$End, tz = "America/Anchorage", format = "%H:%M:%S")
    
    # Assign BCS observations to treatments
    
    mod_fit <- data.frame()
    
    for(ii in 1:nrow(cal_dat)) {
      bcs_dat$Height[bcs_dat$Time >= cal_dat$Start[ii] & bcs_dat$Time <= cal_dat$End[ii]] <- cal_dat$Height[ii]
      
    }
    
    bcs_dat <- bcs_dat[!is.na(bcs_dat$Height), ]
    
    mod <- lm(formula = Height ~ X_Accel, data = bcs_dat)
    
    mod_fit <- rbind(data.frame(bcs_unit = bcs_unit,
                                Height = min(cal_dat$Height):max(cal_dat$Height),
                                fit = predict(mod, newdata = data.frame(X_Accel = min(cal_dat$Height):max(cal_dat$Height))),
                                se = predict(mod, newdata = data.frame(X_Accel = min(cal_dat$Height):max(cal_dat$Height)), se.fit = TRUE)$se.fit),
                     mod_fit)
    
    return(list(bcs = bcs_dat, cal = cal_dat, lm_mod = mod, bcs_unit = bcs_unit, mod_fit = mod_fit))
    
  }
  
  # Function to predict height from BCS data
  predict_bcs_height <- function(haul_paths, cal_data) {
    
    output <- vector(mode = "list", length = length(haul_paths))
    
    for(ii in 1:length(haul_paths)) {
      
      dat <- read.csv(file = haul_paths[[ii]])
      
      bcs_unit <- unlist(strsplit(haul_paths[ii], split = ""))[nchar(haul_paths[ii])-4]
      
      dat$bcs_unit <- bcs_unit
      
      dat$HAUL <- (unlist(strsplit(haul_paths[ii], split = "Haul"))[2] |>
                     strsplit(split = "_") |>
                     unlist())[1] |>
        as.numeric()
      
      bcs_mod <- cal_data[[bcs_unit]]$lm_mod
      
      dat$Height <- predict(bcs_mod, newdata = dat)
      
      output[[ii]] <- dat
      
    }
    
    output <- do.call(what = "rbind", output)
    
    return(output)
    
  }
  
  # Fit models to calibration data
  cal_data <- list(`1` = fit_bcs_calibration(bcs_path = here::here("analysis", "door_experiment", "data", "bcs_calibration", "bcs_1_cal.csv"),
                                             cal_path = here::here("analysis", "door_experiment", "data", "bcs_calibration", "cal_123.csv"),
                                             bcs_unit = 1),
                   `2` = fit_bcs_calibration(bcs_path = here::here("analysis", "door_experiment", "data", "bcs_calibration", "bcs_2_cal.csv"),
                                             cal_path = here::here("analysis", "door_experiment", "data", "bcs_calibration", "cal_123.csv"),
                                             bcs_unit = 2),
                   `3` = fit_bcs_calibration(bcs_path = here::here("analysis", "door_experiment", "data", "bcs_calibration", "bcs_3_cal.csv"),
                                             cal_path = here::here("analysis", "door_experiment", "data", "bcs_calibration", "cal_123.csv"),
                                             bcs_unit = 3),
                   `4` = fit_bcs_calibration(bcs_path = here::here("analysis", "door_experiment", "data", "bcs_calibration", "bcs_4_cal.csv"),
                                             cal_path = here::here("analysis", "door_experiment", "data", "bcs_calibration", "cal_4.csv"),
                                             bcs_unit = 4),
                   `5` = fit_bcs_calibration(bcs_path = here::here("analysis", "door_experiment", "data", "bcs_calibration", "bcs_5_cal.csv"),
                                             cal_path = here::here("analysis", "door_experiment", "data", "bcs_calibration", "cal_5.csv"),
                                             bcs_unit = 5)
  )
  
  
  # Plot calibration fits ----
  height_obs <- data.frame()
  mod_fit <- data.frame()
  
  for(jj in 1:length(cal_data)) {
    
    height_obs <- rbind(cal_data[[jj]]$bcs, height_obs)
    
    mod_fit <- rbind(cal_data[[jj]]$mod_fit, mod_fit)
    
  }
  
  plot_calibration <- ggplot(data = height_obs,
                             mapping = aes(x = X_Accel, y = Height, color = factor(bcs_unit))) +
    geom_point() +
    geom_smooth(method = 'lm') +
    scale_x_continuous(name = "X-Axis Acceleration (g)") +
    scale_y_continuous(name = "Height (cm)") +
    scale_color_viridis_d(name = "BCS Unit") +
    ggtitle(label = "2023 BCS Calibration") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5))
  
  png(filename = here::here("analysis", "door_experiment", "plots", "bcs_calibration", "bcs_calibration_2023.png"),
      width = 80, height = 80, units = "mm", res = 300)
  print(plot_calibration)
  dev.off()
  
  # Estimate BCS heights
  bcs_data <- predict_bcs_height(haul_paths = list.files(here::here("analysis", "door_experiment", "data", "bcs_data"),
                                                         pattern = "Haul0",
                                                         full.names = TRUE), 
                                 cal_data = cal_data) |>
    dplyr::inner_join(hauls |>
                        dplyr::select(VESSEL, CRUISE, HAUL, HAUL_ID))
  
  # Function to assign treatment values to spread/height measurements ----
  set_treatment <- function(wd, trt) {
    
    unique_trt <- unique(trt$treatment)
    
    wd$treatment <- NA
    
    for(ii in 1:length(unique_trt)) {
      
      wd$treatment[wd$DATE_TIME >= trt$start[trt$treatment == unique_trt[ii]] & wd$DATE_TIME <= trt$end[trt$treatment == unique_trt[ii]]] <- unique_trt[ii]
      
    }
    
    return(wd)
    
  }
  
  # Assign treatments to measurement values
  bcs_treatment <- data.frame()
  
  unique_bcs <- unique(dplyr::select(bcs_data, HAUL_ID, bcs_unit))
  
  for(kk in 1:nrow(unique_bcs)) {
    
    sel_treatment <- dplyr::filter(treatment_breaks, HAUL_ID == unique_bcs$HAUL_ID[kk])
    
    sel_bcs <- dplyr::filter(bcs_data, 
                             HAUL_ID == unique_bcs$HAUL_ID[kk],
                             bcs_unit == unique_bcs$bcs_unit[kk])
    
    sel_bcs$DATE_TIME <- as.POSIXct(paste0(sel_treatment$DATE[1], " ", sel_bcs$Time), format = "%m/%d/%Y %H:%M:%S", tz = "America/Anchorage")
    
    sel_bcs <- set_treatment(wd = sel_bcs, trt = sel_treatment)
    
    bcs_treatment <- sel_bcs |>
      dplyr::bind_rows(bcs_treatment)
    
  }
  
  bcs_treatment <- dplyr::select(bcs_loc, HAUL, bcs_unit, location) |>
    dplyr::inner_join(bcs_treatment)
  
  
  bcs_summary <- bcs_treatment |>
    dplyr::filter(!is.na(treatment)) |>
    dplyr::group_by(HAUL_ID, bcs_unit, HAUL, treatment, location) |>
    dplyr::summarise(SD_HEIGHT = sd(Height),
                     SD_X = sd(X_Accel),
                     SD_Y = sd(Y_Accel),
                     SD_Z = sd(Z_Accel),
                     MEAN_HEIGHT = mean(Height),
                     MEAN_X = mean(X_Accel),
                     MEAN_Y = mean(Y_Accel),
                     MEAN_Z = mean(Z_Accel),
                     n = n())
  
  save(cal_data, bcs_treatment, bcs_summary, file = here::here("analysis", "door_experiment", "data", "bottom_contact_2023.rda"))
  
}


get_bcs_data()
