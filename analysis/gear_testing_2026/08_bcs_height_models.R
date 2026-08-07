# Compare effects of doors on bottom contact

# Exclude hauls from height off bottom analysis due to bad bottom contact or sensor orientation errors
bc_haul_data <- 
  readxl::read_xlsx(path = here::here("data", "2026_gear_testing_haul_log.xlsx")) |>
  dplyr::filter(Event %in% c("Brake set", "EQ")) |>
  dplyr::filter(use_bc_analysis) |> # bad bottom contact or questionable orientation
  dplyr::filter(!(Haul %in% c(527:530))) # Exclude hauls with 90' bridles

bc_haul_data$dt <- bc_haul_data$Time_AKDT
lubridate::date(bc_haul_data$dt) <- lubridate::date(bc_haul_data$Date_AKDT)
bc_haul_data$dt <- lubridate::force_tz(bc_haul_data$dt, "America/Anchorage") 

bcs_heights <- readRDS(file = here::here("data", "01_bcs_data", "bcs_heights.rds"))

test <- dplyr::filter(bcs_heights, haul == 501, position == "C0")

# # 1. Fit a structural time series model (runs Kalman filter estimation)
# fit <- StructTS(test$height_fit, type = "level")
# 
# # 2. Extract smoothed low-pass state using the Kalman Smoother
# low_pass_state <- tsSmooth(fit)[, "level"]
# 
# # 1. Fit a structural time series model (runs Kalman filter estimation)
# fit <- StructTS(low_pass_state, type = "level")
# 
# # 2. Extract smoothed low-pass state using the Kalman Smoother
# low_pass_state <- tsSmooth(fit)[, "level"]


plot(test$dt, test$height_fit)
plot(test$dt, multi_pass_kalman(x = test$height_fit,  n_passes = 1, mode = "lowpass", q = 0.01))
plot(test$dt, high_pass_signal)
