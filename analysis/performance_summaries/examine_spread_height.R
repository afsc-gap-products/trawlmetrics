library(trawlmetrics)
library(scales)
library(ggthemes)

make_haul_summary <- function(sid) {
  
  all_hauls <- trawlmetrics::bts_geom |>
    dplyr::filter(SURVEY_DEFINITION_ID %in% sid) |>
    dplyr::mutate(
      SPREAD_HEIGHT_RATIO = NET_WIDTH_M/NET_HEIGHT_M,
      WIRE_DEPTH_RATIO = WIRE_LENGTH_M/DEPTH_M,
      TOW_SPEED_KTS = DISTANCE_FISHED_KM/DURATION_HR/1.852,
      IN2425 = factor(YEAR %in% c(2024, 2025)),
      SID_FAC = factor(SURVEY_DEFINITION_ID)
    )
  
  measured_hauls <- dplyr::filter(all_hauls, NET_MEASURED == TRUE)
  
  all_haul_summary <- dplyr::group_by(
    all_hauls,
    SURVEY_DEFINITION_ID,
    NET_MEASURED
  ) |>
    dplyr::summarise(
      MEAN_NET_WIDTH_M = mean(NET_WIDTH_M, na.rm = TRUE),
      Q25_NET_WIDTH_M = quantile(NET_WIDTH_M, na.rm = TRUE, probs = 0.25),
      Q75_NET_WIDTH_M = quantile(NET_WIDTH_M, na.rm = TRUE, probs = 0.75),
      MEAN_NET_HEIGHT_M = mean(NET_HEIGHT_M, na.rm = TRUE),
      Q25_NET_HEIGHT_M = quantile(NET_HEIGHT_M, na.rm = TRUE, probs = 0.25),
      Q75_NET_HEIGHT_M = quantile(NET_HEIGHT_M, na.rm = TRUE, probs = 0.75),
      MEAN_SPREAD_HEIGHT_RATIO = mean(SPREAD_HEIGHT_RATIO, na.rm = TRUE),
      Q25_SPREAD_HEIGHT_RATIO = quantile(SPREAD_HEIGHT_RATIO, na.rm = TRUE, probs = 0.25),
      Q75_SPREAD_HEIGHT_RATIO = quantile(SPREAD_HEIGHT_RATIO, na.rm = TRUE, probs = 0.75),
      MEAN_WIRE_DEPTH_RATIO = mean(WIRE_DEPTH_RATIO),
      Q25_WIRE_DEPTH_RATIO = quantile(WIRE_DEPTH_RATIO, na.rm = TRUE, probs = 0.25),
      Q75_WIRE_DEPTH_RATIO = quantile(WIRE_DEPTH_RATIO, na.rm = TRUE, probs = 0.75),
      MEAN_TOW_SPEED_KTS = mean(TOW_SPEED_KTS),
      Q25_TOW_SPEED_KTS = quantile(TOW_SPEED_KTS, na.rm = TRUE, probs = 0.25),
      Q75_TOW_SPEED_KTS = quantile(TOW_SPEED_KTS, na.rm = TRUE, probs = 0.75),
      )
  
  year_haul_summary <- dplyr::group_by(
    all_hauls,
    SURVEY_DEFINITION_ID,
    NET_MEASURED,
    YEAR,
  ) |>
    dplyr::summarise(
      MEAN_NET_WIDTH_M = mean(NET_WIDTH_M, na.rm = TRUE),
      Q25_NET_WIDTH_M = quantile(NET_WIDTH_M, na.rm = TRUE, probs = 0.25),
      Q75_NET_WIDTH_M = quantile(NET_WIDTH_M, na.rm = TRUE, probs = 0.75),
      MEAN_NET_HEIGHT_M = mean(NET_HEIGHT_M, na.rm = TRUE),
      Q25_NET_HEIGHT_M = quantile(NET_HEIGHT_M, na.rm = TRUE, probs = 0.25),
      Q75_NET_HEIGHT_M = quantile(NET_HEIGHT_M, na.rm = TRUE, probs = 0.75),
      MEAN_SPREAD_HEIGHT_RATIO = mean(SPREAD_HEIGHT_RATIO, na.rm = TRUE),
      Q25_SPREAD_HEIGHT_RATIO = quantile(SPREAD_HEIGHT_RATIO, na.rm = TRUE, probs = 0.25),
      Q75_SPREAD_HEIGHT_RATIO = quantile(SPREAD_HEIGHT_RATIO, na.rm = TRUE, probs = 0.75),
      MEAN_WIRE_DEPTH_RATIO = mean(WIRE_DEPTH_RATIO),
      Q25_WIRE_DEPTH_RATIO = quantile(WIRE_DEPTH_RATIO, na.rm = TRUE, probs = 0.25),
      Q75_WIRE_DEPTH_RATIO = quantile(WIRE_DEPTH_RATIO, na.rm = TRUE, probs = 0.75),
      MEAN_TOW_SPEED_KTS = mean(TOW_SPEED_KTS),
      Q25_TOW_SPEED_KTS = quantile(TOW_SPEED_KTS, na.rm = TRUE, probs = 0.25),
      Q75_TOW_SPEED_KTS = quantile(TOW_SPEED_KTS, na.rm = TRUE, probs = 0.75),
    )
  
  year_vessel_haul_summary <- dplyr::group_by(
    all_hauls,
    SURVEY_DEFINITION_ID,
    NET_MEASURED,
    VESSEL_ID,
    YEAR,
  ) |>
    dplyr::summarise(
      MEAN_NET_WIDTH_M = mean(NET_WIDTH_M, na.rm = TRUE),
      Q25_NET_WIDTH_M = quantile(NET_WIDTH_M, na.rm = TRUE, probs = 0.25),
      Q75_NET_WIDTH_M = quantile(NET_WIDTH_M, na.rm = TRUE, probs = 0.75),
      MEAN_NET_HEIGHT_M = mean(NET_HEIGHT_M, na.rm = TRUE),
      Q25_NET_HEIGHT_M = quantile(NET_HEIGHT_M, na.rm = TRUE, probs = 0.25),
      Q75_NET_HEIGHT_M = quantile(NET_HEIGHT_M, na.rm = TRUE, probs = 0.75),
      MEAN_SPREAD_HEIGHT_RATIO = mean(SPREAD_HEIGHT_RATIO, na.rm = TRUE),
      Q25_SPREAD_HEIGHT_RATIO = quantile(SPREAD_HEIGHT_RATIO, na.rm = TRUE, probs = 0.25),
      Q75_SPREAD_HEIGHT_RATIO = quantile(SPREAD_HEIGHT_RATIO, na.rm = TRUE, probs = 0.75),
      MEAN_WIRE_DEPTH_RATIO = mean(WIRE_DEPTH_RATIO),
      Q25_WIRE_DEPTH_RATIO = quantile(WIRE_DEPTH_RATIO, na.rm = TRUE, probs = 0.25),
      Q75_WIRE_DEPTH_RATIO = quantile(WIRE_DEPTH_RATIO, na.rm = TRUE, probs = 0.75),
      MEAN_TOW_SPEED_KTS = mean(TOW_SPEED_KTS),
      Q25_TOW_SPEED_KTS = quantile(TOW_SPEED_KTS, na.rm = TRUE, probs = 0.25),
      Q75_TOW_SPEED_KTS = quantile(TOW_SPEED_KTS, na.rm = TRUE, probs = 0.75),
    )
  
  return(list(
    all_hauls = all_hauls,
    measured_hauls = measured_hauls,
    all_haul_summary = all_haul_summary,
    year_haul_summary = year_haul_summary,
    year_vessel_haul_summary = year_vessel_haul_summary
  ))
  
  
}

bs_hauls <- make_haul_summary(sid = c(98, 143))
  
aigoa_hauls <- make_haul_summary(sid = c(47, 52))

# Haul overview ----

# Depth distribution by survey
ggplot() +
  stat_ecdf(
    data = dplyr::bind_rows(bs_hauls$measured_hauls, aigoa_hauls$measured_hauls),
    mapping = aes(x = DEPTH_M, color = factor(SURVEY_DEFINITION_ID))
  ) +
  scale_x_continuous(name = "Depth (m)", breaks = c(0, 25, 50, 100, 200, 300, 500, 700, 1000), expand = c(0,0), limits = c(0, 1000)) +
  scale_y_continuous(name = "Cumulative proportion") +
  scale_color_tableau(name = "SID") +
  theme_bw()

# EBS/NBS plots ----

ggplot() +
  geom_boxplot(
    data = bs_hauls$measured_hauls,
    mapping = aes(x = YEAR, y = NET_WIDTH_M, group = YEAR)
  ) +
  geom_hline(data = dplyr::filter(bs_hauls$all_haul_summary, NET_MEASURED == TRUE),
             mapping = aes(yintercept = MEAN_NET_WIDTH_M, color = factor(SURVEY_DEFINITION_ID)), linetype = 2) +
  scale_y_continuous(name = "Wing spread (m)", breaks = seq(10, 23, 1), limits = c(10, 22.5)) +
  scale_x_continuous(name = "Year") +
  scale_color_colorblind(name = "SID") +
  theme_bw()

ggplot() +
  geom_boxplot(
    data = bs_hauls$measured_hauls,
    mapping = aes(x = YEAR, y = NET_HEIGHT_M, group = YEAR)
  ) +
  geom_hline(data = dplyr::filter(bs_hauls$all_haul_summary, NET_MEASURED == TRUE),
             mapping = aes(yintercept = MEAN_NET_HEIGHT_M, color = factor(SURVEY_DEFINITION_ID)), linetype = 2) +
  scale_y_continuous(name = "Headline height (m)", breaks = seq(1, 6, 1), limits = c(1, 6)) +
  scale_x_continuous(name = "Year") +
  scale_color_colorblind(name = "SID") +
  theme_bw()

ggplot() +
  geom_boxplot(
    data = bs_hauls$measured_hauls,
    mapping = aes(x = YEAR, y = SPREAD_HEIGHT_RATIO, group = YEAR)
  ) +
  geom_hline(data = dplyr::filter(bs_hauls$all_haul_summary, NET_MEASURED == TRUE),
             mapping = aes(yintercept = MEAN_SPREAD_HEIGHT_RATIO, color = factor(SURVEY_DEFINITION_ID)), linetype = 2) +
  scale_y_continuous(name = "Spread:Height", limits = c(3, 15)) +
  scale_x_continuous(name = "Year") +
  scale_color_colorblind(name = "SID") +
  theme_bw()


# GOA/AI Plots

ggplot() +
  geom_boxplot(
    data = aigoa_hauls$measured_hauls,
    mapping = aes(x = YEAR, y = NET_WIDTH_M, group = YEAR)
  ) +
  geom_hline(data = dplyr::filter(aigoa_hauls$all_haul_summary, NET_MEASURED == TRUE),
             mapping = aes(yintercept = MEAN_NET_WIDTH_M, color = factor(SURVEY_DEFINITION_ID)), linetype = 2) +
  scale_y_continuous(name = "Wing spread (m)", breaks = seq(11, 22, 1), limits = c(11, 22)) +
  scale_x_continuous(name = "Year") +
  scale_color_colorblind(name = "SID") +
  theme_bw()

ggplot() +
  geom_boxplot(
    data = aigoa_hauls$measured_hauls,
    mapping = aes(x = YEAR, y = NET_HEIGHT_M, group = YEAR)
  ) +
  geom_hline(data = dplyr::filter(aigoa_hauls$all_haul_summary, NET_MEASURED == TRUE),
             mapping = aes(yintercept = MEAN_NET_HEIGHT_M, color = factor(SURVEY_DEFINITION_ID)), linetype = 2) +
  scale_y_continuous(name = "Headline height (m)", breaks = seq(3, 11, 1), limits = c(3, 11)) +
  scale_x_continuous(name = "Year") +
  scale_color_colorblind(name = "SID") +
  theme_bw()

ggplot() +
  geom_boxplot(
    data = aigoa_hauls$measured_hauls,
    mapping = aes(x = YEAR, y = SPREAD_HEIGHT_RATIO, group = YEAR)
  ) +
  geom_hline(data = dplyr::filter(aigoa_hauls$all_haul_summary, NET_MEASURED == TRUE),
             mapping = aes(yintercept = MEAN_SPREAD_HEIGHT_RATIO, color = factor(SURVEY_DEFINITION_ID)), linetype = 2) +
  scale_y_continuous(name = "Spread:Height", limits = c(1, 6)) +
  scale_x_continuous(name = "Year") +
  scale_color_colorblind(name = "SID") +
  theme_bw()


library(mgcv)
library(ghibli)

bs_model <- mgcv::gam(
  NET_WIDTH_M ~ s(DEPTH_M, by = IN2425, bs = "tp"), 
  data = bs_hauls$measured_hauls
)

bs_fit <- 
  expand.grid(
    DEPTH_M = seq(
      min(bs_hauls$measured_hauls$DEPTH_M),
      max(bs_hauls$measured_hauls$DEPTH_M),
      1
    ),
    IN2425 = factor(c(TRUE, FALSE))
  )

bs_fit <- cbind(
  bs_fit,
  as.data.frame(
    predict(
      bs_model, 
      newdata = bs_fit,
      se.fit = TRUE
    )
  )
)

ggplot() +
  geom_point(
    data = dplyr::arrange(bs_hauls$measured_hauls, IN2425),
    mapping = aes(x = DEPTH_M, y = NET_WIDTH_M, color = IN2425),
    alpha = 0.2,
    size = 0.7
  ) +
  geom_path(
    data = bs_fit, 
    mapping = aes(x = DEPTH_M, y = fit, color = IN2425)
  ) +
  geom_ribbon(
    data = bs_fit, 
    mapping = aes(x = DEPTH_M, ymin = fit - se.fit*2, ymax = fit + se.fit*2, fill = IN2425),
    alpha = 0.5
  ) +
  scale_fill_colorblind(name = "24/25?") +
  scale_color_colorblind(name = "24/25?") +
  scale_x_continuous(name = "Bottom depth (m)") +
  scale_y_continuous(name = "Upper wing spread (m)") +
  theme_bw() +
  facet_wrap(~IN2425)


# AI/GOA Models

aigoa_fit <- 
  expand.grid(
    DEPTH_M = seq(
      min(aigoa_hauls$measured_hauls$DEPTH_M),
      500,
      1
    ),
    IN2425 = factor(c(TRUE, FALSE))
  )

aigoa_model <- mgcv::gam(
  NET_WIDTH_M ~ s(DEPTH_M, by = IN2425), 
  data = dplyr::mutate(aigoa_hauls$measured_hauls, IN2425 = factor(YEAR %in% c(2024, 2025)))
)

aigoa_fit <- cbind(
  aigoa_fit,
  as.data.frame(
    predict(
      aigoa_model, 
      newdata = aigoa_fit,
      se.fit = TRUE
    )
  )
)

ggplot() +
  geom_point(
    data = dplyr::arrange(aigoa_hauls$measured_hauls, IN2425),
    mapping = aes(x = DEPTH_M, y = NET_WIDTH_M, color = IN2425),
    alpha = 0.2,
    size = 0.7
  ) +
  geom_path(
    data = aigoa_fit, 
    mapping = aes(x = DEPTH_M, y = fit, color = IN2425)
  ) +
  geom_ribbon(
    data = aigoa_fit, 
    mapping = aes(x = DEPTH_M, ymin = fit - se.fit*2, ymax = fit + se.fit*2, fill = IN2425),
    alpha = 0.5
  ) +
  scale_fill_colorblind(name = "24/25?") +
  scale_color_colorblind(name = "24/25?") +
  scale_x_continuous(name = "Bottom depth (m)") +
  scale_y_continuous(name = "Upper wing spread (m)") +
  theme_bw()

# combined_model <- mgcv::gam(
#   NET_WIDTH_M ~ s(DEPTH_M, by = IN2425) + s(SID_FAC, bs = "re"), 
#   data = dplyr::bind_rows(bs_hauls$measured_hauls, aigoa_hauls$measured_hauls)
#     
# )
# 
# # Fit model with random effects turned off
# combined_fit <- 
#   expand.grid(
#     DEPTH_M = seq(
#       10,
#       500,
#       1
#     ),
#     IN2425 = factor(c(TRUE, FALSE)),
#     SID_FAC = factor(c(47, 52, 98, 143))
#   )
# 
# combined_fit <- 
#   cbind(
#     combined_fit,
#     as.data.frame(
#       predict(combined_model, newdata = combined_fit, se.fit = TRUE, exclude = "s(SID_FAC)", type = "response"),
#     )
#   )
# 
# ggplot() +
#   geom_point(
#     data = dplyr::arrange(aigoa_hauls$measured_hauls, IN2425),
#     mapping = aes(x = DEPTH_M, y = NET_WIDTH_M, color = IN2425),
#     alpha = 0.2,
#     size = 0.7
#   ) +
#   geom_path(
#     data = combined_fit |>
#       dplyr::select(DEPTH_M, IN2425, fit, se.fit) |>
#       unique(), 
#     mapping = aes(x = DEPTH_M, y = fit, color = IN2425)
#   ) +
#   geom_ribbon(
#     data = combined_fit |>
#       dplyr::select(DEPTH_M, IN2425, fit, se.fit) |>
#       unique(), 
#     mapping = aes(x = DEPTH_M, ymin = fit - se.fit*2, ymax = fit + se.fit*2, fill = IN2425),
#     alpha = 0.5
#   ) +
#   scale_fill_colorblind(name = "24/25?") +
#   scale_color_colorblind(name = "24/25?") +
#   scale_x_continuous(name = "Bottom depth (m)") +
#   scale_y_continuous(name = "Upper wing spread (m)") +
#   theme_bw()

channel <- trawlmetrics::get_connected(schema = "AFSC")

haul_events <- RODBC::sqlQuery(
  channel = channel, 
  query = "select e.date_time, e.haul_id, e.event_type_id, c.vessel_id, c.cruise, h.haul from 
  race_data.events e, race_data.hauls h, race_data.cruises c
                where e.haul_id = h.haul_id 
                and e.event_type_id in (2, 3, 4, 6, 7, 8, 9) 
                and h.cruise_id = c.cruise_id") |>
  dplyr::inner_join(
    data.frame(
      EVENT_TYPE_ID = c(9, 2, 3, 4, 6, 7, 8),
      EVENT_NAME = c("doors_out", "brake_set", "on_bottom", "eq", "haulback", "off_bottom", "doors_up")
               )
  ) |>
  tidyr::pivot_wider(
    id_cols = c("VESSEL_ID", "CRUISE", "HAUL"),
    values_from = "DATE_TIME",
    names_from = "EVENT_NAME"
  )

haul_events_time <-
  haul_events |>
  dplyr::mutate(
    do_to_brakes = as.numeric(brake_set-doors_out),
    do_to_eq = as.numeric(eq-doors_out),
    hb_to_du = as.numeric(doors_up-haulback)
  ) |>
  dplyr::inner_join(
    dplyr::bind_rows(
      bs_hauls$measured_hauls,
      aigoa_hauls$measured_hauls
    )
  )

haul_events_time$do_to_brakes
haul_events_time$hb_to_du

haul_events_time_summary <- haul_events_time |>
  dplyr::group_by(SURVEY_DEFINITION_ID) |>
  dplyr::summarise(MEDIAN_hb_to_du = median(DEPTH_M/hb_to_du, na.rm = TRUE),
                   MEDIAN_do_to_eq = median(DEPTH_M/do_to_eq, na.rm = TRUE))

ggplot() +
  geom_boxplot(
    data = haul_events_time,
    mapping = aes(x = YEAR, y = DEPTH_M/hb_to_du, group = YEAR)
             ) +
  geom_hline(data = haul_events_time_summary,
             mapping = aes(yintercept = MEDIAN_hb_to_du), linetype = 2) +
  scale_y_continuous(name = "Haul back to Doors up - Depth/Time Elapsed (m/s)", limits = c(0, 0.75)) +
  facet_wrap(~SURVEY_DEFINITION_ID, scales = "free")

ggplot() +
  geom_boxplot(
    data = haul_events_time,
    mapping = aes(x = YEAR, y = DEPTH_M/do_to_eq, group = YEAR)
  ) +
  geom_hline(data = haul_events_time_summary,
             mapping = aes(yintercept = MEDIAN_do_to_eq), linetype = 2) +
  scale_y_continuous(name = "Doors Out to EQ - Depth/Time Elapsed (m/s)", limits = c(0, 0.75)) +
  facet_wrap(~SURVEY_DEFINITION_ID, scales = "free")
