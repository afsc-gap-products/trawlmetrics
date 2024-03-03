library(trawlmetrics)
library(mgcv)
library(ggthemes)
library(ggrepel)

channel <- trawlmetrics::get_connected(schema = "AFSC")

min_spread_pings <- 50
min_height_pings <- 50
gear <- 44

# Function to assign treatment values to spread/height measurements ----
set_treatment <- function(wd, trt) {
  
  unique_trt <- unique(trt$treatment)
  
  wd$treatment <- NA
  
  for(ii in 1:length(unique_trt)) {
    
    wd$treatment[wd$DATE_TIME >= trt$start[trt$treatment == unique_trt[ii]] & wd$DATE_TIME <= trt$end[trt$treatment == unique_trt[ii]]] <- unique_trt[ii]
    
  }
  
  return(wd)
  
}


# Get experimental data ----

width <- RODBC::sqlQuery(channel = channel,
                         query = "select nm.date_time, nm.value net_width, h.haul_id 
                         from race_data.net_mensurations nm, race_data.net_mensuration_headers nmh, race_data.hauls h, race_data.cruises c, race_data.surveys s 
                         where nm.net_mensuration_header_id = nmh.net_mensuration_header_id 
                         and h.haul_id = nmh.haul_id 
                         and h.cruise_id = c.cruise_id 
                         and c.survey_id = s.survey_id 
                         and s.survey_definition_id = 98 
                         and c.cruise = 202301 
                         and nmh.data_type_id = 2 
                         and nm.value > 10 
                         and nm.value < 28
                         and h.haul_type = 7") |>
  dplyr::mutate(DATE_TIME = lubridate::with_tz(
    lubridate::force_tz(DATE_TIME, tzone = "UTC"), 
    tzone = "America/Anchorage",
    NET_WIDTH = marport_to_netmind(NET_WIDTH))
  ) |>
  dplyr::arrange(DATE_TIME)

height <- RODBC::sqlQuery(channel = channel,
                          query = "select nm.date_time, nm.value net_height, h.haul_id 
                          from race_data.net_mensurations nm, race_data.net_mensuration_headers nmh, race_data.hauls h, race_data.cruises c, race_data.surveys s 
                          where nm.net_mensuration_header_id = nmh.net_mensuration_header_id 
                          and h.haul_id = nmh.haul_id 
                          and h.cruise_id = c.cruise_id 
                          and c.survey_id = s.survey_id 
                          and s.survey_definition_id = 98 
                          and c.cruise = 202301 
                          and nmh.data_type_id = 3 
                          and nm.value > 0 
                          and nm.value < 6
                          and h.haul_type = 7") |>
  dplyr::mutate(DATE_TIME = lubridate::with_tz(
    lubridate::force_tz(DATE_TIME, tzone = "UTC"), 
    tzone = "America/Anchorage")
  ) |>
  dplyr::arrange(DATE_TIME)

events <- RODBC::sqlQuery(channel = channel,
                          query = "select e.haul_id, e.date_time, e.event_type_id, et.name, h.haul, c.vessel_id vessel, c.cruise 
                          from race_data.events e, race_data.event_types et, race_data.hauls h, race_data.cruises c, race_data.surveys s 
                          where e.haul_id = h.haul_id 
                          and h.cruise_id = c.cruise_id 
                          and c.survey_id = s.survey_id 
                          and s.survey_definition_id = 98 
                          and c.cruise = 202301 
                          and h.haul_type = 7 
                          and et.event_type_id = e.event_type_id 
                          and e.event_type_id in (3, 7)") |>
  dplyr::mutate(DATE_TIME = lubridate::with_tz(
    lubridate::force_tz(DATE_TIME, tzone = "UTC"), 
    tzone = "America/Anchorage")
  ) |>
  dplyr::arrange(DATE_TIME)

hauls <- RODBC::sqlQuery(channel = channel,
                         query = "select * from racebase.haul where vessel = 162 and cruise = 202301 and haul_type = 7") |>
  dplyr::inner_join(dplyr::select(events, VESSEL, CRUISE, HAUL, HAUL_ID) 
                    |> unique()
  )

# Data from standard hauls
standard_hauls <- RODBC::sqlQuery(channel = channel,
                                  query = paste0("select h.*, c.vessel_id vessel, c.cruise from race_data.hauls h, race_data.cruises c, race_data.surveys s 
                                                 where h.performance >= 0 
                                                 and h.haul_type = 3 
                                                 and h.net_spread_method = 7 
                                                 and h.cruise_id = c.cruise_id  
                                                 and s.survey_id = c.survey_id 
                                                 and s.survey_definition_id = 98")) |>
  dplyr::mutate(SCOPE_TO_DEPTH = WIRE_OUT/BOTTOM_DEPTH,
                STATIONID = factor(STATION))

# Load treatment breaks, append HAUL_ID ----
treatment_breaks <- read.csv(here::here("analysis", "door_experiment", "data", "2023_Door_Testing_Treatment_Times.csv")) |>
  dplyr::filter(!is.na(treatment)) |>
  dplyr::mutate(start_treatment = as.POSIXct(paste0(DATE, " ", start_treatment), 
                                             format = "%m/%d/%Y %H:%M", 
                                             tz = "America/Anchorage"),
                end_treatment = as.POSIXct(paste0(DATE, " ", end_treatment), 
                                           format = "%m/%d/%Y %H:%M", 
                                           tz = "America/Anchorage")) |>
  dplyr::rename(start = start_treatment, 
                end = end_treatment) |>
  dplyr::mutate(WIRE_OUT = scope_fm * 1.8288) |>
  dplyr::select(CRUISE, VESSEL, HAUL, DATE, treatment, start, end, scope_fm, WIRE_OUT, target_speed_kn, usable)

treatment_breaks <- dplyr::select(events, VESSEL, CRUISE, HAUL, HAUL_ID) |> 
  unique() |> 
  dplyr::inner_join(treatment_breaks, by = c("VESSEL", "CRUISE", "HAUL"))



# Run sequential outlier rejection on spread data
unique_hauls <- unique(events$HAUL_ID)

sor_width <- data.frame()
sor_height <- data.frame()

for(ii in 1:length(unique_hauls)) {
  
  sel_events <- dplyr::filter(events, HAUL_ID == unique_hauls[ii])
  
  sel_hauls <- dplyr::select(sel_events, VESSEL, CRUISE, HAUL) |>
    unique() |>
    dplyr::inner_join(hauls)
  
  sel_width <- dplyr::filter(width, 
                             HAUL_ID == unique_hauls[ii],
                             DATE_TIME >= sel_events$DATE_TIME[sel_events$EVENT_TYPE_ID == 3],
                             DATE_TIME <= sel_events$DATE_TIME[sel_events$EVENT_TYPE_ID <= 7])
  
  sel_height <- dplyr::filter(height, 
                             HAUL_ID == unique_hauls[ii],
                             DATE_TIME >= sel_events$DATE_TIME[sel_events$EVENT_TYPE_ID == 3],
                             DATE_TIME <= sel_events$DATE_TIME[sel_events$EVENT_TYPE_ID <= 7])
  
  # Sequential outlier rejection when there are at least 50 pings ----
  if(nrow(sel_width) >= min_spread_pings) {
    
    sel_width$vessel <- sel_hauls$VESSEL[1]
    sel_width$cruise <- sel_hauls$CRUISE[1]
    sel_width$haul <- sel_hauls$HAUL[1]
    
    sor_results <- sel_width |>
      dplyr::rename(measurement_value = NET_WIDTH) |>
      sequentialOR(formula = measurement_value ~ DATE_TIME, 
                   method = 'ss', 
                   n.reject = 1, 
                   n.stop = 0.5, 
                   threshold.stop = TRUE)
    
    sel_width <- sor_results$obs_rank |>
      dplyr::filter(is.na(SOR_RANK)) |>
      dplyr::select(DATE_TIME, NET_WIDTH = measurement_value, HAUL_ID)
    
    sor_width <- rbind(sor_width, sel_width)
    
  } else {
    
    sor_width <- rbind(sor_width, sel_width)
    
  }
  
  sor_height <- dplyr::bind_rows(sor_height, sel_height)
  
}


# Assign treatments to measurement values
sor_width_treatment <- data.frame()
sor_height_treatment <- data.frame()

for(jj in 1:length(unique_hauls)) {
  
  sel_sor_width <- dplyr::filter(sor_width, HAUL_ID == unique_hauls[jj])
  
  sel_sor_height <- dplyr::filter(sor_height, HAUL_ID == unique_hauls[jj])
  
  sel_treatment <- dplyr::filter(treatment_breaks, HAUL_ID == unique_hauls[jj])
  
    if(nrow(sel_sor_width) > 1) {
      
      sel_sor_width <- set_treatment(wd = sel_sor_width, trt = sel_treatment)
      
      sel_sor_width$treatment <- sel_sor_width$treatment
      
      sor_width_treatment <- dplyr::bind_rows(sor_width_treatment, sel_sor_width)
      
    }
  
  if(nrow(sel_sor_height) > 1) {
    
    sel_sor_height <- set_treatment(wd = sel_sor_height, trt = sel_treatment)
    
    sel_sor_height$treatment <- sel_sor_height$treatment
    
    sor_height_treatment <- dplyr::bind_rows(sor_height_treatment, sel_sor_height)
    
  }

}


usable_width_treatments <- sor_width_treatment |>
  dplyr::filter(!is.na(treatment)) |>
  dplyr::mutate(treatment = as.numeric(as.character(treatment))) |>
  dplyr::inner_join(treatment_breaks |>
                      dplyr::filter(usable) |>
                      dplyr::select(HAUL_ID, treatment, target_speed_kn, WIRE_OUT) |>
                      unique(),
                    by = c("HAUL_ID", "treatment")) |>
  dplyr::group_by(HAUL_ID, target_speed_kn, WIRE_OUT, treatment) |>
  dplyr::summarise(SPREAD_PINGS = n(),
                   NET_SPREAD = mean(NET_WIDTH),
                   NET_SPREAD_STANDARD_DEVIATION = sd(NET_WIDTH),
                   .groups = "keep"
  ) |>
  dplyr::filter(SPREAD_PINGS > min_spread_pings) |>
  dplyr::inner_join(dplyr::select(hauls, HAUL_ID, VESSEL, CRUISE, HAUL, DISTANCE_FISHED, START_LATITUDE, END_LATITUDE, START_LONGITUDE, END_LONGITUDE, BOTTOM_DEPTH, GEAR_DEPTH, STATIONID, GEAR, ACCESSORIES)) |>
  dplyr::mutate(SCOPE_TO_DEPTH = WIRE_OUT/BOTTOM_DEPTH)

usable_height_treatments <- sor_height_treatment |>
  dplyr::filter(!is.na(treatment)) |>
  dplyr::mutate(treatment = as.numeric(as.character(treatment))) |>
  dplyr::inner_join(treatment_breaks |>
                      dplyr::filter(usable) |>
                      dplyr::select(HAUL_ID, treatment, target_speed_kn, WIRE_OUT) |>
                      unique(),
                    by = c("HAUL_ID", "treatment")) |>
  dplyr::group_by(HAUL_ID, target_speed_kn, WIRE_OUT, treatment) |>
  dplyr::summarise(HEIGHT_PINGS = n(),
                   NET_HEIGHT = mean(NET_HEIGHT),
                   NET_HEIGHT_STANDARD_DEVIATION = sd(NET_HEIGHT),
                   .groups = "keep"
  ) |>
  dplyr::filter(HEIGHT_PINGS > min_height_pings) |>
  dplyr::inner_join(dplyr::select(hauls, HAUL_ID, VESSEL, CRUISE, HAUL, DISTANCE_FISHED, START_LATITUDE, END_LATITUDE, START_LONGITUDE, END_LONGITUDE, BOTTOM_DEPTH, GEAR_DEPTH, STATIONID, GEAR, ACCESSORIES)) |>
  dplyr::mutate(SCOPE_TO_DEPTH = WIRE_OUT/BOTTOM_DEPTH)

# Spread model for 83-112 hauls

mod_spread_83112 <- gam(formula = NET_SPREAD ~ s(BOTTOM_DEPTH, SCOPE_TO_DEPTH, bs = "tp", k = 100) + s(STATIONID, bs = "re"), data = standard_hauls)

fit_spread_83112 <- dplyr::filter(usable_width_treatments, GEAR == 44) |>
  dplyr::mutate(type = "4.5-m doors")

standard_hauls$fit_spread <- predict(mod_spread_83112)
standard_hauls$type <- "Standard 83-112"

fit_spread_83112$fit_spread <- predict(mod_spread_83112, 
                         newdata = fit_spread_83112)
plot_spread_gam <- ggplot() +
  geom_point(data = standard_hauls,
             mapping = aes(x = fit_spread, y = NET_SPREAD, color = type),
             alpha = 0.2) +
  geom_path(data = fit_spread_83112,
            mapping = aes(x = fit_spread, y = NET_SPREAD, group = HAUL),
            color = "red") +
  geom_point(data = fit_spread_83112,
             mapping = aes(x = fit_spread, y = NET_SPREAD, color = type, fill = factor(treatment), group = HAUL),
             size = rel(4), shape = 21) +
  geom_abline(mapping = aes(intercept = 0, slope = 1),
              linetype = 2, linewidth = 1.5) +
  geom_text_repel(data = dplyr::filter(fit_spread_83112) |>
                    dplyr::filter(treatment == 1),
                  mapping = aes(x = fit_spread, y = NET_SPREAD, label = HAUL, color = "4.5-m doors")) +
  scale_color_manual(name = "Haul type", values = c("4.5-m doors" = "red", "Standard 83-112" = "grey50")) +
  scale_fill_colorblind(name = "Treatment", na.translate = TRUE, na.value = "grey50") +
  scale_x_continuous(name = "Predicted wing spread (m)") +
  scale_y_continuous(name = "Observed wing spread (m)") +
  theme_bw() +
  theme(legend.position = c(0.82, 0.18),
        legend.box.background = element_blank(),
        legend.background = element_blank())

png(filename = here::here("analysis", "door_experiment", "plots", "gam_obs_vs_predicted_spread.png"), height = 180, width = 180, units = "mm", res = 300)
print(plot_spread_gam +
        theme(legend.text = element_text(size = 12),
              legend.title = element_text(size = 14),
              axis.title = element_text(size = 20),
              axis.text = element_text(size = 18)))
dev.off()

# Height model for 83-112 hauls
mod_height_83112 <- gam(formula = NET_HEIGHT ~ s(BOTTOM_DEPTH, SCOPE_TO_DEPTH, bs = "tp", k = 100) + s(STATIONID, bs = "re"), data = standard_hauls)

fit_height_83112 <- dplyr::filter(usable_height_treatments, GEAR == 44) |>
  dplyr::mutate(type = "4.5-m doors")

standard_hauls$fit_height <- predict(mod_height_83112)
standard_hauls$type <- "Standard 83-112"

fit_height_83112$fit_height <- predict(mod_height_83112, 
                         newdata = fit_height_83112)

plot_height_gam <- ggplot() +
  geom_point(data = standard_hauls,
             mapping = aes(x = fit_height, y = NET_HEIGHT, color = type),
             alpha = 0.2) +
  geom_path(data = fit_height_83112,
            mapping = aes(x = fit_height, y = NET_HEIGHT, group = HAUL),
            color = "red") +
  geom_point(data = fit_height_83112,
             mapping = aes(x = fit_height, y = NET_HEIGHT, color = type, fill = factor(treatment), group = HAUL),
             size = rel(4),
             shape = 21) +
  geom_abline(mapping = aes(intercept = 0, slope = 1),
              linetype = 2, linewidth = 1.5) +
  geom_text_repel(data = dplyr::filter(fit_height_83112) |>
                    dplyr::filter(treatment == 1),
                  mapping = aes(x = fit_height, y = NET_HEIGHT, label = HAUL, color = "4.5-m doors")) +
  scale_color_manual(name = "Haul type", values = c("4.5-m doors" = "red", "Standard 83-112" = "grey50")) +
  scale_fill_colorblind(name = "Treatment", na.translate = TRUE, na.value = "grey50") +
  scale_x_continuous(name = "Predicted height (m)") +
  scale_y_continuous(name = "Observed height (m)") +
  theme_bw() +
  theme(legend.position = c(0.8, 0.12),
        legend.box.background = element_blank(),
        legend.background = element_blank())


png(filename = here::here("analysis", "door_experiment", "plots", "gam_obs_vs_predicted_height.png"), height = 180, width = 180, units = "mm", res = 300)
print(plot_height_gam +
        theme(legend.text = element_text(size = 12),
              legend.title = element_text(size = 14),
              axis.title = element_text(size = 20),
              axis.text = element_text(size = 18),
              legend.position = c(0.15, 0.82)))
dev.off()


# Plot usable spread pings ----
usable_spread_pings <- sor_width_treatment |>
  dplyr::mutate(treatment = as.numeric(as.character(treatment))) |>
  dplyr::left_join(dplyr::select(usable_width_treatments, HAUL_ID, treatment) |>
                     dplyr::mutate(USE = TRUE)) |>
  dplyr::mutate(USE = dplyr::if_else(is.na(USE), FALSE, USE)) |>
  dplyr::inner_join(dplyr::select(hauls, HAUL_ID, HAUL))

usable_spread_pings <- events |>
  dplyr::filter(EVENT_TYPE_ID == 3) |>
  dplyr::select(HAUL_ID, START_TIME = DATE_TIME) |>
  dplyr::inner_join(usable_spread_pings) |>
  dplyr::mutate(TIME_ELAPSED = as.numeric(difftime(DATE_TIME, START_TIME, units = "mins")))

ping_events <- events |>
  dplyr::filter(EVENT_TYPE_ID == 3) |>
  dplyr::select(HAUL_ID, START_TIME = DATE_TIME) |>
  dplyr::inner_join(events) |>
  dplyr::mutate(TIME_ELAPSED = as.numeric(difftime(DATE_TIME, START_TIME, units = "mins")))

haul_spread_comparison <- usable_width_treatments |>
  dplyr::inner_join(
    dplyr::select(hauls, VESSEL, CRUISE, HAUL, HAUL_ID, BOTTOM_DEPTH, STATIONID)
  ) |> dplyr::rename(`Net spread` = NET_SPREAD,
                     `Net spread standard deviation` = NET_SPREAD_STANDARD_DEVIATION,
                     `Bottom depth` = BOTTOM_DEPTH,
                     `Scope` = WIRE_OUT,
                     STATION = STATIONID) |>
  dplyr::mutate(type = "New doors") |>
  dplyr::bind_rows(standard_hauls |> 
                     dplyr::rename(`Net spread` = NET_SPREAD,
                                   `Net spread standard deviation` = NET_SPREAD_STANDARD_DEVIATION,
                                   `Bottom depth` = BOTTOM_DEPTH,
                                   `Scope` = WIRE_OUT) |>
                     dplyr::mutate(type = "Standard tows")
  ) |>
  tidyr::pivot_longer(cols = c("Net spread", "Net spread standard deviation", "Bottom depth", "Scope")) |>
  dplyr::arrange(type)


plot_usable_spread_pings <- ggplot() +
  geom_point(data = dplyr::filter(usable_spread_pings, HAUL < 15),
             mapping = aes(x = TIME_ELAPSED, y = NET_WIDTH, color = factor(treatment), alpha = USE)) +
  geom_segment(data = dplyr::filter(ping_events, HAUL < 15),
               mapping = aes(x = TIME_ELAPSED,
                             xend = TIME_ELAPSED,
                             y = 9, yend = 25),
               color = "red") +
  scale_alpha_manual(values = c('TRUE' = 1, 'FALSE' = 0.1), guide = "none") +
  scale_color_colorblind(name = "Treatment", na.translate = TRUE, na.value = "grey50") +
  scale_y_continuous(name = "Wing spread (m)") +
  scale_x_continuous(name = "Time elapsed (min)") +
  facet_wrap(~HAUL, scales = "free") +
  theme_bw() +
  theme(legend.position = "bottom")


png(filename = here::here("analysis", "door_experiment", "plots", "usable_83112_spread_pings_2023.png"), 
    width = 240, 
    height = 180, 
    units = "mm", 
    res = 300)
print(plot_usable_spread_pings +
        theme(legend.text = element_text(size = 14),
              legend.title = element_text(size = 14),
              axis.title = element_text(size = 18),
              axis.text = element_text(size = 16),
              strip.text = element_text(size = 14)))
dev.off()


# Plot usable height pings ----

usable_height_pings <- sor_height_treatment |>
  dplyr::mutate(treatment = as.numeric(as.character(treatment))) |>
  dplyr::left_join(dplyr::select(usable_height_treatments, HAUL_ID, treatment) |>
                     dplyr::mutate(USE = TRUE)) |>
  dplyr::mutate(USE = dplyr::if_else(is.na(USE), FALSE, USE)) |>
  dplyr::inner_join(dplyr::select(hauls, HAUL_ID, HAUL))

usable_height_pings <- events |>
  dplyr::filter(EVENT_TYPE_ID == 3) |>
  dplyr::select(HAUL_ID, START_TIME = DATE_TIME) |>
  dplyr::inner_join(usable_height_pings) |>
  dplyr::mutate(TIME_ELAPSED = as.numeric(difftime(DATE_TIME, START_TIME, units = "mins")))

ping_events <- events |>
  dplyr::filter(EVENT_TYPE_ID == 3) |>
  dplyr::select(HAUL_ID, START_TIME = DATE_TIME) |>
  dplyr::inner_join(events) |>
  dplyr::mutate(TIME_ELAPSED = as.numeric(difftime(DATE_TIME, START_TIME, units = "mins")))

haul_height_comparison <- usable_height_treatments |>
  dplyr::inner_join(
    dplyr::select(hauls, VESSEL, CRUISE, HAUL, HAUL_ID, BOTTOM_DEPTH, STATIONID)
  ) |> dplyr::rename(`Net height` = NET_HEIGHT,
                     `Net height standard deviation` = NET_HEIGHT_STANDARD_DEVIATION,
                     `Bottom depth` = BOTTOM_DEPTH,
                     `Scope` = WIRE_OUT,
                     STATION = STATIONID) |>
  dplyr::mutate(type = "New doors") |>
  dplyr::bind_rows(standard_hauls |> 
                     dplyr::rename(`Net height` = NET_HEIGHT,
                                   `Net height standard deviation` = NET_HEIGHT_STANDARD_DEVIATION,
                                   `Bottom depth` = BOTTOM_DEPTH,
                                   `Scope` = WIRE_OUT) |>
                     dplyr::mutate(type = "Standard tows")
  ) |>
  tidyr::pivot_longer(cols = c("Net height", "Net height standard deviation", "Bottom depth", "Scope")) |>
  dplyr::arrange(type)


plot_usable_height_pings <- ggplot() +
  geom_point(data = dplyr::filter(usable_height_pings, HAUL < 15),
             mapping = aes(x = TIME_ELAPSED, y = NET_HEIGHT, color = factor(treatment), alpha = USE)) +
  geom_segment(data = dplyr::filter(ping_events, HAUL < 15),
               mapping = aes(x = TIME_ELAPSED,
                             xend = TIME_ELAPSED,
                             y = 0, yend = 6),
               color = "red") +
  scale_alpha_manual(values = c('TRUE' = 1, 'FALSE' = 0.1), guide = "none") +
  scale_color_colorblind(name = "Treatment", na.translate = TRUE, na.value = "grey50") +
  scale_y_continuous(name = "Net height (m)") +
  scale_x_continuous(name = "Time elapsed (min)") +
  facet_wrap(~HAUL, scales = "free") +
  theme_bw() +
  theme(legend.position = "bottom")


png(filename = here::here("analysis", "door_experiment", "plots", "usable_83112_height_pings_2023.png"), 
    width = 240, 
    height = 180, 
    units = "mm", 
    res = 300)
print(plot_usable_height_pings +
        theme(legend.text = element_text(size = 14),
              legend.title = element_text(size = 14),
              axis.title = element_text(size = 18),
              axis.text = element_text(size = 16),
              strip.text = element_text(size = 14)))
dev.off()


plot_treatments_2023 <- ggplot() +
  geom_jitter(data = standard_hauls,
              mapping = aes(x = BOTTOM_DEPTH, y = SCOPE_TO_DEPTH, color = "Standard 83-112"),
              alpha = 0.04) +
  geom_path(data = dplyr::filter(usable_height_treatments, HAUL < 15),
            mapping = aes(x = BOTTOM_DEPTH, y = SCOPE_TO_DEPTH, group = HAUL, color = "4.5-m doors")) +
  geom_point(data = dplyr::filter(usable_height_treatments, HAUL < 15),
             mapping = aes(x = BOTTOM_DEPTH, y = SCOPE_TO_DEPTH, color = "4.5-m doors", fill = factor(treatment)),
             shape = 21, size = rel(3.3)) +
  geom_text_repel(data = dplyr::filter(usable_height_treatments, HAUL < 15) |>
                    dplyr::group_by(BOTTOM_DEPTH, HAUL) |>
                    dplyr::summarise(SCOPE_TO_DEPTH = max(SCOPE_TO_DEPTH)),
                  mapping = aes(x = BOTTOM_DEPTH, y = SCOPE_TO_DEPTH, label = HAUL, color = "4.5-m doors"),
                  size = rel(5.5)) +
  scale_y_continuous(name = "Scope/depth") +
  scale_x_log10(name = "Bottom Depth (m)", breaks = c(25, 50, 100, 150, 200)) +
  scale_color_manual(name = "Haul type", values = c("4.5-m doors" = "red", "Standard 83-112" = "grey50")) +
  scale_fill_colorblind(name = "Treatment") +
  theme_bw() +
  theme(legend.position = c(0.82, 0.80))


png(filename = here::here("analysis", "door_experiment", "plots", "treatments_by_haul2023.png"), 
    width = 180, 
    height = 180, 
    units = "mm", 
    res = 300)
print(plot_treatments_2023 +
        theme(legend.text = element_text(size = 14),
              legend.title = element_text(size = 16),
              axis.title = element_text(size = 18),
              axis.text = element_text(size = 16),
              strip.text = element_text(size = 14)))
dev.off()

