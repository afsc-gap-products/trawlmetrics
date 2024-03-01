library(trawlmetrics)
library(mgcv)

channel <- trawlmetrics::get_connected(schema = "AFSC")

min_spread_pings <- 50
gear <- 44

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



# Run sequential outlier rejection 
unique_hauls <- unique(events$HAUL_ID)

sor_width <- data.frame()

for(ii in 1:length(unique_hauls)) {
  
  sel_events <- dplyr::filter(events, HAUL_ID == unique_hauls[ii])
  
  sel_hauls <- dplyr::select(sel_events, VESSEL, CRUISE, HAUL) |>
    unique() |>
    dplyr::inner_join(hauls)
  
  sel_width <- dplyr::filter(width, 
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
  
}

sor_width$index <- 1:nrow(sor_width)


# Function to assign treatment values to spread measurements ----
set_treatment <- function(wd, trt) {
  
  unique_trt <- unique(trt$treatment)
  
  wd$treatment <- NA
  
  for(ii in 1:length(unique_trt)) {
    
    wd$treatment[wd$DATE_TIME >= trt$start[trt$treatment == unique_trt[ii]] & wd$DATE_TIME <= trt$end[trt$treatment == unique_trt[ii]]] <- unique_trt[ii]
    
    
    
  }
  
  return(wd)
  
}

sor_width_treatment <- data.frame()

for(jj in 1:length(unique_hauls)) {
  
  sel_sor_width <- dplyr::filter(sor_width, HAUL_ID == unique_hauls[jj])
  
  sel_treatment <- dplyr::filter(treatment_breaks, HAUL_ID == unique_hauls[jj])
  
  if(nrow(sel_sor_width) < 1) {
    next
  }
  sel_sor_width <- set_treatment(wd = sel_sor_width, trt = sel_treatment)
  
  sel_sor_width$treatment <- factor(sel_sor_width$treatment)
  
  sor_width_treatment <- dplyr::bind_rows(sor_width_treatment, sel_sor_width)

}


sor_good_treatments <- sor_width_treatment |>
  dplyr::filter(!is.na(treatment)) |>
  dplyr::mutate(treatment = as.numeric(as.character(treatment))) |>
  dplyr::inner_join(treatment_breaks |>
                      dplyr::filter(usable) |>
                      dplyr::select(HAUL_ID, treatment, target_speed_kn, WIRE_OUT) |>
                      unique(),
                    by = c("HAUL_ID", "treatment")) |>
  dplyr::group_by(HAUL_ID, target_speed_kn, WIRE_OUT, treatment) |>
  dplyr::summarise(n_spread_pings = n(),
                   NET_SPREAD = mean(NET_WIDTH),
                   NET_SPREAD_STANDARD_DEVIATION = sd(NET_WIDTH),
                   .groups = "keep"
  ) |>
  dplyr::filter(n_spread_pings > min_spread_pings) |>
  dplyr::inner_join(dplyr::select(hauls, HAUL_ID, VESSEL, CRUISE, HAUL, DISTANCE_FISHED, START_LATITUDE, END_LATITUDE, START_LONGITUDE, END_LONGITUDE, BOTTOM_DEPTH, GEAR_DEPTH, STATIONID, GEAR, ACCESSORIES)) |>
  dplyr::mutate(SCOPE_TO_DEPTH = WIRE_OUT/BOTTOM_DEPTH)

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

# 83-112 hauls

mod_83112 <- gam(formula = NET_SPREAD ~ s(BOTTOM_DEPTH, SCOPE_TO_DEPTH, bs = "tp", k = 100) + s(STATIONID, bs = "re"), data = standard_hauls)

fit_83112 <- dplyr::filter(sor_good_treatments, GEAR == 44) |>
  dplyr::mutate(type = "4.5-m doors")

fit_83112$fit <- predict(mod_83112, 
                         newdata = fit_83112)

standard_hauls$fit <- predict(mod_83112)
standard_hauls$type <- "Standard 83-112"

plot_spread_gam <- ggplot() +
  geom_point(data = standard_hauls,
             mapping = aes(x = fit, y = NET_SPREAD, color = type),
             alpha = 0.5) +
  geom_point(data = fit_83112,
             mapping = aes(x = fit, y = NET_SPREAD, color = type),
             size = rel(2.5)) +
  geom_abline(mapping = aes(intercept = 0, slope = 1),
              linetype = 2, linewidth = 1.5) +
  scale_color_manual(name = "Haul type", values = c("4.5-m doors" = "red", "Standard 83-112" = "grey50")) +
  scale_x_continuous(name = "Predicted wing spread (m)") +
  scale_y_continuous(name = "Observed wing spread (m)") +
  theme_bw() +
  theme(legend.position = c(0.8, 0.12))


png(filename = here::here("analysis", "door_experiment", "plots", "gam_obs_vs_predicted_spread.png"), height = 180, width = 180, units = "mm", res = 300)
print(plot_spread_gam +
        theme(legend.text = element_text(size = 16),
              legend.title = element_text(size = 16),
              axis.title = element_text(size = 20),
              axis.text = element_text(size = 18)))
dev.off()



# standard_width <- RODBC::sqlQuery(channel = channel,
#                                   query = paste0("select nm.date_time, nm.value net_width, h.haul_id 
#                          from race_data.net_mensurations nm, race_data.net_mensuration_headers nmh, race_data.hauls h
#                          where nm.net_mensuration_header_id = nmh.net_mensuration_header_id 
#                          and h.haul_id in (", paste(standard_hauls$HAUL_ID, collapse = ", "), 
#                                                  ") and h.haul_id = nmh.haul_id 
#                          and nmh.data_type_id = 2 
#                          and nm.value > 10 
#                          and nm.value < 30")) |>
#   dplyr::mutate(DATE_TIME = lubridate::with_tz(
#     lubridate::force_tz(DATE_TIME, tzone = "UTC"), 
#     tzone = "America/Anchorage")
#   ) |>
#   dplyr::arrange(DATE_TIME)
# 
# standard_height <- RODBC::sqlQuery(channel = channel,
#                                    query = paste0("select nm.date_time, nm.value net_width, h.haul_id 
#                          from race_data.net_mensurations nm, race_data.net_mensuration_headers nmh, race_data.hauls h
#                          where nm.net_mensuration_header_id = nmh.net_mensuration_header_id 
#                          and h.haul_id in (", paste(standard_hauls$HAUL_ID, collapse = ", "), 
#                                                   ") and h.haul_id = nmh.haul_id 
#                          and nmh.data_type_id = 3")) |>
#   dplyr::mutate(DATE_TIME = lubridate::with_tz(
#     lubridate::force_tz(DATE_TIME, tzone = "UTC"), 
#     tzone = "America/Anchorage")
#   ) |>
#   dplyr::arrange(DATE_TIME)
# 
# standard_events <- RODBC::sqlQuery(channel = channel,
#                                    query = paste0("select e.haul_id, e.date_time, e.event_type_id, et.name, h.haul, c.vessel_id vessel, c.cruise 
#                           from race_data.events e, race_data.event_types et, race_data.hauls h, race_data.cruises c, race_data.surveys s 
#                           where e.haul_id = h.haul_id 
#                           and e.haul_id in (", paste(standard_hauls$HAUL_ID, collapse = ", "),
#                                                   ") and h.cruise_id = c.cruise_id 
#                           and c.survey_id = s.survey_id 
#                           and et.event_type_id = e.event_type_id 
#                           and e.event_type_id in (3, 7)")) |>
#   dplyr::mutate(DATE_TIME = lubridate::with_tz(
#     lubridate::force_tz(DATE_TIME, tzone = "UTC"), 
#     tzone = "America/Anchorage")
#   ) |>
#   dplyr::arrange(DATE_TIME)
# 
# standard_rb_hauls <- RODBC::sqlQuery(channel = channel,
#                                      query = paste0("select * from racebase.haul h 
#                                                  where performance >= 0 
#                                                  and haul_type = 3 
#                                                  and stationid in ('", paste(unique(hauls$STATIONID), collapse = "', '"), "')")) |>
#   dplyr::inner_join(
#     unique(
#       dplyr::select(standard_events, VESSEL, CRUISE, HAUL, HAUL_ID)
#     )
#   )

