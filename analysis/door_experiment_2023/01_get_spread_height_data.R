library(trawlmetrics)
library(mgcv)
library(ggthemes)
library(ggrepel)


get_door_experiment_data <- function(channel = NULL, min_spread_pings = 50, min_height_pings = 50) {
  
  # Function to assign treatment values to spread/height measurements ----
  set_treatment <- function(wd, trt) {
    
    unique_trt <- unique(trt$treatment)
    
    wd$treatment <- NA
    
    for(ii in 1:length(unique_trt)) {
      
      wd$treatment[wd$DATE_TIME >= trt$start[trt$treatment == unique_trt[ii]] & wd$DATE_TIME <= trt$end[trt$treatment == unique_trt[ii]]] <- unique_trt[ii]
      
    }
    
    return(wd)
    
  }
  
  # Connect to oracle
  channel <- trawlmetrics::get_connected(channel, schema = "AFSC")
  
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
                         and h.haul_type = 7") |>
    dplyr::mutate(DATE_TIME = lubridate::with_tz(
      lubridate::force_tz(DATE_TIME, tzone = "UTC"), 
      tzone = "America/Anchorage",
      NET_WIDTH = marport_to_netmind(NET_WIDTH)) # Marport to Netmind conversion
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
  
  # Data from standard 83-112 hauls
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
  
  # Data from standard slope PNE hauls
  slope_hauls <- RODBC::sqlQuery(channel = channel,
                                 query = paste0("select h.*, c.vessel_id vessel, c.cruise from race_data.hauls h, race_data.cruises c, race_data.surveys s 
                                                 where h.performance >= 0 
                                                 and h.haul_type = 3 
                                                 and h.net_spread_method = 1 
                                                 and h.cruise_id = c.cruise_id  
                                                 and s.survey_id = c.survey_id 
                                                 and h.net_height > 0
                                                 and s.survey_definition_id = 78")) |>
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
    
    # Apply gating criteria
    # Maximum spread is 28 instead of 22 because the 4.5-m doors were overspreading
    acceptable_spread_range <- c(10, 28)
    
    acceptable_height_range <- switch(as.character(sel_hauls$GEAR[1]),
           "44" = c(0, 6),
           "172" = c(3, 10))
    
    sel_width <- width |> 
      dplyr::filter(HAUL_ID == unique_hauls[ii],
                    DATE_TIME >= sel_events$DATE_TIME[sel_events$EVENT_TYPE_ID == 3],
                    DATE_TIME <= sel_events$DATE_TIME[sel_events$EVENT_TYPE_ID <= 7],
                    NET_WIDTH >= acceptable_spread_range[1],
                    NET_WIDTH <= acceptable_spread_range[2])
    
    sel_height <- dplyr::filter(height, 
                                HAUL_ID == unique_hauls[ii],
                                DATE_TIME >= sel_events$DATE_TIME[sel_events$EVENT_TYPE_ID == 3],
                                DATE_TIME <= sel_events$DATE_TIME[sel_events$EVENT_TYPE_ID <= 7],
                                NET_HEIGHT >= acceptable_height_range[1],
                                NET_HEIGHT <= acceptable_height_range[2])
    
    # Gating criteria and sequential outlier rejection when the number of pings exceeds the minimum spread ping threshold
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
  
  # Get usable treatment values
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
  
  
  # Get usable spread
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
  
  # Usable height
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
  
  save(hauls, events, width, height, treatment_breaks, usable_height_pings, usable_spread_pings, ping_events, usable_height_treatments, 
       usable_width_treatments, standard_hauls, slope_hauls, file = here::here("analysis", "door_experiment", "data", "spread_height_data.rda"))
  
}

get_door_experiment_data()
