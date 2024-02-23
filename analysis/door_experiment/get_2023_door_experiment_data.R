# Net width and height data from door comparison experiments
# February 22, 2024
# Sean Rohan <sean.rohan@noaa.gov>

# remotes::install_github(repo = "afsc-gap-products/postsurvey_hauldata_processing")
install.packages("plotly")

library(trawlmetrics)

# Get data (net width, net height, events, and haul) ----

channel <- trawlmetrics::get_connected(schema = "AFSC")

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
                         and nm.value < 22 
                         and h.haul_type = 7") |>
  dplyr::mutate(DATE_TIME = lubridate::with_tz(
    lubridate::force_tz(DATE_TIME, tzone = "UTC"), 
    tzone = "America/Anchorage")
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
                          and nm.value > 10 
                          and nm.value < 22
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
                          and e.event_type_id in (2, 3, 5, 6, 7, 8, 9)") |>
  dplyr::mutate(DATE_TIME = lubridate::with_tz(
    lubridate::force_tz(DATE_TIME, tzone = "UTC"), 
    tzone = "America/Anchorage")
    ) |>
  dplyr::arrange(DATE_TIME)

hauls <- RODBC::sqlQuery(channel = channel,
                         query = "select * from racebase.haul where vessel = 162 and cruise = 202301 and haul_type = 7")

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
  if(nrow(sel_width) >= 50) {
    
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

for(jj in 1:length(unique_hauls)) {
  
  sel_sor_width <- dplyr::filter(sor_width, HAUL_ID == unique_hauls[jj])
  
  plotly::ggplotly(
  ggplot() +
    geom_point(data = sel_sor_width, 
               mapping = aes(x = DATE_TIME, y = NET_WIDTH, text = paste0("Index:",  INDEX))) +
    geom_path(data = sel_sor_width, 
               mapping = aes(x = DATE_TIME, y = NET_WIDTH)) +
    scale_x_datetime(name = "Date/time (AKDT)") +
    scale_y_continuous(name = "Net Width (m)") + 
    theme_bw()
  )
  
  readline("Press Enter to advance to the next plot.")

}


# continue <- "n"
# 
# sel_width$COMMENT <- NA
# sel_width$col <- "black"
# 
# plot(x = sel_width$DATE_TIME, 
#      y = sel_width$NET_WIDTH, 
#      col = sel_width$col,
#      pch = 19,
#      xlab = "Time", 
#      ylab = "Width (m)", 
#      main = paste0("Haul: ", sel_events$HAUL[1]))
# 
# 
# plot(x = sel_width$DATE_TIME, 
#      y = sel_width$NET_WIDTH, 
#      col = sel_width$col,
#      pch = 19,
#      xlab = "Time", 
#      ylab = "Width (m)", 
#      main = paste0("Haul: ", sel_events$HAUL[1]),
#      type = 'l')
