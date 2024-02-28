# Net width and height data from 2023 door comparison experiments
# February 26, 2024
# Sean Rohan <sean.rohan@noaa.gov>

# remotes::install_github(repo = "afsc-gap-products/postsurvey_hauldata_processing")
# install.packages("plotly", "ggthemes")

library(trawlmetrics)
library(ggthemes)

dir.create(here::here("analysis", "door_experiment", "plots"))

# Get data from experimental tow stations (net width, net height, events, and haul) ----


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
  dplyr::select(CRUISE, VESSEL, HAUL, DATE, treatment, start, end)

treatment_breaks <- dplyr::select(events, VESSEL, CRUISE, HAUL, HAUL_ID) |> 
  unique() |> 
  dplyr::inner_join(treatment_breaks, by = c("VESSEL", "CRUISE", "HAUL"))


# Get data from standard hauls at experimental tow stations ----

# Contains means and standard deviation for height and spread; spread method 7 = Width from Marport after sequential outlier rejection

standard_hauls <- RODBC::sqlQuery(channel = channel,
                                     query = paste0("select h.*, c.vessel_id vessel, c.cruise from race_data.hauls h, race_data.cruises c 
                                                 where h.station in ('", paste(unique(hauls$STATIONID), collapse = "', '"), "')
                                                 and h.performance >= 0 
                                                 and h.haul_type = 3 
                                                 and h.net_spread_method = 7 
                                                 and h.cruise_id = c.cruise_id")) |>
  dplyr::arrange(CRUISE)

standard_width <- RODBC::sqlQuery(channel = channel,
                                  query = paste0("select nm.date_time, nm.value net_width, h.haul_id 
                         from race_data.net_mensurations nm, race_data.net_mensuration_headers nmh, race_data.hauls h
                         where nm.net_mensuration_header_id = nmh.net_mensuration_header_id 
                         and h.haul_id in (", paste(standard_hauls$HAUL_ID, collapse = ", "), 
                         ") and h.haul_id = nmh.haul_id 
                         and nmh.data_type_id = 2 
                         and nm.value > 10 
                         and nm.value < 30")) |>
  dplyr::mutate(DATE_TIME = lubridate::with_tz(
    lubridate::force_tz(DATE_TIME, tzone = "UTC"), 
    tzone = "America/Anchorage")
  ) |>
  dplyr::arrange(DATE_TIME)

standard_height <- RODBC::sqlQuery(channel = channel,
                                  query = paste0("select nm.date_time, nm.value net_width, h.haul_id 
                         from race_data.net_mensurations nm, race_data.net_mensuration_headers nmh, race_data.hauls h
                         where nm.net_mensuration_header_id = nmh.net_mensuration_header_id 
                         and h.haul_id in (", paste(standard_hauls$HAUL_ID, collapse = ", "), 
                                                 ") and h.haul_id = nmh.haul_id 
                         and nmh.data_type_id = 3")) |>
  dplyr::mutate(DATE_TIME = lubridate::with_tz(
    lubridate::force_tz(DATE_TIME, tzone = "UTC"), 
    tzone = "America/Anchorage")
  ) |>
  dplyr::arrange(DATE_TIME)

standard_events <- RODBC::sqlQuery(channel = channel,
                                   query = paste0("select e.haul_id, e.date_time, e.event_type_id, et.name, h.haul, c.vessel_id vessel, c.cruise 
                          from race_data.events e, race_data.event_types et, race_data.hauls h, race_data.cruises c, race_data.surveys s 
                          where e.haul_id = h.haul_id 
                          and e.haul_id in (", paste(standard_hauls$HAUL_ID, collapse = ", "),
                          ") and h.cruise_id = c.cruise_id 
                          and c.survey_id = s.survey_id 
                          and et.event_type_id = e.event_type_id 
                          and e.event_type_id in (3, 7)")) |>
  dplyr::mutate(DATE_TIME = lubridate::with_tz(
    lubridate::force_tz(DATE_TIME, tzone = "UTC"), 
    tzone = "America/Anchorage")
  ) |>
  dplyr::arrange(DATE_TIME)

standard_rb_hauls <- RODBC::sqlQuery(channel = channel,
                                  query = paste0("select * from racebase.haul h 
                                                 where performance >= 0 
                                                 and haul_type = 3 
                                                 and stationid in ('", paste(unique(hauls$STATIONID), collapse = "', '"), "')")) |>
  dplyr::inner_join(
    unique(
      dplyr::select(standard_events, VESSEL, CRUISE, HAUL, HAUL_ID)
      )
    )

# Sequential outlier rejection on width for experimental hauls
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
  
  sel_events <- dplyr::filter(events, HAUL_ID == unique_hauls[jj])
  
  sel_treatment <- dplyr::filter(treatment_breaks, HAUL_ID == unique_hauls[jj])
  
  if(nrow(sel_sor_width) < 1) {
    next
  }
  sel_sor_width <- set_treatment(wd = sel_sor_width, trt = sel_treatment)
  
  sel_sor_width$treatment <- factor(sel_sor_width$treatment)
  
  sor_width_treatment <- dplyr::bind_rows(sor_width_treatment, sel_sor_width)
  
  treatment_vline <- tidyr::pivot_longer(sel_treatment, cols = c("start", "end")) |>
    dplyr::rename(DATE_TIME = value)
  
  width_range <- range(sel_sor_width$NET_WIDTH) + c(-0.2, 0.2)
  
  # plotly::ggplotly(
  # ggplot() +
  #   geom_path(data = sel_sor_width, 
  #             mapping = aes(x = DATE_TIME, y = NET_WIDTH),
  #             linewidth = 0.5, color = "grey50") +
  #   geom_point(data = sel_sor_width, 
  #              mapping = aes(x = DATE_TIME, 
  #                            y = NET_WIDTH, 
  #                            text = paste0("Index:",  index), 
  #                            color = treatment)) +
  #   geom_segment(data = treatment_vline,
  #                mapping = aes(x = DATE_TIME,
  #                              xend = DATE_TIME,
  #                              color = factor(treatment),
  #                              linetype = name,
  #                              y = width_range[1],
  #                              yend = width_range[2])) +
  #   geom_segment(data = sel_events,
  #              mapping = aes(x = DATE_TIME, xend = DATE_TIME, y = width_range[1], yend = width_range[2], group = NAME)) +
  #   scale_x_datetime(name = "Date/time (AKDT)") +
  #   scale_y_continuous(name = "Net Width (m)") + 
  #   scale_color_discrete(name = "Treatment") +
  #   scale_linetype_manual(name = "Start/End", values = c("start" = 1, "end" = 2)) +
  #   ggtitle(paste0("Year: ", floor(sel_events$CRUISE[1]/100), ", Vessel: ", sel_events$VESSEL[1], ", Haul: ", sel_events$HAUL[1])) +
  #   theme_bw()
  # )
  # 
  # readline("Press Enter to advance to the next plot.")

}

# Plot available data from experiments
ggplot(data = dplyr::select(events, VESSEL, CRUISE, HAUL, HAUL_ID) |>
         unique() |> 
         dplyr::inner_join(width),
       mapping = aes(x = DATE_TIME, y = NET_WIDTH)) +
  geom_segment(data = events,
               mapping = aes(x = DATE_TIME,
                             xend = DATE_TIME,
                             y = 9, yend = 25),
               color = "red") +
  geom_point() +
  facet_wrap(~HAUL, scales = "free") +
  theme_bw()

ggplot() +
  geom_point(data = dplyr::select(events, VESSEL, CRUISE, HAUL, HAUL_ID) |>
               unique() |> 
               dplyr::inner_join(sor_width),
             mapping = aes(x = DATE_TIME, y = NET_WIDTH)) +
  geom_segment(data = events,
               mapping = aes(x = DATE_TIME,
                             xend = DATE_TIME,
                             y = 9, yend = 25),
               color = "red") +
  facet_wrap(~HAUL, scales = "free_x") +
  theme_bw()

ggplot() +
  geom_point(data = dplyr::select(events, VESSEL, CRUISE, HAUL, HAUL_ID) |>
               unique() |> 
               dplyr::inner_join(sor_width_treatment),
             mapping = aes(x = DATE_TIME, y = NET_WIDTH, color = factor(treatment))) +
  geom_segment(data = events,
               mapping = aes(x = DATE_TIME,
                             xend = DATE_TIME,
                             y = 9, yend = 25),
               color = "red") +
  scale_color_discrete(name = "Treatment") +
  facet_wrap(~HAUL, scales = "free_x") +
  theme_bw()


# Plot average width and standard deviation
plot_width_sd <- ggplot() +
  geom_point(data = standard_hauls |> 
               dplyr::rename(`Net spread` = NET_SPREAD,
                             `Net spread standard deviation` = NET_SPREAD_STANDARD_DEVIATION,
                             `Bottom depth` = BOTTOM_DEPTH,
                             `Scope` = WIRE_OUT) |>
               tidyr::pivot_longer(cols = c("Net spread", "Net spread standard deviation", "Bottom depth", "Scope")),
             mapping = aes(x = STATION, y = value, color = STATION)) +
  scale_y_continuous(name = "Meters") +
  scale_x_discrete(name = "Station") +
  scale_color_colorblind(name = "Station") +
  facet_wrap(~factor(name, levels = c("Net spread", "Net spread standard deviation", "Scope", "Bottom depth")), nrow = 4, scales = "free") +
  theme_bw()

plot_width_timeseries <- ggplot() +
  geom_hline(data = standard_hauls |>
               dplyr::group_by(STATION) |>
               dplyr::summarise(MEAN_SPREAD = mean(NET_SPREAD)), 
             mapping = aes(yintercept = MEAN_SPREAD, color = STATION),
             linetype = 2) +
  geom_path(data = standard_hauls,
            mapping = aes(x = floor(CRUISE/100),
                          y = NET_SPREAD, 
                          color = STATION)) +
  geom_errorbar(data = standard_hauls,
                mapping = aes(x = floor(CRUISE/100),
                              ymin = NET_SPREAD-2*NET_SPREAD_STANDARD_DEVIATION,
                              ymax = NET_SPREAD+2*NET_SPREAD_STANDARD_DEVIATION,
                              color = STATION), width = 0.1) +
  geom_point(data = standard_hauls,
             mapping = aes(x = floor(CRUISE/100),
                           y = NET_SPREAD,
                           color = STATION)) +
  geom_text(data = standard_hauls,
            mapping = aes(x = floor(CRUISE/100),
                          y = NET_SPREAD + 2*NET_SPREAD_STANDARD_DEVIATION,
                          color = STATION,
                          label = round(WIRE_OUT/BOTTOM_DEPTH, 2)), hjust = -0.15) +
  scale_y_continuous(name = "Net spread (m)") +
  scale_x_continuous(name = "Year", limits = c(2012, 2025)) + 
  scale_color_colorblind(name = "Station") +
  facet_wrap(~STATION, nrow = 4) +
  theme_bw()

png(filename = here::here("analysis", "door_experiment", "plots", "historical_width_sd.png"), 
    width = 169, 
    height = 200, 
    units = "mm", 
    res = 300)
print(plot_width_sd)
dev.off()

png(filename = here::here("analysis", "door_experiment", "plots", "historical_width_scope2depth_timeseries.png"), 
    width = 169, 
    height = 200, 
    units = "mm", 
    res = 300)
print(plot_width_timeseries)
dev.off()


