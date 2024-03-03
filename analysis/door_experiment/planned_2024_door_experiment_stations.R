# Net width and height observations for planned 2024 hauls
# March 3, 2024
# Sean Rohan <sean.rohan@noaa.gov>

# remotes::install_github(repo = "afsc-gap-products/postsurvey_hauldata_processing")
# install.packages("plotly", "ggthemes")

library(trawlmetrics)
library(ggthemes)

dir.create(here::here("analysis", "door_experiment", "plots"))

# Get data for planned hauls

planned_stations <- read.csv(file = here::here("analysis", "door_experiment", "data", "planned_stations_2024.csv"))

channel <- trawlmetrics::get_connected(schema = "AFSC")

# Means and standard deviation for height and spread; spread method 7 = Width from Marport after sequential outlier rejection

planned_hauls <- RODBC::sqlQuery(channel = channel,
                                  query = paste0("select h.*, c.vessel_id vessel, c.cruise from race_data.hauls h, race_data.cruises c 
                                                 where h.station in ('", paste(planned_stations$STATIONID, collapse = "', '"), "')
                                                 and h.performance >= 0 
                                                 and h.haul_type = 3 
                                                 and h.net_spread_method = 7 
                                                 and h.cruise_id = c.cruise_id")) |>
  dplyr::arrange(CRUISE)

planned_width <- RODBC::sqlQuery(channel = channel,
                                  query = paste0("select nm.date_time, nm.value net_width, h.haul_id 
                         from race_data.net_mensurations nm, race_data.net_mensuration_headers nmh, race_data.hauls h
                         where nm.net_mensuration_header_id = nmh.net_mensuration_header_id 
                         and h.haul_id in (", paste(planned_hauls$HAUL_ID, collapse = ", "), 
                                                 ") and h.haul_id = nmh.haul_id 
                         and nmh.data_type_id = 2 
                         and nm.value > 10 
                         and nm.value < 30")) |>
  dplyr::mutate(DATE_TIME = lubridate::with_tz(
    lubridate::force_tz(DATE_TIME, tzone = "UTC"), 
    tzone = "America/Anchorage")
  ) |>
  dplyr::arrange(DATE_TIME)

planned_height <- RODBC::sqlQuery(channel = channel,
                                   query = paste0("select nm.date_time, nm.value net_width, h.haul_id 
                         from race_data.net_mensurations nm, race_data.net_mensuration_headers nmh, race_data.hauls h
                         where nm.net_mensuration_header_id = nmh.net_mensuration_header_id 
                         and h.haul_id in (", paste(planned_hauls$HAUL_ID, collapse = ", "), 
                                                  ") and h.haul_id = nmh.haul_id 
                         and nmh.data_type_id = 3")) |>
  dplyr::mutate(DATE_TIME = lubridate::with_tz(
    lubridate::force_tz(DATE_TIME, tzone = "UTC"), 
    tzone = "America/Anchorage")
  ) |>
  dplyr::arrange(DATE_TIME)

planned_events <- RODBC::sqlQuery(channel = channel,
                                   query = paste0("select e.haul_id, e.date_time, e.event_type_id, et.name, h.haul, c.vessel_id vessel, c.cruise 
                          from race_data.events e, race_data.event_types et, race_data.hauls h, race_data.cruises c, race_data.surveys s 
                          where e.haul_id = h.haul_id 
                          and e.haul_id in (", paste(planned_hauls$HAUL_ID, collapse = ", "),
                                                  ") and h.cruise_id = c.cruise_id 
                          and c.survey_id = s.survey_id 
                          and et.event_type_id = e.event_type_id 
                          and e.event_type_id in (3, 7)")) |>
  dplyr::mutate(DATE_TIME = lubridate::with_tz(
    lubridate::force_tz(DATE_TIME, tzone = "UTC"), 
    tzone = "America/Anchorage")
  ) |>
  dplyr::arrange(DATE_TIME)

planned_rb_hauls <- RODBC::sqlQuery(channel = channel,
                                     query = paste0("select * from racebase.haul h 
                                                 where performance >= 0 
                                                 and haul_type = 3 
                                                 and stationid in ('", paste(planned_stations$STATIONID, collapse = "', '"), "')")) |>
  dplyr::inner_join(
    unique(
      dplyr::select(planned_events, VESSEL, CRUISE, HAUL, HAUL_ID)
    )
  )


plot_planned_ts <- ggplot() +
  geom_hline(data = planned_hauls |>
               dplyr::group_by(STATION) |>
               dplyr::summarise(MEAN_SPREAD = mean(NET_SPREAD)), 
             mapping = aes(yintercept = MEAN_SPREAD, color = STATION),
             linetype = 2) +
  geom_path(data = planned_hauls,
            mapping = aes(x = floor(CRUISE/100),
                          y = NET_SPREAD, 
                          color = STATION)) +
  geom_errorbar(data = planned_hauls,
                mapping = aes(x = floor(CRUISE/100),
                              ymin = NET_SPREAD-2*NET_SPREAD_STANDARD_DEVIATION,
                              ymax = NET_SPREAD+2*NET_SPREAD_STANDARD_DEVIATION,
                              color = STATION), width = 0.1) +
  geom_point(data = planned_hauls,
             mapping = aes(x = floor(CRUISE/100),
                           y = NET_SPREAD,
                           color = STATION)) +
  geom_text(data = planned_hauls,
            mapping = aes(x = floor(CRUISE/100),
                          y = NET_SPREAD + 2*NET_SPREAD_STANDARD_DEVIATION,
                          color = STATION,
                          label = round(WIRE_OUT/BOTTOM_DEPTH, 2)), hjust = -0.15,
            size = rel(2.5)) +
  scale_y_continuous(name = "Net spread (m)") +
  scale_x_continuous(name = "Year", limits = c(2012, 2025)) + 
  scale_color_discrete(name = "Station") +
  facet_wrap(~STATION, nrow = 4) +
  theme_bw() +
  theme(legend.position = "none",
        strip.text = element_text(size = 18),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14))

plot_planned_stations <- ggplot() +
  geom_point(data = planned_hauls |> 
               dplyr::rename(`Net spread` = NET_SPREAD,
                             `Net spread standard deviation` = NET_SPREAD_STANDARD_DEVIATION,
                             `Bottom depth` = BOTTOM_DEPTH,
                             `Scope` = WIRE_OUT) |>
               tidyr::pivot_longer(cols = c("Net spread", "Net spread standard deviation", "Bottom depth", "Scope")),
             mapping = aes(x = STATION, y = value, color = STATION)) +
  scale_y_continuous(name = "Meters") +
  scale_x_discrete(name = "Station") +
  scale_color_discrete(name = "Station") +
  facet_wrap(~factor(name, levels = c("Net spread", "Net spread standard deviation", "Scope", "Bottom depth")), nrow = 4, scales = "free") +
  theme_bw() +
  theme(legend.position = "none",
        strip.text = element_text(size = 18),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14))

table_planned_stations <- planned_hauls |>
  dplyr::rename(Station = STATION) |>
  dplyr::group_by(Station) |>
  dplyr::summarise(`Depth` = round(mean(BOTTOM_DEPTH, 1)),
                   `Spread` = round(mean(NET_SPREAD), 2),
                   `Spread SD` = round(mean(NET_SPREAD_STANDARD_DEVIATION), 2),
                   `Height` = round(mean(NET_HEIGHT), 2),
                   `Height SD` = round(mean(NET_HEIGHT_STANDARD_DEVIATION, na.rm = TRUE), 2),
                   `Min Scope` = round(min(WIRE_OUT), 2),
                   `Max Scope` = round(max(WIRE_OUT), 2)
                   )

png(filename = here::here("analysis", "door_experiment", "plots", "planned_station_width_sd.png"), 
    width = 169, 
    height = 200, 
    units = "mm", 
    res = 300)
print(plot_planned_stations)
dev.off()

png(filename = here::here("analysis", "door_experiment", "plots", "planned_station_width_scope2depth_timeseries.png"), 
    width = 169, 
    height = 200, 
    units = "mm", 
    res = 300)
print(plot_planned_ts)
dev.off()

write.csv(table_planned_stations, 
          file = here::here("analysis", "door_experiment", "plots", "planned_station_table.csv"), 
          row.names = FALSE)
