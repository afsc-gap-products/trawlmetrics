# Example of a haul with change in height and spread
# Created by Sean Rohan <sean.rohan@noaa.gov>
# March 3, 2024
#
# This example shows the change in net dimensions during 30 minute tow in 2019 on the Alaska Knight 
# that caught ~5600 kg of yellowfin sole.

library(trawlmetrics)
library(ggthemes)
library(cowplot)

channel <- trawlmetrics::get_connected(schema = "AFSC")

events <- RODBC::sqlQuery(channel = channel,
                          query = "select e.haul_id, e.date_time, e.event_type_id, et.name, h.haul, c.vessel_id vessel, c.cruise, h.bottom_depth, h.wire_out 
                          from race_data.events e, race_data.event_types et, race_data.hauls h, race_data.cruises c, race_data.surveys s 
                          where e.haul_id = h.haul_id 
                          and h.cruise_id = c.cruise_id 
                          and c.survey_id = s.survey_id 
                          and s.survey_definition_id = 98 
                          and c.cruise = 201901
                          and h.haul = 9
                          and c.vessel_id = 162
                          and et.event_type_id = e.event_type_id 
                          and e.event_type_id in (3, 7)") |>
  dplyr::mutate(DATE_TIME = lubridate::with_tz(
    lubridate::force_tz(DATE_TIME, tzone = "UTC"), 
    tzone = "America/Anchorage")
  ) |>
  dplyr::arrange(DATE_TIME)

ex_width <- RODBC::sqlQuery(channel = channel, 
                            query = "select nm.date_time, nm.value net_width, h.haul_id 
                         from race_data.net_mensurations nm, race_data.net_mensuration_headers nmh, race_data.hauls h, race_data.cruises c, race_data.surveys s 
                         where nm.net_mensuration_header_id = nmh.net_mensuration_header_id 
                         and h.haul_id = nmh.haul_id 
                         and h.cruise_id = c.cruise_id 
                         and c.survey_id = s.survey_id 
                         and s.survey_definition_id = 98 
                         and c.cruise = 201901
                         and h.haul = 9
                         and c.vessel_id = 162
                         and nmh.data_type_id = 2 
                         and nm.value >= 10
                         and nm.value <= 22
                         and h.haul_type = 3") |>
  dplyr::mutate(DATE_TIME = lubridate::with_tz(
    lubridate::force_tz(DATE_TIME, tzone = "UTC"), 
    tzone = "America/Anchorage")
  ) |>
  dplyr::arrange(DATE_TIME) |>
  dplyr::filter(DATE_TIME >= events$DATE_TIME[events$EVENT_TYPE_ID == 3],
                DATE_TIME <= events$DATE_TIME[events$EVENT_TYPE_ID == 7])



ex_width$vessel <- sel_hauls$VESSEL[1]
ex_width$cruise <- sel_hauls$CRUISE[1]
ex_width$haul <- sel_hauls$HAUL[1]

sor_results <- ex_width |>
  dplyr::rename(measurement_value = NET_WIDTH) |>
  sequentialOR(formula = measurement_value ~ DATE_TIME, 
               method = 'ss', 
               n.reject = 1, 
               n.stop = 0.5, 
               threshold.stop = TRUE)

ex_sor_width <- dplyr::filter(sor_results$obs_rank, is.na(SOR_RANK)) |>
  dplyr::mutate(NET_WIDTH = marport_to_netmind(measurement_value)) |>
  dplyr::select(-SOR_RANK, -index, -measurement_value)

ex_height <- RODBC::sqlQuery(channel = channel, 
                            query = "select nm.date_time, nm.value net_height, h.haul_id 
                         from race_data.net_mensurations nm, race_data.net_mensuration_headers nmh, race_data.hauls h, race_data.cruises c, race_data.surveys s 
                         where nm.net_mensuration_header_id = nmh.net_mensuration_header_id 
                         and h.haul_id = nmh.haul_id 
                         and h.cruise_id = c.cruise_id 
                         and c.survey_id = s.survey_id 
                         and s.survey_definition_id = 98 
                         and c.cruise = 201901
                         and h.haul = 9
                         and c.vessel_id = 162
                         and nmh.data_type_id = 3 
                         and nm.value >= 0
                         and nm.value <= 6
                         and h.haul_type = 3") |>
  dplyr::mutate(DATE_TIME = lubridate::with_tz(
    lubridate::force_tz(DATE_TIME, tzone = "UTC"), 
    tzone = "America/Anchorage")
  ) |>
  dplyr::arrange(DATE_TIME) |>
  dplyr::filter(DATE_TIME >= events$DATE_TIME[events$EVENT_TYPE_ID == 3],
                DATE_TIME <= events$DATE_TIME[events$EVENT_TYPE_ID == 7])



png(here::here("analysis", "door_experiment", "plots", "geometry_changed_during_haul.png"), width = 200, height = 140, units = "mm", res = 300)
print(
  cowplot::plot_grid(
ggplot(data = ex_sor_width,
       mapping = aes(x = DATE_TIME, y = NET_WIDTH)) +
  geom_point() + 
  geom_smooth() +
  scale_x_datetime(name = "Time") +
  scale_y_continuous(name = "Net spread (m)", limits = c(13, 19)) +
  theme_bw() +
  theme(legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 18)),
  ggplot(data = ex_height,
         mapping = aes(x = DATE_TIME, y = NET_HEIGHT)) +
    geom_point() + 
    geom_smooth() +
    scale_x_datetime(name = "Time") +
    scale_y_continuous(name = "Net height (m)", limits = c(1.5, 4)) +
    theme_bw() +
    theme(legend.text = element_text(size = 12),
          legend.title = element_text(size = 14),
          axis.title = element_text(size = 20),
          axis.text = element_text(size = 18))
)
)
dev.off()

