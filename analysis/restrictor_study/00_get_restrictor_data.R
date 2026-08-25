# Get haul data from selected tows

library(trawlmetrics)
library(readxl)
library(ggpp)

# Restrictor haul log

haul_log <- readxl::read_xlsx(path = here::here("data", "restrictor_treatments.xlsx"))

channel <- trawlmetrics::get_connected(schema = "AFSC")

haul_data <- 
  RODBC::sqlQuery(
    channel = channel,
    query = 
      "SELECT 
      h.haul_id,
  h.haul,
  h.haul_type,
  h.edit_wire_out wire_out,
  h.edit_net_height net_height, h.edit_net_spread net_spread, c.cruise, c.vessel_id vessel, 
  h.edit_bottom_depth bottom_depth, h.edit_gear_depth gear_depth
  FROM race_data.edit_hauls h, race_data.cruises c
  WHERE c.cruise_id in (778, 780) 
  AND c.vessel_id in (162, 148) 
  AND h.cruise_id = c.cruise_id"
  ) |>
  dplyr::inner_join(
    haul_log, by = c("VESSEL", "CRUISE", "HAUL")
  )

unique_haul_id <- unique(haul_data$HAUL_ID)

event_data <- 
  RODBC::sqlQuery(
    channel = channel,
    query = 
      paste0("SELECT 
  h.haul_id, h.haul, c.cruise, c.vessel_id vessel, e.edit_date_time date_time, e.event_type_id
  FROM race_data.edit_hauls h, race_data.cruises c, race_data.edit_events e
  WHERE c.cruise_id in (778, 780) 
  AND c.vessel_id in (162, 148) 
  AND h.cruise_id = c.cruise_id 
  AND e.haul_id = h.haul_id 
  AND e.event_type_id in (3,4,7)
  AND h.haul_id in (", paste(unique_haul_id, collapse = ","),  ")")
  ) |>
  dplyr::inner_join(
    data.frame(VESSEL = c(148, 148, 162, 162),
               EVENT_TYPE_ID = c(4, 7, 3, 7),
               EVENT = c("start", "end", "start", "end")),
    by = c("VESSEL", "EVENT_TYPE_ID")
  ) |>
  dplyr::select(-EVENT_TYPE_ID) |>
  tidyr::pivot_wider(
    values_from = "DATE_TIME",
    names_from = "EVENT"
  ) |>
  dplyr::mutate(start = start + 40,
                end = end - 40) # Add 30 second buffer to start time so geometry stabilizes

nm_data <- 
  RODBC::sqlQuery(
    channel = channel,
    query = 
      paste0("SELECT nm.edit_date_time date_time, nm.edit_value value, nmh.cabinet_sensor_flag, 
      h.haul_id, h.haul, c.cruise, c.vessel_id vessel
  FROM race_data.edit_hauls h, race_data.cruises c, race_data.edit_net_mensurations nm, 
    race_data.edit_net_mensuration_headers nmh
  WHERE c.cruise_id in (778, 780) 
  AND c.vessel_id in (162, 148) 
  AND h.cruise_id = c.cruise_id 
  AND nmh.haul_id = h.haul_id 
  AND nm.net_mensuration_header_id = nmh.net_mensuration_header_id
  AND h.haul_id in (", paste(unique_haul_id, collapse = ","),  ")")
  )

# Get data between haul start and end times
nm_data <- 
  nm_data |>
  dplyr::select(-HAUL_ID, -HAUL) |>
  dplyr::inner_join(
    event_data,
    nm_data, 
    by = dplyr::join_by(dplyr::between(DATE_TIME, start, end), VESSEL, CRUISE)
  )


# Apply window filters to height and spread data

cond_flag12  <- nm_data$CABINET_SENSOR_FLAG == 12 & (nm_data$VALUE < 8 | nm_data$VALUE > 22)
cond_vessel148 <- nm_data$CABINET_SENSOR_FLAG == 23 & nm_data$VESSEL == 148 & (nm_data$VALUE < 3 | nm_data$VALUE > 10)
cond_vessel162 <- nm_data$CABINET_SENSOR_FLAG == 23 & nm_data$VESSEL == 162 & (nm_data$VALUE < 1 | nm_data$VALUE > 6)

# Apply NAs across all conditions simultaneously
nm_data$VALUE[cond_flag12 | cond_vessel148 | cond_vessel162] <- NA

nm_filtered <- nm_data[!is.na(nm_data$VALUE), ]

nm_filtered <- 
  nm_filtered |>
  dplyr::select(-start, -end) |>
  dplyr::inner_join(haul_log)

ggplot() +
  geom_point(data = dplyr::filter(nm_filtered, CABINET_SENSOR_FLAG == 12), 
             mapping = aes(x = DATE_TIME, y = VALUE, color = factor(RESTRICTOR))) +
  facet_wrap(~BLOCK, scales = "free")

ggplot() +
  geom_boxplot(
    data = dplyr::filter(nm_filtered, CABINET_SENSOR_FLAG == 12), 
    mapping = 
      aes(
        x = factor(RESTRICTOR, levels = c("None", "Equal", "-2")), 
        y = VALUE, 
        color = factor(RESTRICTOR)),
    size = rel(0.4)
  ) +
  geom_point(
    data = haul_log,
    mapping = aes(
      x = factor(RESTRICTOR, levels = c("None", "Equal", "-2")),
      y = as.numeric(RESTRICTOR_LENGTH),
      color = factor(RESTRICTOR)),
    shape = 19,
    size = rel(3)
  ) +
  facet_wrap(~BLOCK) +
  scale_x_discrete(name = "Restrictor") +
  scale_y_continuous(name = "Upper Wing Spread (m)") +
  scale_color_discrete(guide = "none") +
  theme_bw()


restrictor_results <- 
  dplyr::filter(nm_filtered, CABINET_SENSOR_FLAG == 12) |>
  dplyr::group_by(VESSEL, CRUISE, HAUL, CABINET_SENSOR_FLAG, RESTRICTOR, GEAR, RESTRICTOR_LENGTH, BLOCK) |>
  dplyr::summarise(
    VALUE = mean(VALUE, na.rm = TRUE)
  ) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    VALUE_OFFSET = VALUE - 2,
    VALUE_M2N = trawlmetrics::marport_to_netmind(VALUE),
    VALUE_OFFSET_M2N = trawlmetrics::marport_to_netmind(VALUE_OFFSET)
  )

save(restrictor_results, nm_filtered, nm_data, haul_data, event_data, file = here::here("data", "restrictor_data.rda"))

# Plot restrictor line results

ggplot() +
  geom_point(
    data = restrictor_results,
    mapping = aes(
      x = as.numeric(RESTRICTOR_LENGTH), 
      y = VALUE, 
      color = GEAR, 
      shape = GEAR),
    size = rel(3)
             ) +
  scale_x_continuous(name = "Restrictor length (m)") +
  scale_y_continuous(name = "Observed upper wing tip spread (m)") +
  scale_shape_manual(name = "Gear", values = c(1, 3)) +
  scale_color_discrete(name = "Gear") +
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  theme_bw()

p_restrictor_obs_1 <- 
  ggplot() +
  geom_point(
    data = restrictor_results,
    mapping = aes(
      x = as.numeric(RESTRICTOR_LENGTH), 
      y = VALUE - as.numeric(RESTRICTOR_LENGTH), 
      shape = GEAR,
      color = GEAR
    ),
    size = rel(3)
  ) +
  geom_text_npc(mapping = 
                  aes(
                    npcx = 0.02, 
                    npcy = 0.95, 
                    label = "Raw spread (GOA/AI, 2024-present)"), 
                size = 5.5) +
  geom_hline(yintercept = 0, linetype = 2) +
  scale_y_continuous(name = "Obs.-Restrictor (m)", limits = c(-4.5, 2)) +
  scale_x_continuous(name = "Restrictor Line Length (m)") +
  scale_shape_manual(name = "Gear", values = c(1, 3)) +
  scale_color_discrete(name = "Gear") +
  theme_bw()

p_restrictor_obs_2 <- 
  ggplot() +
  geom_point(
    data = restrictor_results,
    mapping = aes(
      x = as.numeric(RESTRICTOR_LENGTH), 
      y = VALUE_OFFSET - as.numeric(RESTRICTOR_LENGTH), 
      shape = GEAR,
      color = GEAR
    ),
    size = rel(3)
  ) +
  geom_text_npc(mapping = 
                  aes(
                    npcx = 0.02, 
                    npcy = 0.95, 
                    label = "Marport to Netmind conversion (EBS, 2024-present)"), 
                size = 5.5
  ) +
  geom_hline(yintercept = 0, linetype = 2) +
  scale_y_continuous(name = "Obs.-Restrictor (m)", limits = c(-4.5, 2)) +
  scale_x_continuous(name = "Restrictor Line Length (m)") +
  scale_shape_manual(name = "Gear", values = c(1, 3)) +
  scale_color_discrete(name = "Gear") +
  theme_bw()

p_restrictor_obs_4 <- 
  ggplot() +
  geom_point(
    data = restrictor_results,
    mapping = aes(
      x = as.numeric(RESTRICTOR_LENGTH), 
      y = VALUE_OFFSET_M2N- as.numeric(RESTRICTOR_LENGTH), 
      shape = GEAR,
      color = GEAR
    ),
    size = rel(3)
  ) +
  geom_text_npc(mapping = 
                  aes(
                    npcx = 0.02, 
                    npcy = 0.95, 
                    label = "2-m offset and M2N (EBS, 2013-2023)"), 
                size = 5.5) +
  geom_hline(yintercept = 0, linetype = 2) +
  scale_y_continuous(name = "Obs.-Restrictor (m)", limits = c(-4.5, 2)) +
  scale_x_continuous(name = "Restrictor Line Length (m)") +
  scale_shape_manual(name = "Gear", values = c(1, 3)) +
  scale_color_discrete(name = "Gear") +
  theme_bw()

p_restrictor_obs_3 <- 
  ggplot() +
  geom_point(
    data = restrictor_results,
    mapping = aes(
      x = as.numeric(RESTRICTOR_LENGTH), 
      y = VALUE_OFFSET - as.numeric(RESTRICTOR_LENGTH), 
      shape = GEAR,
      color = GEAR
    ),
    size = rel(3)
  ) +
  geom_text_npc(mapping = 
                  aes(
                    npcx = 0.02, 
                    npcy = 0.95, 
                    label = "2-m offset (GOA/AI, 2013-2023)"), 
                size = 5.5) +
  geom_hline(yintercept = 0, linetype = 2) +
  scale_y_continuous(name = "Obs.-Restrictor (m)", limits = c(-4.5, 2)) +
  scale_x_continuous(name = "Restrictor Line Length (m)") +
  scale_shape_manual(name = "Gear", values = c(1, 3)) +
  scale_color_discrete(name = "Gear") +
  theme_bw()


cowplot::plot_grid(
  p_restrictor_obs_1 + 
    theme(
      legend.position = "inside", 
      axis.text = element_text(size = 16),
      axis.title.x = element_blank(),
      legend.direction = "horizontal",
      legend.position.inside = c(0.12, 0.15), 
      axis.title.y = element_text(size = 16)
      ),
  p_restrictor_obs_2 + 
    theme(legend.position = "none",
          axis.text = element_text(size = 16),
          axis.title.x = element_blank(), 
          axis.title.y = element_text(size = 16)),
  p_restrictor_obs_3 + 
    theme(legend.position = "none", 
          axis.text = element_text(size = 16),
          axis.title.x = element_blank(), 
          axis.title.y = element_text(size = 16)),
  p_restrictor_obs_4 + 
    theme(legend.position = "none", 
          axis.text = element_text(size = 16),
          axis.title = element_text(size = 16)),
  nrow = 4,
  align = "hv"
)


mean(restrictor_results$VALUE-as.numeric(restrictor_results$RESTRICTOR_LENGTH), na.rm = TRUE)

range(restrictor_results$VALUE-as.numeric(restrictor_results$RESTRICTOR_LENGTH), na.rm = TRUE)
