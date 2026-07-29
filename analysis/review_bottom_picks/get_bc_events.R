library(trawlmetrics) #GitHub: github.com/afsc-gap-products/trawlmetrics
library(cowplot)

channel <- trawlmetrics::get_connected(schema = "AFSC")

# Edit this part to change the survey, vessel, and region
year <- 2026
survey_definition_id <- 98
vessel_id <- c(134, 162)
region <- "ebs"

if(region == "ebs") {
  e1 <- 3
  e1_name <- "On bottom"
} else {
  e1 <- 4
  e1_name <- "Equilibrium"
}

dir.create(here::here("analysis", "review_bottom_picks", "plots", year), recursive = TRUE)

events <- 
  RODBC::sqlQuery(
    channel = channel,
    query = 
      paste0(
      "SELECT 
          e.HAUL_ID, e.EDIT_DATE_TIME, e.EVENT_TYPE_ID, et.NAME 
        FROM
          RACE_DATA.EDIT_EVENTS e, RACE_DATA.EDIT_HAULS h, RACE_DATA.CRUISES c, RACE_DATA.SURVEYS s, RACE_DATA.EVENT_TYPES et 
        WHERE 
          s.SURVEY_ID = c.SURVEY_ID 
          AND c.CRUISE_ID = h.CRUISE_ID 
          AND h.HAUL_ID = e.HAUL_ID 
          AND e.EVENT_TYPE_ID = et.EVENT_TYPE_ID
          AND c.VESSEL_ID in (", paste(vessel_id, collapse = ","), ")",
          " AND s.survey_definition_ID in (", paste(survey_definition_id, collapse = ","), ")",
          " AND s.YEAR = ", year,
          " ORDER BY e.HAUL_ID, e.EVENT_TYPE_ID"
      )
  )

hauls <- 
  RODBC::sqlQuery(
    channel = channel,
    query = 
      paste0(
      "SELECT 
          h.HAUL_ID, c.CRUISE, h.HAUL, c.VESSEL_ID
        FROM
          RACE_DATA.EDIT_HAULS h, RACE_DATA.CRUISES c, RACE_DATA.SURVEYS s
        WHERE 
          s.SURVEY_ID = c.SURVEY_ID 
          AND c.CRUISE_ID = h.CRUISE_ID 
          AND c.VESSEL_ID in (", paste(vessel_id, collapse = ","), ")",
          " AND s.survey_definition_ID in (", paste(survey_definition_id, collapse = ","), ")",
          " AND s.YEAR = ", year,
          " ORDER BY h.HAUL_ID"
      )
  ) 

bc <- RODBC::sqlQuery(
  channel = channel,
  query = 
    paste0(
    "SELECT 
        h.HAUL_ID, b.EDIT_DATE_TIME, h.BOTTOM_CONTACT_HEADER_ID, b.EDIT_X_AXIS, b.EDIT_Y_AXIS, b.EDIT_Z_AXIS 
      FROM 
        RACE_DATA.EDIT_BOTTOM_CONTACTS b, RACE_DATA.EDIT_BOTTOM_CONTACT_HEADERS h, RACE_DATA.EDIT_HAULS u, RACE_DATA.CRUISES c, RACE_DATA.SURVEYS s
      WHERE
        h.BOTTOM_CONTACT_HEADER_ID = b.BOTTOM_CONTACT_HEADER_ID 
        AND h.HAUL_ID = u.HAUL_ID 
        AND c.CRUISE_ID = u.CRUISE_ID 
        AND s.SURVEY_ID = c.SURVEY_ID 
        AND b.DATUM_CODE = 0 
        AND c.VESSEL_ID in (", paste(vessel_id, collapse = ","), ")",
        " AND s.survey_definition_ID in (", paste(survey_definition_id, collapse = ","), ")",
        "AND s.YEAR = ", year, 
    "ORDER BY b.BOTTOM_CONTACT_HEADER_ID, b.EDIT_DATE_TIME ASC"
))

unique_haul_id <- unique(hauls$HAUL_ID)

for(ii in 1:length(unique_haul_id)) {
  
  
  sel_haul <- dplyr::filter(hauls, HAUL_ID == unique_haul_id[ii])
  
  sel_events <- dplyr::filter(events, HAUL_ID == unique_haul_id[ii]) |>
    dplyr::inner_join(
      sel_haul,
      by = "HAUL_ID"
    )
  
  sel_bottom <- sel_events |>
    dplyr::filter(EVENT_TYPE_ID %in% c(e1, 7))
  
  sel_start <- dplyr::filter(sel_events, EVENT_TYPE_ID == 15)$EDIT_DATE_TIME
  
  sel_end <- dplyr::filter(sel_events, EVENT_TYPE_ID == 16)$EDIT_DATE_TIME
  
  sel_start_end <- dplyr::filter(sel_events, EVENT_TYPE_ID %in% c(e1, 7))
  
  sel_bc <- bc |> 
    dplyr::filter(
      HAUL_ID == unique_haul_id[ii],
      EDIT_DATE_TIME >= sel_start,
      EDIT_DATE_TIME <= sel_end
    ) |>
    tidyr::pivot_longer(cols = c("EDIT_X_AXIS", "EDIT_Y_AXIS", "EDIT_Z_AXIS")) |>
    dplyr::inner_join(
      data.frame(
        name = c("EDIT_X_AXIS", "EDIT_Y_AXIS", "EDIT_Z_AXIS"),
        short_name = c("x", "y", "z")),
      by = "name"
    )
  
  p_accel <- 
    ggplot() +
    geom_point(
      data = sel_bc,
      mapping = aes(x = EDIT_DATE_TIME, y = value, color = name),
      size = 0.1
    ) +
    geom_vline(
      data = sel_start_end,
      mapping = aes(xintercept = EDIT_DATE_TIME, linetype = NAME)
    ) +
    ggtitle(label = paste0("Cruise: ", sel_haul$CRUISE, ", Vessel: ", sel_haul$VESSEL, ", Haul: ", sel_haul$HAUL, ", Haul ID: ", sel_haul$HAUL_ID)) +
    scale_color_discrete(name = "Axis") +
    scale_x_datetime(name = "Time") +
    scale_y_continuous(name = "Acceleration (g)") +
    theme_bw()
  
  onbottom_window <- sel_bottom$EDIT_DATE_TIME[sel_bottom$EVENT_TYPE_ID == e1] + c(-120, 120)
  offbottom_window <- sel_bottom$EDIT_DATE_TIME[sel_bottom$EVENT_TYPE_ID == 7] + c(-120, 120)
  
  p_accel_on <- 
    ggplot() +
    geom_point(
      data = sel_bc,
      mapping = aes(x = EDIT_DATE_TIME, y = value, color = name),
      size = 0.1
    ) +
    geom_vline(
      data = sel_start_end,
      mapping = aes(xintercept = EDIT_DATE_TIME, linetype = NAME)
    ) +
    ggtitle(label = e1_name) +
    scale_color_discrete(name = "Axis") +
    scale_x_datetime(name = "Time", limits = onbottom_window) +
    scale_y_continuous(name = "Acceleration (g)") +
    theme_bw()
  
  p_accel_off <- 
    ggplot() +
    geom_point(
      data = sel_bc,
      mapping = aes(x = EDIT_DATE_TIME, y = value, color = name),
      size = 0.1
    ) +
    geom_vline(
      data = sel_start_end,
      mapping = aes(xintercept = EDIT_DATE_TIME, linetype = NAME)
    ) +
    ggtitle(label = "Off bottom") +
    scale_color_discrete(name = "Axis") +
    scale_x_datetime(name = "Time", limits = offbottom_window) +
    scale_y_continuous(name = "Acceleration (g)") +
    theme_bw()
  
  p_accel_panels <-
    cowplot::plot_grid(
      p_accel + theme(legend.position = "none"),
      p_accel_on,
      p_accel_off + theme(legend.position = "none"),
      align = "hv",
      nrow = 3
    )
  
  png(
    here::here("analysis", "review_bottom_picks", "plots", year, paste0("BC_review_", sel_haul$CRUISE, "_", sel_haul$VESSEL_ID, "_", sel_haul$HAUL, ".png")),
    height = 8, width = 6, units = "in", res = 300
    )
 print(p_accel_panels)
  dev.off()
  
}


