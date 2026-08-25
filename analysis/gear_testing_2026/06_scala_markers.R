# Create annotated markers file for Scala Replay

library(xml2)
library(dplyr)
library(lubridate)

haul_log_path <- here::here("data", "2026_gear_testing_haul_log.xlsx")

haul_events <- readxl::read_xlsx(haul_log_path) |>
  dplyr::filter(Event %in% c("Brake set", "EQ", "Scope change", "Haulback")) |>
  dplyr::mutate(
    Event_abbv = case_when(
      Event == "Brake set" ~ "BS",
      Event == "EQ" ~ "EQ",
      Event == "Scope change" ~ "END",
      Event == "Haulback" ~ "END"
    )
  ) |>
  dplyr::mutate(
    label = paste0(Haul, " (", format(Door_size_m2, nsmall = 1), "): ", Event_abbv, " ", Scope)
  )

haul_events$dt <- haul_events$Time_AKDT
lubridate::date(haul_events$dt) <- lubridate::date(haul_events$Date_AKDT)
haul_events$dt <- lubridate::force_tz(haul_events$dt, "America/Anchorage") 

haul_events$dt_utc <- lubridate::with_tz(haul_events$dt, "UTC")

haul_events$dt_utc <- format(haul_events$dt_utc, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

dir.create(here::here("output", "scala_markers"), showWarnings = FALSE, recursive = TRUE)

doc <- xml_new_root("Markers")

for (kk in seq_len(nrow(haul_events))) {
  xml_add_child(doc, "Marker", date =haul_events$dt_utc[kk], text = haul_events$label[kk])
}

write_xml(doc, here::here("output", "scala_markers", "Markers.xml"))
