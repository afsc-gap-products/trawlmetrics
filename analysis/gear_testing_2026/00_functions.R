library(lubridate)
library(dplyr)
library(tidyr)

split_hauls <-
  function(data_to_split, haul_log_path, buffer_eq_s, buffer_scope_change_s, buffer_hb_s) {
    
    buffer_eq_s = 30
    buffer_scope_change_s = 30
    buffer_hb_s = 30
    
    # Load haul data and configure date/times
    haul_log_path <- here::here("data", "2026_gear_testing_haul_log.xlsx")
    
    haul_events <- readxl::read_xlsx(haul_log_path) |>
      dplyr::filter(Event %in% c("Brake set", "EQ", "Scope change", "Haulback")) |>
      dplyr::mutate(start_end = ifelse(Event %in% c("EQ", "Brake set"), "start", "end"))
    
    # Add buffers
    haul_events$dt[haul_events$event == "Brake set"] <- 
      haul_events$dt[haul_events$event == "Brake set"] + buffer_eq_s
    
    haul_events$dt[haul_events$event == "EQ"] <- 
      haul_events$dt[haul_events$event == "EQ"] + buffer_eq_s
    
    haul_events$dt[haul_events$event == "Scope change"] <- 
      haul_events$dt[haul_events$event == "Scope change"] - buffer_scope_change_s
    
    haul_events$dt[haul_events$event == "Haulback"] <- 
      haul_events$dt[haul_events$event == "Haulback"] - buffer_hb_s
    
    haul_events$dt <- haul_events$Time_AKDT
    lubridate::date(haul_events$dt) <- lubridate::date(haul_events$Date_AKDT)
    haul_events$dt <- lubridate::force_tz(haul_events$dt, "America/Anchorage") 
    
    names(haul_events) <- tolower(names(haul_events))
    
    # QA/QC check
    check_duplicates <-
    haul_events |>
      dplyr::group_by(haul, pass, scope, start_end) |>
      dplyr::summarise(n = n()) |>
      dplyr::filter(n > 1) |> nrow()
    
    stopifnot(check_duplicates == 0)
    
    # Assign scope
    scope_haul <-
    haul_events |>
      dplyr::select(haul, pass, scope, start_end, dt) |> 
      tidyr::pivot_wider(names_from = c("start_end"), values_from = "dt")
    
    # Assign tension, door size, phase, and data flag 
    additional_fields <- 
      dplyr::select(haul_events, -time_akdt, -date_akdt) |>
      dplyr::filter(!is.na(tension_port))
    
    result_dplyr <- x |>
      dplyr::left_join(
        scope_haul,
        by = dplyr::join_by(dplyr::between(dt, start, end), haul)
      ) |>
      dplyr::left_join(
        additional_fields
      )
    
  }



