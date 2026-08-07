# Function to split hauls based on a date time field and logged event times
split_hauls <-
  function(data_to_split, date_time_field = NULL, date_time_format = "%m/%d/%y %H:%M:%S", date_time_tz = "America/Anchorage", 
           haul_log_path = here::here("data", "2026_gear_testing_haul_start_end.xlsx"), time_buffer_s = 30) {
    
    data_to_split['dt'] <- as.POSIXct(data_to_split[[date_time_field]], format = date_time_format, tz = date_time_tz)
    data_to_split['dt'] <- lubridate::with_tz( data_to_split['dt'], tzone = "America/Anchorage")
    
    if("haul" %in% names(data_to_split)) {
      data_to_split <- dplyr::select(data_to_split, -haul)
    }
    
    # Load haul data and configure date/times
    
    door_events <- readxl::read_xlsx(haul_log_path) |>
      dplyr::filter(Event %in% c("Doors Away", "Doors Up")) |>
      dplyr::mutate(start_end = ifelse(Event == "Doors Away", "start", "end"))
    
    door_events$dt <- door_events$Time_AKDT
    lubridate::date(door_events$dt) <- lubridate::date(door_events$Date_AKDT)
    door_events$dt <- lubridate::force_tz(door_events$dt, "America/Anchorage") 
    
    names(door_events) <- tolower(names(door_events))
    
    # Add buffers
    door_events$dt[door_events$event == "Doors Away"] <- 
      door_events$dt[door_events$event == "Doors Away"] - time_buffer_s
    
    door_events$dt[door_events$event == "Doors Up"] <- 
      door_events$dt[door_events$event == "Doors Up"] + time_buffer_s
    
    # QA/QC check
    check_duplicates <-
      door_events |>
      dplyr::group_by(haul, start_end) |>
      dplyr::summarise(n = dplyr::n()) |>
      dplyr::filter(n > 1) |> nrow()
    
    stopifnot(check_duplicates == 0)
    
    # Assign scope
    door_events <-
      door_events |>
      dplyr::select(haul, start_end, dt) |> 
      tidyr::pivot_wider(names_from = c("start_end"), values_from = "dt")
    
    output <- data_to_split |>
      dplyr::left_join(
        door_events,
        by = dplyr::join_by(dplyr::between(dt, start, end))
      ) |>
      dplyr::filter(!is.na(haul))
    
    print(table(output$haul))
    
    output <- dplyr::select(output, -dt, -start, -end)
    
    return(output)
    
  }