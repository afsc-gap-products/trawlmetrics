library(lubridate)
library(dplyr)
library(tidyr)
library(ggpp)

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
      dplyr::summarise(n = n()) |>
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

# 
# # Process data from multi-pass hauls
# 
# dir.create(here::here("data", "01_bcs_data", "split_files"), recursive = TRUE)
# 
# bcs_paths <- 
#   list.files(
#     path = here::here("data", "01_bcs_data", "combined_files"), 
#     recursive = TRUE, pattern = ".csv", full.names = TRUE
#   )
# 
# bcs_basename <- basename(bcs_paths)
# 
# for(ii in 1:length(bcs_paths)) {
#   
#   bcs_with_haul <- split_hauls(
#     data_to_split <- read.csv(
#       file = bcs_paths[ii],
#       fileEncoding = "latin1",
#       skip = 1),
#     date_time_field = "Date.Time..GMT.08.00",
#     date_time_format = "%m/%d/%y %H:%M:%S",
#     date_time_tz = "America/Anchorage",
#     haul_log_path = here::here("data", "2026_gear_testing_haul_start_end.xlsx"),
#     time_buffer_s = 30
#   )
#   
#   header <- readLines(bcs_paths[ii], 2)[2]
#   
#   unique_bcs_hauls <- unique(bcs_with_haul$haul)
#   
#   for(jj in 1:length(unique_bcs_hauls)) {
#     
#     sel_haul <- bcs_with_haul |>
#       dplyr::filter(haul == unique_bcs_hauls[jj]) |>
#       dplyr::select(-haul)
#     
#     new_basename <- sub("^\\d+", replacement = unique_bcs_hauls[jj], x = bcs_basename[ii])
#     
#     fpath <- here::here("data", "01_bcs_data", "split_files", new_basename)
#     
#     writeLines(paste0("Plot Title: ", new_basename, " \n"), con = fpath)
#     writeLines(header, con = fpath)
#     
#     write.table(
#       x = sel_haul,
#       file = fpath,
#       append = TRUE,
#       sep = ",",
#       row.names = FALSE,
#       col.names = FALSE,
#       quote = FALSE
#     )
#     
#   }
#   
#   
# }


# Function to isolate individual treatments within a haul
isolate_treatments <-
  function(data_to_split, haul_log_path = here::here("data", "2026_gear_testing_haul_log.xlsx"), buffer_eq_s = 30, buffer_scope_change_s = 10, buffer_hb_s = 10) {
    
    haul_events <- readxl::read_xlsx(haul_log_path) |>
      dplyr::filter(Event %in% c("Brake set", "EQ", "Scope change", "Haulback")) |>
      dplyr::mutate(start_end = ifelse(Event %in% c("EQ", "Brake set"), "start", "end"))
    
    haul_events$dt <- haul_events$Time_AKDT
    lubridate::date(haul_events$dt) <- lubridate::date(haul_events$Date_AKDT)
    haul_events$dt <- lubridate::force_tz(haul_events$dt, "America/Anchorage") 
    
    names(haul_events) <- tolower(names(haul_events))
    
    # Add buffers
    haul_events$dt[haul_events$event == "Brake set"] <- 
      haul_events$dt[haul_events$event == "Brake set"] + buffer_eq_s
    
    haul_events$dt[haul_events$event == "EQ"] <- 
      haul_events$dt[haul_events$event == "EQ"] + buffer_eq_s
    
    haul_events$dt[haul_events$event == "Scope change"] <- 
      haul_events$dt[haul_events$event == "Scope change"] - buffer_scope_change_s
    
    haul_events$dt[haul_events$event == "Haulback"] <- 
      haul_events$dt[haul_events$event == "Haulback"] - buffer_hb_s
    
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
      dplyr::filter(!is.na(port_tension))
    
    output <- data_to_split |>
      dplyr::left_join(
        scope_haul,
        by = dplyr::join_by(dplyr::between(dt, start, end), haul)
      ) |>
      dplyr::left_join(
        additional_fields
      )
    
    return(output)
    
  }
