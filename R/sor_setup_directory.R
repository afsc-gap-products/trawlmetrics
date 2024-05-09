#' Setup directory
#' 
#' Function to retrieve data for sequential outlier rejection from RACEBASE and split data by haul.
#' 
#' @param channel An RODBC channel. Will prompt user to get connected if NULL.
#' @param region Survey region as a 1L character vector (EBS or NBS)
#' @param cruise Cruise number as a numeric vector (e.g. 202202)
#' @param cruise_idnum Cruise ID number as a numeric vector (e.g. 757)
#' @param vessel vessel ID number as a numeric vector (e.g. 162 for Alaska Knight.
#' @param survey Survey name prefix to use in file name (e.g. NBS_2022)
#' @param haul_types A numeric vector of HAUL_TYPE to use.
#' @param gear_codes A numeric vector of GEAR codes to use.
#' @param width_range Gate filter for net width values as a 2L numeric vector. If not provided, Defaults to survey standards if not provided c(8,22) for GOA and AI, c(10, 22) for EBS/NBS
#' @import RODBC getPass
#' @export

sor_setup_directory <- function(channel = NULL, 
                                region, 
                                cruise, 
                                cruise_idnum, 
                                vessel, 
                                survey, 
                                haul_types = NULL,
                                gear_codes = NULL,
                                width_range = NULL) {
  
  region <- toupper(region)
  
  stopifnot("setup_sor_directory: Region must be 'EBS', 'NBS', 'GOA', or 'AI' " = region %in% c("EBS", "NBS", "GOA", "AI"))  
  channel <- get_connected(schema = "AFSC")
  
  # Setup file paths and directories  
  output_dir <- here::here("output", region, cruise, vessel)
  ping_files_dir <- paste0(output_dir, "/ping_files_", survey)
  
  dir.create(paste0(output_dir, "/SOR_files_", survey) , 
             showWarnings = FALSE,
             recursive = TRUE)
  
  dir.create(paste0(output_dir, "/SOR_graphics_", survey), 
             showWarnings = FALSE, 
             recursive = TRUE)
  
  dir.create(ping_files_dir, 
             showWarnings = FALSE, 
             recursive = TRUE)
  
  # Run queries
  survey_definition_id <- c(98, 143, 47, 52)[match(region, c("EBS", "NBS", "GOA", "AI"))]
  survey_region <- c('BS', 'BS', 'GOA', 'AI')[match(region, c("EBS", "NBS", "GOA", "AI"))]
  survey_year <- floor(cruise/100)
  
  # Acceptable range for net spread values
  if(is.null(width_range)) {
    width_range <- switch(survey_region,
                          'GOA' = c(8, 22),
                          'AI' = c(8, 22),
                          'BS' = c(10, 22))
  }
  
  if(is.null(gear_codes)) {
    gear_codes <- switch(survey_region,
                         'GOA' = 172,
                         'AI' = 172,
                         'BS' = 44)
  }
  
  if(is.null(haul_types)) {
    haul_types <- 3
  }

  # Event codes used for effort calculations
  start_event_code <- switch(survey_region,
                             'GOA' = 4,
                             'AI' = 4,
                             'BS' = 3)
  
  end_event_code <- 7
  
  cat("setup_sor_directory: Retreiving data from racebase and race_data\n")
  # Get spread measurements from race_data
  edit_sgp <- RODBC::sqlQuery(
    channel = channel, 
    query = paste0(" select * from race_data.v_extract_edit_sgp where cruise in (", 
                   cruise,
                   ") and region = '", 
                   survey_region, 
                   "';")
  )
  
  stopifnot("setup_sor_directory: No spread data in RACE_DATA.V_EXTRACT_EDIT_SGP for this vessel/cruise " = nrow(edit_sgp) > 0)
  
  # Get haul events from race_data; TIME_FLAG = EVENT
  edit_sgt <- RODBC::sqlQuery(
    channel = channel, 
    query = paste0("select * from race_data.v_extract_edit_sgt where cruise in (", 
                   cruise, 
                   ") and region = '", 
                   survey_region, "'")
    )
  
  stopifnot("setup_sor_directory: No event data in RACE_DATA.V_EXTRACT_EDIT_SGT for this vessel/cruise " = nrow(edit_sgt) > 0)
  
  # Get Calypso's mean height calculations
  edit_height <- RODBC::sqlQuery(channel, 
                                    query = paste0("select * from race_data.edit_hauls where cruise_id = ", 
                                                   cruise_idnum))
  
  # Speed, net number, and total catch
  speed_gear_df <- RODBC::sqlQuery(channel, 
                                   query = 
                                     paste0("select c.vessel_id vessel, c.cruise, h.haul, 
                                       h.haul_id, m.edit_speed_ob_fb speed, 
                                       h.edit_bottom_depth bottom_depth, h.performance, 
                                       h.edit_wire_out, h.net_number, h.haul_type 
                                      from 
                                      race_data.cruises c, 
                                      race_data.edit_hauls h, 
                                      race_data.edit_haul_measurements m  
                                      where 
                                           c.cruise_id = ",
                                            cruise_idnum,
                                            " and h.haul_id = m.haul_id
                                     and c.cruise_id = h.cruise_id 
                                     and h.haul_type in (", paste(haul_types, collapse = ", "), ")",
                                     "and h.gear in (", paste(gear_codes, collapse = ", "), ")")
  ) |>
    dplyr::mutate(invscope = 1/EDIT_WIRE_OUT,
                  scope_ratio = EDIT_WIRE_OUT/BOTTOM_DEPTH,
                  BOTTOM_DEPTH = dplyr::if_else(BOTTOM_DEPTH == 0, NA, BOTTOM_DEPTH))
  
  total_catch_df <- RODBC::sqlQuery(channel, 
                                    query = 
                                      paste0("select h.haul_id, c.vessel_id vessel, c.cruise,
                                      h.haul, sum(total_weight_in_haul) total_weight 
                                        from 
                                        race_data.cruises c, 
                                        race_data.edit_hauls h, 
                                        race_data.edit_catch_species csp,
                                        race_data.edit_catch_samples csa 
                                        where 
                                        c.cruise_id = ", cruise_idnum, 
                                             " and csa.haul_id = h.haul_id 
                                        and csa.catch_sample_id = csp.catch_sample_id
                                        and c.cruise_id = h.cruise_id 
                                        group by h.haul_id, c.vessel_id, c.cruise, h.haul, c.cruise_id 
                                        order by c.vessel_id, h.haul")
  )
  
  unique_cvh <- dplyr::select(speed_gear_df, CRUISE, VESSEL, HAUL) |>
    unique()
  
  total_catch_df <- total_catch_df |>
    dplyr::inner_join(unique_cvh)
  
  edit_height <- edit_height |>
    dplyr::inner_join(unique_cvh)
  
  edit_sgt <- edit_sgt |>
    dplyr::inner_join(unique_cvh)
  
  speed_net_df <- dplyr::full_join(speed_gear_df, 
                                   total_catch_df, 
                                   by = c("VESSEL", "CRUISE", "HAUL", "HAUL_ID")) |>
    janitor::clean_names()
  
  stopifnot("setup_sor_directory: No height data in RACE_DATA.EDIT_HAULS for this vessel/cruise " = nrow(edit_height) > 0)
  
  cat("setup_sor_directory: Writing racebase data to rds files in ", output_dir, "\n")
  
  # Save files to .rds
  saveRDS(edit_sgp, 
          file = here::here(output_dir, paste0("edit_sgp_", cruise, "_", vessel,  ".rds")))
  saveRDS(edit_sgt, 
          file = here::here(output_dir, paste0("edit_sgt_", cruise, "_", vessel,  ".rds")))
  saveRDS(edit_height, 
          file = here::here(output_dir, paste0("edit_height_", cruise, "_", vessel,  ".rds")))
  saveRDS(speed_net_df, 
          file = here::here(output_dir, paste0("edit_haul_", cruise, "_", vessel,  ".rds")))
  
  event_dat <- edit_sgt |> 
    janitor::clean_names() |> 
    dplyr::rename(event = time_flag) |> 
    dplyr::select(cruise, vessel, haul, date_time, event)
  
  haul_dat <- edit_sgp |> 
    janitor::clean_names() |> 
    dplyr::rename(measurement_value = value) |> 
    dplyr::select(cruise, vessel, haul, date_time, cabinet_sensor_flag, measurement_value, datum_code)
  
  height_dat <- edit_height |> 
    janitor::clean_names() |> 
    dplyr::filter(cruise_id == cruise_idnum) |>
    dplyr::select(cruise_id, haul, haul_id, edit_net_height, edit_net_height_units, 
                  net_height_method, net_height_pings, net_height_standard_deviation,
                  edit_wire_out, edit_wire_out_units, wire_out_method) |> 
    dplyr::mutate(cruise = cruise,
                  vessel = vessel,
                  edit_wire_out_FM = round(dplyr::if_else(edit_wire_out_units == "FT", 
                                                   edit_wire_out/6, 
                                                   as.numeric(edit_wire_out)), 0),
                  edit_wire_out_units_FM = dplyr::if_else(edit_wire_out_units == "FT", "FM", "FM")) |> 
    dplyr::mutate(invscope = 1/edit_wire_out)
  
  edit_hauls_table_raw <- edit_height |> 
    janitor::clean_names()
  
  unique_hauls_df <- dplyr::distinct(event_dat, vessel, cruise, haul)
  
  height_df <- data.frame()
  
  for(jj in 1:nrow(unique_hauls_df)) {
    
    haul_events_dat <- event_dat |> 
      dplyr::filter(vessel == unique_hauls_df$vessel[jj],
                    cruise == unique_hauls_df$cruise[jj],
                    haul == unique_hauls_df$haul[jj])
    
    sor_data <- haul_dat |> 
      dplyr::filter(vessel == unique_hauls_df$vessel[jj],
                    cruise == unique_hauls_df$cruise[jj],
                    haul == unique_hauls_df$haul[jj],
                    measurement_value >= min(width_range),
                    measurement_value <= max(width_range)) |>
      dplyr::full_join(haul_events_dat, by = c("cruise", "vessel", "haul", "date_time")) |> 
      dplyr::arrange(date_time) |> 
      tibble::add_column(start = NA, end = NA)
    
    sel_haul_dat <- speed_net_df |> 
      dplyr::filter(vessel == unique_hauls_df$vessel[jj],
                    cruise == unique_hauls_df$cruise[jj],
                    haul == unique_hauls_df$haul[jj])
    
    spread_pings <- get_pings2(data = sor_data,
                               start_event_code = start_event_code,
                               end_event_code = end_event_code) |>
      dplyr::select(-start, -end)
    
    if(!(nrow(spread_pings) >= 1)) {
      spread_pings <- NULL
    }
    
    height_pings <- height_dat |> 
      dplyr::filter(vessel == unique_hauls_df$vessel[jj],
                    cruise == unique_hauls_df$cruise[jj],
                    haul == unique_hauls_df$haul[jj])
    
    if(!(nrow(height_pings) >= 1)) {
      
      height_pings <- NULL
      
    } else {
      
      height_df <- dplyr::bind_rows(height_df, height_pings)
      
    }
    
    cat("setup_sor_directory: Writing ping data to ", 
            paste0(ping_files_dir, 
                   "/", 
                   paste(unique_hauls_df[jj,], collapse = "_"), 
                   "_pings.rds\n"))
    saveRDS(object = list(spread = spread_pings,
                          height = height_pings,
                          events = haul_events_dat,
                          haul = cbind(sel_haul_dat, region, survey, cruise_idnum),
                          processing_date = Sys.time()),
            file = paste0(ping_files_dir, "/", paste(unique_hauls_df[jj,], collapse = "_"), "_pings.rds")
    )
  }
  
  saveRDS(object = height_df, 
          file = paste0(output_dir, "/", "HEIGHT_", region, "_", cruise, "_", vessel, ".rds"))
  
}
