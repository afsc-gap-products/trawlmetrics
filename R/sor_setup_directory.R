#' Setup directory
#' 
#' Function to retrieve data for sequential outlier rejection from RACEBASE and split data by haul.
#' 
#' @param region Survey region as a 1L character vector (EBS or NBS)
#' @param cruise Cruise number as a numeric vector (e.g. 202202)
#' @param cruise_idnum Cruise ID number as a numeric vector (e.g. 757)
#' @param vessel vessel ID number as a numeric vector (e.g. 162 for Alaska Knight.
#' @param survey Survey name prefix to use in filename (e.g. NBS_2022)
#' @param channel Open RODBC channel. If NULL, function will prompt for user ID.
#' @param convert_marport_to_netmind Should Marport spread measurements be converted to Netmind spread using trawlmetric::marport_to_netmind()? 
#' @param skip_save_rds For testing and demo purposes. Should queried data be written to a directory.
#' @export


sor_setup_directory <- function(cruise, cruise_idnum, vessel, region, survey, channel = NULL, width_range = c(10, 22), convert_marport_to_netmind = TRUE, skip_save_rds = FALSE) {
  
  region <- toupper(region)
  
  if(is.null(width_range)) {
    width_range <- c(10, 22)
  }
  
  stopifnot("setup_sor_directory: Region must be 'EBS' or 'NBS'" = region %in% c("EBS", "NBS"))  
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
  survey_definition_id <- c(98, 143)[match(region, c("EBS", "NBS"))]
  survey_region <- c('BS', 'BS')[match(region, c("EBS", "NBS"))]
  survey_year <- floor(cruise/100)
  
  message("setup_sor_directory: Retreiving data from racebase")
  # Get spread measurements from race_data
  edit_sgp_df <- RODBC::sqlQuery(channel = channel, 
                              query = paste0(" select * from race_data.v_extract_edit_sgp where cruise in (", cruise,") and region = '", region, "';"))
  
  # Get haul events from race_data; TIME_FLAG = EVENT
  edit_sgt_df <- RODBC::sqlQuery(channel = channel, 
                              query = paste0(" select * from race_data.v_extract_edit_sgt where cruise in (", 
                                             cruise, 
                                             ") and region = '", 
                                             survey_region, "';"))
  
  # Get Calypso's mean height calculations
  edit_height_df <- RODBC::sqlQuery(channel, 
                                    query = paste0(" select * from race_data.edit_hauls where cruise_id = ", 
                                                   cruise_idnum, ";"))
  
  message("setup_sor_directory: Writing racebase data to rds files in ", output_dir)
  
  if(skip_save_rds) {
    # Save spread, height, and haul events to output_dir
    saveRDS(edit_sgp_df, file = paste0(here::here(output_dir, paste0("edit_sgp_", cruise, "_", vessel,  ".rds"))))
    saveRDS(edit_sgt_df, file = paste0(here::here(output_dir, paste0("edit_sgt_", cruise, "_", vessel,  ".rds"))))
    saveRDS(edit_height_df, file = paste0(here::here(output_dir, paste0("edit_height_", cruise, "_", vessel,  ".rds"))))
  }
    
    
    # Replace csv files with rds files for 2023
    message("setup_sor_directory: Reading rds files from ", output_dir)
    edit_sgp <- readRDS(file = paste0(here::here(output_dir, paste0("edit_sgp_", cruise, "_", vessel,  ".rds"))))
    edit_sgt <- readRDS(file = paste0(here::here(output_dir, paste0("edit_sgt_", cruise, "_", vessel,  ".rds"))))
    edit_height <- readRDS(file = paste0(here::here(output_dir, paste0("edit_height_", cruise, "_", vessel,  ".rds"))))
  
  # edit_sgp <- readr::read_csv(file = here::here(output_dir, paste0(survey, "_test_edit_sgp.csv")))
  # edit_sgt <- readr::read_csv(file = here::here(output_dir, paste0(survey, "_test_edit_sgt.csv")))
  # edit_height <- readr::read_csv(file = here::here(output_dir, paste0(survey, "_test_edit_height.csv")))
  
  event_dat <- edit_sgt %>% 
    dplyr::as_tibble() %>% 
    janitor::clean_names() %>% 
    dplyr::rename(event = time_flag) %>% 
    dplyr::select(cruise, vessel, haul, date_time, event)
  
  haul_dat <- edit_sgp %>% 
    dplyr::as_tibble() %>% 
    janitor::clean_names() %>% 
    dplyr::rename(measurement_value = value) %>% 
    dplyr::select(cruise, vessel, haul, date_time, cabinet_sensor_flag, measurement_value, datum_code) 
  
  height_dat <- edit_height %>% 
    dplyr::as_tibble() %>% 
    janitor::clean_names() %>% 
    dplyr::filter(cruise_id == cruise_idnum) %>%
    dplyr::select(cruise_id, haul, haul_id, edit_net_height, edit_net_height_units, 
                  net_height_method, net_height_pings, net_height_standard_deviation,
                  edit_wire_out, edit_wire_out_units, wire_out_method) %>% 
    dplyr::mutate(cruise = cruise,
                  vessel = vessel,
                  edit_wire_out_FM = round(if_else(edit_wire_out_units == "FT", edit_wire_out/6, as.numeric(edit_wire_out)),0),
                  edit_wire_out_units_FM = if_else(edit_wire_out_units == "FT", "FM", "FM")) %>% 
    dplyr::mutate(invscope = 1/edit_wire_out)
  
  edit_hauls_table_raw <- edit_height %>% 
    dplyr::as_tibble() %>% 
    janitor::clean_names()
  
  unique_hauls_df <- dplyr::distinct(event_dat, vessel, cruise, haul)
  
  height_df <- data.frame()
  
  for(jj in 1:nrow(unique_hauls_df)) {
    
    haul_events_dat <- event_dat %>% 
      dplyr::filter(vessel == unique_hauls_df$vessel[jj],
                    cruise == unique_hauls_df$cruise[jj],
                    haul == unique_hauls_df$haul[jj])
    
    sor_data <- haul_dat %>% 
      dplyr::filter(vessel == unique_hauls_df$vessel[jj],
                    cruise == unique_hauls_df$cruise[jj],
                    haul == unique_hauls_df$haul[jj]) %>%
      dplyr::filter(measurement_value >= min(width_range), # this is per trawl scope: net spread should be between 10 and 22 m
                    measurement_value <= max(width_range)) %>% 
      dplyr::full_join(haul_events_dat, by = c("cruise", "vessel", "haul", "date_time")) %>% 
      dplyr::arrange(date_time) %>% 
      tibble::add_column(start = NA, end = NA)
    
    spread_pings <- get_pings2(data = sor_data) %>%
      dplyr::select(-start, -end)
    
    if(!(nrow(spread_pings) >= 1)) {
      spread_pings <- NULL
    } else {
      if(convert_marport_to_netmind) {
        # Convert Marport spread to Netmind spread
        spread_pings$measurement_value <- marport_to_netmind(spread_pings$measurement_value)
      }
    }
    
    height_pings <- height_dat %>% 
      dplyr::filter(vessel == unique_hauls_df$vessel[jj],
                    cruise == unique_hauls_df$cruise[jj],
                    haul == unique_hauls_df$haul[jj])
    
    if(!(nrow(height_pings) >= 1)) {
      height_pings <- NULL
    } else {
      height_df <- dplyr::bind_rows(height_df, height_pings)
    }
    
    message("setup_sor_directory: Writing ping data to ", paste0(ping_files_dir, "/", paste(unique_hauls_df[jj,], collapse = "_"), "_pings.rds"))
    saveRDS(object = list(spread = spread_pings,
                          height = height_pings,
                          events = haul_events_dat,
                          haul = cbind(unique_hauls_df[jj,], region, survey, cruise_idnum),
                          processing_date = Sys.time()),
            file = paste0(ping_files_dir, "/", paste(unique_hauls_df[jj,], collapse = "_"), "_pings.rds")
    )
  }
  
  saveRDS(object = height_df, 
          file = paste0(output_dir, "/", "HEIGHT_", region, "_", cruise, "_", vessel, ".rds"))
  
}
