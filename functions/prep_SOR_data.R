# Prep data for SOR function
# Created by: Caitlin Allen Akselrud
# Contact: caitlin.allen_akselrud@noaa.gov
# Created: 2022-07-21
# Modified: 2022-07-21

get_pings <- function(data, hauln, vesseln)
{
  # data = sor_data_test
  # hauln = 1
  # vesseln = vessel[1]
  
  data_sub <- data %>% 
    dplyr::mutate(haul_num = hauln, vessel_num = vesseln) 
  start_t <- data_sub %>% dplyr::filter(event == 3) %>% dplyr::select(date_time)
  end_t <- data_sub %>% dplyr::filter(event == 7) %>% dplyr::select(date_time)
  data_sub <- data_sub %>% 
    mutate(start = start_t$date_time, end = end_t$date_time) %>% 
    dplyr::filter(date_time >= start & date_time <= end) %>% 
    dplyr::filter(!is.na(measurement_value)) 
  return(data_sub)
}

get_pings2 <- function(data)
{
  data_sub <- data %>% 
    dplyr::select(-datum_code, -cabinet_sensor_flag, -measurement_value) %>% 
    distinct() 
  start_t <- data_sub %>% dplyr::filter(event == 3) %>% dplyr::select(date_time)
  end_t <- data_sub %>% dplyr::filter(event == 7) %>% dplyr::select(date_time)
  data_new <- data %>%
    mutate(start = start_t$date_time, end = end_t$date_time) %>%
    dplyr::filter(date_time >= start & date_time <= end) %>%
    dplyr::filter(!is.na(measurement_value))
  return(data_new)
}
