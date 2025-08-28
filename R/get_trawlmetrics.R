#' Get net mensuration data and summary statistics
#' 
#' @param survey Survey names as a character vector. Options are: "EBS", "NBS", "GOA", "AI", "SLOPE", "CHUKCHI"
#' @param year Survey year as a numeric vector.
#' @param select_haul_types Numeric vector of valid haul types. Default: c(3, 13, 20)
#' @param select_gear_code Optional. Numeric vector of valid RACE gear codes. If not specified, selects codes 44 and 172.
#' @param save_rds Should output be written to an RDS file? Options are TRUE, FALSE, or a valid filepath to the location where an .rds file should be saved. 
#' @param channel RODBC connection.
#' @import RODBC getPass
#' @export

get_trawlmetrics <- function(survey, 
                             year, 
                             select_haul_types = c(3, 13, 20), 
                             select_gear_code = NULL,
                             save_rds = FALSE, 
                             channel = NULL
                             ) {
  
  survey <- toupper(survey)
  
  survey_id <- c(98, 143, 47, 52, 78, 6)[match(survey, c("EBS", "NBS", "GOA", "AI", "SLOPE", "CHUKCHI"))]
  
  # Short names for trawl gear
  gear_desc_df <- data.frame(GEAR = c(44, 172),
                             SHORT_NAME = c("83-112", "Poly Nor 'eastern"))
  
  if(is.null(select_gear_code)) {
    select_gear_code <- c(44, 172)
  }
  
  channel <- get_connected(channel = channel)
  
  trawl_data <- RODBC::sqlQuery(channel = channel,
                                query =
                                  paste("select h.hauljoin, rdh.haul_id, h.vessel, h.cruise, h.haul, 
                                  h.net_measured, h.net_height, rdh.net_height_standard_deviation,
                                  rdh.net_height_pings, rdh.net_height_method, h.net_width, 
                                  rdh.net_spread_standard_deviation, rdh.net_spread_pings, 
                                  rdh.net_spread_method, h.wire_length, 
                                  h.distance_fished, h.duration, h.bottom_depth, h.performance, 
                                  h.gear, h.accessories, h.stationid, h.start_time, rdh.net_number, 
                                  rdh.footrope_number, rdh.autotrawl_method, rdh.starboard_door_number, 
                                  rdh.port_door_number, rdh.haul_type, gc.description gear_description, 
                                  p.description performance_description 
                                  from 
                                  racebase.haul h, race_data.cruises c, race_data.surveys s, 
                                  race_data.hauls rdh, race_data.gear_codes gc, racebase.performance p 
                                        where s.survey_definition_id in (", 
                                        paste(survey_id, collapse = ","), ")",
                                        "and h.gear in (", paste(select_gear_code, collapse = ","), ")",
                                        "and s.survey_id = c.survey_id 
                                        and h.cruise = c.cruise
                                        and h.vessel = c.vessel_id
                                        and rdh.cruise_id = c.cruise_id 
                                        and h.haul = rdh.haul 
                                        and gc.gear_code = h.gear 
                                        and h.performance = p.performance"))

  
  if(is(trawl_data, "character")) {
    stop("get_trawlmetrics: Trawl data query failed.")
  }
  
  
  trawl_data <- trawl_data |>
    dplyr::inner_join(gear_desc_df) |>
    dplyr::mutate(YEAR = floor(CRUISE/100),
                  SCOPE_RATIO = WIRE_LENGTH/BOTTOM_DEPTH,
                  TOW_SPEED_KN = DISTANCE_FISHED/DURATION/1.852) |>
    dplyr::filter(HAUL_TYPE %in% select_haul_types,
                  !is.na(NET_MEASURED))
  

  trawl_data$TRAWL_ID <- paste(trawl_data$YEAR, trawl_data$VESSEL, trawl_data$NET_NUMBER, sep = "_")
  
  
  # Scaled height and width
  net_height_scaled <- scale(trawl_data$NET_HEIGHT)
  net_width_scaled <- scale(trawl_data$NET_WIDTH)
  net_scope_ratio <- scale(trawl_data$SCOPE_RATIO)
  
  trawl_data$SCALED_NET_HEIGHT <- net_height_scaled
  trawl_data$SCALED_NET_WIDTH <- net_width_scaled
  trawl_data$SCALED_SCOPE_RATIO <- net_scope_ratio
  
  trawl_mat <- cbind(net_height_scaled[,1],
                     net_width_scaled[,1],
                     net_scope_ratio[,1])
  
  
  # Summarize height and width by trawl
  trawlmetrics_by_year <- trawl_data |>
    dplyr::group_by(YEAR, VESSEL, NET_NUMBER, NET_MEASURED) |>
    dplyr::summarise(MEAN_NET_HEIGHT = mean(NET_HEIGHT, na.rm = TRUE),
                     MIN_NET_HEIGHT = min(NET_HEIGHT, na.rm = TRUE),
                     MAX_NET_HEIGHT = max(NET_HEIGHT, na.rm = TRUE),
                     Q025_NET_HEIGHT = quantile(NET_HEIGHT, probs = 0.025, na.rm = TRUE),
                     Q975_NET_HEIGHT = quantile(NET_HEIGHT, probs = 0.975, na.rm = TRUE),
                     Q25_NET_HEIGHT = quantile(NET_HEIGHT, probs = 0.25, na.rm = TRUE),
                     Q75_NET_HEIGHT = quantile(NET_HEIGHT, probs = 0.75, na.rm = TRUE),
                     SD_NET_HEIGHT = sd(NET_HEIGHT, na.rm = TRUE),
                     MEAN_NET_WIDTH = mean(NET_WIDTH, na.rm = TRUE),
                     MIN_NET_WIDTH = min(NET_WIDTH, na.rm = TRUE),
                     MAX_NET_WIDTH = max(NET_WIDTH, na.rm = TRUE),
                     Q025_NET_WIDTH = quantile(NET_WIDTH, probs = 0.025, na.rm = TRUE),
                     Q975_NET_WIDTH = quantile(NET_WIDTH, probs = 0.975, na.rm = TRUE),
                     Q25_NET_WIDTH = quantile(NET_WIDTH, probs = 0.25, na.rm = TRUE),
                     Q75_NET_WIDTH = quantile(NET_WIDTH, probs = 0.75, na.rm = TRUE),
                     SD_NET_WIDTH = sd(NET_WIDTH, na.rm = TRUE)) |>
    dplyr::inner_join(trawl_data |>
                        dplyr::group_by(YEAR, VESSEL, NET_NUMBER) |>
                        dplyr::summarise(N_HAULS = dplyr::n(),
                                         N_GOOD = sum(PERFORMANCE >= 0, na.rm = TRUE),
                                         N_BAD = sum(PERFORMANCE < 0, na.rm = TRUE))) |>
    dplyr::filter(NET_MEASURED == "Y")
  
  
  # Summarize height and width for all nets and all years
  trawlmetrics_average <- trawl_data |>
    dplyr::filter(NET_MEASURED == "Y", PERFORMANCE >= 0) |>
    dplyr::summarise(MEAN_NET_HEIGHT = mean(NET_HEIGHT, na.rm = TRUE),
                     MIN_NET_HEIGHT = min(NET_HEIGHT, na.rm = TRUE),
                     MAX_NET_HEIGHT = max(NET_HEIGHT, na.rm = TRUE),
                     Q025_NET_HEIGHT = quantile(NET_HEIGHT, probs = 0.025, na.rm = TRUE),
                     Q975_NET_HEIGHT = quantile(NET_HEIGHT, probs = 0.975, na.rm = TRUE),
                     Q25_NET_HEIGHT = quantile(NET_HEIGHT, probs = 0.25, na.rm = TRUE),
                     Q75_NET_HEIGHT = quantile(NET_HEIGHT, probs = 0.75, na.rm = TRUE),
                     SD_NET_HEIGHT = sd(NET_HEIGHT, na.rm = TRUE),
                     MEAN_NET_WIDTH = mean(NET_WIDTH, na.rm = TRUE),
                     MIN_NET_WIDTH = min(NET_WIDTH, na.rm = TRUE),
                     MAX_NET_WIDTH = max(NET_WIDTH, na.rm = TRUE),
                     Q025_NET_WIDTH = quantile(NET_WIDTH, probs = 0.025, na.rm = TRUE),
                     Q975_NET_WIDTH = quantile(NET_WIDTH, probs = 0.975, na.rm = TRUE),
                     Q25_NET_WIDTH = quantile(NET_WIDTH, probs = 0.25, na.rm = TRUE),
                     Q75_NET_WIDTH = quantile(NET_WIDTH, probs = 0.75, na.rm = TRUE),
                     SD_NET_WIDTH = sd(NET_WIDTH, na.rm = TRUE),
                     N_HAULS = dplyr::n()) |>
    dplyr::mutate(NET_NUMBER = "All nets/years")
  
  # Data frame for the year
  trawlmetrics_survey <- trawlmetrics_by_year |>
    dplyr::filter(YEAR == year)
  
  nets_used_survey <- as.character(sort(unique(trawlmetrics_survey$NET_NUMBER)))
  
  plot_data <- trawlmetrics_survey |>
    dplyr::mutate(NET_NUMBER = as.character(NET_NUMBER)) |>
    dplyr::bind_rows(trawlmetrics_average) |>
    dplyr::mutate(NET_NUMBER = factor(NET_NUMBER, 
                                      levels = c(nets_used_survey, "All nets/years")))
  
  plot_data$W25 <- trawlmetrics_average$Q25_NET_WIDTH
  plot_data$W75 <- trawlmetrics_average$Q75_NET_WIDTH
  plot_data$H25 <- trawlmetrics_average$Q25_NET_HEIGHT
  plot_data$H75 <- trawlmetrics_average$Q75_NET_HEIGHT
  
  output <- list(
    year = year,
    survey = survey,
    nets_used_survey = nets_used_survey,
    trawlmetrics_survey = trawlmetrics_survey,
    trawlmetrics_by_year = trawlmetrics_by_year,
    trawlmetrics_average = trawlmetrics_average,
    trawl_data = trawl_data,
    plot_data = plot_data
  )
  
  if(is.character(save_rds)) {
    dir.create(dirname(save_rds))
    saveRDS(object = output, file = save_rds)
  } else {
    if(save_rds) {
      dir.create(here::here("output"), showWarnings = FALSE)
      saveRDS(object = output, 
              file = here::here("output", paste0("trawlmetrics_", survey, year, ".rds")))
    }
  }
  
  return(output)
  
}
