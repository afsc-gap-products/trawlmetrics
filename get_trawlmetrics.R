#' Get net mensuration data and summary statistics
#' 
#' @param survey Survey names as a character vector. Options are: "EBS", "NBS", "GOA", "AI", "SLOPE", "CHUKCHI"
#' @param year Survey year as a numeric vector.
#' @param select_haul_types Numeric vector of valid haul types. Default: c(3, 13, 20)
#' @param save_rds Should output be written to an RDS file? Options are TRUE, FALSE, or a valid filepath to the location where an .rds file should be saved. 
#' @param channel RODBC connection.
#' @export

get_trawlmetrics <- function(survey, year, select_haul_types = c(3, 13, 20), save_rds = FALSE, channel = NULL) {
  
  survey <- toupper(survey)
  
  survey_id <- c(98, 143, 47, 52, 78, 6)[match(survey, c("EBS", "NBS", "GOA", "AI", "SLOPE", "CHUKCHI"))]
  
  # Short names for trawl gear
  gear_desc_df <- data.frame(GEAR = c(44, 172),
                             SHORT_NAME = c("83-112", "Poly Nor 'eastern"))
  
  channel <- trawlmetrics:::get_connected(schema = "AFSC")
  
  trawl_data <- RODBC::sqlQuery(channel = channel,
                                query =
                                  paste("select a.hauljoin, a.vessel, a.cruise, a.haul, a.net_measured, a.net_height, a.net_width, a.wire_length, a.bottom_depth, a.performance, a.gear, a.accessories,
a.stationid, a.start_time, d.net_number, d.footrope_number, d.autotrawl_method, d.starboard_door_number, d.port_door_number, d.haul_type, e.description gear_description, f.description performance_description
from
racebase.haul a, 
race_data.cruises b, 
race_data.surveys c, 
race_data.hauls d, 
race_data.gear_codes e, 
racebase.performance f
where c.survey_definition_id in (", paste(survey_id, collapse = ","), ")",
                                        "and b.survey_id = c.survey_id
and a.cruisejoin = b.racebase_cruisejoin
and d.cruise_id = b.cruise_id
and a.haul = d.haul
and e.gear_code = a.gear
and a.performance = f.performance")) |>
    dplyr::inner_join(gear_desc_df) |>
    dplyr::mutate(YEAR = floor(CRUISE/100),
                  SCOPE_RATIO = WIRE_LENGTH/BOTTOM_DEPTH) |>
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
    dplyr::filter(NET_MEASURED == "Y") |>
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
      saveRDS(object = output, file = here::here("output", paste0("trawlmetrics_", survey, year, ".rds")))
    }
  }
  
  return(output)
  
}
