#' Get bottom contact summary stats
#'
#' A function that builds a dataframe from BCS contacts.
#' 
#' @param channel Optional. An RODBC class ODBC connection
#' @param contact_dat Bottom contact dataframe output by get_bottom_contacts()
#' @export

get_bottom_data <- function(channel = NULL, contact_dat = contact_dat) {
  
  dir.create(here::here("data"))
  
  channel <- trawlmetrics:::get_connected(schema = "AFSC")
  
  onbottom <- RODBC::sqlQuery(channel = channel,
                              query = "SELECT 
e.HAUL_ID, e.DATE_TIME 
from
RACE_DATA.EVENTS e, RACE_DATA.HAULS h, RACE_DATA.CRUISES c, RACE_DATA.SURVEYS s
where
s.SURVEY_ID = c.SURVEY_ID
AND c.CRUISE_ID = h.CRUISE_ID
AND h.HAUL_ID = e.HAUL_ID
AND e.EVENT_TYPE_ID = 3
AND s.survey_definition_ID in (98, 143)
AND s.YEAR >= 2010
ORDER BY e.HAUL_ID, e.EVENT_TYPE_ID")
  
  offbottom <- RODBC::sqlQuery(channel = channel,
                               query = "SELECT 
e.HAUL_ID, e.DATE_TIME
from
RACE_DATA.EVENTS e, RACE_DATA.HAULS h, RACE_DATA.CRUISES c, RACE_DATA.SURVEYS s
where
s.SURVEY_ID = c.SURVEY_ID
AND c.CRUISE_ID = h.CRUISE_ID
AND h.HAUL_ID = e.HAUL_ID
AND e.EVENT_TYPE_ID = 7
AND s.survey_definition_ID in (98, 143)
AND s.YEAR >= 2010
ORDER BY e.HAUL_ID, e.EVENT_TYPE_ID")
  
  bottom_time <- merge(onbottom,offbottom, by ="HAUL_ID")
  
  headers <- RODBC::sqlQuery(channel = channel,
                             query = "SELECT b.BOTTOM_CONTACT_HEADER_ID, b.HAUL_ID
from
RACE_DATA.BOTTOM_CONTACT_HEADERS b
WHERE b.HAUL_ID > 6119
ORDER BY BOTTOM_CONTACT_HEADER_ID ASC")
  
  
  envdat <- RODBC::sqlQuery(channel = channel,
                            query = "SELECT s.YEAR, u.HAUL_ID, u.NET_NUMBER, u.STRATUM, u.STATION
from
RACE_DATA.HAULS u, RACE_DATA.CRUISES c, RACE_DATA.SURVEYS s
where c.CRUISE_ID = u.CRUISE_ID
AND s.SURVEY_ID = c.SURVEY_ID
AND s.survey_definition_ID in (98, 143)
AND s.YEAR >= 2010")
  
  dat <- merge(headers, bottom_time, by = "HAUL_ID")
  
  names(dat)[names(dat) == "DATE_TIME.x"] <- "ONBOTTOM"
  names(dat)[names(dat) == "DATE_TIME.y"] <- "OFFBOTTOM"
  
  lubridate::force_tz(dat$ONBOTTOM, tz = "America/Anchorage")
  force_tz(dat$OFFBOTTOM, tz = "America/Anchorage")
  
  allcontacts <- merge(dat, contact_dat, by = "BOTTOM_CONTACT_HEADER_ID")
  
  BCS_data <- allcontacts |>
    dplyr::filter(DATE_TIME >= ONBOTTOM, DATE_TIME <= OFFBOTTOM)
  
  xstat <- BCS_data |> group_by(HAUL_ID) |> dplyr::summarize(min = min(X_AXIS),
                                                        q1 = quantile(X_AXIS, 0.25),
                                                        median = median(X_AXIS),
                                                        mean = mean(X_AXIS),
                                                        q3 = quantile(X_AXIS, 0.75),
                                                        max = max(X_AXIS))
  
  ystat <- BCS_data |> group_by(HAUL_ID) |> dplyr::summarize(min = min(Y_AXIS),
                                                        q1 = quantile(Y_AXIS, 0.25),
                                                        median = median(Y_AXIS),
                                                        mean = mean(Y_AXIS),
                                                        q3 = quantile(Y_AXIS, 0.75),
                                                        max = max(Y_AXIS))
  
  zstat <- BCS_data |> group_by(HAUL_ID) |> dplyr::summarize(min = min(Z_AXIS),
                                                        q1 = quantile(Z_AXIS, 0.25),
                                                        median = median(Z_AXIS),
                                                        mean = mean(Z_AXIS),
                                                        q3 = quantile(Z_AXIS, 0.75),
                                                        max = max(Z_AXIS))
  
  zstat <- zstat |> dplyr::rename(min.z = min, q1.z = q1, median.z = median, mean.z = mean, q3.z = q3, max.z = max)
  
  stats <- merge(xstat, ystat, by = "HAUL_ID")
  stats <- merge (stats, zstat, by = "HAUL_ID")
  
  full <- merge(envdat, stats, by = "HAUL_ID")
  
  full$YEAR <- as.factor(full$YEAR)
  full$HAUL_ID <- as.factor(full$HAUL_ID)
  full$NET_NUMBER <- as.factor(full$NET_NUMBER)
  full$STRATUM <- as.factor(full$STRATUM)
  full$STATION <- as.factor(full$STATION)
  
  full$NET_YEAR <- paste(full$YEAR, full$NET_NUMBER, sep = "-")
  
  full <- full %>%  filter(!is.na(NET_NUMBER))
  full <- full %>%  filter(!is.na(STATION))
  full <- full %>%  filter(!is.na(STATION))
  full <- full %>%  filter(between(median.x, -2.0, 2.0))
  
  saveRDS(object = full, file = here::here("data", "BCS_data.rds"))
  
  output <- full
  
  return(output)
  
}