#' Get individual bottom contacts
#'
#' A function extracting BCS data from RACEBASE.
#' 
#' @param channel Optional. An RODBC class ODBC connection
#' @import RODBC getPass
#' @export

get_bottom_contacts <- function(channel = NULL) {
  
  dir.create(here::here("data"))
  
  channel <- get_connected(schema = "AFSC")
  
  bc <- RODBC::sqlQuery(channel = channel,
                        query = "SELECT b.DATE_TIME, h.BOTTOM_CONTACT_HEADER_ID, b.X_AXIS, b.Y_AXIS, b.Z_AXIS 
                        FROM 
                        RACE_DATA.BOTTOM_CONTACTS b, RACE_DATA.BOTTOM_CONTACT_HEADERS h, RACE_DATA.HAULS u, RACE_DATA.CRUISES c, 
RACE_DATA.SURVEYS s
where
h.BOTTOM_CONTACT_HEADER_ID = b.BOTTOM_CONTACT_HEADER_ID
AND h.HAUL_ID = u.HAUL_ID
AND c.CRUISE_ID = u.CRUISE_ID
AND s.SURVEY_ID = c.SURVEY_ID
and b.DATUM_CODE in (0, 1, 11)
AND s.survey_definition_ID in (98, 143)
AND s.YEAR >= 2010
AND u.PERFORMANCE >= 0
and X_AXIS IS NOT NULL
ORDER BY b.BOTTOM_CONTACT_HEADER_ID, b.DATE_TIME ASC
")
  
  lubridate::force_tz(bc$DATE_TIME, tz = "America/Anchorage")
  
  saveRDS(object = bc, file = here::here("data", "contact_dat.rds"))
  
  output <- bc
  
  
  return(output)
  
}
