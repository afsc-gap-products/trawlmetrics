# Add flume tank and historical survey data to the package
# Sean Rohan
# January 3, 2025

library(trawlmetrics)
library(xlsx)
library(ggthemes)

channel <- trawlmetrics::get_connected(schema = "AFSC")

survey_abbv <- data.frame(SURVEY_ABBV = c("AI", "GOA", "EBS", "NBS", "BSS"),
                          SURVEY_DEFINITION_ID = c(52, 47, 98, 143, 78))

gp_catch <- RODBC::sqlQuery(
  channel = channel,
  query = 
    "SELECT 
      C.HAULJOIN,
      CR.VESSEL_ID,
      CR.SURVEY_DEFINITION_ID,
      CR.CRUISE,
      CR.SURVEY_NAME,
      C.SPECIES_CODE, 
      C.WEIGHT_KG,
      H.NET_MEASURED,
      H.HAUL,
      H.DEPTH_GEAR_M,
      H.DEPTH_M,
      H.NET_WIDTH_M,
      H.NET_HEIGHT_M,
      H.DISTANCE_FISHED_KM,
      H.DURATION_HR,
      H.STATION,
      H.WIRE_LENGTH_M,
      H.GEAR,
      H.ACCESSORIES
    FROM 
      GAP_PRODUCTS.AKFIN_CATCH C,
      GAP_PRODUCTS.AKFIN_HAUL H,
      GAP_PRODUCTS.AKFIN_CRUISE CR
    WHERE
      H.HAULJOIN = C.HAULJOIN
      AND CR.CRUISEJOIN = H.CRUISEJOIN
        "
) |>
  dplyr::inner_join(survey_abbv, by = 'SURVEY_DEFINITION_ID')

total_catch <- gp_catch |>
  dplyr::group_by(HAULJOIN) |>
  dplyr::summarise(TOTAL_WEIGHT_KG = sum(WEIGHT_KG, na.rm = TRUE),
                   .groups = 'keep') |>
  dplyr::ungroup()

bts_geom <- dplyr::inner_join(gp_catch, total_catch) |>
  dplyr::mutate(NET_MEASURED = NET_MEASURED == 1)

bts_geom$GEAR_NAME <- "83-112"
bts_geom$GEAR_NAME[bts_geom$GEAR == 160] <- "PNE" 
bts_geom$GEAR_NAME[bts_geom$GEAR == 172] <- "PNE"

# Load flume tank data
flume_tank <- read.xlsx(file = here::here("analysis", "flume_tank", "data", "flume_tank_data.xlsx"), 
                        sheetName = 'data')

save(bts_geom, flume_tank, file = here::here("R", "sysdata.rda"))
