# Add flume tank and historical survey data to the package
# Sean Rohan
# January 3, 2025

library(trawlmetrics)
library(xlsx)
library(ggthemes)

# Historical geometry data from hauls where trawl geometry was measured
bts_geom <-  readRDS(file = here::here("analysis", "flume_tank", "data", "HEIGHT_SPREAD_EBS_NBS_GOA_AI.rds"))

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
  dplyr::group_by(HAULJOIN, 
                  VESSEL_ID, 
                  SURVEY_DEFINITION_ID, 
                  SURVEY_NAME,
                  SURVEY_ABBV,
                  HAUL, 
                  CRUISE,
                  DEPTH_GEAR_M, 
                  DEPTH_M, 
                  NET_MEASURED,
                  NET_WIDTH_M, 
                  NET_HEIGHT_M, 
                  DISTANCE_FISHED_KM, 
                  DURATION_HR, 
                  WIRE_LENGTH_M, 
                  GEAR,
                  ACCESSORIES) |>
  dplyr::summarise(TOTAL_WEIGHT_KG = sum(WEIGHT_KG),
                   .groups = 'keep')

bts_geom <- 
  total_catch |>
  dplyr::ungroup() |>
  dplyr::select(VESSEL_ID, CRUISE, HAUL, NET_MEASURED, TOTAL_WEIGHT_KG, SURVEY_ABBV) |> 
  dplyr::inner_join(bts_geom)

# Load flume tank data
flume_tank <- read.xlsx(file = here::here("analysis", "flume_tank", "data", "flume_tank_data.xlsx"), 
                        sheetName = 'data')

save(bts_geom, flume_tank, file = here::here("R", "sysdata.rda"))
