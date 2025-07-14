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
      C.COUNT,
      H.NET_MEASURED,
      H.HAUL,
      H.DEPTH_GEAR_M,
      H.DEPTH_M,
      H.NET_WIDTH_M,
      H.NET_HEIGHT_M,
      H.LONGITUDE_DD_START AS START_LONGITUDE,
      H.LATITUDE_DD_START AS START_LATITUDE,
      H.LONGITUDE_DD_END AS END_LONGITUDE,
      H.LATITUDE_DD_END AS END_LATITUDE,
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

total_catch <- 
  gp_catch |>
  dplyr::group_by(HAULJOIN) |>
  dplyr::summarise(TOTAL_WEIGHT_KG = sum(WEIGHT_KG, na.rm = TRUE),
                   TOTAL_COUNT = sum(COUNT, na.rm = TRUE),
                   .groups = 'keep') |>
  dplyr::ungroup()

bts_geom <- gp_catch |>
  dplyr::select(-SPECIES_CODE, -WEIGHT_KG) |>
  unique() |>
  dplyr::inner_join(total_catch) |>
  dplyr::mutate(NET_MEASURED = NET_MEASURED == 1) |>
  dplyr::mutate(YEAR = floor(CRUISE/100))

bts_geom$GEAR_NAME <- "83-112"
bts_geom$GEAR_NAME[bts_geom$GEAR == 160] <- "PNE" 
bts_geom$GEAR_NAME[bts_geom$GEAR == 172] <- "PNE"

bts_geom <- 
  bts_geom |> 
  dplyr::select(
    HAULJOIN, 
    SURVEY_DEFINITION_ID, 
    SURVEY_ABBV, 
    YEAR, 
    CRUISE, 
    VESSEL_ID, 
    HAUL, 
    STATION, 
    START_LONGITUDE,
    START_LATITUDE,
    END_LONGITUDE,
    END_LATITUDE,
    GEAR_NAME, 
    GEAR, 
    ACCESSORIES, 
    WIRE_LENGTH_M, 
    NET_MEASURED, 
    NET_WIDTH_M, 
    NET_HEIGHT_M, 
    DISTANCE_FISHED_KM, 
    DURATION_HR, 
    DEPTH_M, 
    TOTAL_WEIGHT_KG,
    TOTAL_COUNT
  ) |>
  unique()

# Load data from flume tank experiments conducted at the Marine Institute (St. John's, Newfoundland) in January 2025
# flume_tank <- read.xlsx(file = here::here("analysis", "flume_tank", "data", "flume_tank_data.xlsx"), 
#                         sheetName = 'data') |>
#   dplyr::select(-date_time_nst)

save(bts_geom, file = here::here("data", "bts_geom.rda"), compress = "xz")
# save(flume_tank, file = here::here("data", "flume_tank.rda"), compress = "xz")

