library(trawlmetrics)

create_user = "ROHANS"
cruise_idnum = c(767, 768, 772, 773, 774, 775)
delete_existing = TRUE

channel <- trawlmetrics::get_connected(schema = "AFSC")

haul_data <-
  RODBC::sqlQuery(
    channel = channel,
    query = paste0("SELECT CR.CRUISE_ID,
H.HAUL_ID,
CR.CRUISE,
CR.VESSEL_ID AS VESSEL,
H.HAUL_TYPE,
H.HAUL,
H.NET_SPREAD AS EDIT_NET_SPREAD,
H.NET_SPREAD_PINGS,
H.NET_SPREAD_METHOD,
H.NET_SPREAD_STANDARD_DEVIATION,
H.DOOR_SPREAD AS EDIT_DOOR_SPREAD,
H.DOOR_SPREAD_METHOD,
H.DOOR_SPREAD_PINGS,
H.DOOR_SPREAD_STANDARD_DEVIATION,
H.NET_HEIGHT AS EDIT_NET_HEIGHT,
H.NET_HEIGHT_METHOD,
H.NET_HEIGHT_PINGS,
H.NET_HEIGHT_STANDARD_DEVIATION
FROM 
RACE_DATA.HAULS H,
RACE_DATA.CRUISES CR
WHERE
H.CRUISE_ID = CR.CRUISE_ID
AND CR.CRUISE_ID in (", paste(cruise_idnum, collapse = ","), ")
AND H.PERFORMANCE >= 0
"))

haul_data <- haul_data |>
  dplyr::mutate(CORRECTED_NET_SPREAD = ifelse(EDIT_NET_SPREAD > 1, (EDIT_NET_SPREAD-0.40046503)/0.935684155, EDIT_NET_SPREAD))

# Plot results
ggplot() +
  geom_point(
    data = haul_data,
    mapping = aes(x = HAUL, y = CORRECTED_NET_SPREAD-EDIT_NET_SPREAD)) +
  geom_hline(yintercept = 0, linetype = 2) +
  scale_y_continuous(name = "Corrected-Current spread (m)") +
  facet_grid(CRUISE~VESSEL) +
  theme_bw()

haul_data |>
  dplyr::filter(HAUL_TYPE == 3) |>
  dplyr::group_by(VESSEL, CRUISE) |>
  dplyr::summarise(CHANGE_M = mean(CORRECTED_NET_SPREAD-EDIT_NET_SPREAD, na.rm = TRUE),
                   CHANGE_PCT = 100*mean((CORRECTED_NET_SPREAD-EDIT_NET_SPREAD)/EDIT_NET_SPREAD, na.rm = TRUE))

# Make final values

final_values <-  
  haul_data |>
  dplyr::mutate(
    EDIT_NET_SPREAD = CORRECTED_NET_SPREAD,
    CREATE_USER = toupper(create_user),
    CREATE_DATE = Sys.time()
  ) |>
  dplyr::select(-HAUL_TYPE, -CORRECTED_NET_SPREAD)


# Clear existing data from RACE_DATA.EDIT_HAUL_IMPORT_SOR_UPDATES
if(delete_existing) {
  
  message("sor_save_results: Removing existing cruise data from RACE_DATA.EDIT_HAUL_IMPORT_SOR_UPDATES")
  
  RODBC::sqlQuery(channel = channel, 
                  query = paste0(" DELETE FROM RACE_DATA.EDIT_HAUL_IMPORT_SOR_UPDATES WHERE CRUISE_ID IN (", paste(cruise_idnum, collapse = ", "), ");"))
  
  # Append data to table  
  message("sor_save_results: Appending new data to RACE_DATA.EDIT_HAUL_IMPORT_SOR_UPDATES")
  
  RODBC::sqlSave(channel = channel, 
                 dat = final_values, 
                 tablename = "RACE_DATA.EDIT_HAUL_IMPORT_SOR_UPDATES",
                 append = TRUE, # need append = TRUE because of access permissions
                 rownames = FALSE, 
                 colnames = FALSE, 
                 verbose = FALSE,
                 safer = FALSE, 
                 addPK = FALSE, 
                 fast = TRUE, 
                 test = FALSE, 
                 nastring = NULL)
}

