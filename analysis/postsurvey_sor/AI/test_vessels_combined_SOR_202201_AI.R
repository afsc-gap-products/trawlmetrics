# Test SOR with two vessels on 2022 Aleutian Islands hauls
# Sequential outlier rejection and fill missing spread and height
# Last update: May 11, 2024

library(trawlmetrics)

channel <- trawlmetrics::get_connected(schema = "AFSC")

# Settings -----------------------------------------------------------------------------------------
region = "AI"
survey = "AI_2022"
year = 2022
cruise = 202201
haul_types = 3
gear_codes = 172
width_range = c(8, 22)
convert_marport_to_netmind = FALSE # Only in the EBS
min_pings_for_sor = 50
min_height_pings = 50
fill_method = "goa" # There's a single method for the GOA and AI; see ?sor_fill_missing
create_user = "ROHANS"
delete_existing = FALSE # Set to TRUE to overwrite cruise data in RACE_DATA.EDIT_HAUL_IMPORT_SOR_UPDATES

cruise_idnum1 = 754
vessel1 = 148

cruise_idnum2 = 753
vessel2 = 176

vessel <- c(vessel1, vessel2)
cruise_idnum <- c(cruise_idnum1, cruise_idnum2)
vessel_comb <- paste(vessel, collapse = "_")


# Process 2022 AI Ocean Explorer and Alaska Provider -----------------------------------------------

# Retrieve haul and net mensuration data from race_data then write spread and height data from 
# individual hauls to the [subdirectory]: /output/{region}/{cruise}/{vessel}.
# - Height: [subdirectory]/ping_files_{survey}/HEIGHT_{region}_{cruise}_{vessel}.rds
# - Hauls: [subdirectory]/ping_files_{survey}/{cruise}_{vessel}_{haul}_pings.rds
sor_setup_directory(cruise = cruise,
                    cruise_idnum = cruise_idnum,
                    vessel = vessel,
                    region = region,
                    survey = survey,
                    haul_types = haul_types,
                    gear_codes = gear_codes,
                    channel = channel)

# Run sequential outlier rejection on rds files from each haul and write outputs to .rds files.
# Hauls w/ SOR: [subdirectory]/ping_files_{survey}/{cruise}_{vessel}_{haul}_sor.rds
sor_run(cruise = cruise,
        vessel = vessel,
        region = region,
        survey = survey,
        min_pings_for_sor = min_pings_for_sor,
        overwrite = TRUE)

# Plot results of sequential outlier rejection for visual inspection.
# Plots: [subdirectory]/ping_files_{survey}/SOR_graphics_{survey}/SOR_{cruise}_{vessel}_{haul}.png
sor_plot_results(cruise = cruise,
                 vessel = vessel,
                 region = region,
                 survey = survey)

# Fill in missing height and spread data
# Hauls w/ missing data filled: [subdirectory]/ping_files_{survey}/{cruise}_{vessel}_{haul}_final.rds
sor_fill_missing(height_paths = here::here("output", region, cruise, vessel_comb, 
                                           paste0("HEIGHT_", region, "_", cruise, "_", vessel_comb, ".rds")),
                 spread_paths = here::here("output", region, cruise, vessel_comb, 
                                           paste0("SPREAD_AFTER_SOR_", region, "_", cruise, "_", vessel_comb, ".rds")),
                 haul_path = here::here("output", region, cruise, vessel_comb, 
                                        paste0("edit_haul_", cruise, "_", vessel_comb, ".rds")),
                 rds_dir = here::here("output", region, cruise, vessel_comb, 
                                      paste0("PING_FILES_", region, "_", year)),
                 fill_method = fill_method,
                 convert_marport_to_netmind = convert_marport_to_netmind,
                 min_height_pings = min_height_pings)

# Update tables ------------------------------------------------------------------------------------
# Add updated data to Oracle and write output to .csv
sor_save_results(final_dir = here::here("output", region, cruise, vessel_comb, 
                                          paste0("PING_FILES_", region, "_", year)), 
                 create_user = create_user, 
                 survey = c(survey, survey), 
                 cruise_idnum = cruise_idnum,
                 channel = channel,
                 delete_existing = delete_existing
                 )


# Compare with final values ------------------------------------------------------------------------

comparison_data <- RODBC::sqlQuery(channel = channel, 
                                   query = paste0("
                                   select 
                                    rbh.vessel, 
                                    rbh.cruise, 
                                    rbh.haul, 
                                    rbh.net_height, 
                                    rbh.net_width, 
                                    rbh.net_measured, 
                                    rdh.net_spread_method,
                                    rdh.net_height_method,
                                    rdh.net_spread_pings,
                                    rdh.net_height_pings
                                   from 
                                    racebase.haul rbh, 
                                    race_data.hauls rdh, 
                                    race_data.cruises rdc
                                   where rbh.cruise = ", cruise, 
                                                  "and rdh.cruise_id = rdc.cruise_id
                                    and rbh.cruise = rdc.cruise
                                    and rdc.vessel_id = rbh.vessel
                                    and rbh.haul = rdh.haul
                                    and rbh.vessel in (", vessel1, ", ", vessel2, ")")
) |>
  dplyr::arrange(HAUL)

edit_data <- read.csv(file = here::here("output", 
                                        paste0("race_data_edit_hauls_table_", survey, ".csv"))) |> 
  dplyr::select(c("VESSEL", "CRUISE", "HAUL", "EDIT_NET_HEIGHT", "EDIT_NET_SPREAD",
                  "NET_SPREAD_METHOD", "NET_HEIGHT_METHOD", "NET_SPREAD_PINGS", "NET_HEIGHT_PINGS")) |>
  dplyr::rename(NEW_NET_SPREAD_METHOD = NET_SPREAD_METHOD,
                NEW_NET_HEIGHT_METHOD = NET_HEIGHT_METHOD,
                NEW_NET_SPREAD_PINGS = NET_SPREAD_PINGS,
                NEW_NET_HEIGHT_PINGS = NET_HEIGHT_PINGS) |>
  dplyr::inner_join(comparison_data) |>
  dplyr::mutate(DIFF_HEIGHT = NET_HEIGHT - EDIT_NET_HEIGHT,
                DIFF_WIDTH = NET_WIDTH - EDIT_NET_SPREAD,
                DIFF_WIDTH_PCT = (NET_WIDTH - EDIT_NET_SPREAD)/NET_WIDTH*100,
                DIFF_HEIGHT_PCT = (NET_HEIGHT - EDIT_NET_HEIGHT)/NET_HEIGHT*100)

write.csv(edit_data, file = here::here("output", paste0("compare_", survey, ".csv")))

edit_data |> 
  dplyr::arrange(-abs(DIFF_WIDTH))

edit_data |> 
  dplyr::arrange(-abs(DIFF_WIDTH_PCT))

edit_data |> 
  dplyr::arrange(-abs(DIFF_HEIGHT))
