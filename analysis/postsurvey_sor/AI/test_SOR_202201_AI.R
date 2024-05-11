# Test SOR on 2023 Eastern Bering Sea Shelf Survey 83-112 hauls
# Sequential outlier rejection and fill missing spread and height
# Last update: May 9, 2024

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
convert_marport_to_netmind = FALSE
min_pings_for_sor = 50
min_height_pings = 50
fill_method = "goa" # One method for the GOA and AI; see ?sor_fill_missing
create_user = "ROHANS"

cruise_idnum1 = 754
vessel1 = 148

cruise_idnum2 = 753
vessel2 = 176


# Process 2022 AI Ocean Explorer -------------------------------------------------------------------

# Retrieve haul and net mensuration data from race_data then write spread and height data from 
# individual hauls to the [subdirectory]: /output/{region}/{cruise}/{vessel}.
# - Height: [subdirectory]/ping_files_{survey}/HEIGHT_{region}_{cruise}_{vessel}.rds
# - Hauls: [subdirectory]/ping_files_{survey}/{cruise}_{vessel}_{haul}_pings.rds
sor_setup_directory(cruise = cruise,
                    cruise_idnum = cruise_idnum1,
                    vessel = vessel1,
                    region = region,
                    survey = survey,
                    haul_types = haul_types,
                    gear_codes = gear_codes,
                    channel = channel)

# Run sequential outlier rejection on rds files from each haul and write outputs to .rds files.
# Hauls w/ SOR: [subdirectory]/ping_files_{survey}/{cruise}_{vessel}_{haul}_sor.rds
sor_run(cruise = cruise,
        vessel = vessel1,
        region = region,
        survey = survey,
        min_pings_for_sor = min_pings_for_sor,
        overwrite = TRUE)

# Plot results of sequential outlier rejection for visual inspection.
# Plots: [subdirectory]/ping_files_{survey}/SOR_graphics_{survey}/SOR_{cruise}_{vessel}_{haul}.png
sor_plot_results(cruise = cruise,
                 vessel = vessel1,
                 region = region,
                 survey = survey)

# Fill in missing height and spread data
# Hauls w/ missing data filled: [subdirectory]/ping_files_{survey}/{cruise}_{vessel}_{haul}_final.rds
sor_fill_missing(height_paths = here::here("output", region, cruise, vessel1, 
                                           paste0("HEIGHT_", region, "_", cruise, "_", vessel1, ".rds")),
                 spread_paths = here::here("output", region, cruise, vessel1, 
                                           paste0("SPREAD_AFTER_SOR_", region, "_", cruise, "_", vessel1, ".rds")),
                 haul_path = here::here("output", region, cruise, vessel1, 
                                        paste0("edit_haul_", cruise, "_", vessel1, ".rds")),
                 rds_dir = here::here("output", region, cruise, vessel1, 
                                      paste0("PING_FILES_", region, "_", year)),
                 fill_method = fill_method,
                 convert_marport_to_netmind = convert_marport_to_netmind,
                 min_height_pings = min_height_pings)


# Process 2022 AI Alaska Provider ------------------------------------------------------------------

# Retrieve haul and net mensuration data from race_data then write spread and height data from 
# individual hauls to the [subdirectory]: /output/{region}/{cruise}/{vessel}.
# - Height: [subdirectory]/ping_files_{survey}/HEIGHT_{region}_{cruise}_{vessel}.rds
# - Hauls: [subdirectory]/ping_files_{survey}/{cruise}_{vessel}_{haul}_pings.rds
sor_setup_directory(cruise = cruise,
                    cruise_idnum = cruise_idnum2,
                    vessel = vessel2,
                    region = region,
                    survey = survey,
                    haul_types = haul_types,
                    gear_codes = gear_codes,
                    channel = channel)

# Run sequential outlier rejection on rds files from each haul and write outputs to .rds files.
# Hauls w/ SOR: [subdirectory]/ping_files_{survey}/{cruise}_{vessel}_{haul}_sor.rds
sor_run(cruise = cruise,
        vessel = vessel2,
        region = region,
        survey = survey,
        min_pings_for_sor = min_pings_for_sor,
        overwrite = TRUE)

# Plot results of sequential outlier rejection for visual inspection.
# Plots: [subdirectory]/ping_files_{survey}/SOR_graphics_{survey}/SOR_{cruise}_{vessel}_{haul}.png
sor_plot_results(cruise = cruise,
                 vessel = vessel2,
                 region = region,
                 survey = survey)

# Fill in missing height and spread data
# Hauls w/ missing data filled: [subdirectory]/ping_files_{survey}/{cruise}_{vessel}_{haul}_final.rds
sor_fill_missing(height_paths = here::here("output", region, cruise, vessel2, 
                                           paste0("HEIGHT_", region, "_", cruise, "_", vessel2, ".rds")),
                 spread_paths = here::here("output", region, cruise, vessel2, 
                                           paste0("SPREAD_AFTER_SOR_", region, "_", cruise, "_", vessel2, ".rds")),
                 haul_path = here::here("output", region, cruise, vessel2, 
                                        paste0("edit_haul_", cruise, "_", vessel2, ".rds")),
                 rds_dir = here::here("output", region, cruise, vessel2, 
                                      paste0("PING_FILES_", region, "_", year)),
                 fill_method = fill_method,
                 convert_marport_to_netmind = convert_marport_to_netmind,
                 min_height_pings = min_height_pings)


# Update tables ------------------------------------------------------------------------------------
# Add updated data to Oracle and write output to .csv
sor_save_results(final_dir = c(here::here("output", region, cruise, vessel1, 
                                          paste0("PING_FILES_", region, "_", year)),
                               here::here("output", region, cruise, vessel2, 
                                          paste0("PING_FILES_", region, "_", year))), 
                 create_user = create_user, 
                 survey = c(survey, survey), 
                 cruise_idnum = c(cruise_idnum1, cruise_idnum2),
                 channel = channel)


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
                                    rdh.net_height_method
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
  dplyr::select(c("VESSEL", "CRUISE", "HAUL", "EDIT_NET_HEIGHT", "EDIT_NET_SPREAD")) |>
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

