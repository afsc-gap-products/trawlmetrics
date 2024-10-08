# 2024 Eastern Bering Sea Shelf Survey 83-112 hauls
# Sequential outlier rejection and fill missing spread and height
# Last update: July 18, 2024

library(trawlmetrics)

# Settings -----------------------------------------------------------------------------------------
region = "EBS"
survey = "EBS_2024"
year = 2024
cruise = 202401
haul_types = 3
gear_codes = 44
width_range = c(10, 22)
convert_marport_to_netmind = TRUE
min_pings_for_sor = 50
min_height_pings = 150
fill_method = "ebs"
create_user = "ROHANS"
delete_existing = FALSE # CHANGE TO TRUE FOR UPDATING RESULTS

cruise_idnum1 = 767
vessel1 = 162

cruise_idnum2 = 768
vessel2 = 134


# Process 2024 EBS Alaska Knight 83-112 ------------------------------------------------------------

# Retrieve haul and net mensuration data from race_data then write spread and height data from individual hauls to the [subdirectory]: /output/{region}/{cruise}/{vessel}.
# - Height: [subdirectory]/ping_files_{survey}/HEIGHT_{region}_{cruise}_{vessel}.rds
# - Hauls: [subdirectory]/ping_files_{survey}/{cruise}_{vessel}_{haul}_pings.rds
sor_setup_directory(cruise = cruise,
                    cruise_idnum = cruise_idnum1,
                    vessel = vessel1,
                    region = region,
                    survey = survey,
                    haul_types = haul_types,
                    gear_codes = gear_codes)

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


# Process 2024 EBS Northwest Explorer 83-112 -------------------------------------------------------

# Retrieve haul and net mensuration data from race_data then write spread and height data from individual hauls to the [subdirectory]: /output/{region}/{cruise}/{vessel}.
# - Height: [subdirectory]/ping_files_{survey}/HEIGHT_{region}_{cruise}_{vessel}.rds
# - Hauls: [subdirectory]/ping_files_{survey}/{cruise}_{vessel}_{haul}_pings.rds
sor_setup_directory(cruise = cruise,
                    cruise_idnum = cruise_idnum2,
                    vessel = vessel2,
                    region = region,
                    survey = survey,
                    haul_types = haul_types,
                    gear_codes = gear_codes)

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
                 delete_existing = delete_existing, 
                 cruise_idnum = c(cruise_idnum1, cruise_idnum2))
