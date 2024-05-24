# 2024 Eastern Bering Sea Shelf Survey Poly Nor'Eastern hauls
# Sequential outlier rejection and fill missing spread and height
# Last update: May 9, 2024

library(trawlmetrics)

channel <- trawlmetrics::get_connected(schema = "AFSC")

# Shared settings ----------------------------------------------------------------------------------
region = "EBS"
survey = "EBS_2024"
year = 2024
cruise = 202401
haul_types = 3
gear_codes = 172
width_range = c(8, 22)
convert_marport_to_netmind = TRUE
fill_method = "ebs"
create_user = "ROHANS"
delete_existing = TRUE

cruise_idnum1 = 759
vessel1 = 162


# Process 2024 EBS Alaska Knight 83-112 ------------------------------------------------------------

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
                 delete_existing = delete_existing)


# Add updated data to Oracle and write output to .csv
sor_save_results(final_dir = here::here("output", region, cruise, vessel1, 
                                        paste0("PING_FILES_", region, "_", year)), 
                 create_user = create_user, 
                 survey = survey, 
                 cruise_idnum = cruise_idnum1,
                 channel = channel)