# 2024 Aleutian Islands Survey 
# Sequential outlier rejection and fill missing spread and height
# Last update: May 9, 2024

library(trawlmetrics)

channel <- trawlmetrics::get_connected(schema = "AFSC")

# Settings -----------------------------------------------------------------------------------------
region = "AI"
survey = "AI_2024"
year = 2024
cruise = 202401
haul_types = 3
gear_codes = 172
width_range = c(8, 22)
min_pings_for_sor = 50
min_height_pings = 50
convert_marport_to_netmind = FALSE
fill_method = "goa"
create_user = "ROHANS"
delete_existing = FALSE

cruise_idnum1 = 770
vessel1 = 176

cruise_idnum2 = 769
vessel2 = 148

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
