library(trawlmetrics)

# 2024 EBS Alaska Knight 83-112 --------------------------------------------------------------------
cruise1 = 202401
year1 = floor(cruise1/100)
cruise_idnum1 = 767
vessel1 = 162
region1 = "EBS"
survey1 = "EBS_2024"
width_range1 = c(10, 22)
create_user = "ROHANS"
fill_method = "ebs"
haul_types1 = 3
gear_codes1 = 44

# Retrieve haul and net mensuration data from race_data then write spread and height data from individual hauls to the [subdirectory]: /output/{region}/{cruise}/{vessel}.
# - Height: [subdirectory]/ping_files_{survey}/HEIGHT_{region}_{cruise}_{vessel}.rds
# - Hauls: [subdirectory]/ping_files_{survey}/{cruise}_{vessel}_{haul}_pings.rds
sor_setup_directory(cruise = cruise1, # cruise number
                    cruise_idnum = cruise_idnum1,
                    vessel = vessel1,
                    region = region1,
                    survey = survey1,
                    haul_types = haul_types1,
                    gear_codes = gear_codes1)

# Run sequential outlier rejection on rds files from each haul and write outputs to .rds files.
# Hauls w/ SOR: [subdirectory]/ping_files_{survey}/{cruise}_{vessel}_{haul}_sor.rds
sor_run(cruise = cruise1,
        vessel = vessel1,
        region = region1,
        survey = survey1,
        overwrite = TRUE)

# Plot results of sequential outlier rejection for visual inspection.
# Plots: [subdirectory]/ping_files_{survey}/SOR_graphics_{survey}/SOR_{cruise}_{vessel}_{haul}.png
sor_plot_results(cruise = cruise1,
                 vessel = vessel1,
                 region = region1,
                 survey = survey1)

# Fill in missing height and spread data
# Hauls w/ missing data filled: [subdirectory]/ping_files_{survey}/{cruise}_{vessel}_{haul}_final.rds
sor_fill_missing(height_paths = here::here("output", region1, cruise1, vessel1, 
                                           paste0("HEIGHT_", region1, "_", cruise1, "_", vessel1, ".rds")),
                 spread_paths = here::here("output", region1, cruise1, vessel1, 
                                           paste0("SPREAD_AFTER_SOR_", region1, "_", cruise1, "_", vessel1, ".rds")),
                 haul_path = here::here("output", region1, cruise1, vessel1, 
                                        paste0("edit_haul_", cruise1, "_", vessel1, ".rds")),
                 rds_dir = here::here("output", region1, cruise1, vessel1, 
                                      paste0("PING_FILES_", region1, "_", year1)),
                 fill_method = fill_method,
                 convert_marport_to_netmind = TRUE)

# 2024 EBS Ocean Explorer 83-112 -------------------------------------------------------------------
cruise2 = 202401
year2 = floor(cruise2/100)
cruise_idnum1 = 768
vessel2 = 162
region2 = "EBS"
survey2 = "EBS_2024"
width_range1 = c(10, 22)
create_user = "ROHANS"
fill_method = "ebs"
haul_types2 = c(3, 20)
gear_codes2 = 44

# Retrieve haul and net mensuration data from race_data then write spread and height data from individual hauls to the [subdirectory]: /output/{region}/{cruise}/{vessel}.
# - Height: [subdirectory]/ping_files_{survey}/HEIGHT_{region}_{cruise}_{vessel}.rds
# - Hauls: [subdirectory]/ping_files_{survey}/{cruise}_{vessel}_{haul}_pings.rds
sor_setup_directory(cruise = cruise2, # cruise number
                    cruise_idnum = cruise_idnum1,
                    vessel = vessel2,
                    region = region2,
                    survey = survey2,
                    haul_types = haul_types2,
                    gear_codes = gear_codes2)

# Run sequential outlier rejection on rds files from each haul and write outputs to .rds files.
# Hauls w/ SOR: [subdirectory]/ping_files_{survey}/{cruise}_{vessel}_{haul}_sor.rds
sor_run(cruise = cruise2,
        vessel = vessel2,
        region = region2,
        survey = survey2,
        overwrite = TRUE)

# Plot results of sequential outlier rejection for visual inspection.
# Plots: [subdirectory]/ping_files_{survey}/SOR_graphics_{survey}/SOR_{cruise}_{vessel}_{haul}.png
sor_plot_results(cruise = cruise2,
                 vessel = vessel2,
                 region = region2,
                 survey = survey2)

# Fill in missing height and spread data
# Hauls w/ missing data filled: [subdirectory]/ping_files_{survey}/{cruise}_{vessel}_{haul}_final.rds
sor_fill_missing(height_paths = here::here("output", region2, cruise2, vessel2, 
                                           paste0("HEIGHT_", region2, "_", cruise2, "_", vessel2, ".rds")),
                 spread_paths = here::here("output", region2, cruise2, vessel2, 
                                           paste0("SPREAD_AFTER_SOR_", region2, "_", cruise2, "_", vessel2, ".rds")),
                 haul_path = here::here("output", region2, cruise2, vessel2, 
                                        paste0("edit_haul_", cruise2, "_", vessel2, ".rds")),
                 rds_dir = here::here("output", region2, cruise2, vessel2, 
                                      paste0("PING_FILES_", region2, "_", year2)),
                 fill_method = fill_method,
                 convert_marport_to_netmind = TRUE)


# Update tables ------------------------------------------------------------------------------------
# Add updated data to Oracle and write output to .csv
sor_save_results(final_dir = c(here::here("output", region1, cruise1, vessel1, 
                                        paste0("PING_FILES_", region1, "_", year1)),
                               here::here("output", region2, cruise2, vessel2, 
                                          paste0("PING_FILES_", region2, "_", year2))), 
                 create_user = create_user, 
                 survey = c(survey1, survey2), 
                 cruise_idnum = c(cruise_idnum1, cruise_idnum2))