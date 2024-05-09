library(trawlmetrics)

# 2018 EBS Alaska Knight ---------------------------------------------------------------------------
cruise1 = 201801
cruise_idnum1 = 722
vessel1 = 94
region1 = "EBS"
survey1 = "EBS_2018"
width_range1 = c(10, 22)
create_user = "ROHANS"

# Retrieve haul and net mensuration data from race_data then write spread and height data from individual hauls to the [subdirectory]: /output/{region}/{cruise}/{vessel}.
# - Height: [subdirectory]/ping_files_{survey}/HEIGHT_{region}_{cruise}_{vessel}.rds
# - Hauls: [subdirectory]/ping_files_{survey}/{cruise}_{vessel}_{haul}_pings.rds
sor_setup_directory(cruise = cruise1, # cruise number
                    cruise_idnum = cruise_idnum1,
                    vessel = vessel1,
                    region = region1,
                    survey = survey1)

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

# Fill in missing height data using the mean height for a given scope then estimate missing spread using a GLM. Write results to .rds files.
# Hauls w/ missing data filled: [subdirectory]/ping_files_{survey}/{cruise}_{vessel}_{haul}_final.rds
sor_fill_missing(height_paths = here::here("output", region1, cruise1, vessel1, 
                                           paste0("HEIGHT_", region1, "_", cruise1, "_", vessel1, ".rds")),
                 spread_paths = here::here("output", region1, cruise1, vessel1, 
                                           paste0("SPREAD_AFTER_SOR_", region1, "_", cruise1, "_", vessel1, ".rds")),
                 haul_path = here::here("output", region1, cruise1, vessel1, paste0("edit_haul_", cruise1, "_", vessel1, ".rds")),
                 rds_dir = here::here("output", region1, cruise1, vessel1, 
                                      paste0("PING_FILES_", region1, "_", floor(cruise1/100))),
                 fill_method = "ebs",
                 convert_marport_to_netmind = TRUE)


# Add updated data to Oracle and write output to .csv
sor_save_results(final_dir = here::here("output", region1, cruise1, vessel1, 
                                        paste0("PING_FILES_", region1, "_", floor(cruise1/100))), 
                 create_user = create_user, 
                 survey = survey1, 
                 cruise_idnum = cruise_idnum1)
