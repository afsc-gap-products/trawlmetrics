library(trawlmetrics)

# 2019 GOA Sea Storm ---------------------------------------------------------------------------
cruise1 = 201901
cruise_idnum1 = 730
vessel1 = 143
region1 = "GOA"
survey1 = "GOA_2019"
width_range1 = c(10, 22)

# Copy system files for testing. Remove for actual data processing.
dir.create(here::here("output", region1, cruise1, vessel1), recursive = TRUE)

# Retrieve haul and net mensuration data from race_data then write spread and height data from individual hauls to the [subdirectory]: /output/{region}/{cruise}/{vessel}.
# - Height: [subdirectory]/ping_files_{survey}/HEIGHT_{region}_{cruise}_{vessel}.rds
# - Hauls: [subdirectory]/ping_files_{survey}/{cruise}_{vessel}_{haul}_pings.rds
sor_setup_directory(cruise = cruise1, # cruise number
                    cruise_idnum = cruise_idnum1,
                    vessel = vessel1,
                    region = region1,
                    survey = survey1,
                    width_range = width_range1,
                    convert_marport_to_netmind = TRUE,
                    skip_save_rds = TRUE # For demo/testing purposes. Change to TRUE for actual data processing.
                    )

# Run sequential outlier rejection on rds files from each haul and write outputs to .rds files.
# Hauls w/ SOR: [subdirectory]/ping_files_{survey}/{cruise}_{vessel}_{haul}_sor.rds
sor_run(cruise = cruise1,
        vessel = vessel1,
        region = region1,
        survey = survey1)

# Plot results of sequential outlier rejection for visual inspection.
# Plots: [subdirectory]/ping_files_{survey}/SOR_graphics_{survey}/SOR_{cruise}_{vessel}_{haul}.png
sor_plot_results(cruise = cruise1,
                 vessel = vessel1,
                 region = region1,
                 survey = survey1)

# Fill in missing height data using the mean height for a given scope then estimate missing spread using a GLM. Write results to .rds files.
# Hauls w/ missing data filled: [subdirectory]/ping_files_{survey}/{cruise}_{vessel}_{haul}_final.rds
sor_fill_missing(height_paths = here::here("output", region1, cruise1, vessel1, paste0("HEIGHT_", region1, "_", cruise1, "_", vessel1, ".rds")),
                 spread_paths = here::here("output", region1, cruise1, vessel1, paste0("SPREAD_AFTER_SOR_", region1, "_", cruise1, "_", vessel1, ".rds")),
                 rds_dir = here::here("output", region1, cruise1, vessel1, paste0("PING_FILES_", region1, "_", floor(cruise/100))))
