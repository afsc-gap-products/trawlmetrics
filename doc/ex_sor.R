library(trawlmetrics)

# 2022 NBS Alaska Knight ---------------------------------------------------------------------------

cruise1 = 202202
cruise_idnum1 = 758
vessel1 = 162
region1 = "NBS"
survey1 = "NBS_2022"
width_range1 = c(10, 22)

# Retrieve haul and net mensuration data from race_data then write spread and height data from individual hauls to the [subdirectory]: /output/{region}/{cruise}/{vessel}.
# - Height: [subdirectory]/ping_files_{survey}/HEIGHT_{region}_{cruise}_{vessel}.rds
# - Hauls: [subdirectory]/ping_files_{survey}/{cruise}_{vessel}_{haul}_pings.rds
sor_setup_directory(cruise = cruise1, # cruise number
                    cruise_idnum = cruise_idnum1,
                    vessel = vessel1,
                    region = region1,
                    survey = survey1,
                    width_range = width_range1,
                    convert_marport_to_netmind = TRUE)

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
sor_fill_missing(height_paths = "C:/Users/sean.rohan/Work/afsc/postsurvey_hauldata_processing/output/EBS/202202/162/HEIGHT_EBS_202202_162.rds",
                 spread_paths = "C:/Users/sean.rohan/Work/afsc/postsurvey_hauldata_processing/output/EBS/202202/162/SPREAD_AFTER_SOR_EBS_202202_162.rds",
                 rds_dir = "C:/Users/sean.rohan/Work/afsc/postsurvey_hauldata_processing/output/EBS/202202/162/ping_files_NBS_2022")


# 2022 NBS Vesteraalen  ----------------------------------------------------------------------------

cruise2 = 202202
cruise_idnum2 = 757
vessel2 = 94
region2 = "NBS"
survey2 = "NBS_2022"
width_range2 = c(10, 22)

# Retrieve haul and net mensuration data from race_data then write spread and height data from individual hauls to the [subdirectory]: /output/{region}/{cruise}/{vessel}.
# - Height: [subdirectory]/ping_files_{survey}/HEIGHT_{region}_{cruise}_{vessel}.rds
# - Hauls: [subdirectory]/ping_files_{survey}/{cruise}_{vessel}_{haul}_pings.rds
sor_setup_directory(cruise = cruise2, # cruise number
                    cruise_idnum = cruise_idnum2,
                    vessel = vessel2,
                    region = region2,
                    survey = survey2,
                    width_range = width_range2,
                    convert_marport_to_netmind = TRUE)

# Run sequential outlier rejection on rds files from each haul and write outputs to .rds files.
# Hauls w/ SOR: [subdirectory]/ping_files_{survey}/{cruise}_{vessel}_{haul}_sor.rds
sor_run(cruise = cruise2,
        vessel = vessel2,
        region = region2,
        survey = survey2)

# Plot results of sequential outlier rejection for visual inspection.
# Plots: [subdirectory]/ping_files_{survey}/SOR_graphics_{survey}/SOR_{cruise}_{vessel}_{haul}.png
sor_plot_results(cruise = cruise2,
                 vessel = vessel2,
                 region = region2,
                 survey = survey2)

# Fill in missing height data using the mean height for a given scope then estimate missing spread using a GLM. Write results to .rds files.
# Hauls w/ missing data filled: [subdirectory]/ping_files_{survey}/{cruise}_{vessel}_{haul}_final.rds
sor_fill_missing(height_paths = "C:/Users/sean.rohan/Work/afsc/postsurvey_hauldata_processing/output/EBS/202202/162/HEIGHT_EBS_202202_162.rds",
                 spread_paths = "C:/Users/sean.rohan/Work/afsc/postsurvey_hauldata_processing/output/EBS/202202/162/SPREAD_AFTER_SOR_EBS_202202_162.rds",
                 rds_dir = "C:/Users/sean.rohan/Work/afsc/postsurvey_hauldata_processing/output/EBS/202202/162/ping_files_NBS_2022")


# Combine data from both vessels -------------------------------------------------------------------

# [Need to make a function]
