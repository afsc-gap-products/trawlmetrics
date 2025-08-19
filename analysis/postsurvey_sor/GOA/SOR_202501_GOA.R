# Sequential outlier rejection and fill missing spread and height data for 2025 GOA survey
# Last update: August 12, 2025

library(trawlmetrics)
library(mgcv)
library(ggthemes)
library(cowplot)

channel <- trawlmetrics::get_connected(schema = "AFSC")

# Settings -----------------------------------------------------------------------------------------
region = "GOA"
survey = "GOA_2025"
year = 2025
cruise = 202501
haul_types = 3
gear_codes = 172
width_range = c(8, 28) # Expanded because of the AKP spread issue
convert_marport_to_netmind = FALSE
min_pings_for_sor = 50
min_height_pings = 50
fill_method = "goa" # One method for the GOA and AI; see ?sor_fill_missing
create_user = "SIPLEM"
delete_existing = FALSE # Change to true to update Oracle 

cruise_idnum1 = 776
vessel1 = 148

cruise_idnum2 = 777
vessel2 = 176

vessel <- c(vessel1, vessel2)
cruise_idnum <- c(cruise_idnum1, cruise_idnum2)
vessel_comb <- paste(vessel, collapse = "_")

# Process 2025 GOA Ocean Explorer and Alaska Provider -----------------------------------------------

# Retrieve haul and net mensuration data from race_data then write spread and height data from 
# individual hauls to the [subdirectory]: /output/{region}/{cruise}/{vessel}.
# - Height: [subdirectory]/ping_files_{survey}/HEIGHT_{region}_{cruise}_{vessel}.rds
# - Hauls: [subdirectory]/ping_files_{survey}/{cruise}_{vessel}_{haul}_pings.rds
sor_setup_directory(
  cruise = cruise,
  cruise_idnum = cruise_idnum,
  vessel = vessel,
  region = region,
  survey = survey,
  haul_types = haul_types,
  gear_codes = gear_codes,
  channel = channel
)

# Run sequential outlier rejection on rds files from each haul and write outputs to .rds files.
# Hauls w/ SOR: [subdirectory]/ping_files_{survey}/{cruise}_{vessel}_{haul}_sor.rds
sor_run(
  cruise = cruise,
  vessel = vessel,
  region = region,
  survey = survey,
  min_pings_for_sor = min_pings_for_sor,
  overwrite = TRUE
)

# Plot results of sequential outlier rejection for visual inspection.
# Plots: [subdirectory]/ping_files_{survey}/SOR_graphics_{survey}/SOR_{cruise}_{vessel}_{haul}.png
sor_plot_results(
  cruise = cruise,
  vessel = vessel,
  region = region,
  survey = survey
)

# Change net numbers for haul < 110 to NA unless a net was used later and had enough good hauls
# for spread estimation
haul_data <- 
  readRDS(here::here("output", region, cruise, vessel_comb, 
                     paste0("edit_haul_", cruise, "_", vessel_comb, ".rds")))

haul_data |>
  dplyr::filter(vessel == 176) |>
  dplyr::group_by(vessel, cruise, net_number) |>
  dplyr::summarise(min_haul = min(haul),
                   max_haul = max(haul),
                   n_good = sum(haul > 109 & performance >= 0)) |>
  dplyr::filter(max_haul > 109, min_haul < 110) |>
  dplyr::arrange(net_number)

# Net 20: 26 hauls after haul 109; use as a factor
# Net 23: only 4 good hauls at the end of the survey. Treat as NA (i.e., do not use as a factor when estimating spread)

dplyr::filter(haul_data, net_number == 23) |>
  dplyr::arrange(haul)

# Change net numbers for hauls 1-109 (only for estimation-- not in the data)
for(ii in 1:109) {
  
  sor_path <- here::here("output", region, cruise, vessel_comb,
                         paste0("PING_FILES_", survey),
                         paste0("176_202501_",
                                gsub(pattern = " ", replacement = "0", x = format(ii, width = 4)), "_sor.rds"))
  
  if(file.exists(sor_path)) {
    sor_fix <- readRDS(sor_path)
    
    # Removing net number from ping files ensures they are treated as NA for estimation
      if(!(sor_fix$haul$net_number == 20)) {
      sor_fix$haul$net_number <- NA
      saveRDS(sor_fix,
              sor_path)
    }

  }
  
}

# Fill in missing height and spread data
# Hauls w/ missing data filled: [subdirectory]/ping_files_{survey}/{cruise}_{vessel}_{haul}_final.rds
sor_fill_missing(
  height_paths = here::here("output", region, cruise, vessel_comb, 
                            paste0("HEIGHT_", region, "_", cruise, "_", vessel_comb, ".rds")),
  spread_paths = here::here("output", region, cruise, vessel_comb, 
                            paste0("SPREAD_AFTER_SOR_", region, "_", cruise, "_", vessel_comb, ".rds")),
  haul_path = here::here("output", region, cruise, vessel_comb, 
                         paste0("edit_haul_", cruise, "_", vessel_comb, ".rds")),
  rds_dir = here::here("output", region, cruise, vessel_comb, 
                       paste0("PING_FILES_", survey)),
  fill_method = fill_method,
  convert_marport_to_netmind = convert_marport_to_netmind,
  min_height_pings = min_height_pings
)


# Update tables ------------------------------------------------------------------------------------
# Add updated data to Oracle and write output to .csv
sor_save_results(
  final_dir = here::here("output", region, cruise, vessel_comb, 
                         paste0("PING_FILES_", survey)), 
  create_user = create_user, 
  survey = c(survey, survey), 
  cruise_idnum = cruise_idnum,
  channel = channel,
  delete_existing = delete_existing
)


# Compare spread before and after corrections ------------------------------------------------------

# Load historical GOA haul data and calculate 95% IQR

goa_hist <- bts_geom |>
  dplyr::filter(SURVEY_DEFINITION_ID == 47, NET_MEASURED == TRUE) |>
  dplyr::mutate(VESSEL = 176)

goa_hist <- 
  dplyr::bind_rows(
    goa_hist,
    dplyr::mutate(goa_hist, VESSEL = 148)
  )

spread_range <- quantile(goa_hist$NET_WIDTH_M, probs = c(0.025, 0.975))

# Load haul data that had been retrieved from edit tables

hauls <- 
  readRDS(here::here("output", region, cruise, vessel_comb, 
                     paste0("edit_haul_", cruise, "_", vessel_comb, ".rds"))
  ) |> 
  dplyr::inner_join(
    readRDS(here::here("output", region, cruise, vessel_comb, 
                       paste0("HEIGHT_", region, "_", cruise, "_", vessel_comb, ".rds"))) |>
      dplyr::select(vessel, cruise, haul, edit_net_height)
  ) |>
  dplyr::select(
    vessel,
    cruise,
    haul,
    net_number,
    original_spread = edit_net_spread,
    performance
  )

# Load new spread data and merge with haul data
new_spread <- 
  read.csv(file = here::here("output", "race_data_edit_hauls_table_GOA_2025.csv"))

names(new_spread) <- tolower(names(new_spread))

new_spread <- new_spread |>
  dplyr::full_join(hauls)

# Spread before SOR and estimation (i.e., what had been in the race_data)
p_akp_before <- 
  ggplot() +
  geom_hline(mapping = aes(yintercept = spread_range), linetype = 2) +
  geom_vline(xintercept = 109.5, linetype = 1, color = "red") +
  geom_point(data = 
               dplyr::filter(haul_data, vessel == 176, performance >= 0),
             mapping = aes(x = haul, y = edit_net_spread, color = factor(net_number))) +
  scale_y_continuous(name = "EDIT_NET_SPREAD", limits = c(10, 23)) +
  scale_x_continuous(name = "Haul") +
  ggtitle(label = "Unedited AKP spread (good hauls)") +
  scale_color_colorblind(name = "Net number", na.value = "grey70") +
  theme_bw()

p_akp_after <- 
  ggplot() +
  geom_hline(mapping = aes(yintercept = spread_range), linetype = 2) +
  geom_vline(xintercept = 109.5, linetype = 1, color = "red") +
  geom_point(data = 
               dplyr::filter(new_spread, vessel == 176, performance >= 0),
             mapping = aes(x = haul, y = edit_net_spread, color = factor(net_number))) +
  scale_y_continuous(name = "EDIT_NET_SPREAD", limits = c(10, 23)) +
  scale_x_continuous(name = "Haul") +
  ggtitle(label = "Final AKP spread (good hauls)") +
  scale_color_colorblind(name = "Net number", na.value = "grey70") +
  theme_bw()

cowplot::plot_grid(
  p_akp_before,
  p_akp_after,
  nrow = 2
)

# Plot OEX spread

p_oex_before <- 
  ggplot() +
  geom_hline(mapping = aes(yintercept = spread_range), linetype = 2) +
  geom_vline(xintercept = 109.5, linetype = 1, color = "red") +
  geom_point(data = 
               dplyr::filter(haul_data, vessel == 148, performance >= 0),
             mapping = aes(x = haul, y = edit_net_spread, color = factor(net_number))) +
  scale_y_continuous(name = "EDIT_NET_SPREAD", limits = c(10, 23)) +
  scale_x_continuous(name = "Haul") +
  ggtitle(label = "Unedited OEX spread (good hauls)") +
  scale_color_colorblind(name = "Net number", na.value = "grey70") +
  theme_bw()

p_oex_after <- 
  ggplot() +
  geom_hline(mapping = aes(yintercept = spread_range), linetype = 2) +
  geom_vline(xintercept = 109.5, linetype = 1, color = "red") +
  geom_point(data = 
               dplyr::filter(new_spread, vessel == 148, performance >= 0),
             mapping = aes(x = haul, y = edit_net_spread, color = factor(net_number))) +
  scale_y_continuous(name = "EDIT_NET_SPREAD", limits = c(10, 23)) +
  scale_x_continuous(name = "Haul") +
  ggtitle(label = "Final OEX spread (good hauls)") +
  scale_color_colorblind(name = "Net number", na.value = "grey70") +
  theme_bw()

cowplot::plot_grid(
  p_oex_before,
  p_oex_after,
  nrow = 2
)

