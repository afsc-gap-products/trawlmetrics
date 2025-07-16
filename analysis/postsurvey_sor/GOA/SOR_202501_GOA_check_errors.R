# Evaluate effect of bridle and bail setting issues on spread
# Sequential outlier rejection and fill missing spread and height
# Last update: July 15, 2025

library(trawlmetrics)
library(mgcv)
library(ggthemes)
library(ggrepel)

channel <- trawlmetrics::get_connected(schema = "AFSC")

# Settings -----------------------------------------------------------------------------------------
region = "GOA"
survey = "GOA_2025_check_errors"
year = 2025
cruise = 202501
haul_types = 3
gear_codes = 172
width_range = c(8, 28) # Expanded because of the AKP spread issue
convert_marport_to_netmind = FALSE
min_pings_for_sor = 50
min_height_pings = 50
fill_method = "goa" # One method for the GOA and AI; see ?sor_fill_missing
create_user = "ROHANS"
delete_existing = FALSE

cruise_idnum1 = 776
vessel1 = 148

cruise_idnum2 = 777
vessel2 = 176

vessel <- c(vessel1, vessel2)
cruise_idnum <- c(cruise_idnum1, cruise_idnum2)
vessel_comb <- paste(vessel, collapse = "_")

# Load historical GOA haul data

goa_hist <- bts_geom |>
  dplyr::filter(SURVEY_DEFINITION_ID == 47, NET_MEASURED == TRUE) |>
  dplyr::mutate(VESSEL = 176)

goa_hist <- 
  dplyr::bind_rows(
    goa_hist,
    dplyr::mutate(goa_hist, VESSEL = 148)
  )

# Process 2025 GOA Ocean Explorer and Alaska Provider -----------------------------------------------

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


# Examine Alaska Provider hauls with incorrect bail settings and sensors on the middle bridle ----
# Hauls 1-93: Main wire bail on the first hole, sensors on the middle bridle.
# Hauls 94-109: Sensors on the middle bridle.
# Hauls >= 110: Everything correct

akp_spread <- 
  readRDS(file = 
            here::here("output", "GOA", "202501", "148_176", "SPREAD_AFTER_SOR_GOA_202501_148_176.rds")
  ) |>
  dplyr::filter(vessel == 176) |>
  dplyr::mutate(trt = "inner_bail_middle_bridle",
                trt = ifelse(haul > 93, "middle_bail_middle_bridle", trt),
                trt = ifelse(haul > 109, "middle_bail_top_bridle", trt))

oex_spread <- 
  readRDS(file = 
            here::here("output", "GOA", "202501", "148_176", "SPREAD_AFTER_SOR_GOA_202501_148_176.rds")
  ) |>
  dplyr::filter(vessel == 148) 

akp_height <- 
  readRDS(file = here::here("output", "GOA", "202501", "148_176", "HEIGHT_GOA_202501_148_176.rds")) |>
  dplyr::filter(net_height_pings >= 50,
                vessel == 176)

akp_height_spread <-
  dplyr::inner_join(akp_spread, akp_height) |>
  dplyr::mutate(nn_fac = factor(net_number))

ggplot() +
  geom_vline(xintercept = c(93.5, 109.5), linetype = 2) +
  geom_point(
    data = akp_height_spread,
    mapping = aes(x = haul, y = mean_spread, color = factor(net_number), shape = trt)
  ) +
  scale_color_colorblind(name = "Net Number") +
  scale_shape(name = "Bail/Spread") +
  scale_y_continuous(name = "Measured spread (m)") +
  scale_x_continuous(name = "AKP Haul") +
  theme_bw()

ggplot() +
  geom_vline(xintercept = c(93.5, 109.5), linetype = 2) +
  geom_point(
    data = akp_height_spread,
    mapping = aes(x = haul, y = edit_net_height, color = factor(net_number), shape = trt)
  ) +
  scale_color_colorblind(name = "Net Number") +
  scale_shape(name = "Bail/Spread") +
  scale_x_continuous(name = "Haul") +
  scale_y_continuous(name = "Net Height (m)") +
  theme_bw()

ggplot() +
  geom_point(data = akp_height_spread,
             mapping = aes(x = bottom_depth, y = scope_ratio)) +
  geom_text_repel(data = akp_height_spread,
             mapping = aes(x = bottom_depth, y = scope_ratio, label = haul)) +
  scale_y_continuous(name = "Scope:depth") +
  scale_x_continuous(name = "Bottom depth (m)") +
  theme_bw()


akp_height_spread |>
  dplyr::filter(bottom_depth > 100) |>
  dplyr::arrange(-scope_ratio)


# Base model for estimating spread, except no net number, vessel effect, or net height
m0 <- mgcv::gam(formula = mean_spread ~ s(scope_ratio) + s(total_weight) + s(bottom_depth) + s(speed), data = akp_height_spread)

# Including treatment - model that will be used to predict 
m1 <- mgcv::gam(formula = mean_spread ~ s(scope_ratio) + s(total_weight) + s(bottom_depth) + s(speed) + trt + 0 , data = akp_height_spread)

# Including treatment and net height -- effectively the model that will be used for predicting spread (sans vessel effects)
m2 <- mgcv::gam(formula = mean_spread ~ s(scope_ratio) + s(total_weight) + s(bottom_depth) + s(speed) + s(edit_net_height) + trt + 0 , data = akp_height_spread)


summary(m0)
summary(m1)
plot(m1)
summary(m2) # Models are prescribed-- there is no model selection so insignificant variables are retained
plot(m2)

# Estimate difference in spread across depth, speed, depth, and scope ratio conditions
spread_fit <- 
  expand.grid(
  bottom_depth = c(50, 75, 100, 200, 300, 400),
  speed = c(2.8, 3, 3.2)/1.852,
  total_weight = 700,
  trt = unique(akp_height_spread$trt)
) |>
  dplyr::inner_join(
    data.frame(bottom_depth = c(50, 75, 100, 200, 300, 400),
               scope_ratio = c(2.75, 2, 1.8, 1.5, 1.3, 1.3))
  ) |>
  dplyr::inner_join(
    data.frame(speed = c(2.8, 3, 3.2)/1.852,
               speed_kn = c(2.8, 3, 3.2))
  )

spread_fit$spread_fit <- predict(m1, newdata = spread_fit)

# Plot m1 fits
ggplot(data = 
         spread_fit,
       mapping = aes(x = bottom_depth, y = spread_fit, color = trt)) +
  geom_point() +
  geom_path() +
  facet_wrap(~speed_kn) +
  scale_x_continuous(name = "Bottom depth (m)") +
  scale_y_continuous(name = "Estimated spread (m)")

spread_fit_wide <- 
  tidyr::pivot_wider(
  data = spread_fit,
  values_from = "spread_fit",
  names_from = "trt"
)

ggplot(data = 
         spread_fit_wide,
       mapping = aes(x = bottom_depth, y = middle_bail_top_bridle - middle_bail_middle_bridle)) +
  geom_point() +
  geom_path() +
  facet_wrap(~speed_kn) +
  scale_x_continuous(name = "Bottom depth (m)") +
  scale_y_continuous(name = "Correct - MBMB (m)")

ggplot(data = 
         spread_fit_wide,
       mapping = aes(x = bottom_depth, y = middle_bail_top_bridle - inner_bail_middle_bridle)) +
  geom_point() +
  geom_path() +
  facet_wrap(~speed_kn) +
  scale_x_continuous(name = "Bottom depth (m)") +
  scale_y_continuous(name = "Correct - IBMB (m)")

ggplot() +
  geom_point(
    data = 
      spread_fit_wide,
    mapping = aes(
      x = bottom_depth, 
      y = middle_bail_top_bridle - middle_bail_middle_bridle,
      color = "Correct - MBMB (m)")
  ) +
  geom_path(
    data = 
      spread_fit_wide,
    mapping = aes(
      x = bottom_depth, 
      y = middle_bail_top_bridle - middle_bail_middle_bridle, ,
      color = "Correct - MBMB (m)")
  ) +
  geom_point(
    data = 
      spread_fit_wide,
    mapping = aes(
      x = bottom_depth, 
      y = middle_bail_top_bridle - inner_bail_middle_bridle,
      color = "Correct - IBMB (m)")
  ) +
  geom_path(
    data = 
      spread_fit_wide,
    mapping = aes(
      x = bottom_depth, 
      y = middle_bail_top_bridle - inner_bail_middle_bridle, ,
      color = "Correct - IBMB (m)")
  ) +
  geom_point(
    data = 
      spread_fit_wide,
    mapping = aes(
      x = bottom_depth, 
      y = middle_bail_middle_bridle - inner_bail_middle_bridle,
      color = "MBMB - IBMB (m)")
  ) +
  geom_path(
    data = 
      spread_fit_wide,
    mapping = aes(
      x = bottom_depth, 
      y = middle_bail_middle_bridle - inner_bail_middle_bridle, ,
      color = "MBMB - IBMB (m)")
  ) +
  facet_wrap(~speed_kn) +
  scale_x_continuous(name = "Bottom depth (m)") +
  scale_y_continuous(name = "Difference (m)") +
  scale_color_tableau(name = "Comparison") +
  theme_bw()

# Fill missing spreads

akp_corr_spread <- 
  akp_spread |>
  dplyr::select(-trt) |>
  dplyr::mutate(net_number = ifelse(haul < 96, NA, net_number),
                mean_spread = ifelse(haul < 110, NA, mean_spread),
                n_pings = ifelse(haul < 110, 0, n_pings),
                )

dplyr::bind_rows(akp_corr_spread, oex_spread) |>
  saveRDS(
    here::here("output", region, cruise, vessel_comb, 
               paste0("SPREAD_AFTER_SOR_CORR_", region, "_", cruise, "_", vessel_comb, ".rds")
    )
  )

# Remove spread values from hauls 1-109
for(ii in 1:109) {
  
  sor_path <- here::here("output", region, cruise, vessel_comb,
                         paste0("PING_FILES_", survey),
                         paste0("176_202501_",
                                gsub(pattern = " ", replacement = "0", x = format(ii, width = 4)), "_sor.rds"))

  if(file.exists(sor_path)) {
    sor_fix <- readRDS(sor_path)
    
    sor_fix$spread <- NULL
    
    sor_fix$haul$net_number <- NA
    
    sor_fix$sor_ping_ranks <- NULL
    sor_fix$sor_results <- NULL
    sor_fix$sor_rmse <- NULL
    
    saveRDS(sor_fix,
            sor_path)
  }

}


# Fill in missing height and spread data
# Hauls w/ missing data filled: [subdirectory]/ping_files_{survey}/{cruise}_{vessel}_{haul}_final.rds
sor_fill_missing(height_paths = here::here("output", region, cruise, vessel_comb, 
                                           paste0("HEIGHT_", region, "_", cruise, "_", vessel_comb, ".rds")),
                 spread_paths = here::here("output", region, cruise, vessel_comb, 
                                           paste0("SPREAD_AFTER_SOR_CORR_", region, "_", cruise, "_", vessel_comb, ".rds")),
                 haul_path = here::here("output", region, cruise, vessel_comb, 
                                        paste0("edit_haul_", cruise, "_", vessel_comb, ".rds")),
                 rds_dir = here::here("output", region, cruise, vessel_comb, 
                                      paste0("PING_FILES_", survey)),
                 fill_method = fill_method,
                 convert_marport_to_netmind = convert_marport_to_netmind,
                 min_height_pings = min_height_pings)

# Update tables ------------------------------------------------------------------------------------
# Add updated data to Oracle and write output to .csv
sor_save_results(final_dir = here::here("output", region, cruise, vessel_comb, 
                                        paste0("PING_FILES_", survey)), 
                 create_user = create_user, 
                 survey = c(survey, survey), 
                 cruise_idnum = cruise_idnum,
                 channel = channel,
                 delete_existing = delete_existing
)

# Check new spread values
new_spread <- read.csv(file = here::here("output", "race_data_edit_hauls_table_GOA_2025_check_errors.csv")) |>
  dplyr::inner_join(
    readRDS(
      here::here("output", region, cruise, vessel_comb, 
                 paste0("SPREAD_AFTER_SOR_CORR_", region, "_", cruise, "_", vessel_comb, ".rds")
      )
    ) |>
    dplyr::select(VESSEL = vessel, CRUISE = cruise, HAUL = haul, PERFORMANCE = performance)
  )

# Before corrections
before_corrections <- readRDS(file = 
                                here::here("output", "GOA", "202501", "148_176", "SPREAD_AFTER_SOR_GOA_202501_148_176.rds")
) |>
  dplyr::inner_join(
    readRDS(file = here::here("output", "GOA", "202501", "148_176", "HEIGHT_GOA_202501_148_176.rds"))
  )

ggplot() +
  geom_point(
    data = goa_hist,
    mapping = aes(x = NET_HEIGHT_M, y = NET_WIDTH_M),
    color = "grey",
    alpha = 0.5,
    size = rel(.15)
  ) +
  geom_point(data = dplyr::filter(before_corrections, performance >= 0),
             mapping = aes(x = edit_net_height, y = mean_spread), alpha = 0.5) +
  facet_wrap(~vessel) +
  scale_x_continuous(name = "EDIT_NET_HEIGHT", limits = c(0, 10)) +
  scale_y_continuous(name = "EDIT_NET_SPREAD", limits = c(10, 22)) +
  theme_bw()

# After corrections
ggplot() +
  geom_point(
    data = goa_hist,
    mapping = aes(x = NET_HEIGHT_M, y = NET_WIDTH_M),
    color = "grey",
    alpha = 0.5,
    size = rel(.15)
  ) +
  geom_point(data = dplyr::filter(new_spread, PERFORMANCE >= 0),
             mapping = aes(x = EDIT_NET_HEIGHT, y = EDIT_NET_SPREAD, color = factor(NET_SPREAD_METHOD)),
             alpha = 0.5) +
  geom_text_repel(data = dplyr::filter(new_spread, PERFORMANCE >= 0),
             mapping = aes(x = EDIT_NET_HEIGHT, y = EDIT_NET_SPREAD, color = factor(NET_SPREAD_METHOD), label = HAUL)) +
  facet_wrap(~VESSEL) +
  scale_x_continuous(limits = c(0, 10)) +
  scale_y_continuous(limits = c(10, 22)) +
  scale_color_fivethirtyeight(name = "NET_SPREAD_METHOD") +
  theme_bw()


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
  dplyr::arrange(-abs(DIFF_WIDTH_PCT))

edit_data |> 
  dplyr::arrange(-abs(DIFF_HEIGHT_PCT))
