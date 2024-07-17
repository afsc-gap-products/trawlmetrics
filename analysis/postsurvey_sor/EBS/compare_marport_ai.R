# Check for spread differences between old and new Marport sensors in the AI
# Created by Sean Rohan <sean.rohan@noaa.gov>
# July 16, 2024

library(trawlmetrics)
library(akgfmaps)
library(lme4)
library(RODBC)
library(brms)

min_year <- 2012
max_year <- 2024

# File *WITH* Marport to Netmind conversion
results_file <- here::here("analysis", 
                           "postsurvey_sor",
                           "AI",
                           "race_data_edit_hauls_table_AI_2024.csv")


# Retrieve data ----
con <- trawlmetrics::get_connected(schema = "AFSC")

hsprior <- RODBC::sqlQuery(channel = con,
                           query = paste0("select * from racebase.haul 
                     where cruise > ", min_year, "00
                     and cruise < ", max_year, "00
                     and haul_type = 3
                     and gear = 172
                     and performance >= 0")) |>
  dplyr::mutate(CURRENT_YEAR = FALSE,
                STN_STRATUM = paste0(STRATUM, "-", STATIONID)) |>
  dplyr::rename(NET_SPREAD = NET_WIDTH,
                WIRE_OUT = WIRE_LENGTH) |>
  dplyr::select(-STATIONID, -STRATUM)

hsnetnumber <- RODBC::sqlQuery(channel = con,
                               paste0("select h.net_number, h.haul, c.vessel_id as vessel, c.cruise 
                               from race_data.hauls h, 
                               race_data.cruises c, 
                               race_data.surveys s 
                               where c.cruise_id = h.cruise_id 
                               and c.survey_id = s.survey_id 
                               and s.survey_definition_id = 52 
                               and c.cruise > ", min_year, "00"))

hscatch <- RODBC::sqlQuery(channel = con,
                query = paste0("select * from racebase.catch 
                where cruise > ", min_year, "00 
                               and region = 'AI'")) |>
  dplyr::group_by(VESSEL, CRUISE, HAUL) |>
  dplyr::summarise(TOTAL_WEIGHT = sum(WEIGHT, na.rm = TRUE)) |>
  dplyr::ungroup()

hsprior <- dplyr::inner_join(hsprior, hscatch) |>
  dplyr::inner_join(hsnetnumber)

# Get station info for current year
stations2024 <- RODBC::sqlQuery(channel = con,
                                query = "select 
                                vessel, 
                                cruise, 
                                haul, 
                                wire_out,
                                stratum,
                                station as stationid,
                                net_number,
                                average_bottom_depth as bottom_depth
                                from race_data.v_extract_edit_haul
                                where region = 'AI'
                                and cruise = 202401") |>
  dplyr::mutate(STN_STRATUM = paste0(STRATUM, "-", STATIONID)) |>
  dplyr::select(-STATIONID, -STRATUM)

speed2024 <- RODBC::sqlQuery(channel = con,
                             query = "select vessel, 
                             cruise, 
                             haul, 
                             distance_fished_1_eq_hb as distance_fished, duration_eq_hb as duration
                             from race_data.v_extract_edit_hpm
                             where region = 'AI'
                                and cruise = 202401")

catch2024 <- RODBC::sqlQuery(channel = con,
                             query = "select vessel, cruise, haul, total_weight 
                             from race_data.v_extract_edit_catchall") |>
  dplyr::group_by(VESSEL, CRUISE, HAUL) |>
  dplyr::summarise(TOTAL_WEIGHT = sum(TOTAL_WEIGHT, na.rm = TRUE))

# Get SOR output
hs2024 <- read.csv(file = results_file) |>
  dplyr::rename(NET_SPREAD = EDIT_NET_SPREAD,
                NET_HEIGHT = EDIT_NET_HEIGHT) |>
  dplyr::filter(NET_SPREAD > 0) |>
  dplyr::inner_join(stations2024) |>
  dplyr::inner_join(catch2024) |>
  dplyr::inner_join(speed2024) |>
  dplyr::mutate(CURRENT_YEAR = TRUE)

all_hauls <- dplyr::bind_rows(hsprior, hs2024) |>
  dplyr::filter(STN_STRATUM %in% hs2024$STN_STRATUM,
                STN_STRATUM %in% hsprior$STN_STRATUM) |>
  dplyr::mutate(SCOPE_RATIO = WIRE_OUT/BOTTOM_DEPTH,
                SPEED = DISTANCE_FISHED/DURATION,
                CRUISE_VESSEL = paste0(CRUISE, ".", VESSEL),
                CRUISE_NET_NUMBER = paste0(CRUISE, ".", NET_NUMBER))

# Number of stations used for the comparison
length(unique(all_hauls$STN_STRATUM))


# Models with CURRENT YEAR (T/F) as a fixed effect and STN_STRATUM as a random effect ----
mod_height_stn <- brms::brm(formula = NET_HEIGHT ~ CURRENT_YEAR + (1|STN_STRATUM), data = all_hauls)

# Increased adapt_delta for divergent transitions; default in brms = 0.8
mod_spread_stn <- brms::brm(formula = NET_SPREAD ~ CURRENT_YEAR + (1|STN_STRATUM), data = all_hauls,
                            control = list(adapt_delta = 0.9)) 

plot(mod_height_stn)
summary(mod_height_stn)
fixef(mod_height_stn)

plot(mod_spread_stn)
summary(mod_spread_stn)
fixef(mod_spread_stn)

# Model used to estimate missing spread in the AI ---- NEED THE AI MODEL
mod_spread_cy_est <- mgcv::gam(formula = NET_SPREAD ~ CRUISE_VESSEL + 
                                 CRUISE_NET_NUMBER + 
                              s(BOTTOM_DEPTH) + 
                              s(SPEED) + 
                              s(SCOPE_RATIO) + 
                              s(TOTAL_WEIGHT) +
                              s(NET_HEIGHT) +
                              CURRENT_YEAR,
                            data = all_hauls)

mgcv::anova.gam(mod_spread_cy_est)

summary(mod_spread_all_hauls)


# Annual effects -- How large might we expect year effects to be?
# Fixed effect of survey
mod_height_year_stn <- brms::brm(formula = NET_HEIGHT ~ 0 + factor(CRUISE) + (1|STN_STRATUM), 
                                 data = all_hauls)

mod_spread_year_stn <- brms::brm(formula = NET_SPREAD ~ 0 + factor(CRUISE) + (1|STN_STRATUM), 
                                 data = all_hauls,
                                 control = list(adapt_delta = 0.95))

plot(mod_height_year_stn)
summary(mod_height_year_stn)
fixef(mod_height_year_stn)

plot(mod_spread_year_stn)
summary(mod_spread_year_stn)
fixef(mod_spread_year_stn)

# Difference between 
fixef(mod_spread_year_stn)[,1] - mean(fixef(mod_spread_year_stn)[,1])

# Difference between average and observed spread - bimodal distribution of differences in spread
spread_delta <- all_hauls |>
  dplyr::filter(CRUISE < 202401) |>
  dplyr::group_by(STN_STRATUM) |>
  dplyr::summarise(MEAN_SPREAD = mean(NET_SPREAD, na.rm = TRUE),
                   N = n()) |>
  dplyr::filter(N > 2) |>
  dplyr::inner_join(all_hauls |>
                      dplyr::filter(CRUISE == 202401) |>
                      dplyr::select(STN_STRATUM, NET_SPREAD, NET_NUMBER)) |>
  dplyr::mutate(SPREAD_DELTA = NET_SPREAD - MEAN_SPREAD)

ggplot() +
  geom_histogram(data = spread_delta,
                 mapping = aes(x = SPREAD_DELTA))

ggplot() +
  geom_density(data = spread_delta,
                 mapping = aes(x = SPREAD_DELTA, 
                               fill = factor(NET_NUMBER)), 
               alpha = 0.5)

# Plots
ggplot() +
  geom_boxplot(data = dplyr::filter(all_hauls, !CURRENT_YEAR),
               mapping = aes(x = STN_STRATUM, y = NET_SPREAD)) +
  geom_point(data = dplyr::filter(all_hauls, CURRENT_YEAR),
             mapping = aes(x = STN_STRATUM, y = NET_SPREAD,
                           color = CURRENT_YEAR))
