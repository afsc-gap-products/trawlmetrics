# Check for spread differences between old and new Marport sensors
# Created by Sean Rohan <sean.rohan@noaa.gov>
# July 16, 2024

library(trawlmetrics)
library(akgfmaps)
library(lme4)
library(RODBC)
library(brms)

min_year <- 2014
max_year <- 2024

# File *WITH* Marport to Netmind conversion
results_file <- here::here("analysis", 
                           "postsurvey_sor",
                           "EBS",
                           "race_data_edit_hauls_table_EBS_2024.csv")

# File *WITHOUT* Marport to Netmind conversion
# results_file <- here::here("analysis", 
#                            "postsurvey_sor",
#                            "EBS",
#                            "race_data_edit_hauls_table_EBS_2024_no_M2N.csv")


# Retrieve data ----
con <- trawlmetrics::get_connected(schema = "AFSC")

hsprior <- RODBC::sqlQuery(channel = con,
                     query = paste0("select * from racebase.haul 
                     where cruise > ", min_year, "00
                     and cruise < ", max_year, "00
                     and haul_type = 3
                     and gear = 44
                     and performance >= 0")) |>
  dplyr::mutate(CURRENT_YEAR = FALSE) |>
  dplyr::rename(NET_SPREAD = NET_WIDTH,
                WIRE_OUT = WIRE_LENGTH) |>
  dplyr::filter(STATIONID %in% akgfmaps::get_survey_stations(select.region = "sebs"))

# Get station info for current year
stations2024 <- RODBC::sqlQuery(channel = con,
                                query = "select 
                                vessel, 
                                cruise, 
                                haul, 
                                wire_out,
                                station as stationid,
                                net_number,
                                average_bottom_depth as bottom_depth
                                from race_data.v_extract_edit_haul
                                where region = 'BS'
                                and cruise = 202401") |>
  dplyr::mutate(WIRE_OUT = WIRE_OUT * 1.8288)

# Get SOR output
hs2024 <- read.csv(file = results_file) |>
  dplyr::rename(NET_SPREAD = EDIT_NET_SPREAD,
                NET_HEIGHT = EDIT_NET_HEIGHT) |>
  dplyr::inner_join(stations2024) |>
  dplyr::mutate(CURRENT_YEAR = TRUE) |>
  dplyr::filter(STATIONID %in% akgfmaps::get_survey_stations(select.region = "sebs"))

all_hauls <- dplyr::bind_rows(hsprior, hs2024) |>
  dplyr::filter(STATIONID %in% hs2024$STATIONID) |>
  dplyr::mutate(STATIONID = as.factor(STATIONID),
                SCOPE_RATIO = WIRE_OUT/BOTTOM_DEPTH)

# Scope to depth ratio - No difference in scope ratios
ggplot() +
  geom_point(data = all_hauls,
             mapping = aes(x = BOTTOM_DEPTH, y = SCOPE_RATIO,
                           color = CURRENT_YEAR),
             alpha = 0.5)

# Models with CURRENT YEAR (T/F) as a fixed effect and STATIONID as a random effect ----
mod_height_stn <- brms::brm(formula = NET_HEIGHT ~ CURRENT_YEAR + (1|STATIONID), data = all_hauls)

mod_spread_stn <- brms::brm(formula = NET_SPREAD ~ CURRENT_YEAR + (1|STATIONID), data = all_hauls)

plot(mod_height_stn)
summary(mod_height_stn)
fixef(mod_height_stn)

plot(mod_spread_stn)
summary(mod_spread_stn)
fixef(mod_spread_stn)

# Are residuals patterns associated with differences in net geometry?
# Perhaps. Some overspreading, some underspreading.

mod_spread_stn_no_year <- brms::brm(formula = NET_SPREAD ~ STATIONID, 
                                    data = all_hauls)

all_hauls$RESID_NET_SPREAD <- resid(mod_spread_stn_no_year)[,1]

ggplot() +
  geom_point(data = dplyr::filter(all_hauls, CURRENT_YEAR), 
             mapping = aes(x = HAUL, 
                           y = RESID_NET_SPREAD, 
                           color = factor(NET_NUMBER))) +
  geom_hline(yintercept = 0, 
             linetype = 2) +
  facet_grid(~VESSEL)

ggplot() +
  geom_boxplot(data = dplyr::filter(all_hauls, CURRENT_YEAR), 
             mapping = aes(x = NET_NUMBER, 
                           y = RESID_NET_SPREAD,
                           color = factor(NET_NUMBER))) +
  geom_hline(yintercept = 0, linetype = 2) +
  facet_grid(~VESSEL)

ggplot() +
  geom_density(data = all_hauls,
                 mapping = aes(x = RESID_NET_SPREAD, fill = CURRENT_YEAR), alpha = 0.5)


# Model used to estimate missing spread in the EBS ----
mod_spread_est <- glm(formula = NET_SPREAD ~ 0 + CURRENT_YEAR + I(1/WIRE_OUT) + 
      NET_HEIGHT + 
      I(1/WIRE_OUT*NET_HEIGHT),
    data = all_hauls)

mod_spread_no_year_est <- glm(formula = NET_SPREAD ~ 0 + I(1/WIRE_OUT) + 
                        NET_HEIGHT + 
                        I(1/WIRE_OUT*NET_HEIGHT),
                      data = all_hauls)

AIC(mod_spread_est, mod_spread_no_year_est)

anova(mod_spread_no_year_est, mod_spread_est, test = "Chisq") # No evidence of YEAR effect

# Vessel effect?
mod_spread_est <- glm(formula = NET_SPREAD ~ 0 + CURRENT_YEAR:factor(VESSEL) + I(1/WIRE_OUT) + 
                        NET_HEIGHT + 
                        I(1/WIRE_OUT*NET_HEIGHT),
                      data = all_hauls)

# Annual effects -- How large might we expect year effects to be?
# Fixed effect of survey
mod_height_year_stn <- brms::brm(formula = NET_HEIGHT ~ 0 + factor(CRUISE) + (1|STATIONID), data = all_hauls)

mod_spread_year_stn <- brms::brm(formula = NET_SPREAD ~ 0 + factor(CRUISE) + (1|STATIONID), data = all_hauls)

plot(mod_height_year_stn)
summary(mod_height_year_stn)
fixef(mod_height_year_stn)

plot(mod_spread_year_stn)
summary(mod_spread_year_stn)
fixef(mod_spread_year_stn)


# Difference between 2024 and mean among other years
fixef(mod_spread_year_stn)[,1] - mean(fixef(mod_spread_year_stn)[,1])

# Plots
ggplot() +
  geom_boxplot(data = dplyr::filter(all_hauls, !CURRENT_YEAR),
               mapping = aes(x = STATIONID, y = NET_SPREAD)) +
  geom_point(data = dplyr::filter(all_hauls, CURRENT_YEAR),
             mapping = aes(x = STATIONID, y = NET_SPREAD,
                           color = CURRENT_YEAR))
