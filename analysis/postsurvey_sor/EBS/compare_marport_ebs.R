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
results_file <- here::here("analysis", 
                           "postsurvey_sor",
                           "EBS",
                           "race_data_edit_hauls_table_EBS_2024_no_M2N.csv")


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
                                station as stationid
                                from race_data.v_extract_edit_haul
                                where region = 'BS'
                                and cruise = 202401")

# Get SOR output
hs2024 <- read.csv(file = results_file) |>
  dplyr::rename(NET_SPREAD = EDIT_NET_SPREAD,
                NET_HEIGHT = EDIT_NET_HEIGHT) |>
  dplyr::inner_join(stations2024) |>
  dplyr::mutate(CURRENT_YEAR = TRUE) |>
  dplyr::filter(STATIONID %in% akgfmaps::get_survey_stations(select.region = "sebs"))

all_hauls <- dplyr::bind_rows(hsprior, hs2024) |>
  dplyr::filter(STATIONID %in% hs2024$STATIONID) |>
  dplyr::mutate(STATIONID = as.factor(STATIONID))


# Models with CURRENT YEAR (T/F) as a fixed effect and STATIONID as a random effect ----
mod_height_stn <- brms::brm(formula = NET_HEIGHT ~ CURRENT_YEAR + (1|STATIONID), data = all_hauls)

mod_spread_stn <- brms::brm(formula = NET_SPREAD ~ CURRENT_YEAR + (1|STATIONID), data = all_hauls)

plot(mod_height_stn)
summary(mod_height_stn)
fixef(mod_height_stn)

plot(mod_spread_stn)
summary(mod_spread_stn)
fixef(mod_spread_stn)

# Model used to estimate missing spread in the EBS ----
mod_spread_est <- glm(formula = NET_SPREAD ~ 0 + CURRENT_YEAR + I(1/WIRE_OUT) + 
      NET_HEIGHT + 
      I(1/WIRE_OUT*NET_HEIGHT),
    data = all_hauls)

mod_spread_no_year_est <- glm(formula = NET_SPREAD ~ I(1/WIRE_OUT) + 
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
