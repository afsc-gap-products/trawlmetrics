# Check for spread differences between old and new Marport sensors in the AI
# Created by Sean Rohan <sean.rohan@noaa.gov>
# July 18, 2024

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
  dplyr::select(-STATIONID, -STRATUM) |>
  dplyr::mutate(WIRE_OUT = WIRE_OUT * 1.8288)

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

nrow(hs2024)

all_hauls <- dplyr::bind_rows(hsprior, hs2024) |>
  dplyr::filter(STN_STRATUM %in% hs2024$STN_STRATUM,
                STN_STRATUM %in% hsprior$STN_STRATUM) |>
  dplyr::mutate(SCOPE_RATIO = WIRE_OUT/BOTTOM_DEPTH,
                SPEED = DISTANCE_FISHED/DURATION,
                CRUISE_VESSEL = paste0(CRUISE, ".", VESSEL),
                CRUISE_NET_NUMBER = paste0(CRUISE, ".", NET_NUMBER)) |>
  dplyr::filter(SCOPE_RATIO < 15)

# Number of stations used for the comparison
length(unique(all_hauls$STN_STRATUM))

# Scope to depth ratio - No difference in scope ratios
(p_scope_ratios <- ggplot() +
    geom_point(data = all_hauls,
               mapping = aes(x = BOTTOM_DEPTH, y = SCOPE_RATIO,
                             color = CURRENT_YEAR),
               alpha = 0.5) +
    scale_y_continuous(limits = c(1,8)))

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

# Are residuals patterns associated with differences in net geometry?
# Perhaps. Some overspreading, some underspreading.

mod_spread_stn_no_year <- brms::brm(formula = NET_SPREAD ~ 0 + STN_STRATUM, 
                                    data = all_hauls,
                                    iter = 1e4)

all_hauls$RESID_NET_SPREAD <- resid(mod_spread_stn_no_year)[,1]

(p_resid_by_haul <- ggplot() +
    geom_point(data = dplyr::filter(all_hauls, CURRENT_YEAR), 
               mapping = aes(x = HAUL, 
                             y = RESID_NET_SPREAD, 
                             color = factor(NET_NUMBER))) +
    geom_hline(yintercept = 0, 
               linetype = 2) +
    facet_grid(~VESSEL) +
    ggtitle("AI spread residual by haul"))

(p_resid_by_net <- ggplot() +
    geom_boxplot(data = dplyr::filter(all_hauls, CURRENT_YEAR), 
                 mapping = aes(x = NET_NUMBER, 
                               y = RESID_NET_SPREAD,
                               color = factor(NET_NUMBER))) +
    geom_hline(yintercept = 0, linetype = 2) +
    facet_grid(~VESSEL) +
    ggtitle("AI spread residual by net"))

(p_resid_by_year <- ggplot() +
    geom_density(data = all_hauls,
                 mapping = aes(x = RESID_NET_SPREAD, fill = CURRENT_YEAR), 
                 alpha = 0.5) +
    ggtitle("AI spread residual 2024 vs. historical"))

(p_resid_all_years <- ggplot() +
    geom_boxplot(data = all_hauls,
                 mapping = aes(x = floor(CRUISE/100), 
                               y = RESID_NET_SPREAD,
                               group = CRUISE)) +
    scale_x_continuous(name = "Year", breaks = seq(2010, 2024, 2)) +
    ggtitle("AI spread residuals by year"))

pdf(here::here("analysis", "postsurvey_sor", "AI", "AI_spread_resids.pdf"), 
    width = 7.5, height = 10.5)
print(
  cowplot::plot_grid(p_resid_by_haul,
                     p_resid_by_net,
                     p_resid_by_year,
                     nrow = 3)
)
dev.off()

pdf(here::here("analysis", "postsurvey_sor", "AI", "AI_resids_by_year.pdf"), 
    width = 7.5, height = 5)
print(p_resid_all_years)
dev.off()

# Model used to estimate missing spread in the AI
mod_spread_cy_est <- mgcv::gam(formula = NET_SPREAD ~ 0 + CRUISE_VESSEL + 
                              s(BOTTOM_DEPTH) + 
                              s(SPEED) + 
                              s(SCOPE_RATIO) + 
                              s(TOTAL_WEIGHT) +
                              s(NET_HEIGHT),
                            data = all_hauls)

mgcv::anova.gam(mod_spread_cy_est)

summary(mod_spread_cy_est)

mod_spread_cy_est$coefficients[1:12] - mean(mod_spread_cy_est$coefficients[1:12])


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

# Plots
(p_spread_by_station <- ggplot() +
  geom_boxplot(data = dplyr::filter(all_hauls, !CURRENT_YEAR),
               mapping = aes(x = STN_STRATUM, y = NET_SPREAD)) +
  geom_point(data = dplyr::filter(all_hauls, CURRENT_YEAR),
             mapping = aes(x = STN_STRATUM, y = NET_SPREAD,
                           color = CURRENT_YEAR)))
