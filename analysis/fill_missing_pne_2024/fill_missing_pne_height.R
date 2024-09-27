# Fill missing height and spread for slope gear sampling during 2023 and 2024 shelf/slope tows
# Created by: Sean Rohan
# Date: September 27, 2024

library(trawlmetrics) # v0.0.3
library(dplyr)
library(mgcv)

dir.create(path = here::here("analysis", "fill_missing_pne_2024"),
           showWarnings = FALSE)

CRUISE_ID <- 767

channel <- trawlmetrics::get_connected(schema = "AFSC")

# Retrieving PNE hauls from 2024
pne_hauls <- trawlmetrics::get_trawlmetrics(channel = channel, 
                                            survey = "EBS", 
                                            year = 2024, 
                                            select_gear_code = 172)

bss_hauls <- trawlmetrics::get_trawlmetrics(channel = channel, 
                                            survey = "SLOPE", 
                                            year = 2024, 
                                            select_gear_code = 172)


fit_dat <- pne_hauls$trawl_data |>
  dplyr::filter(HAUL < 100 | HAUL > 200, CRUISE == 202401) |> # Exclude hauls that used the wrong footrope
  dplyr::mutate(NET_WIDTH = if_else(NET_WIDTH == 0, NA, NET_WIDTH),
                NET_HEIGHT = if_else(NET_HEIGHT == 0, NA, NET_HEIGHT),
                BOTTOM_DEPTH = if_else(BOTTOM_DEPTH == 0, NA, BOTTOM_DEPTH)) |>
  dplyr::mutate(NET_WIDTH = if_else(NET_HEIGHT < 5, NA, NET_WIDTH)) |> # Remove data from hauls with 83-112 pings
  dplyr::mutate(NET_HEIGHT = if_else(NET_HEIGHT < 5, NA, NET_HEIGHT)) # Remove data from hauls with 83-112 pings

head(fit_dat)

# Select height model to fill missing data
h1 <- mgcv::gam(NET_HEIGHT ~ s(I(WIRE_LENGTH/BOTTOM_DEPTH)) + 
                  s(BOTTOM_DEPTH), 
                data = fit_dat)

h2 <- mgcv::gam(NET_HEIGHT ~ s(I(WIRE_LENGTH/BOTTOM_DEPTH)), 
                data = fit_dat)

h3 <- mgcv::gam(NET_HEIGHT ~ s(BOTTOM_DEPTH), 
                data = fit_dat)

h4 <- mgcv::gam(NET_HEIGHT ~ 1, 
                data = fit_dat)

h5 <- lm(NET_HEIGHT ~ BOTTOM_DEPTH, data = fit_dat)

# Use h2 to fill missing height
AIC(h1, h2, h3, h4)

height_mod <- h3

gam.check(height_mod)
qq.gam(height_mod)

plot(height_mod)

summary(height_mod)


# Select spread model to fill missing data
s1 <- mgcv::gam(NET_WIDTH ~ s(NET_HEIGHT), 
                data = fit_dat)

s2 <- mgcv::gam(NET_WIDTH ~ s(NET_HEIGHT) + s(I(WIRE_LENGTH/BOTTOM_DEPTH)), 
                data = fit_dat)

s3 <- mgcv::gam(NET_WIDTH ~ s(NET_HEIGHT) + s(BOTTOM_DEPTH), 
                data = fit_dat)

s4 <- mgcv::gam(NET_WIDTH ~ s(NET_HEIGHT) + s(BOTTOM_DEPTH) + s(I(WIRE_LENGTH/BOTTOM_DEPTH)), 
                data = fit_dat)

s5 <- lm(NET_WIDTH ~ 1, data = fit_dat)

# Use s2 to fill missing spread
AIC(s1, s2, s3, s4, s5)

spread_mod <- s2

gam.check(spread_mod)

qq.gam(spread_mod)

plot(spread_mod)

summary(spread_mod)



# Fill missing height and spread values
filled_dat <- fit_dat

filled_dat$NET_HEIGHT_METHOD <- 6
filled_dat$NET_HEIGHT_METHOD[is.na(filled_dat$NET_HEIGHT)] <- 4

filled_dat$NET_SPREAD_METHOD <- 6
filled_dat$NET_SPREAD_METHOD[is.na(filled_dat$NET_WIDTH)] <- 4


filled_dat$NET_HEIGHT[is.na(filled_dat$NET_HEIGHT)] <- predict(height_mod, 
                                                               newdata = filled_dat[is.na(filled_dat$NET_HEIGHT), ])

filled_dat$NET_WIDTH[is.na(filled_dat$NET_WIDTH)] <- predict(spread_mod, 
                                                             newdata = filled_dat[is.na(filled_dat$NET_WIDTH), ])


output_dat <- data.frame(CRUISE_ID = CRUISE_ID,
                         HAUL_ID = filled_dat$HAUL_ID,
                         CRUISE = filled_dat$CRUISE,
                         VESSEL = filled_dat$VESSEL,
                         HAUL = filled_dat$HAUL,
                         EDIT_NET_SPREAD = filled_dat$NET_WIDTH,
                         EDIT_NET_HEIGHT = filled_dat$NET_HEIGHT,
                         NET_SPREAD_PINGS = filled_dat$NET_SPREAD_PINGS,
                         NET_SPREAD_METHOD = filled_dat$NET_SPREAD_METHOD,
                         NET_SPREAD_STANDARD_DEVIATION = filled_dat$NET_SPREAD_STANDARD_DEVIATION,
                         EDIT_NET_HEIGHT = filled_dat$NET_HEIGHT,
                         NET_HEIGHT_METHOD = filled_dat$NET_HEIGHT_METHOD,
                         NET_HEIGHT_PINGS = filled_dat$NET_HEIGHT_PINGS,
                         NET_HEIGHT_STANDARD_DEVIATION = filled_dat$NET_HEIGHT_STANDARD_DEVIATION,
                         CREATE_DATE = Sys.time(),
                         CREATE_USER = "ROHANS")

# Spread plots
ggplot() +
  geom_point(data = filled_dat,
             mapping = aes(x = WIRE_LENGTH, 
                           y = NET_WIDTH, 
                           color = factor(NET_SPREAD_METHOD),
                           shape = factor(NET_SPREAD_METHOD))) +
  theme_bw()

ggplot() +
  geom_point(data = filled_dat,
             mapping = aes(x = BOTTOM_DEPTH, 
                           y = NET_WIDTH, 
                           color = factor(NET_SPREAD_METHOD),
                           shape = factor(NET_SPREAD_METHOD))) +
  theme_bw()

ggplot() +
  geom_point(data = filled_dat,
             mapping = aes(x = WIRE_LENGTH/BOTTOM_DEPTH, 
                           y = NET_WIDTH, 
                           color = factor(NET_SPREAD_METHOD),
                           shape = factor(NET_SPREAD_METHOD))) +
  theme_bw()

# Height plots
ggplot() +
  geom_point(data = filled_dat,
             mapping = aes(x = WIRE_LENGTH, 
                           y = NET_HEIGHT, 
                           color = factor(NET_HEIGHT_METHOD),
                           shape = factor(NET_HEIGHT_METHOD))) +
  theme_bw()

ggplot() +
  geom_point(data = filled_dat,
             mapping = aes(x = BOTTOM_DEPTH, 
                           y = NET_HEIGHT, 
                           color = factor(NET_HEIGHT_METHOD),
                           shape = factor(NET_HEIGHT_METHOD))) +
  theme_bw()

ggplot() +
  geom_point(data = filled_dat,
             mapping = aes(x = WIRE_LENGTH/BOTTOM_DEPTH, 
                           y = NET_HEIGHT, 
                           color = factor(NET_HEIGHT_METHOD),
                           shape = factor(NET_HEIGHT_METHOD))) +
  theme_bw()


# Height versus spread plot
ggplot() +
  geom_point(data = filled_dat,
             mapping = aes(x = NET_HEIGHT,
                           y = NET_WIDTH, 
                           color = factor(NET_SPREAD_METHOD),
                           shape = factor(NET_SPREAD_METHOD))) +
  theme_bw()


write.csv(output_dat,
          file = here::here("analysis", "fill_missing_pne_2024", "race_data_edit_hauls_table_EBS_PNE_2024.csv"),
          row.names = FALSE)
