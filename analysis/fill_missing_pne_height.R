library(trawlmetrics)
library(dplyr)
library(mgcv)

channel <- trawlmetrics::get_connected(schema = "AFSC")

pne_hauls <- get_trawlmetrics(channel = channel, survey = "EBS", year = 2024, select_gear_code = 172)

bss_hauls <-  get_trawlmetrics(channel = channel, survey = "SLOPE", year = 2024, select_gear_code = 172)


fit_dat <- pne_hauls$trawl_data |>
  dplyr::mutate(NET_WIDTH = if_else(NET_WIDTH == 0, NA, NET_WIDTH),
                NET_HEIGHT = if_else(NET_HEIGHT == 0, NA, NET_HEIGHT),
                BOTTOM_DEPTH = if_else(BOTTOM_DEPTH == 0, NA, BOTTOM_DEPTH))


# Select height and spread models to fill missing data
h1 <- mgcv::gam(NET_HEIGHT ~ s(I(WIRE_LENGTH/BOTTOM_DEPTH)) + 
                  s(BOTTOM_DEPTH), 
                data = fit_dat)

h2 <- mgcv::gam(NET_HEIGHT ~ s(I(WIRE_LENGTH/BOTTOM_DEPTH)), 
                data = fit_dat)

h3 <- mgcv::gam(NET_HEIGHT ~ s(BOTTOM_DEPTH), 
                data = fit_dat)

h4 <- mgcv::gam(NET_HEIGHT ~ 1, 
                data = fit_dat)

# Use h3 to fill missing height
AIC(h1, h2, h3, h4)

height_mod <- h3


s1 <- mgcv::gam(NET_WIDTH ~ s(NET_HEIGHT), 
                data = fit_dat)

s2 <- mgcv::gam(NET_WIDTH ~ s(NET_HEIGHT) + s(I(WIRE_LENGTH/BOTTOM_DEPTH)), 
                data = fit_dat)

s3 <- mgcv::gam(NET_WIDTH ~ s(NET_HEIGHT) + s(BOTTOM_DEPTH), 
                data = fit_dat)

# Use s3 to fill missing spread
AIC(s1, s2, s3)

spread_mod <- s3

plot(spread_mod)
plot(height_mod)

summary(spread_mod)
summary(height_mod)

filled_dat <- fit_dat

filled_dat$NET_HEIGHT[is.na(filled_dat$NET_HEIGHT)] <- predict(height_mod, 
                                                               newdata = filled_dat[is.na(filled_dat$NET_HEIGHT), ])

filled_dat$NET_WIDTH[is.na(filled_dat$NET_WIDTH)] <- predict(spread_mod, 
                                                             newdata = filled_dat[is.na(filled_dat$NET_WIDTH), ])

filled_dat[13:14,]


ggplot() +
  geom_point(data = filled_dat,
             mapping = aes(x = WIRE_LENGTH, 
                           y = NET_WIDTH, 
                           shape = NET_MEASURED))

ggplot() +
  geom_point(data = filled_dat,
             mapping = aes(x = BOTTOM_DEPTH, 
                           y = NET_WIDTH, 
                           shape = NET_MEASURED))


ggplot() +
  geom_point(data = filled_dat,
             mapping = aes(x = WIRE_LENGTH/BOTTOM_DEPTH, 
                           y = NET_WIDTH, 
                           shape = NET_MEASURED))
