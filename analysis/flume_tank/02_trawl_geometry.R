# Visualize flume tank results

library(trawlmetrics)
library(xlsx)
library(ggthemes)

# ggthemes::colorblind_pal()(8)
#colors
# "#000000" "#E69F00" "#56B4E9" "#009E73" "#F0E442" "#0072B2" "#D55E00" "#CC79A7"

# Load flume tank data

path_flume_xlsx <- here::here("analysis", "flume_tank", "data", "flume_tank_data.xlsx")
sheet_flume_xlsx <- "data"
# sheet_flume_xlsx <- "example"

flume_data <- xlsx::read.xlsx(file = path_flume_xlsx,
                              sheetName = sheet_flume_xlsx) |>
  dplyr::mutate(GEAR_NAME = "83-112",
                type = "Flume tank",
                fac_trial = factor(trial))

# Plot field vs. flume height/spread ----
ggplot() +
  geom_point(data = trawlmetrics::bts_geom |>
               dplyr::filter(SURVEY_DEFINITION_ID != 78) |>
               dplyr::mutate(type = "Survey"),
             mapping = aes(x = NET_WIDTH_M,
                           y = NET_HEIGHT_M,
                           color = type),
             alpha = 0.5,
             size = 0.1) +
  geom_point(data = flume_data,
             mapping = aes(x = spread_u_wing_m,
                           y = opening_headline_m,
                           color = type)) +
  facet_wrap(~GEAR_NAME, scales = "free") +
  scale_x_continuous(name = "Net width (m)") +
  scale_y_continuous(name = "Net height (m)") +
  scale_color_manual(name = "Type",
                     values = c("Survey" = "#000000", 
                                "Flume tank" = "#E69F00"))

ggplot() +
  geom_point(data = trawlmetrics::bts_geom |>
               dplyr::filter(SURVEY_DEFINITION_ID %in% c(98, 143)) |>
               dplyr::mutate(type = "Survey"),
             mapping = aes(x = NET_WIDTH_M,
                           y = NET_HEIGHT_M,
                           color = type),
             alpha = 0.5,
             size = 0.1) +
  geom_point(data = dplyr::filter(flume_data, catch == "empty"),
             mapping = aes(x = spread_u_wing_m,
                           y = opening_headline_m,
                           color = type)) +
  facet_wrap(~GEAR_NAME, scales = "free") +
  scale_x_continuous(name = "Net width (m)") +
  scale_y_continuous(name = "Net height (m)") +
  scale_color_manual(name = "Type",
                     values = c("Survey" = "#000000", 
                                "Flume tank" = "#E69F00"))

ggplot() +
  geom_point(data = trawlmetrics::bts_geom |>
               dplyr::filter(SURVEY_DEFINITION_ID %in% c(98, 143)) |>
               dplyr::mutate(type = "Survey"),
             mapping = aes(x = NET_WIDTH_M,
                           y = NET_HEIGHT_M),
             alpha = 0.5,
             size = 0.7, color = "grey70") +
  geom_point(data = dplyr::filter(flume_data, catch == "empty"),
             mapping = aes(x = spread_u_wing_m,
                           y = opening_headline_m,
                           color = factor(towing_speed_kn)), size = rel(3)) +
  facet_wrap(~paste0("Empty net - ", GEAR_NAME), scales = "free") +
  scale_x_continuous(name = "Net width (m)") +
  scale_y_continuous(name = "Net height (m)") +
  scale_color_viridis_d(name = "Towing speed (kn)", option = "C") +
  theme_bw()

ggplot() +
  geom_density2d_filled(data = trawlmetrics::bts_geom |>
               dplyr::filter(SURVEY_DEFINITION_ID %in% c(98, 143)) |>
               dplyr::mutate(type = "Survey"),
             mapping = aes(x = NET_WIDTH_M,
                           y = NET_HEIGHT_M),
             size = 0.7, 
             color = NA) +
  geom_point(data = dplyr::filter(flume_data, catch == "empty"),
             mapping = aes(x = spread_u_wing_m,
                           y = opening_headline_m,
                           color = factor(towing_speed_kn)), size = rel(3)) +
  facet_wrap(~paste0("Empty net - ", GEAR_NAME), scales = "free") +
  scale_x_continuous(name = "Net width (m)") +
  scale_y_continuous(name = "Net height (m)") +
  scale_color_viridis_d(name = "Towing speed (kn)", option = "C") +
  scale_fill_brewer() +
  theme_dark()

ggplot() +
  geom_point(data = dplyr::filter(flume_data, towing_speed_kn == 3),
             mapping = aes(x = spread_u_wing_m,
                           y = opening_headline_m,
                           color = catch), size = rel(3)) +
  facet_wrap(~paste0("Empty net - ", GEAR_NAME), scales = "free") +
  scale_x_continuous(name = "Net width (m)") +
  scale_y_continuous(name = "Net height (m)") +
  scale_color_viridis_d(name = "Catch trt.", option = "C") +
  scale_fill_brewer() +
  theme_bw()


# Effect of tow speed on trawl geometry across spread values

mod_test_spread_gam <- mgcv::gam(
  formula = spread_mean_we_m ~ s(towing_speed_kn, by = fac_trial, bs = "tp", k = 3),
  data = flume_data, 
  method = "ML"
    )

mod_test_spread_lm <- lm(
  formula = spread_mean_we_m ~ towing_speed_kn:fac_trial,
  data = flume_data
)

AIC(mod_test_spread_gam, mod_test_spread_lm)


summary(mod_test_spread_lm)

plot(mod_test_spread_gam)

    
# 

ggplot() +
  geom_point(data = flume_data,
             mapping = aes(x = towing_speed_kn,
                           y = spread_mean_we_m)) +
  facet_wrap(~round(spread_door_m))

ggplot(data = flume_data,
       mapping = aes(x = bridle_angle_deg,
                     y = spread_mean_we_m,
                     color = factor(towing_speed_kn))) +
  geom_point() +
  geom_smooth()

ggplot(data = flume_data,
       mapping = aes(x = bridle_angle_deg,
                     y = spread_mean_we_m,
                     color = factor(towing_speed_kn))) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~towing_speed_kn)
