# Visualize flume tank results versus observed survey data

library(trawlmetrics)
library(xlsx)
library(ggthemes)

# ggthemes::colorblind_pal()(8)
#colors
"#000000" "#E69F00" "#56B4E9" "#009E73" "#F0E442" "#0072B2" "#D55E00" "#CC79A7"

# Load flume tank data

survey_id <- c(47, 52, 98, 143)
trawl_name <- c("PNE", "83-112")
sheet_flume_xlsx <- "data"
# sheet_flume_xlsx <- "example"

path_flume_xlsx <- here::here("analysis", "flume_tank", "data", "flume_tank_data.xlsx")

gear_table <- data.frame(trawl = c("PNE", "83-112"),
                         GEAR_NAME = c("PNE", "83-112"))

flume_data <- xlsx::read.xlsx(file = path_flume_xlsx,
                              sheetName = sheet_flume_xlsx) |>
  dplyr::filter(!is.na(bridles), 
                pulling_point_elevation_mm < 300,
                additional_floatation_kg == 0, 
                trial > 0,
                bridles != "2-weighted") |>
  dplyr::inner_join(gear_table) |>
  dplyr::mutate(type = "Flume tank",
                fac_trial = factor(trial),
                type = paste0("Flume ", bridles))

flume_data <- flume_data |> 
  dplyr::filter(trawl %in% trawl_name)

# Plot field vs. flume height/spread ----


fit_models <- function(data_survey,
                       data_flume,
                       survey_definition_id,
                       flume_trawl) {
  
  data_survey <- trawlmetrics::bts_geom
  data_flume <- flume_data
  survey_definition_id = c(98, 143)
  flume_trawl = "83-112"
  
  # Height ~ spread at 3 knots ----
  data_survey <- data_survey |> 
    dplyr::filter(SURVEY_DEFINITION_ID %in% survey_definition_id) |>
    dplyr::mutate(TOWING_SPEED_KN = DISTANCE_FISHED_KM/DURATION_HR/1.852)
  
  data_flume_3kn <- dplyr::filter(data_flume,
                              trawl %in% flume_trawl,
                              towing_speed_kn == 3)
  
  lm_hs_survey <- lm(
    formula = NET_HEIGHT_M ~ NET_WIDTH_M, 
    data = data_survey
  )
  
  lm_hs_flume <- lm(
    formula = opening_headline_m ~ spread_u_wing_m,
    data = data_flume
  )
  
  summary(lm_survey)
  summary(lm_flume)
  
  # Towing speed versus height at fixed spread
  
  lm_height_speed_survey <- 
    lm(formula = NET_HEIGHT_M ~ TOWING_SPEED_KN,
       data = data_survey)
  
  lm_height_speed_flume <- 
    lm(formula = opening_headline_m ~ towing_speed_kn,
       data = data_flume)
  
}


lm_spread_v_height_3kn <- 
  lm(NET_HEIGHT_M ~ NET_WIDTH_M,
     data = dplyr::filter(
       trawlmetrics::bts_geom,
       
                          )
     )


ggplot() +
  geom_point(data = trawlmetrics::bts_geom |>
               dplyr::filter(SURVEY_DEFINITION_ID %in% survey_id) |>
               dplyr::mutate(type = "Survey"),
             mapping = aes(x = NET_WIDTH_M,
                           y = NET_HEIGHT_M,
                           color = type),
             alpha = 0.1,
             size = 0.1) +
  geom_smooth(data = trawlmetrics::bts_geom |>
                dplyr::filter(SURVEY_DEFINITION_ID %in% survey_id) |>
                dplyr::mutate(type = "Survey"),
              mapping = aes(x = NET_WIDTH_M,
                            y = NET_HEIGHT_M,
                            color = type)) +
  geom_point(data = dplyr::filter(flume_data, towing_speed_kn == 3),
             mapping = aes(x = spread_u_wing_m,
                           y = opening_headline_m,
                           color = type)) +
  geom_smooth(data = dplyr::filter(flume_data, towing_speed_kn == 3),
             mapping = aes(x = spread_u_wing_m,
                           y = opening_headline_m,
                           color = type), 
             method = 'lm') +
  facet_wrap(~GEAR_NAME, scales = "free") +
  scale_x_continuous(name = "Net width (m)") +
  scale_y_continuous(name = "Net height (m)") +
  scale_color_manual(name = "Type",
                     values = c('Survey' = "grey70",
                                'Flume standard' = "#E69F00",
                                'Flume 2-weighted' = "#CC79A7",
                                'Flume 2' = "#0072B2")) +
  theme_bw()

ggplot() +
  geom_point(data = trawlmetrics::bts_geom |>
               dplyr::filter(SURVEY_DEFINITION_ID %in% survey_id) |>
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
  scale_color_colorblind(name = "Type")
  # scale_color_manual(name = "Type",
  #                    values = c("Survey" = "#000000", 
  #                               "Flume tank" = "#E69F00"))

ggplot() +
  geom_point(data = trawlmetrics::bts_geom |>
               dplyr::filter(SURVEY_DEFINITION_ID %in% survey_id) |>
               dplyr::mutate(type = "Survey"),
             mapping = aes(x = NET_WIDTH_M,
                           y = NET_HEIGHT_M),
             alpha = 0.5,
             size = 0.7, color = "grey70") +
  geom_point(data = dplyr::filter(flume_data, catch == "empty"),
             mapping = aes(x = spread_u_wing_m,
                           y = opening_headline_m,
                           color = factor(towing_speed_kn),
                           shape = factor(additional_floatation_kg)), size = rel(3)) +
  facet_wrap(~paste0("Empty net - ", GEAR_NAME), scales = "free") +
  scale_x_continuous(name = "Net width (m)") +
  scale_y_continuous(name = "Net height (m)") +
  scale_color_viridis_d(name = "Towing speed (kn)", option = "C") +
  theme_bw()

ggplot() +
  geom_density2d_filled(data = trawlmetrics::bts_geom |>
               dplyr::filter(SURVEY_DEFINITION_ID %in% survey_id) |>
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
  # scale_fill_brewer() +
  scale_color_colorblind(name = "Type")
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


#


ggplot() +
  geom_boxplot(data = trawlmetrics::bts_geom |>
               dplyr::filter(SURVEY_DEFINITION_ID %in% survey_id),
               mapping = aes(x = YEAR, group = YEAR, y = NET_HEIGHT_M))

ggplot() +
  geom_boxplot(data = trawlmetrics::bts_geom |>
                 dplyr::filter(SURVEY_DEFINITION_ID %in% survey_id),
               mapping = aes(x = YEAR, group = YEAR, y = NET_WIDTH_M))
