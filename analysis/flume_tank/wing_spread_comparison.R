library(trawlmetrics)
library(gapindex)
library(akgfmaps)

sebs_layers <- akgfmaps::get_base_layers(select.region = "sebs", set.crs = "EPSG:3338")

dat <- trawlmetrics::flume_tank |> 
  dplyr::filter(footrope %in% c("83-112", "PNE", "EBS_v2", "GOA/AI_v6"), 
                catch == "empty",
                pulling_point_elevation_mm <= 200,
                additional_floatation_kg == 0, 
                !(bridles == 2 & trawl == "83-112"),
                !(bridles == 2 & trawl == "PNE"),
                !(bridles == "2-weighted" & trawl == "PNE"),
                trial > 0,
                !benthic_bag)

lm_wing_83112 <- 
  lm(
    spread_l_wing_m ~ spread_u_wing_m, 
    data = dplyr::filter(dat, trawl == "83-112")
  )

ebs_dat <- gapindex::get_data(year_set = 1982:2024,
                              survey_set = "EBS")

haul_df <- ebs_dat$haul |>
  dplyr::mutate(YEAR = floor(CRUISE/100))

haul_df$NET_WIDTH_LWR_EST <- 
  predict(
    lm_wing_83112,
    newdata = data.frame(spread_u_wing_m = haul_df$NET_WIDTH)
    )

haul_df$PCT_DIFF <- (haul_df$NET_WIDTH_LWR_EST - haul_df$NET_WIDTH)/haul_df$NET_WIDTH * 100

haul_sf <- haul_df |>
  sf::st_as_sf(
    coords = c("START_LONGITUDE", "START_LATITUDE"), 
    crs = "WGS84")

station_diff <- haul_df |>
  dplyr::group_by(STATIONID) |>
  dplyr::summarise(MEDIAN_DIFF = median(PCT_DIFF))

station_diff <- sebs_layers$survey.grid |>
  dplyr::inner_join(station_diff |> 
                      dplyr::select(STATION = STATIONID, MEDIAN_DIFF),
                    by = "STATION")

ggplot() +
  geom_histogram(data = haul_df,
                 mapping = aes(x = PCT_DIFF)) +
  geom_text(data = 
              data.frame(
                x = -10.5, 
                y = 200,
                YEAR = c(1982:2019, 2021:2024)
              ),
            mapping = aes(x = x, y = y, label = YEAR)) +
  geom_vline(xintercept = 0, linetype = 2) +
  scale_x_continuous(name = "Difference (%)") +
  scale_y_continuous(name = "Hauls (#)") +
  theme_bw() +
  facet_wrap(~YEAR) +
  theme(strip.background = element_blank(),
        strip.text = element_blank())

ggplot() +
  geom_sf(data = sebs_layers$survey.area,
          fill = NA, 
          color = "black") +
  geom_sf(data = station_diff,
          mapping = aes(fill = MEDIAN_DIFF)) +
  geom_sf(data = sebs_layers$akland) +
  scale_fill_viridis_c(name = "Spread diff. (%)") +
  scale_x_continuous(limits = sebs_layers$plot.boundary$x,
                     breaks = sebs_layers$lon.breaks) +
  scale_y_continuous(limits = sebs_layers$plot.boundary$y,
                     breaks = sebs_layers$lat.breaks) +
  theme_bw()


# lm_wing_coef <- data.frame(slope = c(lm_wing_83112$coefficients[[2]],
#                                      lm_wing_pne$coefficient[[2]],
#                                      lm_wing_race_ebs$coefficient[[2]],
#                                      lm_wing_race_goaai$coefficient[[2]]),
#                            intercept =
#                              c(lm_wing_83112$coefficients[[1]],
#                                lm_wing_pne$coefficient[[1]],
#                                lm_wing_race_ebs$coefficient[[1]],
#                                lm_wing_race_goaai$coefficient[[1]]),
#                            trawl = c("83-112", "PNE", "RACE", "RACE"),
#                            footrope = c("83-112", "PNE", "EBS_v2", "GOA/AI_v6"))

# p_wing_spread <- ggplot() +
#   geom_smooth(data = dat,
#               mapping = aes(x = spread_u_wing_m,  
#                             y = spread_l_wing_m),
#               method = 'lm',
#               color = "black") +
#   geom_point(data = dat,
#              mapping = aes(x = spread_u_wing_m,  
#                            y = spread_l_wing_m,
#                            color = format(towing_speed_kn, nsmall = 1)),
#              size = rel(2.5)) +
#   geom_text(data = lm_wing_coef,
#             mapping = 
#               aes(x = 13.2, 
#                   y = 23.5, 
#                   label = paste0("y=", format(intercept, nsmall = 1, digits = 3), " + ", format(slope, nsmall = 1,digits = 3), "x"))) +
#   geom_abline(intercept = 0, slope = 1, linetype = 2) +
#   scale_color_manual(name = "Towing speed (kn)", values = colors_speed) +
#   scale_x_continuous(name = "Upper wing spread (m)") +
#   scale_y_continuous(name = "Lower wing spread (m)") +
#   facet_wrap(~paste0(trawl, " (", footrope, ")")) +
#   theme_bw()

