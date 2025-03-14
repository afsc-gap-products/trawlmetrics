library(trawlmetrics)
library(ggthemes)
library(plyr)
library(mgcv)

survey_abbv <- data.frame(SURVEY_ABBV = c("AI", "GOA", "EBS", "NBS", "BSS"),
                          SURVEY_DEFINITION_ID = c(52, 47, 98, 143, 78))

target_angle_of_attack <- c(18, 21)

angle_of_attack_dat <- trawlmetrics::flume_tank |> 
  dplyr::filter(footrope %in% c("83-112", "PNE"), 
                catch == "empty",
                pulling_point_elevation_mm <= 200,
                additional_floatation_kg == 0, 
                bridles == "standard",
                trial > 0,
                !benthic_bag)

gam_bridle_angle_83112 <- 
  mgcv::gam(formula = bridle_angle_deg ~ s(spread_u_wing_m, bs = "tp", k = 5),
            data = dplyr::filter(angle_of_attack_dat, trawl == "83-112"))

gam_bridle_angle_pne <- 
  mgcv::gam(formula = bridle_angle_deg ~ s(spread_u_wing_m, bs = "tp", k = 5),
            data = dplyr::filter(angle_of_attack_dat, trawl == "PNE"))

bridle_angle_83112 <-
  trawlmetrics::bts_geom |>
  dplyr::filter(
    SURVEY_DEFINITION_ID %in% c(98, 143),
    CRUISE >= 201300,
    NET_MEASURED == TRUE
  ) |>
  dplyr::select(
    VESSEL_ID, 
    CRUISE, 
    HAUL, 
    SURVEY_DEFINITION_ID, 
    DEPTH_M, 
    NET_WIDTH_M, 
    WIRE_LENGTH_M
  ) |>
  dplyr::inner_join(survey_abbv)

bridle_angle_83112$EST_BRIDLE_ANGLE <-
  predict(
    gam_bridle_angle_83112,
    newdata = 
      data.frame(
        spread_u_wing_m = trawlmetrics::netmind_to_marport(bridle_angle_83112$NET_WIDTH_M)
      )
  )

bridle_angle_pne <-
  trawlmetrics::bts_geom |>
  dplyr::filter(
    SURVEY_DEFINITION_ID %in% c(47, 52),
    CRUISE >= 201300,
    NET_MEASURED == TRUE
  ) |>
  dplyr::select(
    VESSEL_ID, 
    CRUISE, 
    HAUL, 
    SURVEY_DEFINITION_ID, 
    DEPTH_M, 
    NET_WIDTH_M,
    WIRE_LENGTH_M
  ) |>
  dplyr::inner_join(survey_abbv)

bridle_angle_pne$EST_BRIDLE_ANGLE <-
  predict(
    gam_bridle_angle_pne,
    newdata = 
      data.frame(
        spread_u_wing_m = bridle_angle_pne$NET_WIDTH_M
      )
  )

p_bridle_angle_pne <-
  ggplot() +
  geom_histogram(
    data = bridle_angle_pne,
    mapping = aes(x = EST_BRIDLE_ANGLE),
    breaks = seq(9,38, 1)
  ) +
  geom_vline(
    xintercept = c(18, 21), 
    linetype = 2
  ) +
  scale_x_continuous(
    name = expression('Est. bridle angle of attack ('*degree*')')
  ) +
  scale_y_continuous(name = "Frequency") +
  theme_bw() +
  facet_wrap(~"GOA/AI PNE")


p_bridle_angle_83112 <-
  ggplot() +
  geom_histogram(
    data = bridle_angle_83112,
    mapping = aes(x = EST_BRIDLE_ANGLE),
    breaks = seq(5, 38, 1)
  ) +
  geom_vline(
    xintercept = c(18, 21), 
    linetype = 2
  ) +
  scale_x_continuous(
    name = expression('Est. bridle angle of attack ('*degree*')')
  ) +
  scale_y_continuous(name = "Frequency") +
  theme_bw() +
  facet_wrap(~"EBS/NBS 83-112")

n_hauls_83112 <- nrow(bridle_angle_83112)
n_hauls_18_21_83112 <- sum(bridle_angle_83112$EST_BRIDLE_ANGLE >= 18 & bridle_angle_83112$EST_BRIDLE_ANGLE <= 21)
pct_hauls_18_21_83112 <- round(n_hauls_18_21_83112/n_hauls_83112*100, 1)

n_hauls_pne <- nrow(bridle_angle_pne)
n_hauls_18_21_pne <- sum(bridle_angle_pne$EST_BRIDLE_ANGLE >= 18 & bridle_angle_pne$EST_BRIDLE_ANGLE <= 21)
pct_hauls_18_21_pne <- round(n_hauls_18_21_pne/n_hauls_pne*100, 1)

sd(bridle_angle_83112$EST_BRIDLE_ANGLE)
sd(bridle_angle_pne$EST_BRIDLE_ANGLE)

cowplot::plot_grid(
  p_bridle_angle_83112, 
  p_bridle_angle_pne
)





bridle_angle_depth <-
  dplyr::bind_rows(
    bridle_angle_83112 |>
      dplyr::mutate(GEAR = "83-112"),
    dplyr::filter(bridle_angle_pne, DEPTH_M < 600) |>
      dplyr::mutate(GEAR = "PNE")
  ) |>
  dplyr::mutate(DEPTH_M = round(DEPTH_M))

gam_angle_depth_83112 <- 
  mgcv::gam(EST_BRIDLE_ANGLE ~ s(DEPTH_M, bs = "tp"),
            data = dplyr::filter(bridle_angle_depth, GEAR == "83-112"))

gam_angle_depth_83112_de <- 
  round(
  100 * (gam_angle_depth_83112$null.deviance-gam_angle_depth_83112$deviance)/gam_angle_depth_83112$null.deviance,
  1)

gam_angle_depth_pne <- 
  mgcv::gam(EST_BRIDLE_ANGLE ~ s(DEPTH_M, bs = "tp"),
            data = dplyr::filter(bridle_angle_depth, GEAR == "PNE"))

gam_angle_depth_pne_de <- 
  round(
    100 * (gam_angle_depth_pne$null.deviance-gam_angle_depth_pne$deviance)/gam_angle_depth_pne$null.deviance,
    1)

p_depth_v_angle <- 
  ggplot() +
  geom_point(
    data = bridle_angle_depth,
    mapping = aes(x = DEPTH_M, 
                  y = EST_BRIDLE_ANGLE,
                  color = GEAR),
    size = rel(0.1),
    alpha = 0.2
  ) +
  geom_smooth(
    data = bridle_angle_depth,
    mapping = aes(x = DEPTH_M, 
                  y = EST_BRIDLE_ANGLE,
                  color = GEAR),
    method = 'gam'
  ) +
  geom_text(mapping = aes(x = 225,
                          y = 28,
                          label = paste0("DE: ", 
                                         gam_angle_depth_83112_de, "%"),
                          color = "83-112")) +
  geom_text(mapping = aes(x = 500,
                          y = 24,
                          label = paste0("DE: ", 
                                         gam_angle_depth_pne_de, "%"),
                          color = "PNE")) +
  geom_hline(
    yintercept = c(18, 21), 
    linetype = 2
  ) +
  scale_color_manual(
    name = "Gear", 
    values = c("#4E79A7", "#F28E2B")) +
  scale_x_continuous(name = "Bottom depth (m)") +
  scale_y_continuous(
    name = expression('Est. bridle angle of attack ('*degree*')')
  ) +
  theme_bw()

print(p_depth_v_angle)





ggplot() +
  geom_point(
    data = bridle_angle_depth,
    mapping = aes(x = WIRE_LENGTH_M/DEPTH_M, 
                  y = EST_BRIDLE_ANGLE,
                  color = GEAR),
    size = rel(0.1),
    alpha = 0.2
  ) +
  geom_smooth(
    data = bridle_angle_depth,
    mapping = aes(x = WIRE_LENGTH_M/DEPTH_M, 
                  y = EST_BRIDLE_ANGLE,
                  color = GEAR),
    method = 'gam'
  ) +
  geom_hline(
    yintercept = c(18, 21), 
    linetype = 2
  ) +
  scale_color_manual(
    name = "Gear", 
    values = c("#4E79A7", "#F28E2B")) +
  scale_x_continuous(name = "Scope-to-depth ratio") +
  scale_y_continuous(
    name = expression('Est. bridle angle of attack ('*degree*')')
  ) +
  theme_bw()

depth_in_range <-
  bridle_angle_depth |>
  dplyr::mutate(DEPTH_RANGE = plyr::round_any(DEPTH_M, 5),
                WITHIN_RANGE = EST_BRIDLE_ANGLE >= 18 & EST_BRIDLE_ANGLE <= 21)  |>
  dplyr::group_by(GEAR, DEPTH_M) |>
  dplyr::summarise(HAULS_WITHIN_RANGE = sum(WITHIN_RANGE),
                   TOTAL_HAULS = dplyr::n()) |>
  dplyr::arrange(DEPTH_M)

depth_in_range_83112 <- 
  dplyr::filter(
    depth_in_range, 
    GEAR == "83-112"
    )

ggplot() +
  geom_bar(
    data = depth_in_range_83112,
    mapping = aes(x = DEPTH_M,
                  y = HAULS_WITHIN_RANGE/TOTAL_HAULS,
                  group = DEPTH_M,
                  fill = "18<BA<21"),
    stat = "identity",
    width = 1
  ) +
  stat_ecdf(data = dplyr::filter(bridle_angle_depth, GEAR == "83-112"),
            mapping = aes(x = DEPTH_M,
                          color = "Cumulative hauls")) +
  scale_x_continuous(name = "Bottom depth (m)", expand = c(0,0)) +
  scale_y_continuous(name = "Proportion", expand = c(0,0)) +
  scale_color_manual(values = "black") +
  scale_fill_manual(values = "salmon") +
  theme_bw() +
  theme(legend.title = element_blank())


ggplot() +
  geom_point(
    data = depth_in_range_83112,
    mapping = aes(x = DEPTH_M,
                  y = HAULS_WITHIN_RANGE/TOTAL_HAULS,
                  color = TOTAL_HAULS)
  ) +
  # geom_ribbon(
  #   data = depth_in_range_83112,
  #   mapping = 
  #     aes(
  #       x = DEPTH_M,
  #       ymin = P_IN_RANGE - 2*P_IN_RANGE_SE,
  #       ymax = P_IN_RANGE + 2*P_IN_RANGE_SE
  #     ),
  #   alpha = 0.5
  # ) +
  # geom_path(data = depth_in_range_83112,
  #           mapping = aes(x = DEPTH_M, y = P_IN_RANGE)) +
  scale_x_continuous(name = "Bottom depth (m)") +
  scale_y_continuous(
    name = expression('P(18'*degree <=  'Bridle angle ' * ' ≤ ' * '21'*degree*')')
  ) +
  scale_color_viridis_c(name = "Hauls (#)", option = "F", direction = -1) +
  theme_bw()

bridle_angle_depth |>
  dplyr::group_by(GEAR) |>
  dplyr::summarise(min_depth = min(DEPTH_M),
                   max_depth = max(DEPTH_M))
