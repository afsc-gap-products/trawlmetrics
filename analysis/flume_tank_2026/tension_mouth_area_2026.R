library(trawlmetrics)
library(ggthemes)
library(ggrepel)

colors_speed <- c('2.0' = "#0D0887FF",
                  '2.5' = "#6A00A8FF",
                  '3.0' = "#B12A90FF",
                  '3.5' = "#E16462FF",
                  '4.0' = "#FCA636FF",
                  'Field obs.' = "grey",
                  'Field fit' = "black")

colors_spread_all <- c(`12.5` = "#440154FF",
                      `14.5` = "#443A83FF",
                      `16.0` = "#31688EFF",
                      `17.5` = "#21908CFF",
                      `19.0` = "#35B779FF",
                      `20.0` = "#8FD744FF")

flume_2025 <- trawlmetrics::flume_tank |>
  dplyr::filter(!is.na(bridles), 
                pulling_point_elevation_mm < 300,
                additional_floatation_kg == 0, 
                year == 2025,
                is.na(bcs_unit),
                trial > 0,
                catch == "empty",
                bridles != "2-weighted",
                # trawl != "83-112",
                footrope %in% c("EBS_v2", "GOA/AI_v6", "PNE", "83-112")) |>
  dplyr::mutate(
    type = "Flume tank",
    fac_trial = factor(trial),
    type = paste0("Flume ", trawl, ", ", bridles),
    footrope = ifelse(footrope %in% c("PNE", "83-112"), footrope, ifelse(footrope == "EBS_v2", "EBS", "GOA"))
  )


flume_2026 <- 
  trawlmetrics::flume_tank |>
  dplyr::filter(year == 2026) |>
  # read.csv(file = here::here("analysis", "flume_tank_2026", "data", "flume_tank_data_2026_all.csv")) |>
  # dplyr::mutate(
  #   bridles = paste0("2026-", bridles),
  #   trawl = paste0("New ", trawl)
  # ) |> 
  dplyr::filter(trial > 0 & trial < 89)

trawl_data_all <- dplyr::bind_rows(flume_2026, flume_2025)
trawl_data_no83112 <- dplyr::bind_rows(flume_2026, flume_2025) |>
  dplyr::filter(trawl != "83-112")


ggplot() +
  geom_point(
    data = trawl_data_no83112, 
    mapping = aes(x = towing_speed_kn, y = mouth_area_m2, shape = footrope)
  ) +
  facet_grid(spread_treatment~trawl)

ggplot() +
  geom_point(
    data = trawl_data_no83112, 
    mapping = aes(x = spread_treatment, y = mouth_area_m2, shape = footrope, color = format(spread_treatment, nsmall = 1))
  ) +
  scale_color_manual(values = colors_spread_all) +
  facet_grid(bridles~trawl)


ggplot() +
  geom_point(
    data = trawl_data_no83112, 
    mapping = aes(x = towing_speed_kn, y = mouth_drag_kgf_m2, label = trial)
  ) +
  geom_text_repel(
    data = trawl_data_no83112, 
    mapping = aes(x = towing_speed_kn, y = mouth_drag_kgf_m2, label = trial)
  ) +
  facet_wrap(~trawl)


ggplot() +
  geom_point(
    data = trawl_data_no83112, 
    mapping = aes(x = bridle_tension_total_t, y = mouth_area_m2, shape = bridles, color = format(spread_treatment, nsmall = 1))
  ) +
  scale_color_viridis_d() +
  facet_grid(~trawl)

ggplot(
  data = dplyr::filter(trawl_data_all, spread_treatment %in% c(14.5, 16, 17.5)), 
  mapping = aes(
    x = towing_speed_kn, 
    y = mouth_area_m2, 
    shape = bridles, 
    color = footrope
  )
) +
  geom_point(size = 2.5) +
  scale_shape(name = "Bridles") +
  scale_color_colorblind(name = "Footrope") +
  scale_x_continuous(name = "Towing speed (knots)") +
  scale_y_continuous(name = expression('Mouth area ('*m^2*')')) +
  facet_grid(paste0(spread_treatment, "m")~trawl) +
  theme_bw()

ggplot(
  data = dplyr::filter(trawl_data_all, spread_treatment %in% c(14.5, 16, 17.5)), 
  mapping = aes(
    x = towing_speed_kn, 
    y = bridle_tension_total_t, 
    shape = bridles, 
    color = footrope
  )
) +
  geom_point(size = 2.5) +
  scale_shape(name = "Bridles") +
  scale_color_colorblind(name = "Footrope") +
  scale_x_continuous(name = "Towing speed (knots)") +
  scale_y_continuous(name =  "Bridle tension (tons)") +
  facet_grid(paste0(spread_treatment, "m")~trawl) +
  theme_bw()

# ggplot(
#   data = trawl_data_no83112, 
#   mapping = aes(
#     x = towing_speed_kn, 
#     y = mouth_area_m2, 
#     shape = bridles, 
#     color = footrope
#   )
# ) +
#   geom_point(size = 2.5) +
#   scale_shape(name = "Bridles") +
#   scale_color_colorblind(name = "Footrope") +
#   scale_x_continuous(name = "Towing speed (knots)") +
#   scale_y_continuous(name = "Mouth area (m2)") +
#   facet_grid(paste0(spread_treatment, "m")~trawl) +
#   theme_bw()

ggplot(
  data = trawl_data_no83112, 
  # data = dplyr::filter(trawl_data_no83112, spread_treatment < 19), 
  mapping = aes(
    x = towing_speed_kn, 
    y = bridle_tension_total_t, 
    shape = bridles, 
    color = footrope
    # color = format(spread_treatment, nsmall = 1)
  )
) +
  geom_point(size = 2.5) +
  # geom_smooth(method = 'lm', se= FALSE) +
  scale_shape(name = "Bridles") +
  scale_color_colorblind(name = "Footrope") +
  scale_x_continuous(name = "Towing speed (knots)") +
  scale_y_continuous(name = "Bridle tension (tons)") +
  facet_grid(paste0(spread_treatment, "m")~trawl) +
  theme_bw()


ggplot() +
  geom_point(
    data = trawl_data_no83112, 
    mapping = aes(x = towing_speed_kn, y = bridle_tension_total_t, label = trial)
  ) +
  geom_text_repel(
    data = trawl_data_no83112, 
    mapping = aes(x = towing_speed_kn, y =  bridle_tension_total_t, label = trial)
  ) +
  facet_wrap(~trawl)
