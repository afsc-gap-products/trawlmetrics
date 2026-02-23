library(trawlmetrics)
library(ggthemes)
library(ggrepel)
library(scales)
# trawlmetrics::flume_tank$bridle_angle_deg-
# calc_bridle_angle(
#   door_spread_m = trawlmetrics::flume_tank$door_m, 
#   wing_spread_m = trawlmetrics::flume_tank$spread_l_wing_m, 
#   total_bridle_length_m = 54.9 + 13
# )

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
  dplyr::mutate(type = "Flume tank",
                fac_trial = factor(trial),
                type = paste0("Flume ", trawl, ", ", bridles),
                footrope = ifelse(footrope %in% c("PNE", "83-112"), footrope, ifelse(footrope == "EBS_v2", "EBS", "GOA")))


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

projected_spread <- 
  xlsx::read.xlsx(
    file = here::here("analysis", "flume_tank_2026", "data", "projected_height_spread.xlsx"),
    sheetIndex = 1
  ) |>
  dplyr::filter(
    trawl %in% unique(c(flume_2025$trawl, flume_2026$trawl))
  )

projected_spread_no83112 <- dplyr::filter(projected_spread, trawl != "83-112")

ggplot(
  data = trawl_data_no83112,
           mapping = aes(x = spread_u_wing_m, y = opening_headline_m, color = factor(towing_speed_kn))
  ) +
  geom_polygon(
    data = data.frame(x = c(15, 20, 20, 15, 15), y = c(5, 5, 6, 6, 5)),
    mapping = aes(x = x, y = y), color = "red", fill = NA) +
  geom_hline(data = projected_spread_no83112,
             mapping = aes(yintercept = opening_headline_m), color = "grey40", linetype = 2) + 
  geom_vline(data = projected_spread_no83112,
             mapping = aes(xintercept = spread_u_wing_m), color = "grey40", linetype = 2) + 
  geom_point(mapping = aes(shape = footrope)) +
  geom_smooth(se = FALSE, method = 'lm') +
  scale_color_manual(name = "Speed (kn)", values = scales::viridis_pal(option = "C")(6)) +
  scale_shape(name = "Footrope") +
  scale_x_continuous(name = "Upper wing spread (m)") +
  scale_y_continuous(name = "Opening headline (m)") +
  facet_grid(bridles~trawl) +
  theme_bw()

ggplot(
  data = trawl_data_no83112,
  mapping = aes(x = spread_u_wing_m, y = spread_u_wing_m/opening_headline_m, color = factor(towing_speed_kn))
) +
  # geom_polygon(
  #   data = data.frame(x = c(15, 20, 20, 15, 15), y = c(5, 5, 6, 6, 5)),
  #   mapping = aes(x = x, y = y), color = "red", fill = NA) +
  geom_hline(data = projected_spread_no83112,
             mapping = aes(yintercept = spread_u_wing_m/opening_headline_m), color = "grey40", linetype = 2) + 
  geom_vline(data = projected_spread_no83112,
             mapping = aes(xintercept = spread_u_wing_m), color = "grey40", linetype = 2) + 
  geom_point(mapping = aes(shape = footrope)) +
  geom_smooth(se = FALSE, method = 'lm') +
  scale_color_manual(name = "Speed (kn)", values = scales::viridis_pal(option = "C")(6)) +
  scale_shape(name = "Footrope") +
  scale_x_continuous(name = "Upper wing spread (m)") +
  scale_y_continuous(name = "Spread:Height Ratio") +
  facet_grid(bridles~trawl) +
  theme_bw()


ggplot(
  data = trawl_data_no83112,
  mapping = aes(x = spread_u_wing_m, y = spread_u_wing_m/opening_headline_m, color = factor(towing_speed_kn))
) +
  # geom_polygon(
  #   data = data.frame(x = c(15, 20, 20, 15, 15), y = c(5, 5, 6, 6, 5)),
  #   mapping = aes(x = x, y = y), color = "red", fill = NA) +
  geom_hline(data = projected_spread_no83112,
             mapping = aes(yintercept = spread_u_wing_m/opening_headline_m), color = "grey40", linetype = 2) + 
  geom_vline(data = projected_spread_no83112,
             mapping = aes(xintercept = spread_u_wing_m), color = "grey40", linetype = 2) + 
  geom_point(mapping = aes(shape = footrope)) +
  geom_smooth(se = FALSE, method = 'lm') +
  scale_color_manual(name = "Speed (kn)", values = scales::viridis_pal(option = "C")(6)) +
  scale_shape(name = "Footrope") +
  scale_x_continuous(name = "Upper wing spread (m)") +
  scale_y_continuous(name = "Spread:Height Ratio") +
  facet_grid(bridles~trawl) +
  theme_bw()


ggplot() +
  geom_point(
    data = flume_2026,
    mapping = aes(x = towing_speed_kn, y = spread_u_wing_m/opening_headline_m, color = factor(spread_treatment))
  ) +
  geom_path(
    data = flume_2026,
    mapping = aes(x = towing_speed_kn, y = spread_u_wing_m/opening_headline_m, color = factor(spread_treatment))
  ) +
  scale_color_viridis_d(name = "Upper wing spread (m)", option = "C") +
  scale_x_continuous(name = "Speed (kn)") +
  scale_y_continuous(name = "Spread:Height Ratio") +
  facet_grid(footrope~paste0(trawl, " (", bridles, ")")) +
  theme_bw()


ggplot() +
  geom_point(
    data = flume_2026,
    mapping = aes(x = towing_speed_kn, y = spread_u_wing_m/opening_headline_m, color = factor(spread_treatment))
  ) +
  geom_path(
    data = flume_2026,
    mapping = aes(x = towing_speed_kn, y = spread_u_wing_m/opening_headline_m, color = factor(spread_treatment))
  ) +
  # geom_vline(xintercept = 3, color = "grey40", linetype = 2) + 
  # geom_hline(yintercept = 5, color = "grey40", linetype = 2) +
  # geom_hline(yintercept = 6, color = "grey40", linetype = 2) +
  scale_color_viridis_d(name = "Upper wing spread (m)", option = "C") +
  scale_x_continuous(name = "Speed (kn)") +
  scale_y_continuous(name = "Spread:Height Ratio") +
  facet_wrap(~paste0(trawl, " (Footrope: ", footrope, ", Bridles: ", bridles, ")")) +
  theme_bw()


ggplot() +
  geom_path(
    data = trawl_data_no83112,
    mapping = aes(x = towing_speed_kn, y = spread_u_wing_m/opening_headline_m, color = paste0(trawl, " (", footrope, ", ", bridles, ")"))
  ) +
  geom_point(
    data = trawl_data_no83112,
    mapping = 
      aes(
        x = towing_speed_kn, 
        y = spread_u_wing_m/opening_headline_m, 
        color = paste0(trawl, " (", footrope, ", ", bridles, ")"),
        # shape = paste0(trawl, " (", footrope, ", ", bridles, ")")
        )
  ) +
  # geom_vline(xintercept = 3, color = "grey40", linetype = 2) + 
  # geom_hline(yintercept = 5, color = "grey40", linetype = 2) +
  # geom_hline(yintercept = 6, color = "grey40", linetype = 2) +
  scale_color_tableau(name = "Gear", palette = "Tableau 20") +
  scale_shape(name = "Gear") +
  scale_x_continuous(name = "Speed (kn)") +
  scale_y_continuous(name = "Spread:Height Ratio") +
  facet_wrap(~paste0("Spread: ", spread_treatment, " m")) +
  theme_bw() +
  theme(legend.position = "inside",
        legend.position.inside = c(0.85, 0.2))

ggplot() +
  geom_path(
    data = flume_2026,
    mapping = aes(x = towing_speed_kn, y = spread_u_wing_m/opening_headline_m, color = paste0(trawl, " (", footrope, ", ", bridles, ")"))
  ) +
  geom_point(
    data = flume_2026,
    mapping = 
      aes(
        x = towing_speed_kn, 
        y = spread_u_wing_m/opening_headline_m, 
        color = paste0(trawl, " (", footrope, ", ", bridles, ")"),
        # shape = paste0(trawl, " (", footrope, ", ", bridles, ")")
      )
  ) +
  # geom_vline(xintercept = 3, color = "grey40", linetype = 2) + 
  # geom_hline(yintercept = 5, color = "grey40", linetype = 2) +
  # geom_hline(yintercept = 6, color = "grey40", linetype = 2) +
  scale_color_colorblind(name = "Gear") +
  scale_shape(name = "Gear") +
  scale_x_continuous(name = "Speed (kn)") +
  scale_y_continuous(name = "Spread:Height Ratio") +
  facet_wrap(~paste0("Spread: ", spread_treatment, " m")) +
  theme_bw() +
  theme(legend.position = "inside",
        legend.position.inside = c(0.85, 0.2))

ggplot(
  data = trawl_data_all,
  mapping = 
    aes(
      x = spread_u_wing_m, 
      y = bridle_angle_deg, 
      color = trawl,
      shape = trawl)
      # color = paste0(trawl, " (", footrope, ", ", bridles, ")"))
) +
  # geom_smooth(se = FALSE, method = 'lm') +
  geom_smooth(se = FALSE, method = 'gam', formula = y ~ s(x, bs = "cs", k = 4)) +
  geom_point(size = 2.5) +
  geom_hline(yintercept = 18, color = "grey40", linetype = 2) +
  geom_hline(yintercept = 21, color = "grey40", linetype = 2) +
  scale_color_tableau(name = "Gear", palette = "Tableau 20") +
  scale_shape(name = "Gear") +
  scale_x_continuous(name = "Upper wing spread (m)") +
  scale_y_continuous(name = "Bridle angle of attack (degrees)") +
  theme_bw() +
  theme(
    legend.position = "right", 
    axis.text = element_text(size = 16), 
    axis.title = element_text(size = 18),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14)
  )

ggplot(
  data = trawl_data_no83112,
  mapping = 
    aes(
      x = spread_u_wing_m, 
      y = bridle_angle_deg, 
      color = footrope)
) +
  geom_vline(
    data = projected_spread_no83112,
    mapping = aes(xintercept = spread_u_wing_m), 
    linetype = 2,
    color = "grey40"
  ) +
  geom_polygon(
    data = data.frame(x = c(15, 20, 20, 15, 15), y = c(18, 18, 21, 21, 18)),
    mapping = aes(x = x, y = y), color = "red", fill = NA) +
  geom_smooth(se = FALSE, method = 'lm') +
  geom_point() +
  scale_color_colorblind(name = "Footrope") +
  facet_grid(bridles~trawl) +
  scale_x_continuous(name = "Upper wing spread (m)") +
  scale_y_continuous(name = "Bridle angle of attack (degrees)") +
  theme_bw()

ggplot() +
  geom_path(
    data = trawl_data_no83112,
    mapping = aes(x = towing_speed_kn, y = bridle_angle_deg, color = paste0(trawl, " (", footrope, ", ", bridles, ")"))
  ) +
  geom_text(
    data = trawl_data_no83112,
    mapping = 
      aes(
        x = towing_speed_kn, 
        y = bridle_angle_deg, 
        color = paste0(trawl, " (", footrope, ", ", bridles, ")"),
        shape = paste0(trawl, " (", footrope, ", ", bridles, ")"),
        label = trial)
  ) +
  geom_hline(yintercept = 18, color = "grey40", linetype = 2) +
  geom_hline(yintercept = 21, color = "grey40", linetype = 2) +
  scale_color_tableau(name = "Gear") +
  scale_shape(name = "Gear") +
  scale_x_continuous(name = "Speed (kn)") +
  scale_y_continuous(name = "Bridle angle of attack (degrees)") +
  facet_wrap(~paste0("Spread: ", spread_treatment, " m")) +
  theme_bw() +
  theme(legend.position = "inside",
        legend.position.inside = c(0.85, 0.2))

# Upper versus lower wing tip spread

ggplot(data = dplyr::filter(trawl_data_all, !(trial == 13 & year == 2026)),
       mapping = 
         aes(
           x = spread_u_wing_m, 
           y = spread_l_wing_m, 
           color = trawl,
           shape = trawl)) +
  geom_point() +
  geom_smooth(
    method = 'lm'
  ) +
  geom_abline(slope = 1, interecept = 0, linetype = 2) +
  facet_wrap(~trawl) +
  scale_color_tableau(name = "Gear", palette = "Tableau 20") +
  scale_shape(name = "Gear") +
  scale_x_continuous(name = "Upper wing tip spread (m)", limits = c(12,24), breaks = seq(12, 24, 4)) +
  scale_y_continuous(name = "Lower wing tip spread (m)", limits = c(12,24), breaks = seq(12, 24, 4)) +
  theme_bw() +
  theme(
    legend.position = "none", 
    axis.text = element_text(size = 16), 
    axis.title = element_text(size = 18),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    strip.text = element_text(size = 14)
  )


ggplot(data = dplyr::filter(trawl_data_all, !(trial == 13 & year == 2026)),
       mapping = 
         aes(
           x = spread_u_wing_m, 
           y = spread_u_wing_m/spread_l_wing_m, 
           color = trawl,
           shape = trawl)) +
  geom_point() +
  geom_smooth(
    method = 'lm'
  ) +
  geom_hline(yintercept = 1, linetype = 2) +
  facet_wrap(~trawl) +
  scale_color_tableau(name = "Gear", palette = "Tableau 20") +
  scale_shape(name = "Gear") +
  scale_y_continuous(name = "UWT/LWT") +
  scale_x_continuous(name = "Upper wing tip spread (m)", limits = c(12,24), breaks = seq(12, 24, 4)) +
  theme_bw() +
  theme(
    legend.position = "none", 
    axis.text = element_text(size = 16), 
    axis.title = element_text(size = 18),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    strip.text = element_text(size = 14)
  )

