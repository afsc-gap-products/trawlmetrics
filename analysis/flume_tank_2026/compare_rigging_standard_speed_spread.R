library(trawlmetrics)
library(ggthemes)
library(ggrepel)

# Compare rigging options

flume_2026 <- 
  trawlmetrics::flume_tank |>
  dplyr::filter(
    towing_speed_kn == 3, 
    spread_treatment == 16, 
    trial >= 1,
    year == 2026) |>
  dplyr::inner_join(
    dplyr::select(
      trawlmetrics::flume_tank_rigging,
      top_setback_links,
      middle_setback_links,
      bottom_setback_links,
      rig,
      footrope
    ),
     by = c("rig", "footrope")
  )

flume_2025 <- 
  trawlmetrics::flume_tank |>
  dplyr::filter(
    towing_speed_kn == 3, 
    spread_treatment == 17.5, 
    year == 2025,
    footrope %in% c("PNE", "EBS_v1", "GOA/AI_v6"),
    catch == "empty",
    pulling_point_elevation_mm <= 200,
    !is.na(bridle_angle_deg),
    benthic_bag == FALSE
  ) |>
  dplyr::mutate(
    bridles = ifelse(bridles == "2-weighted", 2, bridles)
                )

projected_spread <- 
  xlsx::read.xlsx(
    file = here::here("analysis", "flume_tank_2026", "data", "projected_height_spread.xlsx"),
    sheetIndex = 1
  ) |>
  dplyr::filter(
    trawl %in% unique(c(flume_2025$trawl, flume_2026$trawl))
  )


ggplot() +
  geom_hline(yintercept = c(5,6), linetype = 2) +
  geom_text_repel(
    data = flume_2026,
    mapping = aes(x = total_buoyancy_kgf, y = opening_headline_m, color = bridles, label = paste0(trial, " ", rig, " ", round(top_setback_links*2.5, 1), "\u0022"))
  ) +
  geom_point(
    data = flume_2026,
    mapping = aes(x = total_buoyancy_kgf, y = opening_headline_m, color = bridles, shape = footrope)
  ) +
  scale_color_colorblind(name = "Bridle") +
  scale_x_continuous(name = "Total buoyancy (kgf)") +
  scale_y_continuous(name = "Headrope height (m)", limits = c(3,6)) +
  scale_shape(name = "Footrope") +
  ggtitle(label = "Effects of rigging on height at 3 knots and 16 m upper wing-tip spread") +
  facet_wrap(~trawl, scales = "free_x") +
  theme_bw()

ggplot() +
  geom_hline(
    data = projected_spread,
    mapping = aes(yintercept = opening_headline_m), color = "red"
  ) +
  geom_hline(yintercept = c(5,6), linetype = 2) +
  geom_text_repel(
    data = dplyr::bind_rows(flume_2026, flume_2025),
    mapping = aes(
      x = bridles,
      y = opening_headline_m, 
      color = total_buoyancy_kgf, 
      label = paste0(ifelse(is.na(top_setback_links), "-", round(top_setback_links*2.5, 1)), "/", round(total_buoyancy_kgf))
    )
  ) +
  geom_point(
    data = dplyr::bind_rows(flume_2026, flume_2025),
    mapping = aes(
      x = bridles,
      y = opening_headline_m, 
      color = total_buoyancy_kgf, 
      shape = footrope
    )
  ) +
  scale_color_viridis_c(name = "Buoyancy (kgf)", direction = -1) +
  scale_x_discrete(name = "Bridle") +
  scale_y_continuous(name = "Headrope height (m)", limits = c(3,6)) +
  scale_shape(name = "Footrope") +
  ggtitle(label = "Effects of bridle rigging and headrope buoyancy on opening height\nTreatments: 3 knots @ projected upper wing-tip spread.\nLabels show upper setback (inches)/buoyancy (kgf)") +
  facet_wrap(~trawl) +
  theme_bw()


ggplot() +
  geom_hline(yintercept = c(18, 21), linetype = 2) +
  geom_text_repel(
    data = dplyr::bind_rows(flume_2026, flume_2025),
    mapping = aes(
      x = bridles,
      y = bridle_angle_deg, 
      color = total_buoyancy_kgf, 
      label = paste0(ifelse(is.na(top_setback_links), "-", round(top_setback_links*2.5, 1)), "/", round(total_buoyancy_kgf))
    )
  ) +
  geom_point(
    data = dplyr::bind_rows(flume_2026, flume_2025),
    mapping = aes(
      x = bridles,
      y = bridle_angle_deg, 
      color = total_buoyancy_kgf, 
      shape = footrope
    )
  ) +
  scale_color_viridis_c(name = "Buoyancy (kgf)", direction = -1) +
  scale_x_discrete(name = "Bridle") +
  scale_y_continuous(name = "Bridle angle of attack (degrees)", limits = c(17, 27), breaks = seq(18, 27, 3)) +
  scale_shape(name = "Footrope") +
  ggtitle(label = "Effects of bridle rigging and headrope buoyancy on angle of attack\nTreatments: 3 knots @ projected upper wing-tip spread.\nLabels show upper setback (inches)/buoyancy (kgf)") +
  facet_wrap(~trawl) +
  theme_bw()

ggplot() +
  geom_hline(yintercept = c(5,6), linetype = 2) +
  geom_text_repel(
    data = flume_2026,
    mapping = aes(x = total_buoyancy_kgf, y = opening_headline_m, color = bridles, label = paste0(rig, " ", round(top_setback_links*2.5, 1), "\u0022"))
  ) +
  geom_point(
    data = flume_2026,
    mapping = aes(x = total_buoyancy_kgf, y = opening_headline_m, color = bridles, shape = footrope)
  ) +
  scale_color_colorblind(name = "Bridle") +
  scale_x_continuous(name = "Total buoyancy (kgf)") +
  scale_y_continuous(name = "Headline (m)", limits = c(4,6)) +
  scale_shape(name = "Footrope") +
  ggtitle(label = "Effects of rigging on height at 3 knots and 16 m upper wing-tip spread") +
  facet_wrap(~trawl, scales = "free_x") +
  theme_bw()


ggplot() +
  geom_hline(yintercept = c(18,21), linetype = 2) +
  geom_text_repel(
    data = flume_2026,
    mapping = aes(x = total_buoyancy_kgf, y = bridle_angle_deg, color = bridles, label = paste0(rig, " ", round(top_setback_links*2.5, 1), "\u0022"))
  ) +
  geom_point(
    data = flume_2026,
    mapping = aes(x = total_buoyancy_kgf, y = bridle_angle_deg, color = bridles, shape = footrope)
  ) +
  scale_color_colorblind(name = "Bridle") +
  scale_x_continuous(name = "Total buoyancy (kgf)") +
  scale_y_continuous(name = "Bridle angle of attack (degrees)") +
  scale_shape(name = "Footrope") +
  ggtitle(label = "Effects of rigging on height at 3 knots and 16 m upper wing-tip spread") +
  facet_wrap(~trawl, scales = "free_x") +
  theme_bw()

ggplot() +
  geom_hline(yintercept = 1, linetype = 3) +
  geom_text_repel(
    data = flume_2026,
    mapping = aes(x = total_buoyancy_kgf, y = spread_u_wing_m/spread_l_wing_m, color = bridles, label = paste0(rig, " ", round(top_setback_links*2.5, 1), "\u0022"))
  ) +
  geom_point(
    data = flume_2026,
    mapping = aes(x = total_buoyancy_kgf, y = spread_u_wing_m/spread_l_wing_m, color = bridles, shape = footrope)
  ) +
  scale_color_colorblind(name = "Bridle") +
  scale_x_continuous(name = "Total buoyancy (kgf)") +
  scale_y_continuous(name = "Upper spread/Lower spread") +
  scale_shape(name = "Footrope") +
  ggtitle(label = "Effects of rigging on upper/lower wing-tip ratio at 3 knots and 16 m upper wing-tip spread") +
  facet_wrap(~trawl, scales = "free_x") +
  theme_bw()
