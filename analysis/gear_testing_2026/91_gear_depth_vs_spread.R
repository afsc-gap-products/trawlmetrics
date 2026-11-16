source("00_functions.R")

dat <- trawlmetrics::bts_geom |>
  dplyr::filter(NET_MEASURED == TRUE) |>
  dplyr::inner_join(
    data.frame(
      SURVEY_ABBV = c("EBS", "NBS", "GOA", "AI", "BSS"),
      GEAR_SURVEY = c(
        "EBS shelf/NBS (83-112)", 
        "EBS shelf/NBS (83-112)", 
        "GOA/AI (PNE w/ roller gear)", 
        "GOA/AI (PNE w/ roller gear)", 
        "EBS slope (PNE w/ mudsweep)"
      )
    ),
    by = "SURVEY_ABBV"
  )


get_gam_labels <- function(df, y_expr) {
  df |>
    dplyr::mutate(y_val = {{ y_expr }}) |>
    dplyr::filter(!is.na(DEPTH_M), !is.na(y_val), is.finite(y_val), DEPTH_M > 0) |>
    dplyr::group_by(GEAR_SURVEY) |>
    dplyr::summarise(
      dev_expl = summary(mgcv::gam(y_val ~ s(log10(DEPTH_M), bs = "cs")))$dev.expl,
      label = sprintf("Deviance explained: %.1f%%", dev_expl * 100),
      .groups = "drop"
    )
}

gear_colors <- c(
  "EBS shelf/NBS (83-112)"      = "#0072B2", # Blue
  "GOA/AI (PNE w/ roller gear)" = "#D55E00", # Vermilion
  "EBS slope (PNE w/ mudsweep)" = "#009E73"  # Bluish green
)

# Wing spread ----
dev_labels_1 <- get_gam_labels(dat, NET_WIDTH_M)

p_spread <- ggplot(dat, aes(x = DEPTH_M, y = NET_WIDTH_M, color = GEAR_SURVEY)) +
  geom_point(size = 0.2, alpha = 0.15) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), linewidth = 0.7) +
  geom_text(
    data = dev_labels_1,
    aes(x = Inf, y = Inf, label = label),
    hjust = 1.05, vjust = 1.5,
    size = 3, fontface = "bold", color = "grey20",
    inherit.aes = FALSE
  ) +
  facet_wrap(~GEAR_SURVEY) +
  scale_color_manual(values = gear_colors) +
  scale_x_log10(name = "Bottom depth (m)") +
  scale_y_continuous(name = "Upper wing spread (m)") +
  theme_bw(base_size = 10) +
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "grey95", color = "grey40"),
    strip.text = element_text(face = "bold", size = 9)
  )

# Spread/height ----
dev_labels_2 <- get_gam_labels(dat, NET_WIDTH_M / NET_HEIGHT_M)

p_ratio <- ggplot(data = dat, mapping = aes(x = DEPTH_M, y = NET_WIDTH_M / NET_HEIGHT_M, color = GEAR_SURVEY)) +
  geom_point(size = 0.2, alpha = 0.15) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), linewidth = 0.7) +
  geom_text(
    data = dev_labels_2,
    aes(x = Inf, y = Inf, label = label),
    hjust = 1.05, vjust = 1.5,
    size = 3, fontface = "bold", color = "grey20",
    inherit.aes = FALSE
  ) +
  facet_wrap(~GEAR_SURVEY) +
  scale_color_manual(values = gear_colors) +
  scale_x_log10(name = "Bottom depth (m)") +
  scale_y_continuous(name = "Spread/height") +
  theme_bw(base_size = 10) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "grey95", color = "grey40"),
    strip.text = element_text(face = "bold", size = 9)
  )

p_combined <- (p_spread / p_ratio)

ggsave(
  filename = here::here("plots", "trawl_geometry_by_depth.png"),
  plot = p_combined,
  width = 169,
  height = 120,
  units = "mm",
  dpi = 300
)
