# Depth changes

crosses_threshold <- function(x, threshold, direction = c("both", "up", "down"), strict = TRUE) {
  direction <- match.arg(direction)
  n <- length(x)
  
  if (n == 0) return(logical(0))
  if (n == 1) return(FALSE)
  
  prev <- x[-n]
  curr <- x[-1]
  
  if (strict) {
    # Strictly moving from one side of the threshold to the other
    crossed_up   <- (prev < threshold) & (curr > threshold)
    crossed_down <- (prev > threshold) & (curr < threshold)
  } else {
    # Allows transitions that land on or move off the exact boundary
    crossed_up   <- (prev < threshold & curr >= threshold) | (prev <= threshold & curr > threshold)
    crossed_down <- (prev > threshold & curr <= threshold) | (prev >= threshold & curr < threshold)
  }
  
  result <- switch(
    direction,
    both = crossed_up | crossed_down,
    up   = crossed_up,
    down = crossed_down
  )
  
  # Prepend FALSE because the first point has no preceding point to cross from
  c(FALSE, result)
}

survey_dat <- dplyr::filter(
  trawlmetrics::bts_geom,
  YEAR >= 2025 & SURVEY_DEFINITION_ID %in% c(47, 52)
) |>
  dplyr::bind_rows(
    dplyr::filter(
      trawlmetrics::bts_geom,
      YEAR >= 2025 & SURVEY_DEFINITION_ID == 143
    )
  ) |>
  dplyr::bind_rows(
    dplyr::filter(
      trawlmetrics::bts_geom,
      YEAR >= 2026 & SURVEY_DEFINITION_ID == 98
    )
  ) |>
  dplyr::arrange(HAUL)

survey_dat <- trawlmetrics::bts_geom |>
  dplyr::filter(
    (YEAR >= 2025 & SURVEY_DEFINITION_ID %in% c(47, 52, 143)) |
      (YEAR >= 2026 & SURVEY_DEFINITION_ID == 98)
  ) |>
  dplyr::inner_join(
    data.frame(
      VESSEL_ID = c(148, 176, 134, 162),
      VESSEL_NAME = c("Ocean Explorer", "Alaska Provider", "Northwest Explorer", "Alaska Knight")
    )
  ) |>
  dplyr::arrange(VESSEL_NAME, SURVEY_ABBV, YEAR, HAUL) |>
  dplyr::group_by(VESSEL_NAME, SURVEY_ABBV, YEAR) |>
  dplyr::mutate(
    CROSSED_30M = crosses_threshold(DEPTH_M, threshold = 30, direction = "both"),
    CROSSED_40M = crosses_threshold(DEPTH_M, threshold = 40, direction = "both"),
    CROSSED_50M = crosses_threshold(DEPTH_M, threshold = 50, direction = "both"),
    CROSSED_60M = crosses_threshold(DEPTH_M, threshold = 60, direction = "both"),
    CROSSED_70M = crosses_threshold(DEPTH_M, threshold = 70, direction = "both"),
    CROSSED_100M = crosses_threshold(DEPTH_M, threshold = 100, direction = "both"),
    CROSSED_200M = crosses_threshold(DEPTH_M, threshold = 200, direction = "both")
  ) |>
  dplyr::ungroup()



ggplot(
  data = survey_dat,
  mapping = aes(
    x = HAUL, 
    y = DEPTH_M,
    group = factor(VESSEL_NAME),
  )
) +
  geom_path(alpha = 0.5) +
  geom_point(mapping = aes(color = CROSSED_50M, alpha = CROSSED_50M)) +
  geom_hline(yintercept = 50, linetype = 2) +
  facet_wrap(~paste0(YEAR, " ", SURVEY_ABBV), scales = "free", nrow = 1) +
  scale_y_continuous(name = "Bottom depth (m)", limits = c(0, NA)) +
  scale_x_continuous(name = "Haul") +
  scale_color_colorblind(name = "Crossed 50 m") +
  scale_alpha_manual(name = "Crossed 50 m", values = c('TRUE' = 1, 'FALSE' = 0.2)) +
  theme_bw()



crossed_threshold <- 
  survey_dat |>
  dplyr::group_by(
    VESSEL_NAME, YEAR, SURVEY_ABBV
  ) |>
  dplyr::summarise(
    `30 m` = sum(CROSSED_30M),
    `40 m` = sum(CROSSED_40M),
    `50 m` = sum(CROSSED_50M),
    `60 m` = sum(CROSSED_60M),
    `70 m` = sum(CROSSED_70M),
    `100 m` = sum(CROSSED_100M),
    `200 m` = sum(CROSSED_200M)
  ) |>
  dplyr::arrange(
    YEAR, SURVEY_ABBV, VESSEL_NAME
  )

write.csv(
  crossed_threshold,
  file = here::here("plots", "scope_threshold", "crossed_threshold.csv"),
  row.names = FALSE
)

