library(trawlmetrics)
library(patchwork)
library(readxl)
library(dplyr)
library(car)

# Door trial data ------------

trawl_data_by_haul <- readRDS(here::here("data", "trawl_data_by_haul.rds"))

door_blocks <- readxl::read_xlsx(
  path = here::here("data", "2026_gear_testing_haul_log.xlsx"),
  sheet = "door_blocks"
) |>
  dplyr::select(-DOOR_SIZE_M2)

marport_pings <- readRDS(here::here("data", "marport_pings.rds"))

# Blocks and wire lengths with at least one good treatment per door size
matched_door_treatments <- 
  door_trial_data |>
  dplyr::group_by(
    BLOCK,
    WIRE_LENGTH_FM,
    DOOR_SIZE_M2
  ) |>
  dplyr::summarise(
    n = n(),
    .groups = "drop"
  ) |>
  tidyr::pivot_wider(
    names_from = DOOR_SIZE_M2,
    values_from = n
  ) |>
  dplyr::filter(
    !is.na(`4.5`),
    !is.na(`4.0`)
  ) |>
  dplyr::select(BLOCK, WIRE_LENGTH_FM)

door_trial_data <- trawl_data_by_haul |>
  dplyr::filter(GEAR_ON_BOTTOM == "Yes") |>
  dplyr::inner_join(door_blocks) |>
  dplyr::inner_join(matched_door_treatments) |>
  dplyr::arrange(SCOPE_RATIO)

format_mean_sd <- function(x, digits = 1) {
  m <- mean(x, na.rm = TRUE)
  s <- sd(x, na.rm = TRUE)
  
  if (is.na(m)) return("-")
  if (is.na(s)) return(sprintf(paste0("%.", digits, "f (-)"), m)) # Handles N = 1 cases
  
  sprintf(paste0("%.", digits, "f (%.", digits, "f)"), m, s)
}

table_door_treatment_measurements <- 
  door_trial_data |>
  dplyr::group_by(BLOCK, DOOR_SIZE_M2) |>
  dplyr::summarise(
    N_TRT               = dplyr::n(),
    BOTTOM_DEPTH        = format_mean_sd(BOTTOM_DEPTH),
    DOOR_SPREAD         = format_mean_sd(DOOR_SPREAD),
    NET_SPREAD          = format_mean_sd(NET_WIDTH),
    NET_HEIGHT          = format_mean_sd(NET_HEIGHT),
    DOOR_ROLL_MAX       = format_mean_sd(DOOR_ROLL_MAX),
    WINCH_TENSION_TOTAL = format_mean_sd(WINCH_TENSION_TOTAL),
    .groups = "drop"
  ) |>
  dplyr::arrange(BLOCK, DOOR_SIZE_M2)

write.csv(
  table_door_treatment_measurements,
  here::here("plots", "door_comparison", "door_treatment_table.csv"),
  row.names = FALSE
          )

table_door_treatment_measurements <- 
  door_trial_data |>
  dplyr::group_by(BLOCK, DOOR_SIZE_M2) |>
  dplyr::summarise(
    N_TRT               = dplyr::n(),
    BOTTOM_DEPTH        = format_mean_sd(BOTTOM_DEPTH),
    DOOR_SPREAD         = format_mean_sd(DOOR_SPREAD),
    NET_SPREAD          = format_mean_sd(NET_WIDTH),
    NET_HEIGHT          = format_mean_sd(NET_HEIGHT),
    DOOR_ROLL_MAX       = format_mean_sd(DOOR_ROLL_MAX),
    WINCH_TENSION_TOTAL = format_mean_sd(WINCH_TENSION_TOTAL),
    .groups = "drop"
  ) |>
  dplyr::arrange(BLOCK, DOOR_SIZE_M2)

write.csv(
  table_door_treatment_measurements,
  here::here("plots", "door_comparison", "door_treatment_table.csv"),
  row.names = FALSE
)




# -------------------------------------------------------------------------
# 1. Summary Table Export
# -------------------------------------------------------------------------
table_door_treatment_measurements <- 
  door_trial_data |>
  dplyr::group_by(BLOCK, DOOR_SIZE_M2) |>
  dplyr::summarise(
    N_TRT               = dplyr::n(),
    BOTTOM_DEPTH        = format_mean_sd(BOTTOM_DEPTH),
    DOOR_SPREAD         = format_mean_sd(DOOR_SPREAD),
    NET_SPREAD          = format_mean_sd(NET_WIDTH),
    NET_HEIGHT          = format_mean_sd(NET_HEIGHT),
    DOOR_ROLL_MAX       = format_mean_sd(DOOR_ROLL_MAX),
    WINCH_TENSION_TOTAL = format_mean_sd(WINCH_TENSION_TOTAL),
    .groups = "drop"
  ) |>
  dplyr::arrange(BLOCK, DOOR_SIZE_M2)

write.csv(
  table_door_treatment_measurements,
  here::here("plots", "door_comparison", "door_treatment_table.csv"),
  row.names = FALSE
)

# -------------------------------------------------------------------------
# 2. Statistical Tests (BF Test & Conditional ANOVA / Welch's Test)
# -------------------------------------------------------------------------
vars <- c(
  "NET_WIDTH", "DOOR_SPREAD", "NET_HEIGHT",
  "BRIDLE_ANGLE_DEG", "WINCH_TENSION_TOTAL", "DOOR_ROLL_MAX"
)

# Helper to format F-test with subscripted degrees of freedom: F[list(df1, df2)]
format_f_plotmath <- function(test_name, df1, df2, f_val, p_val) {
  p_str <- if (is.na(p_val)) {
    "' = NA'"
  } else if (p_val < 0.001) {
    "' < 0.001'"
  } else {
    sprintf("' = %.3f'", p_val)
  }
  
  df2_str <- if (is.numeric(df2) && df2 %% 1 != 0) sprintf("%.1f", df2) else sprintf("%d", as.integer(df2))
  df1_str <- sprintf("%d", as.integer(df1))
  
  sprintf(
    "paste('%s: ', italic(F)[list(%s, %s)], ' = %.2f, ', italic(p), %s)",
    test_name, df1_str, df2_str, f_val, p_str
  )
}

test_results <- lapply(vars, function(var) {
  fml <- as.formula(paste(var, "~ factor(DOOR_SIZE_M2)"))
  
  # A. Test for Equal Variances: Brown-Forsythe
  bf_test <- car::leveneTest(fml, data = door_trial_data, center = median)
  bf_df1  <- bf_test$Df[1]
  bf_df2  <- bf_test$Df[2] 
  bf_f    <- bf_test$`F value`[1]
  bf_p    <- bf_test$`Pr(>F)`[1]
  
  # B. Test for Equal Means: Standard ANOVA vs. Welch's Test
  if (!is.na(bf_p) && bf_p < 0.05) {
    welch_fit <- oneway.test(fml, data = door_trial_data, var.equal = FALSE)
    mean_name <- "Welch"
    mean_df1  <- welch_fit$parameter[1]
    mean_df2  <- welch_fit$parameter[2]
    mean_f    <- welch_fit$statistic[[1]]
    mean_p    <- welch_fit$p.value
  } else {
    aov_fit   <- aov(fml, data = door_trial_data)
    aov_sum   <- summary(aov_fit)[[1]]
    mean_name <- "ANOVA"
    mean_df1  <- aov_sum$Df[1]
    mean_df2  <- aov_sum$Df[2] 
    mean_f    <- aov_sum$`F value`[1]
    mean_p    <- aov_sum$`Pr(>F)`[1]
  }
  
  data.frame(
    var        = var,
    mean_label = format_f_plotmath(mean_name, mean_df1, mean_df2, mean_f, mean_p),
    bf_label   = format_f_plotmath("BF", bf_df1, bf_df2, bf_f, bf_p),
    stringsAsFactors = FALSE
  )
}) |>
  dplyr::bind_rows()

mean_lookup <- setNames(test_results$mean_label, test_results$var)
bf_lookup   <- setNames(test_results$bf_label, test_results$var)

# -------------------------------------------------------------------------
# 3. Plotting Helper Function
# -------------------------------------------------------------------------
door_theme <- theme_bw() + 
  theme(
    axis.text        = element_text(size = 8),
    axis.title       = element_text(size = 9),
    panel.grid.minor = element_blank(),
    legend.position  = "none"
  )

presentation_theme <- theme_bw() + 
  theme(
    axis.text        = element_text(size = 14),
    axis.title       = element_text(size = 16),
    panel.grid.minor = element_blank(),
    legend.position  = "none"
  )

make_door_plot <- function(var_name, y_label, hlines = NULL, annotation_size = 2.5, set_theme = door_theme) {
  # Group summary stats (Mean ± SD)
  stats_df <- door_trial_data |>
    dplyr::filter(!is.na(.data[[var_name]])) |>
    dplyr::group_by(DOOR_SIZE_M2) |>
    dplyr::summarise(
      mean_val = mean(.data[[var_name]], na.rm = TRUE),
      sd_val   = sd(.data[[var_name]], na.rm = TRUE),
      y_pos    = max(.data[[var_name]], na.rm = TRUE),
      .groups  = "drop"
    ) |>
    dplyr::mutate(
      label = dplyr::if_else(
        is.na(sd_val),
        sprintf("%.1f", mean_val),
        sprintf("%.1f ± %.1f", mean_val, sd_val)
      )
    )
  
  p <- ggplot(
    door_trial_data, 
    aes(
      x = factor(DOOR_SIZE_M2), 
      y = .data[[var_name]], 
      fill = factor(DOOR_SIZE_M2)
    )
  ) +
    geom_boxplot(
      linewidth = 0.5, 
      alpha = 0.8, 
      outlier.size = 1.2
    ) +
    # Group-level Mean ± SD above each box
    geom_text(
      data = stats_df,
      aes(x = factor(DOOR_SIZE_M2), y = y_pos, label = label),
      vjust = -0.5,
      size = annotation_size,
      inherit.aes = FALSE
    ) +
    # Line 1: Equality of means test (ANOVA or Welch)
    annotate(
      "text",
      x = -Inf, y = Inf,
      hjust = -0.05, vjust = 1.15,
      label = mean_lookup[[var_name]],
      parse = TRUE,
      size = annotation_size
    ) +
    # Line 2: Brown-Forsythe test (tightened vjust to stay near the top border)
    annotate(
      "text",
      x = -Inf, y = Inf,
      hjust = -0.05, vjust = 2.35,
      label = bf_lookup[[var_name]],
      parse = TRUE,
      size = annotation_size
    ) +
    scale_fill_brewer(palette = "Blues") +
    scale_x_discrete(name = expression('Door size (m'^2*')')) +
    scale_y_continuous(
      name = y_label,
      # 45% top expansion reserves dedicated vertical space for the test stats
      expand = expansion(mult = c(0.06, 0.45))
    ) +
    set_theme
  
  if (!is.null(hlines)) {
    p <- p + geom_hline(yintercept = hlines, linetype = 2, color = "grey40")
  }
  
  return(p)
}

# -------------------------------------------------------------------------
# 4. Generate Individual Plots
# -------------------------------------------------------------------------
p_net_width     <- make_door_plot("NET_WIDTH", "Upper wing spread (m)", hlines = c(15, 20))
p_door_spread   <- make_door_plot("DOOR_SPREAD", "Door spread (m)")
p_net_height    <- make_door_plot("NET_HEIGHT", "Opening height (m)", hlines = c(5, 6))
p_bridle_angle  <- make_door_plot("BRIDLE_ANGLE_DEG", "Bridle angle of attack (°)", hlines = c(18, 21))
p_winch_tension <- make_door_plot("WINCH_TENSION_TOTAL", "Winch tension total")
p_door_roll     <- make_door_plot("DOOR_ROLL_MAX", "Door roll max (°)", hlines = c(0, 30))

p_net_width_pres     <- make_door_plot(
  "NET_WIDTH", "Net width (m)",
  hlines = c(15, 20), 
  annotation_size = 4, 
  set_theme = presentation_theme
)

p_door_spread_pres   <- make_door_plot(
  "DOOR_SPREAD", "Door spread (m)", 
  annotation_size = 4, 
  set_theme = presentation_theme
)

p_bridle_angle_pres  <- make_door_plot(
  "BRIDLE_ANGLE_DEG", "Bridle angle of attack (°)", 
  hlines = c(18, 21), 
  annotation_size = 4, 
  set_theme = presentation_theme
)

# -------------------------------------------------------------------------
# 5. Combine with Patchwork
# -------------------------------------------------------------------------
combined_door_figure <- 
  wrap_plots(
    p_net_width, p_door_spread, p_net_height,
    p_bridle_angle, p_winch_tension, p_door_roll,
    ncol = 3
  )

combined_door_figure_pres <- 
  wrap_plots(
    p_net_width_pres,
    p_door_spread_pres,
    p_bridle_angle_pres,
    ncol = 3
  )

# Preview
combined_door_figure

ggsave(
  filename = here::here("plots", "door_comparison", "dc_trawl_measurements_tests.png"),
  plot = combined_door_figure,
  width = 169,
  height = 120,
  units = "mm",
  dpi = 300
)

ggsave(
  filename = here::here("plots", "door_comparison", "dc_trawl_measurements_tests_pres.png"),
  plot = combined_door_figure_pres,
  width = 10,
  height = 5,
  units = "in",
  dpi = 300
)
