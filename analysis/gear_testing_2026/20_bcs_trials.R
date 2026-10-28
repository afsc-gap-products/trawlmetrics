# Evaluating effects of rigging on footrope and bridle elevation

library(trawlmetrics)
library(ggthemes)
library(dplyr)
library(scales)
library(ggnewscale)
library(glmmTMB)

# Load data
trawl_data_by_haul <- readRDS(here::here("data", "trawl_data_by_haul.rds"))

bcs_data_by_haul <- readRDS(here::here("data", "bcs_data_by_haul.rds"))

# bcs_pings <- 
#   readRDS(here::here("data", "bcs_pings.rds"))

marport_pings <- readRDS(here::here("data", "marport_pings.rds"))

bcs_position_labels <-
  data.frame(
    DISTANCE = c(0, 2, 8, 13, 16, 18, 21, 29),
    POS_NAME = c(
      "Footrope center", 
      "Footrope near center", 
      "Footrope off-center", 
      "Footrope wingtip", 
      "Bridle 2-m ahead of footrope", 
      "Bridle 4-m ahead of footrope", 
      "Bridle 7-m ahead of footrope", 
      "Mid bridle"
    )
  ) |>
  dplyr::mutate(
    DISTANCE_FAC = 
      factor(
        DISTANCE, 
        levels = DISTANCE,
        labels = paste0(DISTANCE, " m BCS")
      ),
    DISTANCE_DETAIL = 
      factor(
        DISTANCE, 
        levels = DISTANCE,
        labels = paste0(DISTANCE, " m BCS: ", POS_NAME)
      )
  )

# Elevation time series ----------------------------------------------------------------------------

p_dtb_by_side <- 
  ggplot() +
  annotate(
    "rect",
    xmin = -Inf,
    xmax = Inf,
    ymin = 0,
    ymax = 2.54 * 3,
    alpha = 0.25,
    fill = "grey50"
  ) +
  geom_vline(xintercept = 533.5, linetype = 2) +
  geom_path(
    data = 
      dplyr::filter(bcs_data_by_haul, !is.na(GEAR_ON_BOTTOM)),
    mapping =
      aes(
        x = HAUL,
        y = BCS_HEIGHT_CM,
        group = interaction(HAUL, WIRE_LENGTH_FM),
        alpha =  DOORS_ON_BOTTOM),
    color = "grey30"
  ) +
  geom_point(
    data =  dplyr::filter(bcs_data_by_haul, !is.na(GEAR_ON_BOTTOM)),
    mapping =
      aes(x = HAUL, 
          y = BCS_HEIGHT_CM, 
          color = SIDE,
          alpha = DOORS_ON_BOTTOM),
    size = 0.6
  ) +
  scale_color_manual(name = "Side", values = c('P'="#264EFF", 'C' = "grey60", 'S' = "#D92632")) +
  scale_alpha_manual(name = "Doors on bottom?", values = c('Yes' = 1, 'No' = 0.1)) +
  scale_y_continuous("Elevation (cm)",
                     limits = c(-2, 40),
                     oob = scales::squish_infinite) +
  scale_x_continuous(name = "Haul") +
  facet_wrap(~DISTANCE) +
  theme_bw() +
  theme(legend.position = "bottom")

png(here::here("plots", "bcs_dtb", "bcs_dtb_by_side_all_hauls.png"), 
    width = 169, height = 169, units = "mm", res = 300)
print(p_dtb_by_side)
dev.off()

events <-
  data.frame(
    event_number = c(1:8),
    event = c(
      "1: Bridle 1.2\n    Half-bobbin\n    Door setback 4-in\n    Headline 349.2 kgf",
      "2: Bridle 2\n    Half-bobbin\n    Door setback 4-in\n    Headline 349.2 kgf",
      "3: Bridle 1.2\n    Bare\n    Door setback 4-in\n    Headline 349.2 kgf",
      "4: Bridle 1.2\n    Chain 120 kg\n    Door setback 8-in\n    Headline 349.2 kgf", 
      "5: Bridle 1.2\n    Chain 68 kg\n    Door setback 8-in\n    Headline 349.2 kgf", 
      "6: Change scope\n    procedure",
      "7: Bridle 1.2\n    Chain 68 kg\n    Door setback 4-in\n    Headline 349.2 kgf",
      "8: Bridle 1.2\n    Chain 68 kg\n    Door setback 4-in\n    Headline 271.6 kgf"),
    haul = c(499.5, 526.5, 530.5, 533.5, 535.5, 539.5, 543.5, 550.5)
  )


# Open codend
p_dtb_events <-
  ggplot() +
  annotate(
    "rect", 
    xmin = -Inf, 
    xmax = Inf, 
    ymin = 0, 
    ymax = 2.54 * 3, 
    alpha = 0.15,  
    fill = "green4"
  ) +
  geom_vline(
    data = events,
    mapping = aes(
      xintercept = haul,
      color = factor(event, levels = events$event)
    ),
    linetype = 2,
    linewidth = 0.5
  ) +
  scale_color_discrete(name = "Change") +
  new_scale_color() +
  geom_text(
    data = events,
    mapping = aes(
      x = haul,
      y = 46.5,
      label = event_number
    ),
    hjust = -0.3,
    color = "grey30",
    size = 2.8
  ) +
  geom_path(
    data = 
      dplyr::filter(
        bcs_data_by_haul, 
        !is.na(GEAR_ON_BOTTOM),
        !(DISTANCE %in% c(2, 18)),
        CODEND == "Open"
      ) |>
      dplyr::inner_join(
        bcs_position_labels
      ),
    mapping = 
      aes(
        x = HAUL, 
        y = BCS_HEIGHT_CM, 
        group = interaction(HAUL, WIRE_LENGTH_FM), 
        color = DOORS_ON_BOTTOM,
        alpha = DOORS_ON_BOTTOM
      ),
    linewidth = 0.25
  ) +
  geom_point(
    data =
      dplyr::filter(
        bcs_data_by_haul, 
        !is.na(GEAR_ON_BOTTOM),
        !(DISTANCE %in% c(2, 18)),
        CODEND == "Open"
      ) |>
      dplyr::inner_join(
        bcs_position_labels
      ),
    mapping = 
      aes(x = HAUL, 
          y = BCS_HEIGHT_CM,
          color = DOORS_ON_BOTTOM,
          alpha = DOORS_ON_BOTTOM,
          shape = DOOR_SIZE_M2
      ),
    size = 0.6
  ) +
  scale_color_manual("Doors on bottom?", values = c('Yes' = "black", 'No' = "red", 'Inconclusive' = "grey40")) +
  scale_alpha_manual("Doors on bottom?", values = c('Yes' = 1, 'No' = 0.2, 'Inconclusive' = 0.5)) +
  scale_shape_manual(name = expression('Door size ('*m^2*')'), values = c(17, 16)) +
  scale_y_continuous("Elevation (cm)", 
                     limits = c(-5, 50), 
                     oob = scales::squish_infinite,
                     breaks = seq(0,40, 10)) +
  scale_x_continuous(name = "Haul") +
  facet_wrap(~DISTANCE_DETAIL, nrow = 7) +
  theme_bw() +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(size = 9, face = "bold", hjust = 0),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 9),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 9),
    legend.key.size = unit(4, "mm"),
    panel.grid = element_line(linewidth = 0.25)
  )

ggsave(
  p_dtb_events,
  filename = here::here("plots", "bcs_dtb", "bcs_dtb_open.png"), 
  width = 169, 
  height = 169, 
  units = "mm", 
  dpi = 300  
)

# Closed codend 
p_dtb_closed <- 
  ggplot() +
  annotate(
    "rect", 
    xmin = -Inf, 
    xmax = Inf, 
    ymin = 0, 
    ymax = 2.54 * 3, 
    alpha = 0.15,  
    fill = "green4"
  ) +
  geom_path(
    data = 
      dplyr::filter(
        bcs_data_by_haul, 
        !is.na(GEAR_ON_BOTTOM),
        DISTANCE %in% c(0, 8, 13, 16, 21),
        CODEND == "Closed"
      ) |>
      dplyr::inner_join(dplyr::select(trawl_data_by_haul, HAUL, BOTTOM_DEPTH)) |>
      dplyr::inner_join(
        bcs_position_labels
      ),
    mapping = 
      aes(
        x = HAUL, 
        y = BCS_HEIGHT_CM, 
        group = HAUL 
      )
  ) +
  geom_point(
    data =
      dplyr::filter(
        bcs_data_by_haul, 
        !is.na(GEAR_ON_BOTTOM),
        DISTANCE %in% c(0, 8, 13, 16, 21),
        CODEND == "Closed"
      ) |>
      dplyr::inner_join(dplyr::select(trawl_data_by_haul, HAUL, BOTTOM_DEPTH)) |>
      dplyr::inner_join(
        bcs_position_labels
      ),
    mapping = 
      aes(
        x = HAUL, 
        y = BCS_HEIGHT_CM,
        shape = DOOR_SIZE_M2
      )
  ) +
  scale_fill_viridis_c(name = "Bottom depth (m)", direction = -1, option = "E") +
  scale_color_viridis_c(name = "Bottom depth (m)", direction = -1, option = "E") +
  scale_y_continuous("Elevation (cm)", 
                     limits = c(-2, 40), 
                     oob = scales::squish_infinite) +
  scale_x_continuous(name = "Haul") +
  facet_wrap(~DISTANCE_DETAIL, nrow = 5) +
  theme_bw() +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", hjust = 0)
  )


ggsave(
  p_dtb_closed,
  filename = here::here("plots", "bcs_dtb", "bcs_dtb_closed.png"),
  width = 169, 
  height = 120, 
  units = "mm", 
  dpi = 300
)


bcs_closed_open_summary <- bcs_data_by_haul |>
  dplyr::filter(GEAR_ON_BOTTOM == "Yes") |>
  dplyr::group_by(DISTANCE, CODEND, DOOR_SIZE_M2, BRIDLE_CHAIN_WEIGHT) |>
  dplyr::summarise(
    BCS_HEIGHT_CM = mean(BCS_HEIGHT_CM, na.rm = TRUE)
  )

# Mean elevation by bridle treatment ---------------------------------------------------------------

bcs_hline <- 
  dplyr::filter(
    bcs_position_labels, DISTANCE < 16
  ) |>
  dplyr::mutate(
    min = 0,
    max = 2.54*3
  )

codend_symbols <- c('Open' = 1, 'Closed' = 16)
codend_symbol_size <- c("Closed" = 3.5, "Open" = 1.8)

p_mean_bcs_height_45 <- 
  ggplot() + 
  geom_point(
    data = dplyr::filter(bcs_closed_open_summary, DOOR_SIZE_M2 == 4.5),
    mapping = 
      aes(
        x = DISTANCE,
        y = BCS_HEIGHT_CM,
        shape = CODEND,
        color = factor(BRIDLE_CHAIN_WEIGHT)
      ),
    size = 2.3
  ) +
  geom_hline(yintercept = c(0, 2.54*3), linetype = 2) +
  scale_x_continuous(name = "Distance from center (m)", breaks = c(0, 2, 8, 13, 16, 18, 21, 29)) +
  scale_y_continuous(name = "Footrope/bridle elevation (cm)") +
  scale_shape_manual(name = "Codend", values = codend_symbols) +
  scale_color_manual(name = "Addl. bridle weight (kg)", values = viridis_pal(option = "C", direction = -1)(4)[2:4]) +
  theme_bw() +
  theme(
    strip.text = element_text(face = "bold", hjust = 0, size = 9),
    strip.background = element_blank(),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 8),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 8),
    legend.key.size = unit(4, "mm"),
    panel.grid = element_line(linewidth = 0.25)
  )

png(here::here("plots", "bcs_dtb", "bcs_dtb_45m2_doors.png"), width = 120, height = 80, units = "mm", res = 300)
print(p_mean_bcs_height_45)
dev.off()


bcs_data_by_haul |>
  dplyr::filter(GEAR_ON_BOTTOM == "Yes") |>
  dplyr::group_by(DISTANCE, CODEND, DOOR_SIZE_M2, BRIDLE_CHAIN_WEIGHT, BRIDLE_OPTION, DOOR_SETTINGS) |>
  dplyr::summarise(
    BCS_HEIGHT_CM = mean(BCS_HEIGHT_CM, na.rm = TRUE),
    n = n()
  )


# Models comapring effects of bridle chain weights on elevation ----


library(dplyr)

# 1. Target distances (add or remove values as needed)
target_distances <- c(0, 8, 13, 21)

# 2. Unified data prep across all distances
bcs_data <- bcs_data_by_haul |>
  dplyr::inner_join(
    dplyr::select(trawl_data_by_haul, HAUL, WIRE_LENGTH_FM, BOTTOM_DEPTH),
    by = c("HAUL", "WIRE_LENGTH_FM")
  ) |>
  dplyr::filter(
    GEAR_ON_BOTTOM == "Yes",
    DISTANCE %in% target_distances,
    BRIDLE_CHAIN_WEIGHT < 120,
    DOOR_SIZE_M2 == "4.5",
    BRIDLE_OPTION == 1.2,
    FLOAT_BUOYANCY_KGF == 349.2
  ) |>
  dplyr::group_by(DISTANCE, HAUL, BRIDLE_CHAIN_WEIGHT, CODEND, BOTTOM_DEPTH, WIRE_LENGTH_FM) |>
  dplyr::summarise(
    BCS_HEIGHT_CM = mean(BCS_HEIGHT_CM, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    HAUL = factor(HAUL),
    BRIDLE_CHAIN_WEIGHT = factor(BRIDLE_CHAIN_WEIGHT),
    CODEND = factor(CODEND)
  )

calc_aicc <- function(fit) {
  k <- attr(logLik(fit), "df")
  n <- nobs(fit)
  aic <- AIC(fit)
  aic + (2 * k * (k + 1)) / (n - k - 1)
}

# Model fitting, AICc calculation, parameter estimation, and parsimonious selection
fit_and_compare <- function(df) {
  dist_val <- unique(df$DISTANCE)
  
  m_null <- glmmTMB(
    BCS_HEIGHT_CM ~ 1 + (1 | HAUL),
    data = df,
    REML = FALSE
  )
  
  m_chain <- glmmTMB(
    BCS_HEIGHT_CM ~ 0 + BRIDLE_CHAIN_WEIGHT + (1 | HAUL),
    data = df,
    REML = FALSE
  )
  
  models <- list("Null" = m_null, "Bridle Chain Weight" = m_chain)
  
  # Compute model-level selection metrics
  model_stats <- data.frame(
    model = names(models),
    df = vapply(models, function(m) attr(logLik(m), "df"), numeric(1)),
    AICc = vapply(models, calc_aicc, numeric(1)),
    stringsAsFactors = FALSE
  )
  
  # Delta AICc and Akaike weights (computed across models, not parameters)
  model_stats$delta_AICc <- round(model_stats$AICc - min(model_stats$AICc), 2)
  model_stats$AICc <- round(model_stats$AICc, 2)
  model_stats$weight <- round(exp(-0.5 * model_stats$delta_AICc) / sum(exp(-0.5 * model_stats$delta_AICc)), 3)
  
  # Parsimony rule: among models with delta AICc < 2, pick the one with the fewest parameters
  competitive <- model_stats[model_stats$delta_AICc < 2, ]
  min_df <- min(competitive$df)
  selected_model <- competitive$model[competitive$df == min_df][1]
  model_stats$best_model <- ifelse(model_stats$model == selected_model, "Yes", "No")
  
  # Extract fixed-effect estimates and 95% Wald confidence intervals
  param_list <- lapply(names(models), function(m_name) {
    ci <- confint(models[[m_name]], parm = "beta_", level = 0.95)
    data.frame(
      model = m_name,
      term = sub("^cond\\.", "", rownames(ci)),
      estimate = round(ci[, "Estimate"], 2),
      conf_low = round(ci[, "2.5 %"], 2),
      conf_high = round(ci[, "97.5 %"], 2),
      stringsAsFactors = FALSE
    )
  })
  param_df <- do.call(rbind, param_list)
  
  res <- dplyr::left_join(model_stats, param_df, by = "model") |>
    dplyr::mutate(distance = dist_val) |>
    dplyr::relocate(distance, model, term, estimate, conf_low, conf_high, df, AICc, delta_AICc, weight, best_model)
  
  return(res)
}

bcs_model_table <- 
  split(bcs_data, bcs_data$DISTANCE) |>
  lapply(fit_and_compare) |>
  dplyr::bind_rows()


bcs_model_table_formatted <- 
  bcs_model_table |>
  dplyr::mutate(par_label = sprintf("%.1f [%.1f, %.1f]", estimate, conf_low, conf_high)) |>
  dplyr::select(distance, model, term, AICc, delta_AICc, best_model, par_label) |>
  tidyr::pivot_wider(
    names_from = term, values_from = par_label, values_fill = "-"
  )


write.csv(
  bcs_model_table_formatted, 
  here::here("plots", "bcs_dtb", "bcs_model_table.csv"),
  row.names = FALSE
)
