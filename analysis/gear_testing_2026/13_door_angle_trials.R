# Comparing door angle settings

library(trawlmetrics)
library(dplyr)
library(patchwork)
library(readxl)

# Get data ------------

# Closed codend door angle of attack trial analysis ----

trawl_data_by_haul <- readRDS(here::here("data", "trawl_data_by_haul.rds"))

da_blocks <- readxl::read_xlsx(
  path = here::here("data", "2026_gear_testing_haul_log.xlsx"),
  sheet = "door_angle_blocks"
) |>
  dplyr::select(-DOOR_SETTINGS)

trawl_data_by_haul |>
  dplyr::inner_join(da_blocks) |>
dplyr::group_by(CODEND) |>
  dplyr::summarise(MIN_DEPTH = min(BOTTOM_DEPTH),
                   MAX_DEPTH = max(BOTTOM_DEPTH))

bcs_13m_height <- readRDS(here::here("data", "bcs_data_by_haul.rds")) |>
  dplyr::filter(
    DISTANCE == 13
  ) |>
  dplyr::group_by(HAUL, WIRE_LENGTH_FM) |>
  dplyr::summarise(
    BCS_13MHEIGHT_CM = mean(BCS_HEIGHT_CM, na.rm = TRUE)
  )

da_settings_data_open <- 
  trawl_data_by_haul |>
  dplyr::inner_join(
    da_blocks
  ) |>
  dplyr::filter(CODEND == "Open") |>
  dplyr::left_join(
    bcs_13m_height
  ) |>
  dplyr::mutate(
    WIRE_LENGTH_FAC = factor(WIRE_LENGTH_FM),
                DOOR_SETTINGS_LABEL = as.character(DOOR_SETTINGS),
    DOOR_SETTINGS_LABEL = gsub(x = DOOR_SETTINGS_LABEL, replacement = "\n", pattern = " "))

da_mapping_open <- aes(
  x = DOOR_SETTINGS_LABEL,
  group = interaction(TOW_DIRECTION, WIRE_LENGTH_FM),
  color = WIRE_LENGTH_FAC,
  shape = TOW_DIRECTION
)

legend_colors <- viridis_pal()(length(unique(da_settings_data_open$WIRE_LENGTH_FM))+1)

da_theme <- theme_bw() + 
  theme(
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 9),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 9),
    legend.position = "bottom",
    legend.direction = "vertical"
  )

p_da_doors_open <- 
  ggplot(data = da_settings_data_open, mapping = da_mapping_open) +
  geom_path(linewidth = 0.4) +
  geom_point(size = 1.2) +
  aes(y = DOOR_SPREAD) +
  scale_color_manual(name = "Scope (fm)", values = legend_colors) +
  scale_shape(name = "Tow direction", solid = FALSE) +
  scale_x_discrete(name = NULL) +
  scale_y_continuous(name = "Door\nspread (m)") +
  da_theme

p_da_wing_open <- 
  ggplot(data = dplyr::filter(da_settings_data_open, !is.na(NET_WIDTH)), mapping = da_mapping_open) +
  geom_path(linewidth = 0.4) +
  geom_point(size = 1.2) +
  aes(y = NET_WIDTH) +
  scale_color_manual(name = "Scope (fm)", values = legend_colors) +
  scale_shape(name = "Tow direction", solid = FALSE) +
  scale_x_discrete(name = NULL) +
  scale_y_continuous(name = "Upper wing\nspread (m)") +
  da_theme

p_da_height_open <- 
  ggplot(data = da_settings_data_open, mapping = da_mapping_open) +
  geom_path(linewidth = 0.4) +
  geom_point(size = 1.2) +
  aes(y = NET_HEIGHT) +
  scale_color_manual(name = "Scope (fm)", values = legend_colors) +
  scale_shape(name = "Tow direction", solid = FALSE) +
  scale_x_discrete(name = NULL) + 
  scale_y_continuous(name = "Opening\nheight (m)") +
  da_theme

p_da_footrope_ele_open <- 
  ggplot(data = da_settings_data_open, mapping = da_mapping_open) +
  geom_path(linewidth = 0.4) +
  geom_point(size = 1.2) +
  aes(y = BCS_13MHEIGHT_CM) +
  scale_color_manual(name = "Scope (fm)", values = legend_colors) +
  scale_shape(name = "Tow direction", solid = FALSE) +
  scale_x_discrete(name = NULL) +
  scale_y_continuous(name = "Footrope 13-m\nelevation (cm)") +
  da_theme

p_da_door_roll_open <- 
  ggplot(data = da_settings_data_open, mapping = da_mapping_open) +
  geom_path(linewidth = 0.4) +
  geom_point(size = 1.2) +
  aes(y = DOOR_ROLL_MAX) +
  scale_color_manual(name = "Scope (fm)", values = legend_colors) +
  scale_shape(name = "Tow direction", solid = FALSE) +
  scale_x_discrete(name = "Door angle of attack setting") +

  scale_y_continuous(name = "Maximum\ndoor roll (\u00B0)") +
  da_theme

p_da_door_pitch_open <- 
  ggplot(data = da_settings_data_open, mapping = da_mapping_open) +
  geom_path(linewidth = 0.4) +
  geom_point(size = 1.2) +
  aes(y = DOOR_PITCH_MEAN) +
  scale_color_manual(name = "Scope (fm)", values = legend_colors) +
  scale_shape(name = "Tow direction", solid = FALSE) +
  scale_x_discrete(name = NULL) +
  scale_y_continuous(name = "Door pitch (\u00B0)") +
  da_theme

p_da_tension_open <- 
  ggplot(data = da_settings_data_open, mapping = da_mapping_open) +
  geom_path(linewidth = 0.4) +
  geom_point(size = 1.2) +
  aes(y = WINCH_TENSION_TOTAL) +
  scale_color_manual(name = "Scope (fm)", values = legend_colors) +
  scale_shape(name = "Tow direction", solid = FALSE) +
  scale_x_discrete(name = NULL) +
  scale_y_continuous(name = "Total winch\ntension (t)") +
  da_theme

p_da_multipanel_open <- (p_da_doors_open + p_da_wing_open + p_da_height_open + p_da_footrope_ele_open + p_da_door_roll_open + p_da_door_pitch_open ) + 
  plot_layout(guides = "collect", nrow = 2, ncol = 3)

ggsave(p_da_multipanel_open,
       filename = here::here("plots", "rigging_comparison", "door_da_settings_open.png"),
       width = 169, height = 100, units = "mm", dpi = 300)

da_settings_data_closed <- 
  trawl_data_by_haul |>
  dplyr::inner_join(
    da_blocks
  ) |>
  dplyr::filter(
    CODEND == "Closed",
    DOOR_ROLL_MAX < 45
  ) |>
  dplyr::left_join(
    bcs_13m_height
  )

da_summary_closed <- 
  da_settings_data_closed |>
  dplyr::group_by(DOOR_SETTINGS) |>
  dplyr::summarise(
    N_HAUL = dplyr::n(),
    BOTTOM_DEPTH = mean(BOTTOM_DEPTH),
    DOOR_SPREAD = mean(DOOR_SPREAD, na.rm = TRUE),
    NET_SPREAD = mean(NET_WIDTH, na.rm = TRUE),
    NET_HEIGHT = mean(NET_HEIGHT, na.rm = TRUE),
    DOOR_ROLL_MAX = mean(DOOR_ROLL_MAX, na.rm = TRUE),
    WINCH_TENSION_TOTAL = mean(WINCH_TENSION_P, na.rm = TRUE),
    BCS_13MHEIGHT_CM = mean(BCS_13MHEIGHT_CM, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::arrange(DOOR_SETTINGS) |>
  dplyr::mutate(
    dplyr::across(
      c(BOTTOM_DEPTH, DOOR_SPREAD, NET_SPREAD, NET_HEIGHT, DOOR_ROLL_MAX, WINCH_TENSION_TOTAL),
      ~ dplyr::if_else(is.na(.x), "-", sprintf("%.1f", .x))
    )
  )

# Compare responses based on linear regression models using AICc
responses <- c("DOOR_ROLL_MAX", "DOOR_SPREAD", "WINCH_TENSION_TOTAL", "NET_HEIGHT", "NET_WIDTH", "BCS_13MHEIGHT_CM")
rhs_formulas <- c("1", 
                  "BOTTOM_DEPTH", 
                  "DOOR_SETTINGS",
                  "BOTTOM_DEPTH + DOOR_SETTINGS", 
                  "BOTTOM_DEPTH * DOOR_SETTINGS")

model_grid <- expand.grid(
  Response = responses, 
  Predictors = rhs_formulas, 
  stringsAsFactors = FALSE
)

model_eval <- model_grid |>
  dplyr::rowwise() |>
  dplyr::mutate(
    Formula_full = paste(Response, "~", Predictors),
    Model = list(lm(as.formula(Formula_full), data = da_settings_data_closed)),
    k = attr(logLik(Model), "df"), # Number of parameters (including variance)
    n = nobs(Model),                # Sample size
    AICc = AIC(Model) + (2 * k * (k + 1)) / (n - k - 1), # Bias-corrected AIC
    n_parameters = length(coef(Model))
  ) |>
  dplyr::ungroup() |>
  dplyr::group_by(Response) |>
  dplyr::mutate(
    Delta_AICc = AICc - min(AICc),
    Top_Tier = Delta_AICc < 2,
    Parsimony_Score = ifelse(Top_Tier, n_parameters * 1000 + Delta_AICc, Inf),
    Evaluation = ifelse(Parsimony_Score == min(Parsimony_Score), "Best", "")
  ) |>
  dplyr::ungroup()

final_table <- model_eval |>
  dplyr::mutate(Formula = paste0("~", Predictors)) |>
  dplyr::select(-Formula_full, -Model, -Top_Tier, -Parsimony_Score, -Predictors, -k, -n) |>
  dplyr::arrange(Response, AICc)

write.csv(
  final_table,
  here::here("plots", "rigging_comparison", "door_da_models_closed.csv"),
  row.names = FALSE
)

best_models <- model_eval |> dplyr::filter(Evaluation == "Best")

pred_grid <- expand.grid(
  BOTTOM_DEPTH = seq(
    min(da_settings_data_closed$BOTTOM_DEPTH, na.rm = TRUE),
    max(da_settings_data_closed$BOTTOM_DEPTH, na.rm = TRUE),
    length.out = 100
  ),
  DOOR_SETTINGS = unique(da_settings_data_closed$DOOR_SETTINGS[!is.na(da_settings_data_closed$DOOR_SETTINGS)])
)

# Populate grid with predicted values and standard errors across all best models
for(i in seq_len(nrow(best_models))) {
  resp <- best_models$Response[i]
  mod <- best_models$Model[[i]]
  
  # Predict with standard errors
  pred_obj <- predict(mod, newdata = pred_grid, se.fit = TRUE)
  
  # Store fit and calculate bounds (+/- 1 Standard Error)
  pred_grid[[resp]] <- pred_obj$fit
  pred_grid[[paste0(resp, "_ymin")]] <- pred_obj$fit - pred_obj$se.fit
  pred_grid[[paste0(resp, "_ymax")]] <- pred_obj$fit + pred_obj$se.fit
}

# Helper function to dynamically add ribbon and line
add_best_fit <- function(resp_name) {
  has_settings <- grepl("DOOR_SETTINGS", best_models$Predictors[best_models$Response == resp_name])
  
  ymin_col <- paste0(resp_name, "_ymin")
  ymax_col <- paste0(resp_name, "_ymax")
  
  if(has_settings) {
    list(
      geom_ribbon(
        data = pred_grid,
        mapping = aes(
          y = .data[[resp_name]], 
          ymin = .data[[ymin_col]], 
          ymax = .data[[ymax_col]],
          fill = DOOR_SETTINGS
        ),
        alpha = 0.2,
        color = NA
      ),
      geom_line(data = pred_grid, mapping = aes(y = .data[[resp_name]]), linewidth = 0.4)
    )
  } else {
    # If DOOR_SETTINGS isn't a predictor, plot one overall fit using distinct rows
    list(
      geom_ribbon(
        data = dplyr::distinct(pred_grid, BOTTOM_DEPTH, .keep_all = TRUE),
        mapping = aes(
          x = BOTTOM_DEPTH, 
          ymin = .data[[ymin_col]], 
          ymax = .data[[ymax_col]]
        ),
        fill = "black",
        alpha = 0.2,
        inherit.aes = FALSE
      ),
      geom_line(
        data = dplyr::distinct(pred_grid, BOTTOM_DEPTH, .keep_all = TRUE),
        mapping = aes(x = BOTTOM_DEPTH, y = .data[[resp_name]]), 
        linewidth = 0.4, 
        color = "blue3",
        inherit.aes = FALSE
      )
    )
  }
}

# Plots of closed codend tows

da_mapping_closed <- aes(
  x = BOTTOM_DEPTH,
  color = DOOR_SETTINGS
)


p_da_doors_closed <- 
  ggplot(data = da_settings_data_closed, mapping = da_mapping_closed) +
  add_best_fit("DOOR_SPREAD") +
  geom_point(size = 1.2) +
  aes(y = DOOR_SPREAD) +
  scale_color_manual(name = "Door angle of attack", values = legend_colors) +
  scale_fill_manual(name = "Door angle of attack", values = legend_colors) +
  scale_x_continuous(name = NULL) + 
  scale_y_continuous(name = "Door\nspread (m)") +
  da_theme

p_da_wing_closed <- 
  ggplot(data = dplyr::filter(da_settings_data_closed, !is.na(NET_WIDTH)), mapping = da_mapping_closed) +
  add_best_fit("NET_WIDTH") +
  geom_point(size = 1.2) +
  aes(y = NET_WIDTH) +
  scale_color_manual(name = "Door angle of attack", values = legend_colors) +
  scale_fill_manual(name = "Door angle of attack", values = legend_colors) +
  scale_x_continuous(name = NULL) + 
  scale_y_continuous(name = "Upper wing\nspread (m)") +
  da_theme

p_da_height_closed <- 
  ggplot(data = da_settings_data_closed, mapping = da_mapping_closed) +
  add_best_fit("NET_HEIGHT") +
  geom_point(size = 1.2) +
  aes(y = NET_HEIGHT) +
  scale_color_manual(name = "Door angle of attack", values = legend_colors) +
  scale_fill_manual(name = "Door angle of attack", values = legend_colors) +
  scale_x_continuous(name = NULL) + 
  scale_y_continuous(name = "Opening\nheight (m)") +
  da_theme

p_da_footrope_ele_closed <- 
  ggplot(data = da_settings_data_closed, mapping = da_mapping_closed) +
  add_best_fit("BCS_13MHEIGHT_CM") +
  geom_point(size = 1.2) +
  aes(y = BCS_13MHEIGHT_CM) +
  scale_color_manual(name = "Door angle of attack", values = legend_colors) +
  scale_fill_manual(name = "Door angle of attack", values = legend_colors) +
  scale_x_continuous(name = NULL) + 
  scale_y_continuous(name = "Footrope 13-m\nelevation (cm)") +
  da_theme

p_da_tension_closed <- 
  ggplot(data = da_settings_data_closed, mapping = da_mapping_closed) +
  add_best_fit("WINCH_TENSION_TOTAL") +
  geom_point(size = 1.2) +
  aes(y = WINCH_TENSION_TOTAL) +
  scale_color_manual(name = "Door angle of attack", values = legend_colors) +
  scale_fill_manual(name = "Door angle of attack", values = legend_colors) +
  scale_x_continuous(name = "Bottom depth (m)") +
  scale_y_continuous(name = "Total winch\ntension (t)") +
  da_theme

p_da_door_roll_closed <- 
  ggplot(data = da_settings_data_closed, mapping = da_mapping_closed) +
  add_best_fit("DOOR_ROLL_MAX") +
  geom_point(size = 1.2) +
  aes(y = DOOR_ROLL_MAX) +
  scale_color_manual(name = "Door angle of attack", values = legend_colors) +
  scale_fill_manual(name = "Door angle of attack", values = legend_colors) +
  scale_x_continuous(name = NULL) +
  scale_y_continuous(name = "Maximum\ndoor roll (\u00B0)") +
  da_theme

p_da_multipanel_closed <- (p_da_doors_closed / p_da_wing_closed / 
                               p_da_height_closed / p_da_footrope_ele_closed / p_da_door_roll_closed / p_da_tension_closed ) + 
  plot_layout(guides = "collect")

ggsave(
  p_da_multipanel_closed,
  filename = here::here("plots", "rigging_comparison", "door_da_settings_closed.png"),
  width = 140, height = 140, units = "mm", dpi = 300
)
