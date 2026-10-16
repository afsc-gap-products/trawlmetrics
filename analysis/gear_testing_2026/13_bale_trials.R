# Comparing bale settings

library(trawlmetrics)
library(dplyr)
library(patchwork)
library(readxl)

# Get data ------------

# Closed codend bale trial analysis ----

trawl_data_by_haul <- readRDS(here::here("data", "trawl_data_by_haul.rds"))

bale_blocks <- readxl::read_xlsx(
  path = here::here("data", "2026_gear_testing_haul_log.xlsx"),
  sheet = "bale_blocks"
) |>
  dplyr::select(-DOOR_SETTINGS)

bale_settings_data_open <- trawl_data_by_haul |>
  dplyr::inner_join(
    bale_blocks
  ) |>
  dplyr::filter(CODEND == "Open")

bale_mapping_open <- aes(
  x = SCOPE_RATIO,
  group = HAUL,
  color = DOOR_SETTINGS,
  shape = TOW_DIRECTION
)

legend_colors <- c("#000000", "#999999")

bale_theme <- theme_bw() + 
  theme(
    axis.text = element_text(size = 7),
    axis.title = element_text(size = 8),
    legend.text = element_text(size = 7),
    legend.title = element_text(size = 8),
    legend.position = "bottom",
    legend.direction = "vertical"
  )

p_bale_doors_open <- 
  ggplot(data = bale_settings_data_open, mapping = bale_mapping_open) +
  geom_path(linewidth = 0.4) +
  geom_point(size = 1.2) +
  aes(y = DOOR_SPREAD) +
  scale_color_manual(name = "Door angle of attack", values = legend_colors) +
  scale_shape(name = "Tow direction") +
  scale_x_continuous(name = NULL) + 
  scale_y_continuous(name = "Door\nspread (m)") +
  bale_theme

p_bale_wing_open <- 
  ggplot(data = dplyr::filter(bale_settings_data_open, !is.na(NET_WIDTH)), mapping = bale_mapping_open) +
  geom_path(linewidth = 0.4) +
  geom_point(size = 1.2) +
  aes(y = NET_WIDTH) +
  scale_color_manual(name = "Door angle of attack", values = legend_colors) +
  scale_shape(name = "Tow direction") +
  scale_x_continuous(name = NULL) + 
  scale_y_continuous(name = "Upper wing\nspread (m)") +
  bale_theme

p_bale_height_open <- 
  ggplot(data = bale_settings_data_open, mapping = bale_mapping_open) +
  geom_path(linewidth = 0.4) +
  geom_point(size = 1.2) +
  aes(y = NET_HEIGHT) +
  scale_color_manual(name = "Door angle of attack", values = legend_colors) +
  scale_shape(name = "Tow direction") +
  scale_x_continuous(name = NULL) + 
  scale_y_continuous(name = "Opening\nheight (m)") +
  bale_theme

p_bale_tension_open <- 
  ggplot(data = bale_settings_data_open, mapping = bale_mapping_open) +
  geom_path(linewidth = 0.4) +
  geom_point(size = 1.2) +
  aes(y = WINCH_TENSION_TOTAL) +
  scale_color_manual(name = "Door angle of attack", values = legend_colors) +
  scale_shape(name = "Tow direction") +
  scale_x_continuous(name = "Scope/Depth") +
  scale_y_continuous(name = "Total winch\ntension (t)") +
  bale_theme

p_bale_door_roll_open <- 
  ggplot(data = bale_settings_data_open, mapping = bale_mapping_open) +
  geom_path(linewidth = 0.4) +
  geom_point(size = 1.2) +
  aes(y = DOOR_ROLL_MAX) +
  scale_color_manual(name = "Door angle of attack", values = legend_colors) +
  scale_shape(name = "Tow direction") +
  scale_x_continuous(name = "Scope/Depth") +
  scale_y_continuous(name = "Maximum\ndoor roll (\u00B0)") +
  bale_theme

p_bale_multipanel_open <- (p_bale_doors_open / p_bale_wing_open / p_bale_height_open / p_bale_door_roll_open / p_bale_tension_open ) + 
  plot_layout(guides = "collect")

ggsave(p_bale_multipanel_open,
       filename = here::here("plots", "rigging_comparison", "door_bale_settings_open.png"),
       width = 100, height = 120, units = "mm", dpi = 300)



# Bottom contact sensor data for center (0), outboard footrope (8), and wing tip (13)
bcs_timeseries <- 
  readRDS(file = here::here("output", "bcs_segments.rds")) |>
  dplyr::rename(WIRE_LENGTH_FM = scope) |>
  dplyr::rename_with(toupper) |>
  dplyr::filter(!is.na(WIRE_LENGTH_FM), DISTANCE %in% c(0, 8, 13)) |>
  dplyr::select(DT, BCS_ID, POSITION, DISTANCE, SIDE, HEIGHT_FIT, HAUL, WIRE_LENGTH_FM) |>
  dplyr::left_join(
    door_footrope_bc
  ) |>
  dplyr::left_join(
    gear_config
  )
# 
# # Load trawl measurement timeseries
# 
# cc_trawl_measurement_summary <-
#   readRDS(file = here::here("output", "haul_summary.rds")) |>
#   dplyr::filter(haul >= 600) |> # Use fully processed means for paired hauls
#   dplyr::group_by(scope, haul) |>
#   dplyr::slice_max(pass, n = 1) |>
#   dplyr::ungroup() |>
#   dplyr::select(
#     HAUL = haul,
#     WIRE_LENGTH_FM = scope,
#     NET_WIDTH = MEAN_NET_SPREAD,
#     SD_NET_WIDTH = SD_NET_SPREAD,
#     NET_HEIGHT = MEAN_NET_HEIGHT,
#     SD_NET_HEIGHT = SD_NET_HEIGHT,
#     DOOR_SPREAD = MEAN_DOOR_SPREAD,
#     SD_DOOR_SPREAD = SD_DOOR_SPREAD,
#     BOTTOM_DEPTH_FM,
#     GEAR_DEPTH = BT_DEPTH_M,
#     GEAR_DEPTH_FM = BT_DEPTH_FM
#   ) |>
#   dplyr::mutate(
#     WIRE_LENGTH = WIRE_LENGTH_FM * 1.8288,
#     BOTTOM_DEPTH = BOTTOM_DEPTH_FM * 1.8288,
#     SCOPE_RATIO = WIRE_LENGTH_FM/BOTTOM_DEPTH_FM,
#     INV_SCOPE = 1/SCOPE_RATIO
#   ) |>
#   dplyr::inner_join(
#     gear_config
#   )
# 
# cc_doors <-
#   readxl::read_xlsx(
#   path = here::here("data", "2026_gear_testing_haul_log.xlsx"),
#   sheet = "treatments"
# )  |>
#   dplyr::filter(Haul >= 600) |> # Use fully processed means for paired hauls
#   dplyr::group_by(Scope, Haul) |>
#   dplyr::slice_max(Pass, n = 1) |>
#   dplyr::ungroup() |>
#   dplyr::filter(Event == "EQ") |>
#   dplyr::select(
#     HAUL = Haul,
#     WIRE_LENGTH_FM = Scope,
#     WINCH_TENSION_P = Port_Tension,
#     WINCH_TENSION_S = Stbd_Tension,
#     DOOR_PITCH_P = Port_Door_Pitch,
#     DOOR_ROLL_P = Port_Door_Roll,
#     DOOR_DEPTH_P = Port_Door_Depth,
#     DOOR_PITCH_S = Stbd_Door_Pitch,
#     DOOR_ROLL_S = Stbd_Door_Roll,
#     DOOR_DEPTH_S = Stbd_Door_Depth
#   ) |>
#   dplyr::rename_with(toupper) |>
#   dplyr::mutate(
#     WINCH_TENSION_TOTAL = WINCH_TENSION_S + WINCH_TENSION_P,
#     DOOR_ROLL_MAX = pmax(abs(DOOR_ROLL_S), abs(DOOR_ROLL_P), na.rm = TRUE)
#   ) |>
#   dplyr::inner_join(gear_config) |>
#   dplyr::inner_join(cc_trawl_measurement_summary) |>
#   dplyr::filter(DOOR_ROLL_MAX < 45) # Remove the bad haul where the doors tipped over

bale_settings_data_closed <- trawl_data_by_haul |>
  dplyr::inner_join(
    bale_blocks
  ) |>
  dplyr::filter(CODEND == "Closed",
                DOOR_ROLL_MAX < 45)

bale_summary_closed <- 
  bale_settings_data_closed |>
  dplyr::group_by(DOOR_SETTINGS) |>
  dplyr::summarise(
    N_HAUL = dplyr::n(),
    BOTTOM_DEPTH = mean(BOTTOM_DEPTH),
    DOOR_SPREAD = mean(DOOR_SPREAD, na.rm = TRUE),
    NET_SPREAD = mean(NET_WIDTH, na.rm = TRUE),
    NET_HEIGHT = mean(NET_HEIGHT, na.rm = TRUE),
    DOOR_ROLL_MAX = mean(DOOR_ROLL_MAX, na.rm = TRUE),
    WINCH_TENSION_TOTAL = mean(WINCH_TENSION_P, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::arrange(DOOR_SETTINGS) |>
  dplyr::mutate(
    dplyr::across(
      c(BOTTOM_DEPTH, DOOR_SPREAD, NET_SPREAD, NET_HEIGHT, DOOR_ROLL_MAX, WINCH_TENSION_TOTAL),
      ~ dplyr::if_else(is.na(.x), "-", sprintf("%.1f", .x))
    )
  )

# Compare responses based on linear regression models
responses <- c("DOOR_ROLL_MAX", "DOOR_SPREAD", "WINCH_TENSION_TOTAL", "NET_HEIGHT", "NET_WIDTH")
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
    Model = list(lm(as.formula(Formula_full), data = bale_settings_data_closed )),
    AIC = AIC(Model),
    n_parameters = length(coef(Model))
  ) |>
  dplyr::ungroup() |>
  dplyr::group_by(Response) |>
  dplyr::mutate(
    Delta_AIC = AIC - min(AIC),
    Top_Tier = Delta_AIC < 2,
    Parsimony_Score = ifelse(Top_Tier, n_parameters * 1000 + Delta_AIC, Inf),
    Evaluation = ifelse(Parsimony_Score == min(Parsimony_Score), "Best", "")
  ) |>
  dplyr::ungroup()

final_table <- model_eval |>
  dplyr::mutate(Formula = paste0("~", Predictors)) |>
  dplyr::select(-Formula_full, -Model, -Top_Tier, -Parsimony_Score, -Predictors) |>
  dplyr::arrange(Response, AIC)

write.csv(
  final_table,
  here::here("plots", "rigging_comparison", "door_bale_models_closed.csv"),
  row.names = FALSE
)

best_models <- model_eval |> dplyr::filter(Evaluation == "Best")

pred_grid <- expand.grid(
  BOTTOM_DEPTH = seq(
    min(bale_settings_data_closed$BOTTOM_DEPTH, na.rm = TRUE),
    max(bale_settings_data_closed$BOTTOM_DEPTH, na.rm = TRUE),
    length.out = 100
  ),
  DOOR_SETTINGS = unique(bale_settings_data_closed$DOOR_SETTINGS[!is.na(bale_settings_data_closed$DOOR_SETTINGS)])
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

bale_mapping_closed <- aes(
  x = BOTTOM_DEPTH,
  color = DOOR_SETTINGS
)


p_bale_doors_closed <- 
  ggplot(data = bale_settings_data_closed, mapping = bale_mapping_closed) +
  add_best_fit("DOOR_SPREAD") +
  geom_point(size = 1.2) +
  aes(y = DOOR_SPREAD) +
  scale_color_manual(name = "Door angle of attack", values = legend_colors) +
  scale_fill_manual(name = "Door angle of attack", values = legend_colors) +
  scale_x_continuous(name = NULL) + 
  scale_y_continuous(name = "Door\nspread (m)") +
  bale_theme

p_bale_wing_closed <- 
  ggplot(data = dplyr::filter(bale_settings_data_closed, !is.na(NET_WIDTH)), mapping = bale_mapping_closed) +
  add_best_fit("NET_WIDTH") +
  geom_point(size = 1.2) +
  aes(y = NET_WIDTH) +
  scale_color_manual(name = "Door angle of attack", values = legend_colors) +
  scale_fill_manual(name = "Door angle of attack", values = legend_colors) +
  scale_x_continuous(name = NULL) + 
  scale_y_continuous(name = "Upper wing\nspread (m)") +
  bale_theme

p_bale_height_closed <- 
  ggplot(data = bale_settings_data_closed, mapping = bale_mapping_closed) +
  add_best_fit("NET_HEIGHT") +
  geom_point(size = 1.2) +
  aes(y = NET_HEIGHT) +
  scale_color_manual(name = "Door angle of attack", values = legend_colors) +
  scale_fill_manual(name = "Door angle of attack", values = legend_colors) +
  scale_x_continuous(name = NULL) + 
  scale_y_continuous(name = "Opening\nheight (m)") +
  bale_theme

p_bale_tension_closed <- 
  ggplot(data = bale_settings_data_closed, mapping = bale_mapping_closed) +
  add_best_fit("WINCH_TENSION_TOTAL") +
  geom_point(size = 1.2) +
  aes(y = WINCH_TENSION_TOTAL) +
  scale_color_manual(name = "Door angle of attack", values = legend_colors) +
  scale_fill_manual(name = "Door angle of attack", values = legend_colors) +
  scale_x_continuous(name = NULL) +
  scale_y_continuous(name = "Total winch\ntension (t)") +
  bale_theme

p_bale_door_roll_closed <- 
  ggplot(data = bale_settings_data_closed, mapping = bale_mapping_closed) +
  add_best_fit("DOOR_ROLL_MAX") +
  geom_point(size = 1.2) +
  aes(y = DOOR_ROLL_MAX) +
  scale_color_manual(name = "Door angle of attack", values = legend_colors) +
  scale_fill_manual(name = "Door angle of attack", values = legend_colors) +
  scale_x_continuous(name = "Bottom depth (m)") +
  scale_y_continuous(name = "Maximum\ndoor roll (\u00B0)") +
  bale_theme

p_bale_multipanel_closed <- (p_bale_doors_closed / p_bale_wing_closed / 
                               p_bale_height_closed / p_bale_door_roll_closed / p_bale_tension_closed ) + 
  plot_layout(guides = "collect")

ggsave(
  p_bale_multipanel_closed,
  filename = here::here("plots", "rigging_comparison", "door_bale_settings_closed.png"),
  width = 100, height = 120, units = "mm", dpi = 300
)
