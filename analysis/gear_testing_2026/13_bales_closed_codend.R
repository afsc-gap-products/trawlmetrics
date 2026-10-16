# Comparing bale settings

library(trawlmetrics)
library(dplyr)
library(patchwork)
library(readxl)

# Get data ------------

# Gear configuration data
gear_config <- 
  readxl::read_xlsx(path = here::here("data", "2026_gear_config.xlsx")) |>
  dplyr::rename_with(toupper) |>
  dplyr::mutate(
    TOTAL_GEAR_LENGTH_M = (TOTAL_BRIDLE_LENGTH_FT + DOOR_LEG_TAIL_CHAIN_LENGTH_FT + BRIDLE_CHAIN_LENGTH_FT)/3.281,
    DOOR_SIZE_M2 = factor(DOOR_SIZE_M2, levels = c(4, 4.5), labels = c("4.0", "4.5"))
  )


# Door and footrope bottom contact evaluation

# SR and NC reviewed data from individual treatments to evaluate whether 
# sensor data showed the footrope and doors were on bottom. Values were assigned 'Yes', 'No', or
# 'Inconclusive', where the latter indicated sensor data were insufficient to make a determination.

door_footrope_bc <- 
  readxl::read_xlsx(
    path = here::here("data", "scope_performance.xlsx"),
    sheet = "consensus"
  ) |>
  dplyr::rename(WIRE_LENGTH_FM = scope) |>
  dplyr::rename_with(toupper) |>
  dplyr::mutate(
    GEAR_ON_BOTTOM = ifelse(FOOTROPE_ON_BOTTOM == "Yes" & DOORS_ON_BOTTOM == "Yes", "Yes", "No"),
    GEAR_ON_BOTTOM = ifelse(FOOTROPE_ON_BOTTOM == "Inconclusive" | DOORS_ON_BOTTOM == "Inconclusive", "Inconclusive", GEAR_ON_BOTTOM)
  )

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

# Load trawl measurement timeseries 

cc_trawl_measurement_summary <-
  readRDS(file = here::here("output", "haul_summary.rds")) |>
  dplyr::filter(haul >= 600) |> # Use fully processed means for paired hauls
  dplyr::group_by(scope, haul) |>
  dplyr::slice_max(pass, n = 1) |>
  dplyr::ungroup() |>
  dplyr::select(
    HAUL = haul,
    WIRE_LENGTH_FM = scope,
    NET_WIDTH = MEAN_NET_SPREAD,
    SD_NET_WIDTH = SD_NET_SPREAD,
    NET_HEIGHT = MEAN_NET_HEIGHT,
    SD_NET_HEIGHT = SD_NET_HEIGHT,
    DOOR_SPREAD = MEAN_DOOR_SPREAD,
    SD_DOOR_SPREAD = SD_DOOR_SPREAD,
    BOTTOM_DEPTH_FM,
    GEAR_DEPTH = BT_DEPTH_M,
    GEAR_DEPTH_FM = BT_DEPTH_FM
  ) |>
  dplyr::mutate(
    WIRE_LENGTH = WIRE_LENGTH_FM * 1.8288,
    BOTTOM_DEPTH = BOTTOM_DEPTH_FM * 1.8288,
    SCOPE_RATIO = WIRE_LENGTH_FM/BOTTOM_DEPTH_FM,
    INV_SCOPE = 1/SCOPE_RATIO
  ) |>
  dplyr::inner_join(
    gear_config
  )

cc_doors <- 
  readxl::read_xlsx(
  path = here::here("data", "2026_gear_testing_haul_log.xlsx"),
  sheet = "treatments"
)  |>
  dplyr::filter(Haul >= 600) |> # Use fully processed means for paired hauls
  dplyr::group_by(Scope, Haul) |>
  dplyr::slice_max(Pass, n = 1) |>
  dplyr::ungroup() |>
  dplyr::filter(Event == "EQ") |>
  dplyr::select(
    HAUL = Haul,
    WIRE_LENGTH_FM = Scope,
    WINCH_TENSION_P = Port_Tension,
    WINCH_TENSION_S = Stbd_Tension,
    DOOR_PITCH_P = Port_Door_Pitch,
    DOOR_ROLL_P = Port_Door_Roll,
    DOOR_DEPTH_P = Port_Door_Depth,
    DOOR_PITCH_S = Stbd_Door_Pitch,
    DOOR_ROLL_S = Stbd_Door_Roll,
    DOOR_DEPTH_S = Stbd_Door_Depth
  ) |>
  dplyr::rename_with(toupper) |>
  dplyr::mutate(
    WINCH_TENSION_TOTAL = WINCH_TENSION_S + WINCH_TENSION_P,
    DOOR_ROLL_MAX = pmax(abs(DOOR_ROLL_S), abs(DOOR_ROLL_P), na.rm = TRUE)
  ) |>
  dplyr::inner_join(gear_config) |>
  dplyr::inner_join(cc_trawl_measurement_summary) |>
  dplyr::filter(DOOR_ROLL_MAX < 45) # Remove the bad haul where the doors tipped over

cc_door_summary <- 
  cc_doors |>
  dplyr::mutate(DOOR_SETTINGS =  factor(
    DOOR_SETTINGS, 
    levels = c("B-D", "B-E"), 
    labels = c("30.25\u00B0 (B-D)", "32.60\u00B0 (B-E)"),
  )) |>
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
    Model = list(lm(as.formula(Formula_full), data = cc_doors)),
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
  here::here("plots", "rigging_comparison", "cc_door_bale_models.csv"),
  row.names = FALSE
)

best_models <- model_eval |> dplyr::filter(Evaluation == "Best")

pred_grid <- expand.grid(
  BOTTOM_DEPTH = seq(
    min(cc_doors$BOTTOM_DEPTH, na.rm = TRUE),
    max(cc_doors$BOTTOM_DEPTH, na.rm = TRUE),
    length.out = 100
  ),
  DOOR_SETTINGS = unique(cc_doors$DOOR_SETTINGS[!is.na(cc_doors$DOOR_SETTINGS)])
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
          fill = factor(DOOR_SETTINGS, levels = c("B-D", "B-E"), labels = c("30.25\u00B0 (B-D)", "32.60\u00B0 (B-E)"))
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

cc_bale_mapping <- aes(
  x = BOTTOM_DEPTH,
  color = factor(
    DOOR_SETTINGS, 
    levels = c("B-D", "B-E"), 
    labels = c("30.25\u00B0 (B-D)", "32.60\u00B0 (B-E)")
  )
)

# Added scale_fill_manual to all plots so the ribbon fill legend merges perfectly with the point color legend
legend_colors <- c("#000000", "#999999")

p_cc_bale_doors <- 
  ggplot(data = cc_doors, mapping = cc_bale_mapping) +
  add_best_fit("DOOR_SPREAD") +
  geom_point(size = 1.2) +
  aes(y = DOOR_SPREAD) +
  scale_color_manual(name = "Door angle of attack", values = legend_colors) +
  scale_fill_manual(name = "Door angle of attack", values = legend_colors) +
  scale_x_continuous(name = NULL) + 
  scale_y_continuous(name = "Door spread (m)") +
  bale_theme

p_cc_bale_wing <- 
  ggplot(data = dplyr::filter(cc_doors, !is.na(NET_WIDTH)), mapping = cc_bale_mapping) +
  add_best_fit("NET_WIDTH") +
  geom_point(size = 1.2) +
  aes(y = NET_WIDTH) +
  scale_color_manual(name = "Door angle of attack", values = legend_colors) +
  scale_fill_manual(name = "Door angle of attack", values = legend_colors) +
  scale_x_continuous(name = NULL) + 
  scale_y_continuous(name = "Upper wing\nspread (m)") +
  bale_theme

p_cc_bale_height <- 
  ggplot(data = cc_doors, mapping = cc_bale_mapping) +
  add_best_fit("NET_HEIGHT") +
  geom_point(size = 1.2) +
  aes(y = NET_HEIGHT) +
  scale_color_manual(name = "Door angle of attack", values = legend_colors) +
  scale_fill_manual(name = "Door angle of attack", values = legend_colors) +
  scale_x_continuous(name = NULL) + 
  scale_y_continuous(name = "Opening\nheight (m)") +
  bale_theme

p_cc_bale_tension <- 
  ggplot(data = cc_doors, mapping = cc_bale_mapping) +
  add_best_fit("WINCH_TENSION_TOTAL") +
  geom_point(size = 1.2) +
  aes(y = WINCH_TENSION_TOTAL) +
  scale_color_manual(name = "Door angle of attack", values = legend_colors) +
  scale_fill_manual(name = "Door angle of attack", values = legend_colors) +
  scale_x_continuous(name = NULL) +
  scale_y_continuous(name = "Total winch\ntension (t)") +
  bale_theme

p_cc_bale_door_roll <- 
  ggplot(data = cc_doors, mapping = cc_bale_mapping) +
  add_best_fit("DOOR_ROLL_MAX") +
  geom_point(size = 1.2) +
  aes(y = DOOR_ROLL_MAX) +
  scale_color_manual(name = "Door angle of attack", values = legend_colors) +
  scale_fill_manual(name = "Door angle of attack", values = legend_colors) +
  scale_x_continuous(name = "Bottom depth (m)") +
  scale_y_continuous(name = "Maximum\ndoor roll (\u00B0)") +
  bale_theme

p_cc_bale_multipanel <- (p_cc_bale_doors / p_cc_bale_wing / p_cc_bale_height / p_cc_bale_door_roll / p_cc_bale_tension ) + 
  plot_layout(guides = "collect")

ggsave(
  p_cc_bale_multipanel,
  filename = here::here("plots", "rigging_comparison", "cc_door_bale_settings.png"),
  width = 100, height = 120, units = "mm", dpi = 300
)
