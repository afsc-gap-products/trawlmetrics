# Compare bridle treatments

library(trawlmetrics)
library(readxl)
library(dplyr)

bridle_treatments <- 
  readxl::read_xlsx(path = here::here("data", "2026_gear_testing_haul_log.xlsx"),
                                       sheet = "bridle_blocks") |>
  dplyr::select(-DOOR_SIZE_M2)

bcs_13m_height <- readRDS(here::here("data", "bcs_data_by_haul.rds")) |>
  dplyr::filter(
    DISTANCE == 13
  ) |>
  dplyr::group_by(HAUL, WIRE_LENGTH_FM) |>
  dplyr::summarise(
    BCS_13MHEIGHT_CM = mean(BCS_HEIGHT_CM, na.rm = TRUE)
  )

trawl_data_by_haul <- readRDS(here::here("data", "trawl_data_by_haul.rds"))

# Compare effects of bridle rigging
bridle_settings_data <- 
  trawl_data_by_haul |>
  dplyr::inner_join(
    bridle_treatments
  ) |>
  dplyr::filter(GEAR_ON_BOTTOM == "Yes") |>
  dplyr::mutate(
    BLOCK = factor(gsub(x = BLOCK, pattern = "Bridles_", replacement = "")),
    BRIDLE_OPTION = factor(BRIDLE_OPTION)
  ) |>
  dplyr::left_join(
    bcs_13m_height
  )

dplyr::group_by(
  bridle_settings_data, BLOCK, BRIDLE_OPTION, WIRE_LENGTH_FM
) |>
  dplyr::summarise(
    n = n()
  )

bridle_theme <- theme_bw() + 
  theme(
    axis.text = element_text(size = 7),
    axis.title = element_text(size = 8),
    legend.text = element_text(size = 7),
    legend.title = element_text(size = 8),
    legend.position = "bottom",
    legend.direction = "vertical"
  )
  
  bridle_summary <- 
  bridle_settings_data |>
  dplyr::group_by(BRIDLE_OPTION) |>
  dplyr::summarise(
    N_HAUL = dplyr::n(),
    BOTTOM_DEPTH = mean(BOTTOM_DEPTH),
    DOOR_SPREAD = mean(DOOR_SPREAD, na.rm = TRUE),
    NET_SPREAD = mean(NET_WIDTH, na.rm = TRUE),
    NET_HEIGHT = mean(NET_HEIGHT, na.rm = TRUE),
    BCS_13MHEIGHT_CM = mean(BCS_13MHEIGHT_CM, na.rm = TRUE),
    DOOR_ROLL_MAX = mean(DOOR_ROLL_MAX, na.rm = TRUE),
    WINCH_TENSION_TOTAL = mean(WINCH_TENSION_P, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::arrange(BRIDLE_OPTION) |>
  dplyr::mutate(
    dplyr::across(
      c(BOTTOM_DEPTH, DOOR_SPREAD, NET_SPREAD, NET_HEIGHT, DOOR_ROLL_MAX, WINCH_TENSION_TOTAL),
      ~ dplyr::if_else(is.na(.x), "-", sprintf("%.1f", .x))
    )
  )

# Compare responses based on linear regression models
responses <- c("DOOR_ROLL_MAX", "DOOR_SPREAD", "WINCH_TENSION_TOTAL", "NET_HEIGHT")
rhs_formulas <- c("1", 
                  "BOTTOM_DEPTH", 
                  "BRIDLE_OPTION",
                  "BOTTOM_DEPTH + BRIDLE_OPTION", 
                  "BOTTOM_DEPTH * BRIDLE_OPTION")

model_grid <- expand.grid(
  Response = responses, 
  Predictors = rhs_formulas, 
  stringsAsFactors = FALSE
)

model_eval <- model_grid |>
  dplyr::rowwise() |>
  dplyr::mutate(
    Formula_full = paste(Response, "~", Predictors),
    Model = list(lm(as.formula(Formula_full), data = bridle_settings_data )),
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
  here::here("plots", "rigging_comparison", "door_bridle_models.csv"),
  row.names = FALSE
)

best_models <- model_eval |> dplyr::filter(Evaluation == "Best")

pred_grid <- expand.grid(
  BOTTOM_DEPTH = seq(
    min(bridle_settings_data$BOTTOM_DEPTH, na.rm = TRUE),
    max(bridle_settings_data$BOTTOM_DEPTH, na.rm = TRUE),
    length.out = 100
  ),
  BRIDLE_OPTION = unique(bridle_settings_data$BRIDLE_OPTION[!is.na(bridle_settings_data$BRIDLE_OPTION)])
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
  has_settings <- grepl("BRIDLE_OPTION", best_models$Predictors[best_models$Response == resp_name])
  
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
          fill = BRIDLE_OPTION
        ),
        alpha = 0.2,
        color = NA
      ),
      geom_line(data = pred_grid, mapping = aes(y = .data[[resp_name]]), linewidth = 0.4)
    )
  } else {
    # If BRIDLE_OPTION isn't a predictor, plot one overall fit using distinct rows
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

bridle_mapping <- aes(
  x = BOTTOM_DEPTH,
  color = BRIDLE_OPTION
)

legend_colors <- c('1.2' = "darkred", '2' = "orange")

p_bridle_doors <- 
  ggplot(data = bridle_settings_data, mapping = bridle_mapping) +
  add_best_fit("DOOR_SPREAD") +
  geom_point(size = 1.2) +
  aes(y = DOOR_SPREAD) +
  scale_color_manual(name = "Bridle option", values = legend_colors) +
  scale_fill_manual(name = "Bridle option", values = legend_colors) +
  scale_x_continuous(name = NULL) + 
  scale_y_continuous(name = "Door\nspread (m)") +
  bridle_theme

p_bridle_wing <- 
  ggplot(data = dplyr::filter(bridle_settings_data, !is.na(NET_WIDTH)), mapping = bridle_mapping) +
  # add_best_fit("NET_WIDTH") +
  geom_point(size = 1.2) +
  aes(y = NET_WIDTH) +
  scale_color_manual(name = "Bridle option", values = legend_colors, drop = FALSE) +
  scale_fill_manual(name = "Bridle option", values = legend_colors, drop = FALSE) +
  scale_x_continuous(name = NULL) + 
  scale_y_continuous(name = "Upper wing\nspread (m)") +
  bridle_theme

p_bridle_footrope_ele <- 
  ggplot(data = bridle_settings_data, mapping = bridle_mapping) +
  geom_point(size = 1.2) +
  aes(y = BCS_13MHEIGHT_CM) +
  scale_color_manual(name = "Bridle option", values = legend_colors) +
  scale_fill_manual(name = "Bridle option", values = legend_colors) +
  scale_x_continuous(name = "Bottom depth (m)") +
  scale_y_continuous(name = "Footrope 13-m\nelevation (cm)") +
  bridle_theme

p_bridle_height <- 
  ggplot(data = bridle_settings_data, mapping = bridle_mapping) +
  add_best_fit("NET_HEIGHT") +
  geom_point(size = 1.2) +
  aes(y = NET_HEIGHT) +
  scale_color_manual(name = "Bridle option", values = legend_colors) +
  scale_fill_manual(name = "Bridle option", values = legend_colors) +
  scale_x_continuous(name = NULL) + 
  scale_y_continuous(name = "Opening\nheight (m)") +
  bridle_theme

p_bridle_tension <- 
  ggplot(data = bridle_settings_data, mapping = bridle_mapping) +
  add_best_fit("WINCH_TENSION_TOTAL") +
  geom_point(size = 1.2) +
  aes(y = WINCH_TENSION_TOTAL) +
  scale_color_manual(name = "Bridle option", values = legend_colors) +
  scale_fill_manual(name = "Bridle option", values = legend_colors) +
  scale_x_continuous(name = NULL) +
  scale_y_continuous(name = "Total winch\ntension (t)") +
  bridle_theme

p_bridle_door_roll <- 
  ggplot(data = bridle_settings_data, mapping = bridle_mapping) +
  add_best_fit("DOOR_ROLL_MAX") +
  geom_point(size = 1.2) +
  aes(y = DOOR_ROLL_MAX) +
  scale_color_manual(name = "Bridle option", values = legend_colors) +
  scale_fill_manual(name = "Bridle option", values = legend_colors) +
  scale_x_continuous(name = "Bottom depth (m)") +
  scale_y_continuous(name = "Maximum\ndoor roll (\u00B0)") +
  bridle_theme

p_bridle_multipanel <- (p_bridle_doors / p_bridle_wing / 
                               p_bridle_height / p_bridle_footrope_ele / p_bridle_door_roll / p_bridle_tension ) + 
  plot_layout(guides = "collect")

ggsave(
  p_bridle_multipanel,
  filename = here::here("plots", "rigging_comparison", "door_bridle_settings.png"),
  width = 169, height = 169, units = "mm", dpi = 300
)

