# Contextual plots showing actual trawl performance 

library(trawlmetrics)

# Scope to depth ratio for surveys other than the slope
trawl_data <- trawlmetrics::bts_geom |>
  dplyr::mutate(SCOPE_TO_DEPTH = WIRE_LENGTH_M/DEPTH_M,
                TOW_SPEED_KMHR = DISTANCE_FISHED_KM/DURATION_HR,
                TOW_SPEED_KN = DISTANCE_FISHED_KM/DURATION_HR/1.852,
                ID_VC = paste(VESSEL_ID, CRUISE, sep = "_")) |>
  dplyr::filter(SURVEY_DEFINITION_ID != 78) |>
  dplyr::filter(!is.na(SCOPE_TO_DEPTH), !is.na(DEPTH_M))

# Bottom depth versus scope-to-depth ratio by gear
p_depth_vs_scope_ratio <- 
  ggplot() +
  geom_point(data = trawl_data,
             mapping = aes(x = DEPTH_M, 
                           y = SCOPE_TO_DEPTH)) +
  scale_x_continuous(name = "Depth (m)") +
  scale_y_continuous(name = "Scope to depth ratio") +
  facet_wrap(~GEAR_NAME, scales = "free_x")

p_depth_vs_scope <- 
  ggplot(data = trawl_data,
                           mapping = aes(x = DEPTH_M, 
                                         y = WIRE_LENGTH_M,
                                         color = GEAR_NAME)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  facet_wrap(~GEAR_NAME, scales = "free") +
  scale_x_continuous(name = "Depth (m)") +
  scale_y_continuous(name = "Wire out (m)") +
  scale_color_viridis_d(option = "D")

p_scope_vs_width <- 
  ggplot(data = trawl_data |>
                             dplyr::filter(SCOPE_TO_DEPTH > 2.3, SCOPE_TO_DEPTH < 10),
                           mapping = aes(x = SCOPE_TO_DEPTH,
                                         y = NET_WIDTH_M)) +
  geom_point(size = 0.2) +
  geom_smooth(formula = y ~ s(x, bs = "tp")) +
  facet_wrap(~GEAR_NAME) +
  scale_x_continuous(name = "Scope to depth ratio") +
  scale_y_continuous(name = "Net width (m)")

p_scope_vs_height <- 
  ggplot(data = trawl_data,
                            mapping = aes(x = SCOPE_TO_DEPTH,
                                          y = NET_HEIGHT_M)) +
  geom_point(size = 0.2) +
  geom_smooth(formula = y ~ s(x, bs = "tp")) +
  facet_wrap(~GEAR_NAME, scales = "free_y") +
  scale_x_continuous(name = "Scope to depth ratio") +
  scale_y_continuous(name = "Net height (m)")

p_depth_vs_width <- 
  ggplot(data = trawl_data,
                           mapping = aes(x = DEPTH_M, y = NET_WIDTH_M)) +
  geom_point() +
  scale_x_continuous(name = "Bottom depth (m)") +
  scale_y_continuous(name = "Net width (m)") +
  geom_smooth(formula = y ~ s(x, bs = "tp")) +
  facet_wrap(~GEAR_NAME, scales = "free")

p_depth_vs_height <- 
  ggplot(data = trawl_data,
                            mapping = aes(x = DEPTH_M, y = NET_HEIGHT_M)) +
  geom_point() +
  scale_x_continuous(name = "Bottom depth (m)") +
  scale_y_continuous(name = "Net height (m)") +
  geom_smooth(formula = y ~ s(x, bs = "tp")) +
  facet_wrap(~GEAR_NAME, scales = "free")

p_tow_speed_vs_width <- 
  ggplot(data = trawl_data |> 
           dplyr::filter(TOW_SPEED_KN > 2 & TOW_SPEED_KN < 4),
         mapping = aes(x = TOW_SPEED_KN,
                       y = NET_WIDTH_M)) +
  geom_point() +
  geom_smooth(formula = y ~ s(x, bs = "tp")) +
  facet_wrap(~GEAR_NAME) +
  scale_x_continuous(name = "Tow speed (knots)") +
  scale_y_continuous(name = "Net width (m)")

p_tow_speed_vs_height <- 
  ggplot(data = trawl_data |> 
           dplyr::filter(TOW_SPEED_KN > 2 & TOW_SPEED_KN < 4),
         mapping = aes(x = TOW_SPEED_KN,
                       y = NET_HEIGHT_M)) +
  geom_point() +
  geom_smooth(formula = y ~ s(x, bs = "tp")) +
  facet_wrap(~GEAR_NAME, scales = "free_y") +
  scale_x_continuous(name = "Tow speed (knots)") +
  scale_y_continuous(name = "Net height (m)")


# Select best spread model for 83-112

fit_gams <- function(gam_formulas, data) {
  
  n_mod <- length(gam_formulas)
  
  model_list <- vector(mode = "list", n_mod)
  
  aic_table <- data.frame()
  
  for(ii in 1:n_mod) {
    
    cat("Fitting model", ii, "\n")
    
    model_list[[ii]] <- mgcv::gam(formula = gam_formulas[[ii]],
                                  data = data)
    
    aic_table <- rbind(aic_table, as.data.frame(AIC(model_list[[1]], model_list[[ii]]))[2, ])
  }
  
  rownames(aic_table) <- 1:n_mod
  
  output <- list(
    models = model_list,
    aic_table = aic_table)
  
  return(output)
  
}

gam_formulas_spread <- 
  list(f0 = NET_WIDTH_M ~ s(TOW_SPEED_KN, bs = "tp") + s(NET_HEIGHT_M, bs = "tp") + te(SCOPE_TO_DEPTH, DEPTH_M),
       f1 = NET_WIDTH_M ~ s(NET_HEIGHT_M, bs = "tp") + te(SCOPE_TO_DEPTH, DEPTH_M),
       f2 = NET_WIDTH_M ~ s(TOW_SPEED_KN, bs = "tp") + s(NET_HEIGHT_M, bs = "tp"),
       f3 = NET_WIDTH_M ~ s(TOW_SPEED_KN, bs = "tp") + s(NET_HEIGHT_M, bs = "tp") + s(SCOPE_TO_DEPTH, bs = "tp"),
       f4 = NET_WIDTH_M ~ s(TOW_SPEED_KN, bs = "tp") + s(NET_HEIGHT_M, bs = "tp") + s(DEPTH_M, bs = "tp"))

gam_formulas_height <- 
  list(f0 = NET_HEIGHT_M ~ s(TOW_SPEED_KN, bs = "tp") + s(NET_WIDTH_M, bs = "tp") + te(SCOPE_TO_DEPTH, DEPTH_M),
       f1 = NET_HEIGHT_M ~ s(NET_WIDTH_M, bs = "tp") + te(SCOPE_TO_DEPTH, DEPTH_M),
       f2 = NET_HEIGHT_M ~ s(TOW_SPEED_KN, bs = "tp") + s(NET_WIDTH_M, bs = "tp"),
       f3 = NET_HEIGHT_M ~ s(TOW_SPEED_KN, bs = "tp") + s(NET_WIDTH_M, bs = "tp") + s(SCOPE_TO_DEPTH, bs = "tp"),
       f4 = NET_HEIGHT_M ~ s(TOW_SPEED_KN, bs = "tp") + s(NET_WIDTH_M, bs = "tp") + s(DEPTH_M, bs = "tp"))

gam_83112_spread <- fit_gams(gam_formulas = gam_formulas_spread,
                             data = dplyr::filter(trawl_data, GEAR_NAME == "83-112"))

gam_83112_height <- fit_gams(gam_formulas = gam_formulas_height,
                             data = dplyr::filter(trawl_data, GEAR_NAME == "83-112"))

gam_pne_spread <- fit_gams(gam_formulas = gam_formulas_spread,
                             data = dplyr::filter(trawl_data, GEAR_NAME == "PNE"))

gam_pne_height <- fit_gams(gam_formulas = gam_formulas_height,
                             data = dplyr::filter(trawl_data, GEAR_NAME == "PNE"))

# Select based on AIC
gam_83112_spread$aic_table # Model 1
gam_83112_height$aic_table # Model 5

gam_pne_spread$aic_table # Model 1
gam_pne_height$aic_table # Model 1

# Model plots

# Model diagnostics
