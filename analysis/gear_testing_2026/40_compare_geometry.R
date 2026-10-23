# Compare RRT and 83-112 geometry in closed codend catch comparison tows

library(trawlmetrics)
library(akgfmaps)
library(ggthemes)
library(shadowtext)
library(dplyr)
library(ggpp)
library(readxl)

# Data prep ----------------------------------------------------------------------------------------

# Haul pairs
catch_treatments <-
  readxl::read_xlsx(
    path = here::here("data", "2026_gear_testing_haul_log.xlsx"),
    sheet = "catch_treatments"
  )

catch_records <- readRDS(file = here::here("data", "06_catch_data", "catch_records.rds"))

# Distance between tow pairs
tow_pair_distance <- 
  dplyr::select(catch_records, VESSEL, CRUISE, HAUL, START_LONGITUDE, START_LATITUDE) |>
  sf::st_as_sf(coords = c("START_LONGITUDE", "START_LATITUDE"),
                   crs = "WGS84") |>
  unique() |>
  dplyr::inner_join(catch_treatments) |>
  dplyr::group_by(BLOCK) |>
  dplyr::summarise(do_union = TRUE) |>
  sf::st_cast(to = "LINESTRING") |>
  sf::st_length()/1000 |>
  as.numeric()

# Time elapsed between tow pairs
dplyr::select(catch_records, VESSEL, CRUISE, HAUL, START_TIME) |>
  unique() |>
  dplyr::inner_join(catch_treatments) |>
  dplyr::group_by(BLOCK) |>
  dplyr::summarise(TIME_ELAPSED = max(START_TIME)-min(START_TIME))



# Haul geometry statistics -------------------------------------------------------------------------

cc_haul_geom <- catch_records |>
  dplyr::inner_join(catch_treatments) |>
  dplyr::select(GEAR_NAME, BLOCK, NET_HEIGHT, NET_WIDTH, DOOR_SPREAD, BRIDLE_ANGLE, BOTTOM_DEPTH, WIRE_LENGTH) |>
  unique() |>
  dplyr::arrange(BLOCK)


cc_haul_geom_summary <-
  cc_haul_geom |>
  dplyr::group_by(GEAR_NAME) |>
  dplyr::summarise(
    MIN_NET_WIDTH = min(NET_WIDTH),
    MAX_NET_WIDTH = max(NET_WIDTH),
    MEAN_NET_WIDTH = mean(NET_WIDTH),
    SD_NET_WIDTH = sd(NET_WIDTH),
    MIN_DOOR_SPREAD = min(DOOR_SPREAD),
    MAX_DOOR_SPREAD = max(DOOR_SPREAD),
    MEAN_DOOR_SPREAD = mean(DOOR_SPREAD),
    SD_DOOR_SPREAD = sd(DOOR_SPREAD),
    MEAN_NET_HEIGHT = mean(NET_HEIGHT),
    MIN_NET_HEIGHT = min(NET_HEIGHT),
    MAX_NET_HEIGHT = max(NET_HEIGHT),
    SD_NET_HEIGHT = sd(NET_HEIGHT),
    MIN_BRIDLE_ANGLE = min(BRIDLE_ANGLE),
    MAX_BRIDLE_ANGLE = max(BRIDLE_ANGLE),
    MEAN_BRIDLE_ANGLE = mean(BRIDLE_ANGLE),
    SD_BRIDLE_ANGLE = sd(BRIDLE_ANGLE)
  )

cc_haul_geom_table <- 
  cc_haul_geom_summary |>
  dplyr::mutate(
    Gear = GEAR_NAME,
    `Wing spread, mean (m)` = 
      sprintf("%.1f", MEAN_NET_WIDTH),
    `Wing spread, SD (m)` = 
      sprintf("%.2f", SD_NET_WIDTH),
    `Wing spread, range (m)` = sprintf("%.1f-%.1f", MIN_NET_WIDTH, MAX_NET_WIDTH),     
    `Door spread, mean (m)` =    
      sprintf("%.1f", MEAN_DOOR_SPREAD),
    `Door spread, SD (m)` = 
      sprintf("%.1f", SD_DOOR_SPREAD),
    `Door spread, range (m)` = sprintf("%.1f-%.1f", MIN_DOOR_SPREAD, MAX_DOOR_SPREAD),  
    `Opening height, mean (m)` = 
      sprintf("%.1f", MEAN_NET_HEIGHT),
    `Opening height, SD (m)` = 
      sprintf("%.2f", SD_NET_HEIGHT),
    `Opening height, range (m)` = 
      sprintf("%.1f-%.1f", MIN_NET_HEIGHT, MAX_NET_HEIGHT),
    `BAA, mean` = 
      sprintf("%.1f", MEAN_BRIDLE_ANGLE),
    `BAA, SD` = 
      sprintf("%.1f", SD_BRIDLE_ANGLE),
    `BAA, range` = 
      sprintf("%.1f-%.1f", MIN_BRIDLE_ANGLE, MAX_BRIDLE_ANGLE),
  ) |>
  dplyr::select(
    Gear,
    `Wing spread, mean (m)`,
    `Wing spread, SD (m)`,
    `Wing spread, range (m)`,
    `Door spread, mean (m)`,
    `Door spread, SD (m)`,
    `Door spread, range (m)`,
    `Opening height, mean (m)`,
    `Opening height, SD (m)`,
    `Opening height, range (m)`,
    `BAA, mean`,
    `BAA, SD`,
    `BAA, range`    
  ) |>
  tidyr::pivot_longer(
    cols = 2:13,
    names_to = "Value"
  ) |>
  tidyr::pivot_wider(names_from  = "Gear", values_from = value)

write.csv(
  cc_haul_geom_table,
  file = here::here("plots", "geom_comparison", "cc_haul_geometry.csv"), 
  row.names = FALSE)

# Geometry models
cc_haul_geom_long <- 
  cc_haul_geom |>
  tidyr::pivot_longer(cols = c(NET_WIDTH, NET_HEIGHT, DOOR_SPREAD, BRIDLE_ANGLE))

# Wing spread models
m0_cc_spread <- lm(NET_WIDTH ~ BOTTOM_DEPTH, data = cc_haul_geom)
m1_cc_spread <- lm(NET_WIDTH ~ BOTTOM_DEPTH + GEAR_NAME, data = cc_haul_geom)
m2_cc_spread <- lm(NET_WIDTH ~ 0 + GEAR_NAME + BOTTOM_DEPTH:GEAR_NAME, data = cc_haul_geom)

# Net height models
m0_cc_height <- lm(NET_HEIGHT ~ 0 + BOTTOM_DEPTH, data = cc_haul_geom)
m1_cc_height <- lm(NET_HEIGHT ~ 0 + BOTTOM_DEPTH + GEAR_NAME, data = cc_haul_geom)
m2_cc_height <- lm(NET_HEIGHT ~ 0 + GEAR_NAME + BOTTOM_DEPTH:GEAR_NAME, data = cc_haul_geom)

# Door spread model
m0_cc_doors <- lm(DOOR_SPREAD ~ BOTTOM_DEPTH, data = cc_haul_geom)

# Bridle angle model
m0_cc_bridles <- lm(BRIDLE_ANGLE ~ BOTTOM_DEPTH, data = cc_haul_geom)

# Model table
# Helper function to extract summary statistics for a group of models
summarize_model_block <- function(block_label, model_list) {
  data.frame(
    Response_Block = block_label,
    Model          = names(model_list),
    Formula        = sapply(model_list, function(m) paste(format(formula(m)), collapse = " ")),
    K              = sapply(model_list, function(m) attr(logLik(m), "df")),
    N              = sapply(model_list, function(m) nobs(m)),
    R2             = sapply(model_list, function(m) summary(m)$r.squared),
    Adj_R2         = sapply(model_list, function(m) summary(m)$adj.r.squared),
    AIC            = sapply(model_list, AIC),
    stringsAsFactors = FALSE
  ) |>
    dplyr::mutate(
      deltaAIC = AIC - min(AIC)
    ) |>
    dplyr::arrange(deltaAIC)
}

# Organize models by response
wing_spread_models <- list(
  "m0_cc_spread" = m0_cc_spread,
  "m1_cc_spread" = m1_cc_spread,
  "m2_cc_spread" = m2_cc_spread
)

net_height_models <- list(
  "m0_cc_height" = m0_cc_height,
  "m1_cc_height" = m1_cc_height,
  "m2_cc_height" = m2_cc_height
)

door_spread_models <- list(
  "m0_cc_doors" = m0_cc_doors
)

bridle_angle_models <- list(
  "m0_cc_bridles" = m0_cc_bridles
)

model_summary_table <- 
  dplyr::bind_rows(
  summarize_model_block("Upper wing spread", wing_spread_models),
  summarize_model_block("Opening height", net_height_models),
  summarize_model_block("Door spread", door_spread_models),
  summarize_model_block("Bridle angle", bridle_angle_models)
) |>
  dplyr::mutate(
    R2 = sprintf("%.3f", R2),
    Adj_R2 = sprintf("%.3f", Adj_R2),
    AIC = sprintf("%.2f", AIC),
    deltaAIC = sprintf("%.2f", deltaAIC)
  ) |>
  dplyr::select(-Model, -R2)

write.csv(model_summary_table, file = here::here("plots", "geom_comparison", "cc_gear_models.csv"), row.names = FALSE)

p_cc_gear_geom <- 
  ggplot() +
  geom_smooth(data = cc_haul_geom_long,
              mapping = aes(
                x = BOTTOM_DEPTH, y = value, color = GEAR_NAME
              ),
              method = 'lm',
              linewidth = 0.4) +
  geom_point(data = cc_haul_geom_long,
             mapping = aes(
               x = BOTTOM_DEPTH, y = value, color = GEAR_NAME),
             size = 0.9
  ) +
  scale_color_tableau(name = "Gear") +
  scale_y_continuous(name = "Value") +
  scale_x_continuous(name = "Bottom depth (m)") +
  facet_wrap(~factor(
    name, 
    levels = c("NET_WIDTH", "NET_HEIGHT", "DOOR_SPREAD", "BRIDLE_ANGLE"),
    labels= 
      c(
        "Upper Wing\nSpread (m)", 
        "Opening Height (m)", 
        "Door spread (m)", 
        "Bridle angle\nof attack (\u00B0)"
      )  
  ),
  scales = "free_y",
  nrow = 1
  ) +
  theme_bw() +
  theme(legend.position = "right", 
        # legend.position.inside = c(0.92, 0.12),
        strip.text = element_text(face = "bold", size = 8),
        strip.background = element_blank(),
        axis.text = element_text(size = 7),
        axis.title = element_text(size = 8),
        panel.spacing = unit(0.5, units = "mm"),
        legend.text = element_text(size = 7),
        legend.title = element_blank(),
        legend.background = element_blank())

ggsave(
  p_cc_gear_geom,
  filename = here::here("plots", "catch_comparison", "cc_gear_geometry.png"),
  width = 169, 
  height = 50, 
  units = "mm", 
  dpi = 300
)
