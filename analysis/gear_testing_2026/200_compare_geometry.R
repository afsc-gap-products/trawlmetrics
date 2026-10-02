# Data prep ----------------------------------------------------------------------------------------

# Matched catch treatments
catch_treatments <- 
  readxl::read_xlsx(
    path = here::here("data", "2026_gear_testing_haul_log.xlsx"),
    sheet = "catch_treatments"
  )

# Review door trials and assign to hauls
channel <- trawlmetrics::get_connected(schema = "AFSC")

catch_records <- RODBC::sqlQuery(
  channel = channel,
  query = "SELECT 
  C.SPECIES_CODE,
  C.WEIGHT,
  C.NUMBER_FISH,
  C.VESSEL,
  C.CRUISE,
  C.HAUL,
  RDH.DOOR_SPREAD, 
  H.BOTTOM_DEPTH,
  H.HAULJOIN,
  H.NET_WIDTH, 
  H.DISTANCE_FISHED, 
  H.NET_HEIGHT, 
  H.DURATION,
  H.WIRE_LENGTH,
  H.START_LONGITUDE,
  H.START_LATITUDE,
  H.START_TIME
  FROM RACEBASE.HAUL H, 
  RACEBASE.CATCH C,
  RACE_DATA.HAULS RDH,
  RACE_DATA.CRUISES RDC
    WHERE C.CRUISE = 202601 
  AND H.HAUL_TYPE = 23
  AND H.PERFORMANCE >= 0
  AND H.HAULJOIN = C.HAULJOIN 
  AND H.HAUL = RDH.HAUL 
  AND RDH.CRUISE_ID = RDC.CRUISE_ID
  AND H.VESSEL = RDC.VESSEL_ID
  AND RDC.CRUISE = C.CRUISE
  "
) |>
  dplyr::mutate(
    AREA_SWEPT_KM2 = DISTANCE_FISHED * NET_WIDTH/1000,
    BRIDLE_ANGLE = 
      trawlmetrics::calc_bridle_angle(door_spread_m = DOOR_SPREAD, wing_spread_m = NET_WIDTH, total_bridle_length_m = (184/3.281)+10)
  )

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

AIC(m0_cc_spread, m1_cc_spread, m2_cc_spread) # m2 best
summary(m2_cc_spread)

# Net height models
m0_cc_height <- lm(NET_HEIGHT ~ 0 + BOTTOM_DEPTH, data = cc_haul_geom)
m1_cc_height <- lm(NET_HEIGHT ~ 0 + BOTTOM_DEPTH + GEAR_NAME, data = cc_haul_geom)
m2_cc_height <- lm(NET_HEIGHT ~ 0 + GEAR_NAME + BOTTOM_DEPTH:GEAR_NAME, data = cc_haul_geom)

AIC(m0_cc_height, m1_cc_height, m2_cc_height) #m1 best
summary(m2_cc_height)

# Door spread model
m0_cc_doors <- lm(DOOR_SPREAD ~ BOTTOM_DEPTH, data = cc_haul_geom)
summary(m0_cc_doors)

# Bridle angle model
m0_cc_bridles <- lm(BRIDLE_ANGLE ~ BOTTOM_DEPTH, data = cc_haul_geom)
summary(m0_cc_bridles)

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
  ) %>%
    mutate(
      deltaAIC = AIC - min(AIC)
    ) %>%
    arrange(deltaAIC)
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

png(filename = here::here("plots", "catch_comparison", "cc_gear_geometry.png"),
    width = 169, height = 50, units = "mm", res = 300)
print(p_cc_gear_geom)
dev.off()


ggplot() +
  geom_point(
    data = cc_haul_geom,
    mapping = aes(x = BOTTOM_DEPTH, y = WIRE_LENGTH/BOTTOM_DEPTH, color = GEAR_NAME)
  )

# Scope table hauls

gear_treatments_45 <- 
  readxl::read_xlsx(
    path = here::here("data", "2026_gear_testing_haul_log.xlsx"),
    sheet = "treatments"
  ) |>
  dplyr::filter(Door_size_m2 == 4.5)

gt_net_data <- 
  readRDS(file = here::here("output", "haul_summary.rds")) |>
  dplyr::select(
    HAUL = haul,
    WIRE_LENGTH_FM = scope,
    NET_WIDTH = MEAN_NET_SPREAD,
    NET_HEIGHT = MEAN_NET_HEIGHT,
    DOOR_SPREAD = MEAN_DOOR_SPREAD
  ) |>
  dplyr::mutate(
    WIRE_LENGTH = WIRE_LENGTH_FM * 1.8288
  )

gt_btd_data <- 
  readRDS(file = here::here("output", "btd_summary.rds")) |>
  dplyr::group_by(scope, haul) |>
  dplyr::slice_max(pass, n = 1) |>
  dplyr::ungroup() |>
  dplyr::select(
    HAUL = haul,
    WIRE_LENGTH_FM = scope,
    BOTTOM_DEPTH_FM
  ) |>
  dplyr::mutate(
    BOTTOM_DEPTH = BOTTOM_DEPTH_FM * 1.8288,
    WIRE_LENGTH = WIRE_LENGTH_FM * 1.8288
  )

gt_door_data <-
  gear_treatments_45 |>
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
  )

scope_tables <- 
  readxl::read_xlsx(path = here::here("data", "shelf_slope_table.xlsx")) |>
  dplyr::mutate(mean_depth_fm = (min_depth_fm+max_depth_fm)/2,
                mean_depth_m = mean_depth_fm * 1.8288,
                scope_to_depth = wire_out_fm/mean_depth_fm) |>
  dplyr::inner_join(data.frame(table = c("GOA/AI", "EBS shelf", "EBS slope"), gear = c("PNE", "83-112", "PNE-S")))

test <- 
  gt_door_data |>
  dplyr::inner_join(gt_btd_data) |>
  dplyr::inner_join(gt_net_data) |>
  dplyr::bind_rows(
    catch_records |>
      dplyr::filter(VESSEL == 134) |>
      dplyr::select(HAUL, BOTTOM_DEPTH, WIRE_LENGTH, NET_HEIGHT, NET_WIDTH, DOOR_SPREAD) |>
      unique() |>
      dplyr::inner_join(gt_door_data) |>
      dplyr::mutate(
        WIRE_LENGTH_FM = WIRE_LENGTH/1.8288,
        BOTTOM_DEPTH_FM = BOTTOM_DEPTH/1.8288)
  ) |>
  dplyr::mutate(
    CODEND = ifelse(HAUL >= 600, "Closed", "Open"),
    DOOR_ON_BOTTOM_P = DOOR_DEPTH_P > (BOTTOM_DEPTH - NET_HEIGHT),
    DOOR_ON_BOTTOM_S = DOOR_DEPTH_S > (BOTTOM_DEPTH - NET_HEIGHT),
    DOOR_LT30_P = abs(DOOR_ROLL_P) < 30,
    DOOR_LT30_S = abs(DOOR_ROLL_S) < 30,
    DOORS_GOOD = DOOR_ON_BOTTOM_P & DOOR_ON_BOTTOM_S & DOOR_LT30_P & DOOR_LT30_S)

depth_range <- range(test$BOTTOM_DEPTH, na.rm = TRUE)


View(test[,c("DOOR_ON_BOTTOM_P", "DOOR_ON_BOTTOM_S", "DOOR_LT30_P", "DOOR_LT30_S")])


ggplot() +
  geom_point(
    data = test, 
    mapping = aes(x = WIRE_LENGTH/BOTTOM_DEPTH, y = DOOR_ROLL_P, color = CODEND)) +
  geom_point(
    data = test, 
    mapping = aes(x = WIRE_LENGTH/BOTTOM_DEPTH, y = abs(DOOR_ROLL_S), color = CODEND))

door_roll_breaks <- c(-Inf, -10, 0, 10, 25, Inf)
door_roll_labels <- c("<-10", "-10-0", "0-10", "10-25", ">25")
door_roll_colors <- c("#00204DFF", "#414D6BFF", "#7C7B78FF", "#BCAF6FFF", "red")
names(door_roll_colors) <- door_roll_labels

door_roll_breaks <- c(-Inf, 0, 25, 30, Inf)
door_roll_labels <- c("<0", "0-25", "25-30", ">30")
door_roll_colors <- c("#00204DFF", "#BCAF6FFF", "pink", "red")
names(door_roll_colors) <- door_roll_labels


depth_breaks <- c(20, seq(25,150,25), 200, 250)
sdr_breaks <- 2:9
max_sdr <- max(test$WIRE_LENGTH/test$BOTTOM_DEPTH, na.rm = TRUE)

# 
ggplot() +
  geom_path(
    data = scope_tables,
    mapping = aes(x = mean_depth_m, y = scope_to_depth, linetype = gear),
    color = "grey",
    linewidth = 1.2
  ) +
  geom_point(
    data = dplyr::filter(test, !is.na(DOOR_ROLL_P)) |>
      dplyr::arrange(DOOR_ROLL_P), 
    mapping = aes(
      x = BOTTOM_DEPTH, 
      y = WIRE_LENGTH/BOTTOM_DEPTH, 
      color = cut(DOOR_ROLL_P, breaks = door_roll_breaks, labels = door_roll_labels), 
      shape = CODEND,
      size = CODEND),
    alpha = 0.8
  ) + 
  scale_linetype(name = "Scope table/gear") +
  scale_color_manual(name = "Roll (\u00B0)", values = door_roll_colors) +
  scale_x_log10(
    name = "Bottom depth (m)", 
    limits = c(depth_range[1]-2, depth_range[2]), 
    breaks = depth_breaks,
    oob = scales::oob_squish_infinite,
    expand = c(0,0)) +
  scale_y_continuous(
    name = "Scope/Depth", 
    oob = scales::oob_squish_infinite,
    breaks = sdr_breaks,
    limits = c(2, max_sdr)) +
  scale_size_manual(name = "Codend", values = c("Closed" = 3, "Open" = 1.8)) +
  scale_shape(name = "Codend") +
  facet_wrap(~"Port door") +
  theme_bw()

ggplot() +
  geom_path(
    data = scope_tables,
    mapping = aes(x = mean_depth_m, y = scope_to_depth, linetype = gear),
    color = "grey",
    linewidth = 1.2
  ) +
  geom_point(
    data = dplyr::filter(test, !is.na(DOOR_ROLL_S)) |>
      dplyr::arrange(DOOR_ROLL_S), 
    mapping = aes(
      x = BOTTOM_DEPTH, 
      y = WIRE_LENGTH/BOTTOM_DEPTH, 
      color = cut(-1*DOOR_ROLL_S, breaks = door_roll_breaks, labels = door_roll_labels),
      shape = CODEND,
      size = CODEND),
    alpha = 0.8
  ) + 
  scale_linetype(name = "Scope table/gear") +
  scale_color_manual(name = "Roll (\u00B0)", values = door_roll_colors) +
  scale_x_log10(
    name = "Bottom depth (m)", 
    limits = c(NA, depth_range[2]), 
    breaks = depth_breaks,
    oob = scales::oob_squish_infinite) +
  scale_y_continuous(
    name = "Scope/Depth", 
    oob = scales::oob_squish_infinite,
    breaks = sdr_breaks,
    limits = c(2, max_sdr)) +
  scale_size_manual(name = "Codend", values = c("Closed" = 3, "Open" = 1.8)) +
  scale_shape(name = "Codend") +
  facet_wrap(~"Starboard door") +
  theme_bw()

# ggplot() +
#   geom_point(
#     data = dplyr::filter(test, !is.na(DOOR_ROLL_S)) |>
#       dplyr::arrange(DOOR_ROLL_S), 
#     mapping = aes(
#       x = BOTTOM_DEPTH, 
#       y = WIRE_LENGTH/BOTTOM_DEPTH, 
#       color = cut(-1*DOOR_ROLL_S, breaks = door_roll_breaks, labels = door_roll_labels),
#       shape = CODEND,
#       size = CODEND)
#   ) + 
#   # scale_color_viridis_d(name = "Roll (\u00B0)", option = "E", na.value = NA, drop = TRUE) +
#   scale_color_manual(name = "Roll (\u00B0)", values = door_roll_colors) +
#   scale_x_continuous(name = "Bottom depth (m)") +
#   scale_y_continuous(name = "Scope/Depth") +
#   scale_size_manual(name = "Codend", values = c("Closed" = 3, "Open" = 1)) +
#   scale_shape(name = "Codend") +
#   theme_bw()

ggplot() +
  geom_point(
    data = dplyr::filter(test, !is.na(DOORS_GOOD)), 
    mapping = aes(
      x = BOTTOM_DEPTH, 
      y = WIRE_LENGTH/BOTTOM_DEPTH, 
      color = DOORS_GOOD, 
      shape = CODEND,
      size = CODEND),
  ) + 
  scale_color_discrete() +
  # scale_color_manual(name = "Roll (\u00B0)") +
  scale_x_log10(name = "Bottom depth (m)", breaks = seq(0,250,50)) +
  scale_y_continuous(name = "Scope/Depth") +
  scale_size_manual(name = "Codend", values = c("Closed" = 3, "Open" = 1)) +
  scale_shape(name = "Codend") +
  theme_bw()
# 
# ggplot() +
#   geom_point(
#     data = dplyr::filter(test, !is.na(DOOR_ROLL_S)), 
#     mapping = aes(
#       x = BOTTOM_DEPTH, 
#       y = WIRE_LENGTH/BOTTOM_DEPTH, 
#       color = cut(-1*DOOR_ROLL_S, breaks = door_roll_breaks))
#   ) + 
#   scale_color_viridis_d(name = "Roll (\u00B0)", option = "E", na.value = NA, drop = TRUE) +
#   scale_x_continuous(name = "Bottom depth (m)", limits = c(0, NA)) +
#   scale_y_continuous(name = "Scope/Depth", limits = c(2, NA)) +
#   theme_bw()