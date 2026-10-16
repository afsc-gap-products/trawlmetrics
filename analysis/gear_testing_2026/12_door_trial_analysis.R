library(trawlmetrics)
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
  ) |>
  dplyr::filter(GEAR_ON_BOTTOM == "Yes")

door_data <- readxl::read_xlsx(
  path = here::here("data", "2026_gear_testing_haul_log.xlsx"),
  sheet = "door_blocks"
)|>
  dplyr::rename_with(toupper) |>
  dplyr::mutate(
    DOOR_SIZE_M2 = factor(DOOR_SIZE_M2, levels = c(4, 4.5), labels = c("4.0", "4.5")),
    BLOCK = stringr::str_replace(string = BLOCK, pattern = "Doors_", replacement = "")
                ) |>
  dplyr::inner_join(
    door_footrope_bc
  )

door_blocks <- 
  dplyr::select(door_data, BLOCK, HAUL, WIRE_LENGTH_FM, DOOR_SIZE_M2)

# Number of unique hauls
length(unique(door_blocks$HAUL))


door_data_summary <-
  read.csv(here::here("data", "door_blocks_qa_qc.csv")) |>
  dplyr::rename_with(toupper) |>
  dplyr::rename(WIRE_LENGTH_FM = SCOPE) |>
  dplyr::mutate(DOOR_SIZE_M2 = factor(DOOR_SIZE_M2, levels = c(4, 4.5), labels = c("4.0", "4.5")),
                BLOCK = stringr::str_replace(string = BLOCK, pattern = "Doors_", replacement = ""))

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
  ) |>
  dplyr::inner_join(
    door_blocks
  )
  

# Load trawl measurement timeseries 

trawl_measurement_summary <- 
  readRDS(file = here::here("output", "haul_summary.rds")) |>
  dplyr::filter(haul < 600) |> # Use fully processed means for catch comparison hauls
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
  dplyr::inner_join(
    door_blocks
  ) |>
  dplyr::inner_join(door_data_summary) |>
  dplyr::mutate(
    # DOOR_SIZE_M2 = factor(DOOR_SIZE_M2, levels = c(4, 4.5), labels = c("4.0", "4.5")),
    WIRE_LENGTH = WIRE_LENGTH_FM * 1.8288,
    BOTTOM_DEPTH = BOTTOM_DEPTH_FM * 1.8288,
    SCOPE_RATIO = WIRE_LENGTH_FM/BOTTOM_DEPTH_FM,
    INV_SCOPE = 1/SCOPE_RATIO,
    WINCH_TENSION_TOTAL = STBD_TENSION + PORT_TENSION,
    DOOR_ROLL_MAX = pmax(abs(STBD_DOOR_ROLL), abs(PORT_DOOR_ROLL), na.rm = TRUE)
  ) |>
  dplyr::inner_join(
    gear_config
  )
  

table_door_treatment_measurements <- 
  trawl_measurement_summary |>
  dplyr::group_by(BLOCK, DOOR_SIZE_M2) |>
  dplyr::summarise(
    N_TRT = dplyr::n(),
    BOTTOM_DEPTH = mean(BOTTOM_DEPTH),
    DOOR_SPREAD = mean(DOOR_SPREAD, na.rm = TRUE),
    NET_SPREAD = mean(NET_WIDTH, na.rm = TRUE),
    NET_HEIGHT = mean(NET_HEIGHT, na.rm = TRUE),
    DOOR_ROLL_MAX = mean(DOOR_ROLL_MAX, na.rm = TRUE),
    WINCH_TENSION_TOTAL = mean(WINCH_TENSION_TOTAL, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::arrange(BLOCK, DOOR_SIZE_M2) |>
  dplyr::mutate(
    dplyr::across(
      c(BOTTOM_DEPTH, DOOR_SPREAD, NET_SPREAD, NET_HEIGHT, DOOR_ROLL_MAX, WINCH_TENSION_TOTAL),
      ~ dplyr::if_else(is.na(.x), "-", sprintf("%.1f", .x))
    )
  )

write.csv(
  table_door_treatment_measurements,
  here::here("plots", "door_comparison", "door_treatment_table.csv"),
  row.names = FALSE
          )


# Compare winch tension
p_winch_tension <-
  ggplot() +
  geom_point(
    data = trawl_measurement_summary |>
      dplyr::arrange(SCOPE_RATIO),
    mapping = aes(
      x = SCOPE_RATIO,
      y = PORT_TENSION+STBD_TENSION,
      group = interaction(BLOCK, DOOR_SIZE_M2),
      color = BLOCK,
      shape = DOOR_SIZE_M2
    ),
    size = 1.1
  ) +
  geom_path(
    data = trawl_measurement_summary |>
      dplyr::arrange(SCOPE_RATIO),
    mapping = aes(
      x = SCOPE_RATIO,
      y = PORT_TENSION+STBD_TENSION,
      group = interaction(BLOCK, DOOR_SIZE_M2),
      color = BLOCK,
      linetype = DOOR_SIZE_M2
    ),
    linewidth = 0.4
  ) +
  scale_color_viridis_d(name = "Block", option = "H", direction = -1) +
  scale_shape(name = "Door size") +
  scale_linetype(name = "Door size") +
  scale_x_continuous(name = "Scope/Depth") +
  scale_y_continuous(name = "Total winch tension (tons)") +
  theme_bw() +
  theme(axis.text = element_text(size = 7),
        axis.title = element_text(size = 8),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 8),
        legend.key.size = unit(4, "mm"))


png(here::here("plots", "door_comparison", "door_treatment_winch_tension.png"), 
    width = 80, height = 60, units = "mm", res = 300)
print(p_winch_tension)
dev.off()

# Compare effects of bale settings on door and wing spread
bale_settings_data <- trawl_measurement_summary |>
  dplyr::filter(HAUL %in% c(518, 519, 523, 524)) |>
  dplyr::inner_join(
    data.frame(
      HAUL = c(518, 523, 519, 524),
      TOW_DIRECTION = c("SE", "SE", "NW", "NW") # To the nearest secondary intercardinal direction
               )
  )

bale_mapping <- aes(
  x = SCOPE_RATIO,
  group = HAUL,
  color = factor(
    DOOR_SETTINGS, 
    levels = c("B-D", "B-E"), 
    labels = c("30.25\u00B0 (B-D)", "32.60\u00B0 (B-E)"),
  ),
  shape = TOW_DIRECTION
)

bale_theme <- theme_bw() + 
  theme(
    axis.text = element_text(size = 6),
    axis.title = element_text(size = 7),
    legend.text = element_text(size = 6),
    legend.title = element_text(size = 7),
    legend.position = "bottom",
    legend.direction = "vertical"
  )

p_bale_doors <- 
  ggplot(data = bale_settings_data, mapping = bale_mapping) +
  geom_path(linewidth = 0.4) +
  geom_point(size = 1.2) +
  aes(y = DOOR_SPREAD) +
  scale_color_manual(name = "Door angle of attack", values = c("#000000", "#999999")) +
  scale_shape(name = "Tow direction") +
  scale_x_continuous(name = NULL) + 
  scale_y_continuous(name = "Door spread (m)") +
  bale_theme

p_bale_wing <- 
  ggplot(data = dplyr::filter(bale_settings_data, !is.na(NET_WIDTH)), mapping = bale_mapping) +
  geom_path(linewidth = 0.4) +
  geom_point(size = 1.2) +
  aes(y = NET_WIDTH) +
  scale_color_manual(name = "Door angle of attack", values = c("#000000", "#999999")) +
  scale_shape(name = "Tow direction") +
  scale_x_continuous(name = NULL) + 
  scale_y_continuous(name = "Upper wing\nspread (m)") +
  bale_theme

p_bale_height <- 
  ggplot(data = bale_settings_data, mapping = bale_mapping) +
  geom_path(linewidth = 0.4) +
  geom_point(size = 1.2) +
  aes(y = NET_HEIGHT) +
  scale_color_manual(name = "Door angle of attack", values = c("#000000", "#999999")) +
  scale_shape(name = "Tow direction") +
  scale_x_continuous(name = NULL) + 
  scale_y_continuous(name = "Opening\nheight (m)") +
  bale_theme

p_bale_tension <- 
  ggplot(data = bale_settings_data, mapping = bale_mapping) +
  geom_path(linewidth = 0.4) +
  geom_point(size = 1.2) +
  aes(y = PORT_TENSION + STBD_TENSION) +
  scale_color_manual(name = "Door angle of attack", values = c("#000000", "#999999")) +
  scale_shape(name = "Tow direction") +
  scale_x_continuous(name = "Scope/Depth") +
  scale_y_continuous(name = "Total winch\ntension (t)") +
  bale_theme

p_bale_door_roll <- 
  ggplot(data = bale_settings_data, mapping = bale_mapping) +
  geom_path(linewidth = 0.4) +
  geom_point(size = 1.2) +
  aes(y = DOOR_ROLL_MAX) +
  scale_color_manual(name = "Door angle of attack", values = c("#000000", "#999999")) +
  scale_shape(name = "Tow direction") +
  scale_x_continuous(name = "Scope/Depth") +
  scale_y_continuous(name = "Maximum\ndoor roll (\u00B0)") +
  bale_theme

p_bale_multipanel <- (p_bale_doors / p_bale_wing / p_bale_height / p_bale_door_roll / p_bale_tension ) + 
  plot_layout(guides = "collect")

ggsave(p_bale_multipanel,
       filename = here::here("plots", "rigging_comparison", "door_bale_settings.png"),
       width = 100, height = 120, units = "mm", dpi = 300)


# Bridle comparison

bridle_treatments <- readxl::read_xlsx(path = here::here("data", "2026_gear_testing_haul_log.xlsx"),
                                       sheet = "bridle_blocks")

# Compare effects of bridle rigging
bridle_settings_data <- trawl_measurement_summary |>
  dplyr::inner_join(
    bridle_treatments
  )

# Exploring models to estimate door-size effects on door spread


m0 <- glmmTMB::glmmTMB(
  formula = DOOR_SPREAD ~ 0 + DOOR_SIZE_M2*SCOPE_RATIO + (1 | BLOCK),
    data = trawl_measurement_summary
)

m1 <- glmmTMB::glmmTMB(
  formula = DOOR_SPREAD ~ 0 + DOOR_SIZE_M2 + SCOPE_RATIO + (1 | BLOCK),
  data = trawl_measurement_summary
)

test <- mgcv::gam(
  formula = DOOR_SPREAD ~ DOOR_SIZE_M2 + s(SCOPE_RATIO) + s(BLOCK, bs = "re"),
          data = dplyr::mutate(trawl_measurement_summary, BLOCK = factor(BLOCK)))

test0 <- mgcv::gam(
  formula = DOOR_SPREAD ~ s(SCOPE_RATIO, by = DOOR_SIZE_M2) + s(BLOCK, bs = "re"),
  data = dplyr::mutate(trawl_measurement_summary, BLOCK = factor(BLOCK))
  )

glmmTMB::glmmTMB(
  formula = NET_WIDTH ~ 0 + DOOR_SIZE_M2*SCOPE_RATIO + (1 | BLOCK),
  data = trawl_measurement_summary
)


glmmTMB::glmmTMB(
  formula = NET_HEIGHT ~ 0 + DOOR_SIZE_M2*SCOPE_RATIO + (1 | BLOCK),
  data = trawl_measurement_summary
)


ggplot() +
  geom_point(
    data = trawl_measurement_summary,
    mapping = aes(x = BOTTOM_DEPTH, y = DOOR_SPREAD, color = DOOR_SIZE_M2, label = BLOCK)
  ) +
  geom_smooth(
    data = trawl_measurement_summary,
    mapping = aes(x = BOTTOM_DEPTH, y = DOOR_SPREAD, color = DOOR_SIZE_M2, label = BLOCK)
  ) +
  theme_bw()


ggplot() +
  geom_point(
    data = trawl_measurement_summary,
    mapping = aes(x = BOTTOM_DEPTH, y = PORT_DOOR_ROLL, color = DOOR_SIZE_M2, label = BLOCK)
  ) +
  geom_smooth(
    data = trawl_measurement_summary,
    mapping = aes(x = BOTTOM_DEPTH, y = PORT_DOOR_ROLL, color = DOOR_SIZE_M2, label = BLOCK),
    method = 'gam'
  )

ggplot() +
  geom_point(
    data = trawl_measurement_summary,
    mapping = aes(x = BOTTOM_DEPTH, y = STBD_DOOR_ROLL, color = DOOR_SIZE_M2, label = BLOCK)
  ) +
  geom_smooth(
    data = trawl_measurement_summary,
    mapping = aes(x = BOTTOM_DEPTH, y = STBD_DOOR_ROLL, color = DOOR_SIZE_M2, label = BLOCK),
    method = 'gam'
  )

ggplot() +
  geom_point(
    data = trawl_measurement_summary,
    mapping = aes(x = BOTTOM_DEPTH, y = NET_HEIGHT, color = DOOR_SIZE_M2, label = BLOCK)
  ) +
  geom_smooth(
    data = trawl_measurement_summary,
    mapping = aes(x = BOTTOM_DEPTH, y = NET_HEIGHT, color = DOOR_SIZE_M2, label = BLOCK),
    method = 'gam'
  )


ggplot() +
  geom_point(
    data = trawl_measurement_summary,
    mapping = aes(x = BOTTOM_DEPTH, y = NET_WIDTH, color = DOOR_SIZE_M2, label = BLOCK)
  ) +
  geom_smooth(
    data = trawl_measurement_summary,
    mapping = aes(x = BOTTOM_DEPTH, y = NET_WIDTH, color = DOOR_SIZE_M2, label = BLOCK),
    method = 'gam'
  )

ggplot() +
  geom_point(
    data = trawl_measurement_summary,
    mapping = aes(x = BOTTOM_DEPTH, y = NET_WIDTH, color = DOOR_SIZE_M2)
  ) +
  geom_smooth(
    data = trawl_measurement_summary,
    mapping = aes(x = BOTTOM_DEPTH, y = NET_WIDTH, color = DOOR_SIZE_M2),
    method = 'gam'
  )

ggplot() +
  geom_point(
    data = dplyr::arrange(trawl_measurement_summary, SCOPE_RATIO),
    mapping = aes(x = SCOPE_RATIO, y = DOOR_SPREAD, group = interaction(BLOCK, HAUL), color = DOOR_SIZE_M2)
  ) +
  geom_path(
    data = dplyr::arrange(trawl_measurement_summary, SCOPE_RATIO),
    mapping = aes(x = SCOPE_RATIO, y = DOOR_SPREAD, group = interaction(BLOCK, HAUL), color = DOOR_SIZE_M2)
  ) +
  scale_x_continuous(name = "Scope ratio") +
  scale_y_continuous(name = "Mean door spread (m)") +
  facet_wrap(~BLOCK, ncol = 1) +
  theme_bw()

ggplot() +
  geom_hline(yintercept = c(15, 20), linetype = 2) +
  geom_point(
    data = dplyr::arrange(trawl_measurement_summary, SCOPE_RATIO),
    mapping = aes(x = SCOPE_RATIO, y = NET_WIDTH, group = interaction(BLOCK, HAUL), color = DOOR_SIZE_M2)
  ) +
  geom_path(
    data = dplyr::arrange(trawl_measurement_summary, SCOPE_RATIO),
    mapping = aes(x = SCOPE_RATIO, y = NET_WIDTH, group = interaction(BLOCK, HAUL), color = DOOR_SIZE_M2)
  ) +
  scale_x_continuous(name = "Scope ratio") +
  scale_y_continuous(name = "Mean upper wing spread (m)") +
  facet_wrap(~BLOCK, ncol = 1) +
  theme_bw()

ggplot() +
  geom_point(
    data = dplyr::arrange(trawl_measurement_summary, SCOPE_RATIO),
    mapping = aes(x = SCOPE_RATIO, y = SD_NET_WIDTH, group = interaction(BLOCK, HAUL), color = DOOR_SIZE_M2)
  ) +
  geom_path(
    data = dplyr::arrange(trawl_measurement_summary, SCOPE_RATIO),
    mapping = aes(x = SCOPE_RATIO, y = SD_NET_WIDTH, group = interaction(BLOCK, HAUL), color = DOOR_SIZE_M2)
  ) +
  scale_x_continuous(name = "Scope ratio") +
  scale_y_continuous(name = "SD upper wing spread (m)") +
  facet_wrap(~BLOCK, ncol = 1) +
  theme_bw()

ggplot() +
  geom_point(
    data = dplyr::arrange(trawl_measurement_summary, SCOPE_RATIO),
    mapping = aes(x = SCOPE_RATIO, y = SD_DOOR_SPREAD, group = interaction(BLOCK, HAUL), color = DOOR_SIZE_M2)
  ) +
  geom_path(
    data = dplyr::arrange(trawl_measurement_summary, SCOPE_RATIO),
    mapping = aes(x = SCOPE_RATIO, y = SD_DOOR_SPREAD, group = interaction(BLOCK, HAUL), color = DOOR_SIZE_M2)
  ) +
  scale_x_continuous(name = "Scope ratio") +
  scale_y_continuous(name = "SD door spread (m)") +
  facet_wrap(~BLOCK, ncol = 1) +
  theme_bw()

ggplot() +
  geom_hline(yintercept = c(5, 6), linetype = 2) +
  geom_point(
    data = dplyr::arrange(trawl_measurement_summary, SCOPE_RATIO),
    mapping = aes(x = SCOPE_RATIO, y = NET_HEIGHT, group = interaction(BLOCK, HAUL), color = DOOR_SIZE_M2)
  ) +
  geom_path(
    data = dplyr::arrange(trawl_measurement_summary, SCOPE_RATIO),
    mapping = aes(x = SCOPE_RATIO, y = NET_HEIGHT, group = interaction(BLOCK, HAUL), color = DOOR_SIZE_M2)
  ) +
  scale_x_continuous(name = "Scope ratio") +
  scale_y_continuous(name = "Opening height (m)") +
  facet_wrap(~BLOCK, ncol = 1) +
  theme_bw()


ggplot() +
  geom_point(
    data = trawl_measurement_summary,
    mapping = aes(x = BLOCK, y = BOTTOM_DEPTH, color = DOOR_SIZE_M2)
  )

