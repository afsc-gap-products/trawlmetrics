# Review scope table hauls

library(trawlmetrics)
library(akgfmaps)
library(ggthemes)
library(shadowtext)
library(dplyr)
library(ggpp)
library(ggrepel)
library(scales)

# Load haul summary statistics and time series
trawl_data_by_haul <- readRDS(here::here("data", "trawl_data_by_haul.rds"))

bcs_data_by_haul <- readRDS(here::here("data", "bcs_data_by_haul.rds"))

bcs_pings <- 
  readRDS(here::here("data", "bcs_pings.rds"))

marport_pings <- readRDS(here::here("data", "marport_pings.rds"))

dir.create(here::here("plots", "geometry_by_haul"), recursive = TRUE)

# PNE and 83-112 scope tables
scope_tables <- 
  readxl::read_xlsx(path = here::here("data",  "02_scope_table", "shelf_slope_table.xlsx")) |>
  dplyr::mutate(mean_depth_fm = (min_depth_fm+max_depth_fm)/2,
                mean_depth_m = mean_depth_fm * 1.8288,
                scope_to_depth = wire_out_fm/mean_depth_fm) |>
  dplyr::inner_join(data.frame(table = c("GOA/AI", "EBS shelf", "EBS slope"), gear = c("PNE", "83-112", "PNE-S")))

# Analysis and plot settings
door_roll_breaks <- c(-Inf, -10, 0, 10, 25, Inf)
door_roll_labels <- c("<-10", "-10-0", "0-10", "10-25", ">25")
door_roll_colors <- c("#00204DFF", "#414D6BFF", "#7C7B78FF", "#BCAF6FFF", "red")
names(door_roll_colors) <- door_roll_labels

door_roll_breaks <- c(-Inf, 0, 25, 30, Inf)
door_roll_labels <- c("<0", "0-25", "25-30", ">30")
door_roll_colors <- c("#00204DFF", "#BCAF6FFF", "pink", "red")
names(door_roll_colors) <- door_roll_labels

sdr_breaks <- c(0, 2.5, 3, 3.5, 4, 5, 6, 7, Inf)
sdr_labels <- c("<2.5", "2.5-3", "3.0-3.5", "3.5-4.0", "4-5", "5-6", "6-7", ">7")

depth_breaks <- c(20, 30, 50, 75, 100, 150, 250)
depth_labels <- c("20-30 m", "30-50 m", "50-75 m", "75-100 m", "100-150 m", "150-250 m")

codend_symbols <- c('Open' = 1, 'Closed' = 16)
codend_symbol_size <- c("Closed" = 3.5, "Open" = 1.8)

geom_levels <- 
  data.frame(
    name = c("GEAR_DEPTH", "DOOR_ROLL", "NET_WIDTH", "NET_HEIGHT", "DOOR_SPREAD", "BRIDLE_ANGLE_DEG"),
    label = factor(
      c("Gear depth (m)", "Door roll (\u00B0)", "Upper wing spread (m)", "Opening height (m)", "Door spread (m)", "Bridle angle of attack (\u00B0)"),
      levels = c("Gear depth (m)", "Door roll (\u00B0)", "Upper wing spread (m)", "Opening height (m)", "Door spread (m)", "Bridle angle of attack (\u00B0)")
    ),
    min_value = c(NA, -15, 8, 4, 20, 6),
    max_value = c(NA, 40, 22, 10, 56, 21.5),
    min_target = c(NA, NA, 15, 5, NA, 18),
    max_target = c(NA, 30, 20, 6, NA, 21)
  )

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


# Open codend tow data ----

gt_data <- 
  trawl_data_by_haul  |>
  dplyr::mutate(
    DOORS_GOOD = ifelse(DOOR_ROLL_MAX > 30, "Yes", "No"),
    SDR_FAC = 
      cut(SCOPE_RATIO, breaks = sdr_breaks, label = sdr_labels),
    BOTTOM_DEPTH_FAC = 
      cut(BOTTOM_DEPTH, breaks = depth_breaks, label = depth_labels)
  ) |>
  dplyr::mutate(
    DOORS_GEAR_GOOD = ifelse(DOORS_GOOD == "Yes" & GEAR_ON_BOTTOM == "Yes", "Yes", "No"),
    DOORS_GEAR_GOOD = ifelse(GEAR_ON_BOTTOM == "Inconclusive", "Inconclusive", DOORS_GEAR_GOOD)
  )

gt_bcs_data <- 
  dplyr::inner_join(
    gt_data,
    bcs_data_by_haul
  )

# Review data from each haul -----------------------------------------------------------------------

unique_hauls <- sort(unique(gt_data$HAUL))

for(uu in 1:length(unique_hauls)) {
  
  sel_haul <- unique_hauls[uu]
  
  sel_bcs <- 
    bcs_pings |> 
    dplyr::filter(HAUL == sel_haul) |>
    dplyr::inner_join(
      bcs_position_labels
    )
  
  bcs_panel_labels <- 
    sel_bcs  |>
    dplyr::select(HAUL, DISTANCE, DISTANCE_FAC, DISTANCE_DETAIL) |>
    unique()
  
  bcs_mean <-
    sel_bcs  |>
    dplyr::group_by(HAUL, DISTANCE, DISTANCE_FAC, DISTANCE_DETAIL, WIRE_LENGTH_FM) |>
    dplyr::summarise(
      MEAN_DT = mean(DT),
      MEAN_HEIGHT_FIT = mean(HEIGHT_FIT, na.rm = TRUE)
    ) |>
    dplyr::ungroup()
  
  time_mean <- 
    sel_bcs  |>
    dplyr::group_by(HAUL, WIRE_LENGTH_FM) |>
    dplyr::summarise(
      MEAN_DT = mean(DT),
    ) |>
    dplyr::ungroup()
  
  sel_gt_data <- 
    gt_data |>
    dplyr::filter(HAUL == sel_haul)
  
  sel_door_depth <- 
    sel_gt_data |>
    dplyr::select(HAUL, WIRE_LENGTH_FM, GEAR_DEPTH, DOOR_DEPTH_P, DOOR_DEPTH_S) |>
    tidyr::pivot_longer(
      cols = c("GEAR_DEPTH", "DOOR_DEPTH_P", "DOOR_DEPTH_S"),
      names_to = "var"
    ) |>
    dplyr::inner_join(data.frame(
      var = c("GEAR_DEPTH", "DOOR_DEPTH_P", "DOOR_DEPTH_S"),
      text = c("Headline", "Port door", "Starboard door"))) |>
    dplyr::mutate(name = "GEAR_DEPTH") |>
    dplyr::inner_join(geom_levels) |>
    dplyr::inner_join(time_mean) |>
    dplyr::select(-min_value, -max_value)
  
  sel_door_roll <- sel_gt_data |>
    dplyr::mutate(DOOR_ROLL_S = DOOR_ROLL_S * -1) |>
    dplyr::select(HAUL, WIRE_LENGTH_FM, DOOR_ROLL_P, DOOR_ROLL_S) |>
    tidyr::pivot_longer(
      cols = c("DOOR_ROLL_P", "DOOR_ROLL_S"),
      names_to = "var"
    ) |>
    dplyr::inner_join(data.frame(
      var = c("DOOR_ROLL_P", "DOOR_ROLL_S"),
      text = c("Port door", "Starboard door"))) |>
    dplyr::mutate(name = "DOOR_ROLL") |>
    dplyr::inner_join(geom_levels) |>
    dplyr::inner_join(time_mean)
  
  sel_door_roll_labels <-
    sel_door_roll |>
    dplyr::select(HAUL, WIRE_LENGTH_FM, label, value, var, min_value, max_value) |>
    tidyr::pivot_wider(
      values_from = "value",
      names_from = "var"
    ) |>
    dplyr::inner_join(
      time_mean
    ) |>
    dplyr::mutate(text = sprintf("%.1f", pmax(DOOR_ROLL_P, DOOR_ROLL_S, na.rm = TRUE)))
  
  door_depth_range <- 
    sel_door_depth |>
    dplyr::group_by(label) |>
    dplyr::summarise(
      min_value = min(value, na.rm = TRUE)*0.85,
      max_value = max(value, na.rm = TRUE)*1.02
    )
  
  sel_door_depth <-
    sel_door_depth |>
    dplyr::inner_join(
      door_depth_range
    )
  
  sel_door_depth_labels <-
    sel_door_depth |>
    dplyr::select(HAUL, WIRE_LENGTH_FM, label, value, var, min_value, max_value) |>
    tidyr::pivot_wider(
      values_from = "value",
      names_from = "var"
    ) |>
    dplyr::inner_join(
      time_mean
    ) |>
    dplyr::mutate(text = sprintf("%.1f", GEAR_DEPTH-(DOOR_DEPTH_P+DOOR_DEPTH_S)/2))
  
  sel_trawl_geom <- 
    sel_gt_data |>
    dplyr::select(HAUL, WIRE_LENGTH_FM, NET_WIDTH, NET_HEIGHT, DOOR_SPREAD, BRIDLE_ANGLE_DEG) |>
    tidyr::pivot_longer(
      cols = c(NET_WIDTH, NET_HEIGHT, DOOR_SPREAD, BRIDLE_ANGLE_DEG)
    ) |>
    dplyr::inner_join(
      geom_levels
    ) |>
    dplyr::inner_join(time_mean)
  
  anchor_trawl_geom <-
    sel_trawl_geom |>
    dplyr::bind_rows(sel_door_roll) |>
    dplyr::select(label, MEAN_DT, min_value, max_value) |>
    tidyr::pivot_longer(cols = c(min_value, max_value)) |>
    dplyr::bind_rows(
      door_depth_range |>
        tidyr::pivot_longer(
          cols = c(min_value, max_value)
        )
    )
  
  targets_trawl_geom <-
    sel_trawl_geom |>
    dplyr::bind_rows(sel_door_roll) |>
    dplyr::select(label, MEAN_DT, min_target, max_target) |>
    tidyr::pivot_longer(cols = c(min_target, max_target))
  
  sel_marport_pings <- 
    marport_pings |>
    dplyr::filter(HAUL == sel_haul) |>
    tidyr::pivot_longer(
      cols = c(NET_WIDTH, NET_HEIGHT, DOOR_SPREAD, BRIDLE_ANGLE_DEG)
    ) |>
    dplyr::inner_join(
      geom_levels
    ) |>
    dplyr::select(HAUL, DT, WIRE_LENGTH_FM, name, value, label) |>
    dplyr::arrange(DT)
  
  # test <- dplyr::filter(sel_marport_pings, name == "NET_WIDTH")
  
  scope_palette <- viridis_pal(direction = -1)(length(unique(sel_marport_pings$WIRE_LENGTH_FM)) + 1)[-1]
  
  p_trawl_geometry <- 
    ggplot() +
    geom_point(
      data = sel_marport_pings,
      mapping = aes(
        x = DT,
        y = value,
        color = factor(WIRE_LENGTH_FM)
      ),
      size = 0.15
    ) +
    geom_point(
      data = sel_door_depth,
      mapping = aes(x = MEAN_DT, y = value, color = factor(WIRE_LENGTH_FM), shape = text),
      size = 2.2) +
    geom_point(sel_door_roll,
               mapping = aes(x = MEAN_DT, y = value, color = factor(WIRE_LENGTH_FM), shape = text)) +
    geom_hline(
      data = targets_trawl_geom,
      mapping = aes(yintercept = value),
      linetype = 2,
      color = "grey30",
      size = 0.3
    ) +
    geom_shadowtext(
      data = sel_trawl_geom,
      mapping = aes(x = MEAN_DT, y = min_value*1.09, label = sprintf("%.1f", value)),
      size = 2.2,
      color = "black",
      bg.color = "white") +
    geom_point(
      data = anchor_trawl_geom,
      mapping = aes(x = MEAN_DT, y = value),
      color = NA) +
    geom_shadowtext(
      data = sel_door_depth_labels,
      mapping = aes(x = MEAN_DT, y = min_value*1.05, label = text),
      size = 2.2,
      hjust = 0.5,
      color = "black",
      bg.color = "white") +
    geom_shadowtext(
      data = sel_door_roll_labels,
      mapping = aes(x = MEAN_DT, y = -10, label = text),
      size = 2.2,
      hjust = 0.5,
      color = "black",
      bg.color = "white") +
    ggtitle(paste0("Haul ", sel_haul)) +
    scale_color_manual(name = "Scope (fm)", values = scope_palette) +
    scale_shape_manual(name = "Sensor", values = c('Port door' = 0, 'Starboard door' = 2, 'Headline' = 16)) +
    scale_x_datetime(name = "Date/time") +
    scale_y_continuous(name = "Value", oob = squish_infinite) +
    facet_wrap(~label, scales = "free_y", nrow = 6) +
    theme_bw() +
    theme(
      strip.background = element_blank(),
      strip.text = element_text(face = "bold", hjust = 0, size = 9),
      axis.text = element_text(size = 8),
      axis.title = element_text(size = 8),
      legend.text = element_text(size = 8),
      legend.title = element_text(size = 8),
      legend.key.size = unit(4, "mm")
    )
  
  p_bcs_pings <- 
    ggplot() +
    geom_hline(
      yintercept = c(0, 2.54*3), 
      linetype = 2,
      color = "grey30",
      linewidth = 0.3
    ) +
    geom_point(
      data = sel_bcs,
      mapping = aes(x = DT, y = HEIGHT_FIT, color = factor(WIRE_LENGTH_FM)), 
      size = 0.15
    ) +
    geom_shadowtext(
      data = bcs_mean,
      mapping = aes(
        x = MEAN_DT,
        y = -3.5,
        label = sprintf("%.1f", MEAN_HEIGHT_FIT)
      ),
      size = 2.2,
      color = "black",
      bg.color = "white"
    ) +
    ggtitle(" ") +
    scale_x_datetime(name = "Date/time (AKDT)") +
    scale_y_continuous(name = "Elevation (cm)", limits = c(-5, 40), oob = squish) +
    scale_color_manual(name = "Scope (fm)", values = scope_palette, guide = "none") +
    scale_linetype(name = "Side") +
    facet_wrap(~DISTANCE_DETAIL, ncol = 1) +
    theme_bw() +
    theme(
      strip.text = element_text(face = "bold", hjust = 0, size = 9),
      strip.background = element_blank(),
      axis.text = element_text(size = 8),
      axis.title = element_text(size = 8),
      legend.text = element_text(size = 8),
      legend.title = element_text(size = 8),
      legend.key.size = unit(4, "mm")
    )
  
  p_grid_performance <- 
    cowplot::plot_grid(
      p_trawl_geometry +
        theme(legend.position = "bottom", 
              legend.box = "vertical",
              legend.spacing = unit(1, "mm")),
      p_bcs_pings +
        theme(legend.position = "bottom"), 
      ncol = 2,
      align = "hv"
    )
  
  
  ggsave(
    p_grid_performance,
    filename = here::here("plots", "geometry_by_haul", paste0("geom_by_haul_", sel_haul, ".png")),
    width = 6.5,
    height = 6.5,
    units = "in",
    dpi = 300
  )
  
}


# Plots

depth_range <- range(gt_data$BOTTOM_DEPTH, na.rm = TRUE)
sdr_axis_breaks <- 2:9
max_sdr <- max(gt_data$WIRE_LENGTH/gt_data$BOTTOM_DEPTH, na.rm = TRUE)

# Select hauls after door trials
gt_subset <- 
  dplyr::filter(gt_data, !is.na(SDR_FAC))

gt_bcs_subset <- 
  dplyr::filter(gt_bcs_data, !is.na(SDR_FAC))

bcs_closed_open_summary <- bcs_summary |>
  dplyr::mutate(
    CODEND = ifelse(HAUL < 600, "Open", "Closed")
    ) |>
  dplyr::filter(GEAR_ON_BOTTOM == "Yes") |>
  dplyr::group_by(DISTANCE, CODEND, DOOR_SIZE_M2, BRIDLE_CHAIN_WEIGHT) |>
  dplyr::summarise(
    MIN_HEIGHT = min(MEAN_BCS_HEIGHT, na.rm = TRUE),
    MAX_HEIGHT = max(MEAN_BCS_HEIGHT, na.rm = TRUE),
    MEAN_HEIGHT = mean(MEAN_BCS_HEIGHT, na.rm = TRUE)
  )

bcs_closed_open <- bcs_summary |>
  dplyr::mutate(
    CODEND = ifelse(HAUL < 600, "Open", "Closed")
  )

p_mean_bcs_height_45 <- 
  ggplot() + 
  geom_point(
    data = dplyr::filter(bcs_closed_open_summary, DOOR_SIZE_M2 == 4.5),
    mapping = 
      aes(
        x = DISTANCE,
        y = MEAN_HEIGHT,
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


# Examine BCS height data

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
      dplyr::filter(bcs_summary, !is.na(GEAR_ON_BOTTOM)),
    mapping =
      aes(
        x = HAUL,
        y = MEAN_HEIGHT,
        group = interaction(HAUL, WIRE_LENGTH_FM),
        alpha =  DOORS_ON_BOTTOM),
    color = "grey30"
  ) +
  geom_point(
    data =  dplyr::filter(bcs_summary, !is.na(GEAR_ON_BOTTOM)),
    mapping =
      aes(x = HAUL, y = MEAN_HEIGHT, color = SIDE,
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
  event_number = c(1:7),
  event = c(
    "(1) Bridle 2",
    "(2) Bridle 1.2,\nremove half-bobbin",
    "(3) 120 kg bridle weight,\n8-in door setback", 
    "(4) 68 kg bridle weight", 
    "(5) Change scope\nprocedure",
    "(6) 4-in door setback",
    "(7) Remove 8 floats"),
  haul = c(526.5, 530.5, 533.5, 535.5, 539.5, 543.5, 550.5)
)


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
    linewidth = 0.65
             ) +
  geom_text(
    data = dplyr::mutate(events, DISTANCE = 0) |>
      dplyr::inner_join(
        bcs_position_labels
      ),
    mapping = aes(
      x = haul,
      y = 48,
      label = event_number
    ),
    hjust = -0.3,
    color = "grey30"
  ) +
  geom_path(
    data = 
      dplyr::filter(
        bcs_summary, 
        !is.na(GEAR_ON_BOTTOM),
        !(DISTANCE %in% c(2, 18)),
        HAUL < 600
      ) |>
      dplyr::inner_join(
        bcs_position_labels
      ),
    mapping = 
      aes(
        x = HAUL, 
        y = MEAN_HEIGHT, 
        group = interaction(HAUL, WIRE_LENGTH_FM), 
        alpha =  DOORS_ON_BOTTOM),
    color = "grey30"
  ) +
  geom_point(
    data =
      dplyr::filter(
        bcs_summary, 
        !is.na(GEAR_ON_BOTTOM),
        !(DISTANCE %in% c(2, 18)),
        HAUL < 600
      ) |>
      dplyr::inner_join(
        bcs_position_labels
      ),
    mapping = 
      aes(x = HAUL, 
          y = MEAN_HEIGHT,
          alpha =  DOORS_ON_BOTTOM),
    size = 0.7
  ) +
  scale_color_discrete(name = "Change") +
  scale_alpha_manual("Doors on bottom?", values = c('Yes' = 1, 'No' = 0.2)) +
  scale_y_continuous("Elevation (cm)", 
                     limits = c(-2, 50), 
                     oob = scales::squish_infinite) +
  scale_x_continuous(name = "Haul") +
  facet_wrap(~DISTANCE_DETAIL, nrow = 7) +
  theme_bw() +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", hjust = 0)
  )

png(here::here("plots", "bcs_dtb", "bcs_dtb_events_open.png"), 
    width = 169, height = 169, units = "mm", res = 300)
print(p_dtb_events)
dev.off()


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
        bcs_summary, 
        !is.na(GEAR_ON_BOTTOM),
        DISTANCE %in% c(0, 8, 13, 16, 21),
        HAUL >= 600
      ) |>
      dplyr::inner_join(dplyr::select(cc_hauls, HAUL, PERFORMANCE, BOTTOM_DEPTH)) |>
      # dplyr::filter(PERFORMANCE >= 0) |>
      dplyr::inner_join(
        bcs_position_labels
      ),
    mapping = 
      aes(
        x = HAUL, 
        y = MEAN_HEIGHT, 
        group = HAUL, 
        color = BOTTOM_DEPTH)
  ) +
  geom_point(
    data =
      dplyr::filter(
        bcs_summary, 
        !is.na(GEAR_ON_BOTTOM),
        DISTANCE %in% c(0, 8, 13, 16, 21),
        HAUL >= 600
      ) |>
      dplyr::inner_join(dplyr::select(cc_hauls, HAUL, PERFORMANCE, BOTTOM_DEPTH)) |>
      # dplyr::filter(PERFORMANCE >= 0) |>
      dplyr::inner_join(
        bcs_position_labels
      ),
    mapping = 
      aes(
        x = HAUL, 
          y = MEAN_HEIGHT,
        fill = BOTTOM_DEPTH),
    shape = 21
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

png(here::here("plots", "bcs_dtb", "bcs_dtb_closed.png"), 
    width = 120, height = 120, units = "mm", res = 300)
print(p_dtb_closed)
dev.off()



# SDR facets, bottom depth x-axis
ggplot() +
  geom_hline(yintercept = c(0, 30), linetype = 2) +
  geom_point(
    data = dplyr::filter(gt_subset, GEAR_ON_BOTTOM == "Yes"),
    mapping = aes(
      x = BOTTOM_DEPTH, 
      y = MAX_DOOR_ROLL, 
      shape = CODEND, 
      color = factor(BRIDLE_CHAIN_WEIGHT)
    ),
    alpha = 0.7,
    size = 2.2
  ) +
  geom_rug(
    data = dplyr::filter(gt_subset, GEAR_ON_BOTTOM == "Yes"), 
    mapping = aes(
      x = BOTTOM_DEPTH
    )
  ) +
  scale_color_colorblind(name = "Chain weight (kg)") +
  scale_shape_manual(name = "Codend", values = codend_symbols) +
  scale_x_continuous(name = "Depth (m)") +
  scale_y_continuous(name = "Door roll (\u00B0)", oob = scales::squish, limits = c(-3, 40), expand = c(0, 0)) +
  facet_wrap(~SDR_FAC) +
  theme_bw()

ggplot() +
  geom_hline(yintercept = c(15, 20), linetype = 2) +
  geom_point(
    data = dplyr::filter(gt_subset, GEAR_ON_BOTTOM == "Yes"), 
    mapping = aes(
      x = BOTTOM_DEPTH, 
      y = NET_WIDTH, 
      shape = CODEND, 
      color = factor(BRIDLE_CHAIN_WEIGHT)
    ),
    alpha = 0.7,
    size = 2.2
  ) +
  scale_color_colorblind(name = "Chain weight (kg)") +
  scale_size_manual(name = "Codend", values = codend_symbol_size) +
  scale_shape_manual(name = "Codend", values = codend_symbols) +
  scale_x_continuous(name = "Depth (m)") +
  scale_y_continuous(
    name = "Upper wing spread (m)", 
                     oob = scales::squish_infinite, 
    limits = c(10, 21.5),
    expand = c(0, 0)) +
  facet_wrap(~SDR_FAC) +
  theme_bw()

ggplot() +
  geom_hline(yintercept = c(5, 6), linetype = 2) +
  geom_point(
    data = dplyr::filter(gt_subset, GEAR_ON_BOTTOM == "Yes"), 
    mapping = aes(
      x = BOTTOM_DEPTH, 
      y = NET_HEIGHT, 
      shape = CODEND, 
      color = factor(BRIDLE_CHAIN_WEIGHT)
    ),
    alpha = 0.7,
    size = 2.2
  ) +
  geom_rug(
    data = dplyr::filter(gt_subset, GEAR_ON_BOTTOM == "Yes"), 
    mapping = aes(
      x = BOTTOM_DEPTH
    )
  ) +
  scale_color_colorblind(name = "Chain weight (kg)") +
  scale_shape_manual(name = "Codend", values = codend_symbols) +
  scale_x_continuous(name = "Depth (m)") +
  scale_y_continuous(
    name = "Opening height (m)", 
    oob = scales::squish,
    limits = c(4, 10),
    expand = c(0, 0)) +
  facet_wrap(~SDR_FAC) +
  theme_bw()

ggplot() +
  geom_point(
    data = dplyr::filter(gt_subset, GEAR_ON_BOTTOM == "Yes"), 
    mapping = aes(
      x = BOTTOM_DEPTH, 
      y = DOOR_SPREAD, 
      shape = CODEND, 
       
      color = factor(BRIDLE_CHAIN_WEIGHT)
    ),
    alpha = 0.7,
    size = 2.2
  ) +
  geom_rug(
    data = dplyr::filter(gt_subset, GEAR_ON_BOTTOM == "Yes"), 
    mapping = aes(
      x = BOTTOM_DEPTH
    )
  ) +
  scale_color_colorblind(name = "Chain weight (kg)") +
  scale_shape_manual(name = "Codend", values = codend_symbols) +
  scale_x_continuous(name = "Depth (m)") +
  scale_y_continuous(
    name = "Door spread (m)", 
    oob = scales::squish, 
    limits = c(26, 58),
    expand = c(0, 0)) +
  facet_wrap(~SDR_FAC) +
  theme_bw()

ggplot() +
  geom_hline(yintercept = c(18, 21), linetype = 2) +
  geom_point(
    data = gt_subset, 
    mapping = aes(
      x = BOTTOM_DEPTH, 
      y = BRIDLE_ANGLE_DEG, 
      shape = CODEND, 
       
      color = factor(BRIDLE_CHAIN_WEIGHT)
    ),
    alpha = 0.7,
    size = 2.2
  ) +
  geom_rug(
    data = gt_subset, 
    mapping = aes(
      x = BOTTOM_DEPTH
    )
  ) +
  scale_color_colorblind(name = "Chain weight (kg)") +
  scale_shape_manual(name = "Codend", values = codend_symbols) +
  scale_x_continuous(name = "Depth (m)") +
  scale_y_continuous(
    name = "Bridle angle of attack (\u00B0)", 
    oob = scales::squish, 
    limits = c(6, 21.5),
    expand = c(0, 0)) +
  facet_wrap(~SDR_FAC) +
  theme_bw()


# Bottom depth facets, SDR x-axis

ggplot() +
  geom_hline(yintercept = c(0, 30), linetype = 2) +
  geom_point(
    data = gt_subset, 
    mapping = aes(
      x = SDR, 
      y = MAX_DOOR_ROLL, 
      shape = CODEND, 
       
      color = factor(BRIDLE_CHAIN_WEIGHT)
    ),
    alpha = 0.7,
    size = 2.2
  ) +
  scale_color_colorblind(name = "Chain weight (kg)") +
  scale_shape_manual(name = "Codend", values = codend_symbols) +
  scale_x_continuous(name = "Scope/Depth") +
  scale_y_continuous(name = "Door roll (\u00B0)", oob = scales::squish, limits = c(-1, 40), expand = c(0, 0)) +
  facet_wrap(~BOTTOM_DEPTH_FAC, scales = "free") +
  theme_bw()

ggplot() +
  geom_hline(yintercept = c(15, 20), linetype = 2) +
  geom_point(
    data = gt_subset, 
    mapping = aes(
      x = SDR, 
      y = NET_WIDTH, 
      shape = CODEND, 
       
      color = factor(BRIDLE_CHAIN_WEIGHT)
    ),
    alpha = 0.7,
    size = 2.2
  ) +
  scale_color_colorblind(name = "Chain weight (kg)") +
  scale_shape_manual(name = "Codend", values = codend_symbols) +
  scale_x_continuous(name = "Scope/Depth") +
  scale_y_continuous(
    name = "Upper wing spread (m)", 
    oob = scales::squish, 
    limits = c(10, 22), 
    expand = c(0, 0)) +
  facet_wrap(~BOTTOM_DEPTH_FAC, scales = "free") +
  theme_bw()

ggplot() +
  geom_point(
    data = gt_subset, 
    mapping = aes(
      x = SDR, 
      y = DOOR_SPREAD, 
      shape = CODEND, 
       
      color = factor(BRIDLE_CHAIN_WEIGHT)
    ),
    alpha = 0.7,
    size = 2.2
  ) +
  scale_color_colorblind(name = "Chain weight (kg)") +
  scale_shape_manual(name = "Codend", values = codend_symbols) +
  scale_x_continuous(name = "Scope/Depth") +
  scale_y_continuous(
    name = "Door spread (m)", 
    oob = scales::squish, 
    limits = c(26, 58),
    expand = c(0, 0)) +
  facet_wrap(~BOTTOM_DEPTH_FAC, scales = "free") +
  theme_bw()

ggplot() +
  geom_hline(yintercept = c(5, 6), linetype = 2) +
  geom_point(
    data = gt_subset, 
    mapping = aes(
      x = SDR, 
      y = NET_HEIGHT, 
      shape = CODEND, 
      color = factor(BRIDLE_CHAIN_WEIGHT)
    ),
    alpha = 0.7,
    size = 2.2
  ) +
  scale_color_colorblind(name = "Chain weight (kg)") +
  scale_shape_manual(name = "Codend", values = codend_symbols) +
  scale_x_continuous(name = "Scope/Depth") +
  scale_y_continuous(
    name = "Opening height (m)", 
    oob = scales::squish,
    limits = c(4, 10),
    expand = c(0, 0)) +
  facet_wrap(~BOTTOM_DEPTH_FAC, scales = "free") +
  theme_bw()

ggplot() +
  geom_hline(yintercept = c(18, 21), linetype = 2) +
  geom_point(
    data = gt_subset, 
    mapping = aes(
      x = SDR, 
      y = BRIDLE_ANGLE_DEG, 
      shape = CODEND, 
      color = factor(BRIDLE_CHAIN_WEIGHT)
    ),
    alpha = 0.7,
    size = 2.2
  ) +
  scale_color_colorblind(name = "Chain weight (kg)") +
  scale_shape_manual(name = "Codend", values = codend_symbols) +
  scale_x_continuous(name = "Scope/Depth") +
  scale_y_continuous(
    name = "Bridle angle of attack (\u00B0)", 
    oob = scales::squish, 
    limits = c(6, 21.5),
    expand = c(0, 0)) +
  facet_wrap(~BOTTOM_DEPTH_FAC, scales = "free") +
  theme_bw()

ggplot() +
  geom_hline(yintercept = c(0, 30), linetype = 2) +
  geom_point(
    data = gt_subset, 
    mapping = aes(
      x = TOTAL_WINCH_TENSION, 
      y = MAX_DOOR_ROLL, 
      shape = CODEND, 
      color = factor(BRIDLE_CHAIN_WEIGHT)
    ),
    alpha = 0.7,
    size = 2.2
  ) +
  scale_color_colorblind(name = "Chain weight (kg)") +
  scale_shape_manual(name = "Codend", values = codend_symbols) +
  scale_x_continuous(name = "Total winch tension (t)") +
  scale_y_continuous(
    name = "Door roll (\u00B0)", 
    oob = scales::squish, 
    limits = c(-1, 40), expand = c(0, 0)
    ) +
  facet_wrap(~BOTTOM_DEPTH_FAC) +
  theme_bw()

ggplot() +
  geom_hline(yintercept = c(0, 30), linetype = 2) +
  geom_point(
    data = gt_subset, 
    mapping = aes(
      x = TOTAL_WINCH_TENSION, 
      y = MAX_DOOR_ROLL, 
      shape = CODEND, 
      color = factor(BRIDLE_CHAIN_WEIGHT)
    ),
    alpha = 0.7,
    size = 2.2
  ) +
  scale_color_colorblind(name = "Chain weight (kg)") +
  scale_shape_manual(name = "Codend", values = codend_symbols) +
  scale_x_continuous(name = "Total winch tension (t)") +
  scale_y_continuous(
    name = "Door roll (\u00B0)", 
    oob = scales::squish, 
    limits = c(-1, 40), expand = c(0, 0)
  ) +
  facet_wrap(~SDR_FAC) +
  theme_bw()

ggplot() +
  geom_hline(yintercept = c(0, 2.54*3), linetype = 2) +
  geom_point(
    data = dplyr::filter(gt_bcs_subset, DISTANCE == 13),
    mapping = aes(
      x = SDR, 
      y = MEAN_HEIGHT, 
      shape = CODEND, 
      color = factor(BRIDLE_CHAIN_WEIGHT)
    ),
    alpha = 0.7,
    size = 2.2
  ) +
  scale_color_colorblind(name = "Chain weight (kg)") +
  scale_shape_manual(name = "Codend", values = codend_symbols) +
  scale_x_continuous(name = "Scope/Depth") +
  scale_y_continuous(
    name = "13-m BCS Elevation (cm)", 
    oob = scales::squish, 
    limits = c(-1, 40)) +
  facet_wrap(~BOTTOM_DEPTH_FAC, scales = "free") +
  theme_bw()

ggplot() +
  geom_hline(yintercept = c(0, 2.54*3), linetype = 2) +
  geom_point(
    data = dplyr::filter(gt_bcs_subset, DISTANCE == 16),
    mapping = aes(
      x = SDR, 
      y = MEAN_HEIGHT, 
      shape = CODEND, 
      color = factor(BRIDLE_CHAIN_WEIGHT)
    ),
    alpha = 0.7,
    size = 2.2
  ) +
  scale_color_colorblind(name = "Chain weight (kg)") +
  scale_shape_manual(name = "Codend", values = codend_symbols) +
  scale_x_continuous(name = "Scope/Depth") +
  scale_y_continuous(
    name = "16-m BCS Elevation (cm)", 
    oob = scales::squish, 
    limits = c(-1, 40)) +
  facet_wrap(~BOTTOM_DEPTH_FAC, scales = "free") +
  theme_bw()

ggplot() +
  geom_hline(yintercept = c(0, 2.54*3), linetype = 3) +
  geom_point(
    data = dplyr::filter(gt_bcs_subset, DISTANCE == 8),
    mapping = aes(
      x = SDR, 
      y = MEAN_HEIGHT, 
      shape = CODEND, 
      color = factor(BRIDLE_CHAIN_WEIGHT)
    ),
    alpha = 0.7,
    size = 2.2
  ) +
  scale_color_colorblind(name = "Chain weight (kg)") +
  scale_shape_manual(name = "Codend", values = codend_symbols) +
  scale_x_continuous(name = "Scope/Depth") +
  scale_y_continuous(
    name = "8-m BCS Elevation (cm)", 
    oob = scales::squish, 
    limits = c(-1, 40)) +
  facet_wrap(~BOTTOM_DEPTH_FAC, scales = "free") +
  theme_bw()

ggplot() +
  geom_hline(yintercept = c(0, 2.54*3), linetype = 2) +
  geom_point(
    data = dplyr::filter(gt_bcs_subset, DISTANCE == 0),
    mapping = aes(
      x = SDR, 
      y = MEAN_HEIGHT, 
      shape = CODEND, 
      color = factor(BRIDLE_CHAIN_WEIGHT)
    ),
    alpha = 0.7,
    size = 2.2
  ) +
  scale_color_colorblind(name = "Chain weight (kg)") +
  # scale_size_manual(name = "Codend", values = codend_symbol_size) +
  scale_shape_manual(name = "Codend", values = codend_symbols) +
  scale_x_continuous(name = "Scope/Depth") +
  scale_y_continuous(
    name = "Center BCS Height (cm)", 
    oob = scales::squish, 
    limits = c(-1, 40)) +
  facet_wrap(~BOTTOM_DEPTH_FAC, scales = "free") +
  theme_bw()

ggplot() +
  geom_hline(yintercept = c(0, 2.54*3), linetype = 2) +
  geom_point(
    data = dplyr::filter(gt_bcs_subset, DISTANCE == 21),
    mapping = aes(
      x = SDR, 
      y = MEAN_HEIGHT, 
      shape = CODEND, 
      color = factor(BRIDLE_CHAIN_WEIGHT)
    ),
    alpha = 0.7,
    size = 2.2
  ) +
  scale_color_colorblind(name = "Chain weight (kg)") +
  # scale_size_manual(name = "Codend", values = codend_symbol_size) +
  scale_shape_manual(name = "Codend", values = codend_symbols) +
  scale_x_continuous(name = "Scope/Depth") +
  scale_y_continuous(
    name = "21-m BCS Elevation (cm)", 
    oob = scales::squish, 
    limits = c(-1, 40)) +
  facet_wrap(~BOTTOM_DEPTH_FAC, scales = "free") +
  theme_bw()

# No facets


ggplot() +
  geom_path(
    data = scope_tables,
    mapping = aes(x = mean_depth_m, y = scope_to_depth, linetype = gear),
    color = "grey",
    linewidth = 1.2
  ) +
  geom_point(
    data = dplyr::filter(gt_data, !is.na(MAX_DOOR_ROLL)) |>
      dplyr::arrange(MAX_DOOR_ROLL), 
    mapping = aes(
      x = BOTTOM_DEPTH, 
      y = WIRE_LENGTH/BOTTOM_DEPTH, 
      color = cut(MAX_DOOR_ROLL, breaks = door_roll_breaks, labels = door_roll_labels), 
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
    breaks = sdr_axis_breaks,
    limits = c(2, max_sdr)) +
  # scale_size_manual(name = "Codend", values = c("Closed" = 3, "Open" = 1.8)) +
  scale_shape_manual(name = "Codend", values = codend_symbols) +
  theme_bw()

ggplot() +
  geom_path(
    data = scope_tables,
    mapping = aes(x = mean_depth_m, y = scope_to_depth, linetype = gear),
    color = "grey",
    linewidth = 1.2
  ) +
  geom_point(
    data = dplyr::filter(gt_data, !is.na(MAX_DOOR_ROLL)) |>
      dplyr::arrange(MAX_DOOR_ROLL), 
    mapping = 
      aes(
        x = BOTTOM_DEPTH, 
        y = WIRE_LENGTH/BOTTOM_DEPTH, 
        color = cut(MAX_DOOR_ROLL, breaks = door_roll_breaks, labels = door_roll_labels), 
        shape = CODEND),
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
    breaks = sdr_axis_breaks,
    limits = c(2, max_sdr)) +
  # scale_size_manual(name = "Codend", values = c("Closed" = 3, "Open" = 1.8)) +
  scale_shape_manual(name = "Codend", values = codend_symbols) +
  theme_bw()


ggplot() +
  geom_path(
    data = scope_tables,
    mapping = aes(x = mean_depth_m, y = scope_to_depth, linetype = gear),
    color = "grey",
    linewidth = 1.2
  ) +
  geom_point(
    data = dplyr::filter(gt_data, !is.na(MAX_DOOR_ROLL)), 
    mapping = 
      aes(
        x = BOTTOM_DEPTH, 
        y = WIRE_LENGTH/BOTTOM_DEPTH, 
        color = FOOTROPE_ON_BOTTOM, 
        shape = CODEND),
    alpha = 0.8
  ) + 
  geom_text_repel(
    data = dplyr::filter(gt_data, !is.na(MAX_DOOR_ROLL)), 
    mapping = 
      aes(
        x = BOTTOM_DEPTH, 
        y = WIRE_LENGTH/BOTTOM_DEPTH, 
        color = FOOTROPE_ON_BOTTOM, 
        label = HAUL),
    alpha = 0.8
  ) + 
  scale_linetype(name = "Scope table/gear") +
  scale_color_manual(name = "Footrope on bottom", values = c("No" = "red", "Yes" = "blue4", "Inconclusive" = "grey50")) +
  scale_x_log10(
    name = "Bottom depth (m)", 
    limits = c(depth_range[1]-2, depth_range[2]), 
    breaks = depth_breaks,
    oob = scales::oob_squish_infinite,
    expand = c(0,0)) +
  scale_y_continuous(
    name = "Scope/Depth", 
    oob = scales::oob_squish_infinite,
    breaks = sdr_axis_breaks,
    limits = c(2, max_sdr)) +
  scale_shape_manual(name = "Codend", values = codend_symbols) +
  theme_bw()


ggplot() +
  geom_path(
    data = scope_tables,
    mapping = aes(x = mean_depth_m, y = scope_to_depth, linetype = gear),
    color = "grey",
    linewidth = 1.2
  ) +
  geom_point(
    data = dplyr::filter(gt_data, !is.na(MAX_DOOR_ROLL)), 
    mapping = 
      aes(
        x = BOTTOM_DEPTH, 
        y = WIRE_LENGTH/BOTTOM_DEPTH, 
        color = DOORS_ON_BOTTOM, 
        shape = CODEND),
    alpha = 0.8
  ) + 
  geom_text_repel(
    data = dplyr::filter(gt_data, !is.na(MAX_DOOR_ROLL)), 
    mapping = 
      aes(
        x = BOTTOM_DEPTH, 
        y = WIRE_LENGTH/BOTTOM_DEPTH, 
        color = DOORS_ON_BOTTOM, 
        label = HAUL),
    alpha = 0.8
  ) + 
  scale_linetype(name = "Scope table/gear") +
  scale_color_manual(name = "Doors on bottom", values = c("No" = "red", "Yes" = "blue4", "Inconclusive" = "grey50")) +
  scale_x_log10(
    name = "Bottom depth (m)", 
    limits = c(depth_range[1]-2, depth_range[2]), 
    breaks = depth_breaks,
    oob = scales::oob_squish_infinite,
    expand = c(0,0)) +
  scale_y_continuous(
    name = "Scope/Depth", 
    oob = scales::oob_squish_infinite,
    breaks = sdr_axis_breaks,
    limits = c(2, max_sdr)) +
  scale_shape_manual(name = "Codend", values = codend_symbols) +
  theme_bw()

ggplot() +
  geom_path(
    data = scope_tables,
    mapping = aes(x = mean_depth_m, y = scope_to_depth, linetype = gear),
    color = "grey",
    linewidth = 1.2
  ) +
  geom_point(
    data = dplyr::filter(gt_data, !is.na(MAX_DOOR_ROLL)), 
    mapping = 
      aes(
        x = BOTTOM_DEPTH, 
        y = WIRE_LENGTH/BOTTOM_DEPTH, 
        color = GEAR_ON_BOTTOM, 
        shape = CODEND),
    alpha = 0.8
  ) + 
  geom_text_repel(
    data = dplyr::filter(gt_data, !is.na(MAX_DOOR_ROLL)), 
    mapping = 
      aes(
        x = BOTTOM_DEPTH, 
        y = WIRE_LENGTH/BOTTOM_DEPTH, 
        color = GEAR_ON_BOTTOM, 
        label = HAUL),
    alpha = 0.8
  ) + 
  scale_linetype(name = "Scope table/gear") +
  scale_color_manual(name = "Footrope & doors\non bottom", values = c("No" = "red", "Yes" = "blue4", "Inconclusive" = "grey50")) +
  scale_x_log10(
    name = "Bottom depth (m)", 
    limits = c(depth_range[1]-2, depth_range[2]), 
    breaks = depth_breaks,
    oob = scales::oob_squish_infinite,
    expand = c(0,0)) +
  scale_y_continuous(
    name = "Scope/Depth", 
    oob = scales::oob_squish_infinite,
    breaks = sdr_axis_breaks,
    limits = c(2, max_sdr)) +
  scale_shape_manual(name = "Codend", values = codend_symbols) +
  theme_bw()



ggplot() +
  geom_path(
    data = scope_tables,
    mapping = aes(x = mean_depth_m, y = scope_to_depth, linetype = gear),
    color = "grey",
    linewidth = 1.2
  ) +
  geom_point(
    data = dplyr::filter(gt_data, !is.na(MAX_DOOR_ROLL)), 
    mapping = 
      aes(
        x = BOTTOM_DEPTH, 
        y = WIRE_LENGTH/BOTTOM_DEPTH, 
        color = DOORS_GEAR_GOOD, 
        shape = CODEND),
    alpha = 0.8
  ) + 
  geom_text_repel(
    data = dplyr::filter(gt_data, !is.na(MAX_DOOR_ROLL)), 
    mapping = 
      aes(
        x = BOTTOM_DEPTH, 
        y = WIRE_LENGTH/BOTTOM_DEPTH, 
        color = DOORS_GEAR_GOOD, 
        label = HAUL),
    alpha = 0.8
  ) + 
  scale_linetype(name = "Scope table/gear") +
  scale_color_manual(name = "Footrope/doors on bottom\n& door roll<30", values = c("No" = "red", "Yes" = "blue4", "Inconclusive" = "grey50")) +
  scale_x_log10(
    name = "Bottom depth (m)", 
    limits = c(depth_range[1]-2, depth_range[2]), 
    breaks = depth_breaks,
    oob = scales::oob_squish_infinite,
    expand = c(0,0)) +
  scale_y_continuous(
    name = "Scope/Depth", 
    oob = scales::oob_squish_infinite,
    breaks = sdr_axis_breaks,
    limits = c(2, max_sdr)) +
  scale_shape_manual(name = "Codend", values = codend_symbols) +
  facet_wrap(~paste0(DOOR_LEG_SETBACK_IN, "-in setback")) +
  theme_bw()

ggplot() +
  geom_path(
    data = scope_tables,
    mapping = aes(x = mean_depth_m, y = scope_to_depth, linetype = gear),
    color = "grey",
    linewidth = 1.02
  ) +
  geom_point(
    data = dplyr::filter(gt_data, !is.na(MAX_DOOR_ROLL)), 
    mapping = 
      aes(
        x = BOTTOM_DEPTH, 
        y = WIRE_LENGTH/BOTTOM_DEPTH, 
        color = DOORS_GEAR_GOOD, 
        shape = CODEND),
    alpha = 0.8
  ) + 
  geom_text_repel(
    data = dplyr::filter(gt_data, !is.na(MAX_DOOR_ROLL)), 
    mapping = 
      aes(
        x = BOTTOM_DEPTH, 
        y = WIRE_LENGTH/BOTTOM_DEPTH, 
        color = DOORS_GEAR_GOOD, 
        label = HAUL),
    alpha = 0.8
  ) + 
  scale_linetype(name = "Scope table/gear") +
  scale_color_manual(name = "Footrope/doors on bottom\n& door roll<30", values = c("No" = "red", "Yes" = "blue4", "Inconclusive" = "grey50")) +
  scale_x_log10(
    name = "Bottom depth (m)", 
    limits = c(depth_range[1]-2, depth_range[2]+10), 
    breaks = depth_breaks,
    oob = scales::oob_squish_infinite,
    expand = c(0,0)) +
  scale_y_continuous(
    name = "Scope/Depth", 
    oob = scales::oob_squish_infinite,
    breaks = sdr_axis_breaks,
    limits = c(2, max_sdr)) +
  scale_shape_manual(name = "Codend", values = codend_symbols) +
  facet_grid(BRIDLE_CHAIN_WEIGHT~DOOR_LEG_SETBACK_IN) +
  theme_bw()

ggplot() +
  geom_path(
    data = scope_tables,
    mapping = aes(x = mean_depth_m, y = scope_to_depth, linetype = gear),
    color = "grey",
    linewidth = 1.02
  ) +
  geom_point(
    data = dplyr::filter(gt_data, !is.na(MAX_DOOR_ROLL)), 
    mapping = 
      aes(
        x = BOTTOM_DEPTH, 
        y = WIRE_LENGTH/BOTTOM_DEPTH, 
        color =  GEAR_ON_BOTTOM, 
        shape = CODEND),
    alpha = 0.8
  ) + 
  geom_text_repel(
    data = dplyr::filter(gt_data, !is.na(MAX_DOOR_ROLL)), 
    mapping = 
      aes(
        x = BOTTOM_DEPTH, 
        y = WIRE_LENGTH/BOTTOM_DEPTH, 
        color = GEAR_ON_BOTTOM, 
        label = HAUL),
    alpha = 0.8
  ) + 
  scale_linetype(name = "Scope table/gear") +
  scale_color_manual(name = "Footrope/doors on bottom", values = c("No" = "red", "Yes" = "blue4", "Inconclusive" = "grey50")) +
  scale_x_log10(
    name = "Bottom depth (m)", 
    limits = c(depth_range[1]-2, depth_range[2]), 
    breaks = depth_breaks,
    oob = scales::oob_squish_infinite,
    expand = c(0,0)) +
  scale_y_continuous(
    name = "Scope/Depth", 
    oob = scales::oob_squish_infinite,
    breaks = sdr_axis_breaks,
    limits = c(2, max_sdr)) +
  scale_shape_manual(name = "Codend", values = codend_symbols) +
  facet_grid(BRIDLE_CHAIN_WEIGHT~DOOR_LEG_SETBACK_IN) +
  theme_bw()

ggplot() +
  geom_path(
    data = scope_tables,
    mapping = aes(x = mean_depth_m, y = scope_to_depth, linetype = gear),
    color = "grey",
    linewidth = 1.02
  ) +
  geom_point(
    data = dplyr::filter(gt_data, !is.na(MAX_DOOR_ROLL)), 
    mapping = 
      aes(
        x = BOTTOM_DEPTH, 
        y = WIRE_LENGTH/BOTTOM_DEPTH, 
        color =  DOORS_GOOD, 
        shape = CODEND),
    alpha = 0.8
  ) + 
  geom_text_repel(
    data = dplyr::filter(gt_data, !is.na(MAX_DOOR_ROLL)), 
    mapping = 
      aes(
        x = BOTTOM_DEPTH, 
        y = WIRE_LENGTH/BOTTOM_DEPTH, 
        color = DOORS_GOOD, 
        label = HAUL),
    alpha = 0.8,
    size = 2.7
  ) + 
  scale_linetype(name = "Scope table/gear") +
  scale_color_manual(name = "Door roll <30\u00B0", values = c("No" = "red", "Yes" = "blue4", "Inconclusive" = "grey50")) +
  scale_x_log10(
    name = "Bottom depth (m)", 
    limits = c(depth_range[1]-2, depth_range[2]+20), 
    breaks = depth_breaks,
    oob = scales::oob_squish_infinite,
    expand = c(0,0)) +
  scale_y_continuous(
    name = "Scope/Depth", 
    oob = scales::oob_squish_infinite,
    breaks = sdr_axis_breaks,
    limits = c(2, max_sdr)) +
  scale_shape_manual(name = "Codend", values = codend_symbols) +
  facet_grid(BRIDLE_CHAIN_WEIGHT~DOOR_LEG_SETBACK_IN) +
  theme_bw()


ggplot() +
  geom_path(
    data = scope_tables,
    mapping = aes(x = mean_depth_m, y = scope_to_depth, linetype = gear),
    color = "grey",
    linewidth = 1.2
  ) +
  geom_point(
    data = dplyr::filter(gt_data, !is.na(MAX_DOOR_ROLL)), 
    mapping = 
      aes(
        x = BOTTOM_DEPTH, 
        y = WIRE_LENGTH/BOTTOM_DEPTH, 
        color = DOORS_GEAR_GOOD, 
        shape = CODEND),
    alpha = 0.8
  ) + 
  scale_linetype(name = "Scope table/gear") +
  scale_color_manual(name = "Footrope/doors on bottom\n& door roll<30", values = c("No" = "red", "Yes" = "blue4", "Inconclusive" = "grey50")) +
  scale_x_log10(
    name = "Bottom depth (m)", 
    limits = c(depth_range[1]-2, depth_range[2]), 
    breaks = depth_breaks,
    oob = scales::oob_squish_infinite,
    expand = c(0,0)) +
  scale_y_continuous(
    name = "Scope/Depth", 
    oob = scales::oob_squish_infinite,
    breaks = sdr_axis_breaks,
    limits = c(2, max_sdr)) +
  scale_shape_manual(name = "Codend", values = codend_symbols) +
  facet_grid(DOORS_GOOD ~ GEAR_ON_BOTTOM) +
  theme_bw()

ggplot() +
  geom_path(
    data = scope_tables,
    mapping = aes(x = mean_depth_m, y = scope_to_depth, linetype = gear),
    color = "grey",
    linewidth = 1.02
  ) +
  geom_point(
    data = dplyr::filter(gt_data, !is.na(MAX_DOOR_ROLL)), 
    mapping = 
      aes(
        x = BOTTOM_DEPTH, 
        y = WIRE_LENGTH/BOTTOM_DEPTH, 
        color = factor(DOOR_LEG_SETBACK_IN), 
        shape = CODEND),
    alpha = 0.8
  ) + 
  scale_linetype(name = "Scope table/gear") +
  scale_color_tableau(name = "Door leg setback (in)") +
  scale_x_log10(
    name = "Bottom depth (m)", 
    limits = c(depth_range[1]-2, depth_range[2]), 
    breaks = depth_breaks,
    oob = scales::oob_squish_infinite,
    expand = c(0,0)) +
  scale_y_continuous(
    name = "Scope/Depth", 
    oob = scales::oob_squish_infinite,
    breaks = sdr_axis_breaks,
    limits = c(2, max_sdr)) +
  scale_shape_manual(name = "Codend", values = codend_symbols) +
  facet_grid(paste0("Door roll < 30? ", DOORS_GOOD) ~ paste0("Gear on bottom? ", GEAR_ON_BOTTOM)) +
  theme_bw()




ggplot() +
  geom_point(
    data = dplyr::filter(gt_subset, GEAR_ON_BOTTOM == TRUE, DOOR_SIZE_M2 == 4.5),
    mapping = aes(
      x = BOTTOM_DEPTH, 
      y = DOOR_SPREAD, 
      shape = CODEND, 
      color = factor(BRIDLE_CHAIN_WEIGHT)
    ),
    alpha = 0.7,
    size = 2.2
  ) +
  scale_color_colorblind(name = "Chain weight (kg)") +
  scale_shape_manual(name = "Codend", values = codend_symbols) +
  scale_x_continuous(name = "Depth (m)") +
  scale_y_continuous(name = "Door spread (m)", oob = scales::squish) +
  facet_wrap(~SDR_FAC) +
  theme_bw()

ggplot() +
  geom_hline(yintercept = c(15, 20), linetype = 2) +
  geom_point(
    data = dplyr::filter(gt_subset, GEAR_ON_BOTTOM == TRUE, DOOR_SIZE_M2 == 4.5),
    mapping = aes(
      x = BOTTOM_DEPTH, 
      y = NET_WIDTH, 
      shape = CODEND, 
      color = factor(BRIDLE_CHAIN_WEIGHT)
    ),
    alpha = 0.7,
    size = 2.2
  ) +
  scale_color_colorblind(name = "Chain weight (kg)") +
  scale_shape_manual(name = "Codend", values = codend_symbols) +
  scale_x_continuous(name = "Depth (m)") +
  scale_y_continuous(name = "Upper wing spread (m)", oob = scales::squish) +
  facet_wrap(~SDR_FAC) +
  theme_bw()

ggplot() +
  geom_hline(yintercept = c(18, 21), linetype = 2) +
  geom_point(
    data = dplyr::filter(gt_subset, GEAR_ON_BOTTOM == TRUE, DOOR_SIZE_M2 == 4.5),
    mapping = aes(
      x = BOTTOM_DEPTH, 
      y = BRIDLE_ANGLE_DEG, 
      shape = CODEND, 
      color = factor(BRIDLE_CHAIN_WEIGHT)
    ),
    alpha = 0.7,
    size = 2.2
  ) +
  scale_color_colorblind(name = "Chain weight (kg)") +
  scale_shape_manual(name = "Codend", values = codend_symbols) +
  scale_x_continuous(name = "Depth (m)") +
  scale_y_continuous(name = "Bridle angle of attack (\u00B0)") +
  facet_wrap(~SDR_FAC) +
  theme_bw()


ggplot() +
  geom_hline(yintercept = c(5, 6), linetype = 2) +
  geom_point(
    data = dplyr::filter(gt_subset, GEAR_ON_BOTTOM == TRUE, DOOR_SIZE_M2 == 4.5),
    mapping = aes(
      x = BOTTOM_DEPTH, 
      y = NET_HEIGHT, 
      shape = CODEND, 
      color = factor(BRIDLE_CHAIN_WEIGHT)
    ),
    alpha = 0.7,
    size = 2.2
  ) +
  scale_color_colorblind(name = "Chain weight (kg)") +
  scale_shape_manual(name = "Codend", values = codend_symbols) +
  scale_x_continuous(name = "Depth (m)") +
  scale_y_continuous(name = "Opening height (m)") +
  facet_wrap(~SDR_FAC) +
  theme_bw()


p_doors_vs_bridles <- 
  ggplot() +
  geom_smooth(
    data = dplyr::filter(gt_subset, GEAR_ON_BOTTOM == "Yes", DOOR_SIZE_M2 == 4.5),
    mapping = aes(
      x = DOOR_SPREAD,  
      y = BRIDLE_ANGLE_DEG
    ),
    method = 'gam',
    color = "blue3"
  ) +
  geom_point(
    data = dplyr::filter(gt_subset, GEAR_ON_BOTTOM == "Yes", DOOR_SIZE_M2 == 4.5),
    mapping = aes(
      x = DOOR_SPREAD, 
      y = BRIDLE_ANGLE_DEG, 
      shape = CODEND, 
      color = factor(BRIDLE_CHAIN_WEIGHT)
    ),
    alpha = 0.7,
    size = 2.2
  ) +
  scale_color_colorblind(name = "Chain weight (kg)") +
  scale_shape_manual(name = "Codend", values = codend_symbols) +
  scale_x_continuous(name = "Door spread (m)") +
  scale_y_continuous(name = "Bridle angle of attack (\u00B0)") +
  theme_bw()

p_wings_vs_bridles <- ggplot() +
  geom_smooth(
    data = dplyr::filter(gt_subset, GEAR_ON_BOTTOM == "Yes", DOOR_SIZE_M2 == 4.5),
    mapping = aes(
      x = NET_WIDTH, 
      y = BRIDLE_ANGLE_DEG
    ),
    method = 'gam',
    color = "blue3"
  ) +
  geom_point(
    data = dplyr::filter(gt_subset, GEAR_ON_BOTTOM == "Yes", DOOR_SIZE_M2 == 4.5),
    mapping = aes(
      x = NET_WIDTH, 
      y = BRIDLE_ANGLE_DEG, 
      shape = CODEND, 
      color = factor(BRIDLE_CHAIN_WEIGHT)
    ),
    alpha = 0.7,
    size = 2.2
  ) +
  scale_color_colorblind(name = "Chain weight (kg)") +
  scale_shape_manual(name = "Codend", values = codend_symbols) +
  scale_x_continuous(name = "Upper wing spread (m)") +
  scale_y_continuous(name = "Bridle angle of attack (\u00B0)") +
  theme_bw()


cowplot::plot_grid(
  p_doors_vs_bridles + 
    theme(legend.position = "inside",
          legend.position.inside = c(0.15, 0.8)),
  p_wings_vs_bridles +
    theme(legend.position = "none"),
  align = "h"
)


ggplot() +
  geom_smooth(
    data = dplyr::filter(gt_subset, GEAR_ON_BOTTOM == "Yes", DOOR_SIZE_M2 == 4.5),
    mapping = aes(
      x = BOTTOM_DEPTH, 
      y = BRIDLE_ANGLE_DEG
    ),
    method = 'gam',
    color = "blue3"
  ) +
  geom_point(
    data = dplyr::filter(gt_subset, GEAR_ON_BOTTOM == "Yes", DOOR_SIZE_M2 == 4.5),
    mapping = aes(
      x = BOTTOM_DEPTH, 
      y = BRIDLE_ANGLE_DEG, 
      shape = CODEND, 
      color = factor(BRIDLE_CHAIN_WEIGHT)
    ),
    alpha = 0.7,
    size = 2.2
  ) +
  scale_color_colorblind(name = "Chain weight (kg)") +
  scale_shape_manual(name = "Codend", values = codend_symbols) +
  scale_x_continuous(name = "Bottom depth (m)") +
  scale_y_continuous(name = "Bridle angle of attack (\u00B0)") +
  theme_bw()

ggplot() +
  geom_smooth(
    data = dplyr::filter(gt_subset, GEAR_ON_BOTTOM == "Yes", DOOR_SIZE_M2 == 4.5),
    mapping = aes(
      x = BOTTOM_DEPTH, 
      y = DOOR_SPREAD
    ),
    method = 'gam',
    color = "blue3"
  ) +
  geom_point(
    data = dplyr::filter(gt_subset, GEAR_ON_BOTTOM == "Yes", DOOR_SIZE_M2 == 4.5),
    mapping = aes(
      x = BOTTOM_DEPTH, 
      y = DOOR_SPREAD,
      shape = CODEND, 
      color = factor(BRIDLE_CHAIN_WEIGHT)
    ),
    alpha = 0.7,
    size = 2.2
  ) +
  scale_color_colorblind(name = "Chain weight (kg)") +
  scale_shape_manual(name = "Codend", values = codend_symbols) +
  scale_x_continuous(name = "Bottom depth (m)") +
  scale_y_continuous(name = "Door spread (m)") +
  theme_bw()
