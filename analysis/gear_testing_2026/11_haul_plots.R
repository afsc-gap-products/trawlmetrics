# Review scope table hauls

library(trawlmetrics)
library(ggthemes)
library(shadowtext)
library(dplyr)
library(ggpp)
library(scales)

# Load data
trawl_data_by_haul <- readRDS(here::here("data", "trawl_data_by_haul.rds"))

bcs_data_by_haul <- readRDS(here::here("data", "bcs_data_by_haul.rds"))

bcs_pings <- 
  readRDS(here::here("data", "bcs_pings.rds"))

marport_pings <- readRDS(here::here("data", "marport_pings.rds"))

gt_bcs_data <- 
  dplyr::inner_join(
    trawl_data_by_haul,
    bcs_data_by_haul
  )

# Plot settings
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

bcs_hline <- 
  dplyr::filter(
    bcs_position_labels, DISTANCE < 16
  ) |>
  dplyr::mutate(
    min = 0,
    max = 2.54*3
  )

# Make plots

dir.create(here::here("plots", "geometry_by_haul"), recursive = TRUE)

unique_hauls <- sort(unique(trawl_data_by_haul$HAUL))

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
    dplyr::select(HAUL, DISTANCE, DISTANCE_FAC, DISTANCE_DETAIL, GEAR_ON_BOTTOM) |>
    unique()
  
  bcs_mean <-
    sel_bcs  |>
    dplyr::group_by(HAUL, DISTANCE, DISTANCE_FAC, DISTANCE_DETAIL, WIRE_LENGTH_FM, GEAR_ON_BOTTOM) |>
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
    trawl_data_by_haul |>
    dplyr::filter(HAUL == sel_haul)
  
  sel_door_depth <- 
    sel_gt_data |>
    dplyr::select(HAUL, WIRE_LENGTH_FM, GEAR_DEPTH, DOOR_DEPTH_P, DOOR_DEPTH_S, GEAR_ON_BOTTOM) |>
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
    dplyr::select(HAUL, WIRE_LENGTH_FM, DOOR_ROLL_P, DOOR_ROLL_S, GEAR_ON_BOTTOM) |>
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
    dplyr::select(HAUL, WIRE_LENGTH_FM, NET_WIDTH, NET_HEIGHT, DOOR_SPREAD, BRIDLE_ANGLE_DEG, GEAR_ON_BOTTOM) |>
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
  
  sel_bcs_hline <- 
    dplyr::filter(
      bcs_hline,
      DISTANCE %in% unique(sel_bcs$DISTANCE)
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
    dplyr::select(HAUL, DT, WIRE_LENGTH_FM, name, value, label, GEAR_ON_BOTTOM) |>
    dplyr::arrange(DT)
  
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
      mapping = aes(
        x = MEAN_DT, 
        y = value, 
        color = factor(WIRE_LENGTH_FM), 
        shape = text),
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
    scale_x_datetime(name = "Time") +
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
      data = sel_bcs_hline,
      mapping = aes(yintercept = min),
      linetype = 2,
      color = "grey30",
      linewidth = 0.3
    ) +
    geom_hline(
      data = sel_bcs_hline,
      mapping = aes(yintercept = max),
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
    scale_x_datetime(name = "Time") +
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

# Render to docx
rmarkdown::render(here::here("appendix_haul_plots.Rmd"))
