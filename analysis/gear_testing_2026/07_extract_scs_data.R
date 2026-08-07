# Extract door spread from SCS xml files
library(xml2)
library(ggthemes)
library(dplyr)
library(ggplot2)
library(glmmTMB)
library(ggrepel)

scs_zip <- list.files(here::here("data", "04_scs_data"), full.names = TRUE, pattern = ".zip")

vapply(scs_zip, extract_and_rename_xml, FUN.VALUE = character(1))

# Parse xml files to retrieve net height, net spread, and door spread

library(xml2)

scope_tables <- 
  read_xlsx(path = here::here("data", "shelf_slope_table.xlsx")) |>
  dplyr::mutate(mean_depth_fm = (min_depth_fm+max_depth_fm)/2,
                scope_to_depth = wire_out_fm/mean_depth_fm) |>
  dplyr::inner_join(data.frame(table = c("GOA/AI", "EBS shelf", "EBS slope"), gear = c("PNE", "83-112", "PNE-S")))

scs_xml <- list.files(here::here("data", "04_scs_data"), full.names = TRUE, pattern = ".xml")

# Load gear configuration data

gear_config <- readxl::read_xlsx(path = here::here("data", "2026_gear_testing_gear_log.xlsx"))

trawl_measurements <- 
  lapply(X = scs_xml, FUN = parse_nmea_xml) |>
  do.call(what = dplyr::bind_rows) |> 
  isolate_treatments() |>
  dplyr::filter(!is.na(scope)) |>
  dplyr::select(haul, dt, NET_HEIGHT_M, NET_SPREAD_M, DOOR_SPREAD_M, pass, scope) |>
  dplyr::mutate(BRIDLE_ANGLE_DEG = bridle_angle_wes(
    door_spread = DOOR_SPREAD_M, 
    wing_spread = NET_SPREAD_M,
    bridle_length = (180+30)/3.281
    ))

trawl_measurement_summary <- 
  trawl_measurements |>
  dplyr::group_by(
    haul, scope
  ) |>
  dplyr::summarise(
    MEAN_NET_SPREAD = mean(NET_SPREAD_M, na.rm = TRUE),
    SD_NET_SPREAD = sd(NET_SPREAD_M, na.rm = TRUE),
    MEAN_NET_HEIGHT = mean(NET_HEIGHT_M, na.rm = TRUE),
    SD_NET_HEIGHT = sd(NET_HEIGHT_M, na.rm = TRUE),
    MEAN_DOOR_SPREAD = mean(DOOR_SPREAD_M, na.rm = TRUE),
    SD_DOOR_SPREAD = sd(DOOR_SPREAD_M, na.rm = TRUE),
    MEAN_BRIDLE_ANGLE = mean(BRIDLE_ANGLE_DEG, na.rm = TRUE),
    SD_BRIDLE_ANGLE = mean(BRIDLE_ANGLE_DEG, na.rm = TRUE),
    MIN_DT = min(dt),
    MAX_DT = max(dt),
    MEAN_DT = mean(dt)
  ) |>
  dplyr::mutate(
    CV_NET_SPREAD = SD_NET_SPREAD/MEAN_NET_SPREAD,
    CV_NET_HEIGHT = SD_NET_HEIGHT/MEAN_NET_HEIGHT,
    CV_DOOR_SPREAD = SD_DOOR_SPREAD/MEAN_DOOR_SPREAD
  )

# Parse BT and calculate averages for each treatment

btd_path <- list.files(here::here("data", "05_btd_data"), pattern = ".BTD", full.names = TRUE)

btd_data <- lapply(X = btd_path, FUN = read.csv) |>
  do.call(what = dplyr::bind_rows) |>
  dplyr::mutate(dt = as.POSIXct(DATE_TIME, tz = "America/Anchorage", format = "%m/%d/%Y %H:%M:%S")) |>
  dplyr::select(dt, HAUL, DEPTH)

names(btd_data) <- tolower(names(btd_data))

btd_summary <- isolate_treatments(btd_data) |>
  dplyr::filter(!is.na(scope)) |>
  dplyr::group_by(haul, pass, scope) |>
  dplyr::summarise(
    BT_DEPTH_M = mean(depth, na.rm = TRUE),
    BT_DEPTH_FM = BT_DEPTH_M/1.8288
  ) |>
  dplyr::inner_join(
    trawl_measurement_summary |>
      dplyr::select(haul, scope, MEAN_NET_HEIGHT)) |>
  dplyr::mutate(
    BOTTOM_DEPTH_FM = BT_DEPTH_FM + MEAN_NET_HEIGHT/1.8288,
    SCOPE_TO_DEPTH = scope/BOTTOM_DEPTH_FM 
  )

unique_scs_hauls <- unique(trawl_measurements$haul)

for(ii in 1:length(unique_scs_hauls)) {
  
  haul_pings <- trawl_measurements |>
    dplyr::filter(haul == unique_scs_hauls[ii])
  
  min_time <- min(trawl_measurements$dt)
  max_time <- max(trawl_measurements$dt)
  
  haul_summary <- trawl_measurement_summary |>
    dplyr::filter(haul == unique_scs_hauls[ii])
  
  btd_haul <- btd_summary |>
    dplyr::filter(haul == unique_scs_hauls[ii])
  
  p_net_spread <- 
    ggplot() +
    geom_point(
      data = haul_pings,
      mapping = aes(x = dt, y = NET_SPREAD_M, color = factor(scope)), 
      size = 0.5
    ) +
    geom_segment(
      data = haul_summary,
      mapping = aes(x = MIN_DT, xend = MAX_DT, y = MEAN_NET_SPREAD, color = factor(scope)),
      linewidth = 0.8,
      linetype = 1
    ) +
    geom_text(
      data = haul_summary,
      mapping = aes(x = MEAN_DT, y = 11, 
                    label = MEAN_NET_SPREAD, nsmall = 1, digits = 3)
    ) +
    scale_color_viridis_d(name = "Scope (fm)", direction = -1) +
    scale_x_datetime(name = "Date/time (AKDT)") +
    scale_y_continuous(name = "Net Spread (m)", limits = c(10, 23), breaks = seq(10,23,2)) +
    theme_bw()
  
  p_door_spread <- 
    ggplot() +
    geom_point(
      data = haul_pings,
      mapping = aes(x = dt, y = DOOR_SPREAD_M, color = factor(scope)),
      size = 0.5
    ) +
    geom_segment(
      data = haul_summary,
      mapping = aes(x = MIN_DT, xend = MAX_DT, y = MEAN_DOOR_SPREAD, color = factor(scope)),
      linewidth = 0.8,
      linetype = 1
    ) +
    geom_text(
      data = haul_summary,
      mapping = aes(x = MEAN_DT, y = 23, 
                    label = paste0(format(MEAN_DOOR_SPREAD, nsmall = 1, digits = 3)))
    ) +
    scale_color_viridis_d(name = "Scope (fm)", direction = -1) +
    scale_x_datetime(name = "Date/time (AKDT)") +
    scale_y_continuous(name = "Door Spread (m)", limits = c(20, 62), breaks = seq(20,60,10)) +
    theme_bw()
  
  p_net_height <- 
    ggplot() +
    geom_point(
      data = haul_pings,
      mapping = aes(x = dt, y = NET_HEIGHT_M, color = factor(scope)),
      size = 0.5
    ) +
    geom_segment(
      data = haul_summary,
      mapping = aes(x = MIN_DT, xend = MAX_DT, y = MEAN_NET_HEIGHT, color = factor(scope)),
      linewidth = 0.8,
      linetype = 3
    ) +
    geom_text(
      data = haul_summary,
      mapping = aes(x = MEAN_DT, y = 3.5, 
                    label = format(MEAN_NET_HEIGHT, nsmall = 1, digits = 1))
    ) +
    ggtitle(paste0("Net/Door/Scope/Depth, Haul: ", haul_pings$haul[1])) +
    scale_color_viridis_d(name = "Scope (fm)", direction = -1) +
    scale_x_datetime(name = "Date/time (AKDT)") +
    scale_y_continuous(name = "Net Height (m)", limits = c(3, 10.5), breaks = seq(3,10,1)) +
    theme_bw()
  
  p_bridle_angle <-
    ggplot() +
    geom_point(
      data = haul_pings,
      mapping = aes(x = dt, y = BRIDLE_ANGLE_DEG, color = factor(scope)),
      size = 0.5
    ) +
    geom_segment(
      data = haul_summary,
      mapping = aes(x = MIN_DT, xend = MAX_DT, y = MEAN_BRIDLE_ANGLE, color = factor(scope)),
      linewidth = 0.8,
      linetype = 3
    ) +
    geom_text(
      data = haul_summary,
      mapping = aes(x = MEAN_DT, y = 10, 
                    label = format(MEAN_BRIDLE_ANGLE, nsmall = 1, digits = 2))
    ) +
    scale_color_viridis_d(name = "Scope (fm)", direction = -1) +
    scale_x_datetime(name = "Date/time (AKDT)") +
    scale_y_continuous(name = "Bridle Angle (deg)", limits = c(9, 24), breaks = seq(10,24,2)) +
    theme_bw()
  
  p_sdr <- ggplot() +
    geom_line(
      data = scope_tables,
      mapping = aes(x = mean_depth_fm, y = scope_to_depth, linetype = gear),
      color = "grey"
      ) +
    geom_point(
      data = btd_haul, 
      mapping = aes(x = BT_DEPTH_FM, y = SCOPE_TO_DEPTH, color = factor(scope)),
      size = rel(2.2)) +
    geom_text_repel(
      data = btd_haul,
      mapping = aes(x = BT_DEPTH_FM, y = SCOPE_TO_DEPTH, label = format(SCOPE_TO_DEPTH, digits = 2, nsmall = 1))
    ) +
    scale_color_viridis_d(name = "Scope (ftm)", direction = -1, guide = "none") +
    scale_x_continuous(name = "Bottom depth (fathoms)", limits = c(0, 300/1.8288), oob = scales::oob_keep) +
    scale_y_continuous(name = "Scope/depth", limits = c(0, 9), breaks = seq(1,9,1)) +
    scale_linetype(name = "Survey") +
    theme_bw()
  
  p_net_door <- 
    cowplot::plot_grid(
      p_net_height + 
        theme(
          legend.position = "none", 
          axis.title.x = element_blank(),
          plot.margin = unit(c(5,5,0,5), units = "mm")),
      p_door_spread + 
        theme(
          legend.position = "none", 
          axis.title.x = element_blank(),
          plot.margin = unit(c(0,5,0,5), units = "mm")),
      p_net_spread +
        theme(
          legend.position = "right", 
          plot.margin = unit(c(0,5,0,5), units = "mm")
        ),
      p_bridle_angle +
        theme(
          legend.position = "none", 
          plot.margin = unit(c(0,5,0,5), units = "mm")
        ),
      p_sdr  +
        theme(
          legend.position = "inside", 
          legend.position.inside = c(0.7, 0.8),
          legend.direction = "horizontal",
          legend.title = element_blank(),
          plot.margin = unit(c(0,5,0,5), units = "mm")
        ),
      rel_heights = c(1,1,1,1.2),
      nrow = 5,
      align = "v"
    )
  
  png(filename = here::here("plots", "nm_dist", paste0(haul_pings$haul[1], "_net_door.png")), width = 6, height = 8, units = "in", res = 300)
  print(p_net_door)
  dev.off()
  
}

treatment_net_results <-
  dplyr::left_join(trawl_measurement_summary, btd_summary, by = c("haul", "scope")) |>
  dplyr::mutate(BOTTOM_DEPTH_M = MEAN_NET_HEIGHT + BT_DEPTH_M,
                SCOPE_RATIO = scope/BOTTOM_DEPTH_M)

# No difference in spread varaibility between doors?
# model_door_sd0 <- 
#   glmmTMB(formula = SD_DOOR_SPREAD ~ 1 + (1|depth_treatment) + (1|haul),
#           data = treatment_net_results)
# 
# model_door_sd1 <- 
#   glmmTMB(formula = SD_DOOR_SPREAD ~ factor(door_size_m2) + (1|depth_treatment) + (1|haul),
#         data = treatment_net_results)
# 
# summary(model_door_sd1)
# 
# AIC(model_door_sd0, model_door_sd1)
