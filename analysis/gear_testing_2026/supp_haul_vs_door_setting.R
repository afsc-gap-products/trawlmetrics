# Review scope table hauls

library(trawlmetrics)
library(akgfmaps)
library(ggthemes)
library(shadowtext)
library(dplyr)
library(ggpp)
library(ggrepel)
library(scales)
library(ggnewscale)

trawl_data_by_haul <- readRDS(here::here("data", "trawl_data_by_haul.rds")) |>
  dplyr::filter(GEAR_ON_BOTTOM == "Yes")


depth_vs_door_setting <- 
  dplyr::group_by(trawl_data_by_haul, HAUL, DOOR_SETTINGS, DOOR_BALE_POSITION, CODEND, BRIDLE_OPTION, BRIDLE_CHAIN_WEIGHT, FLOAT_BUOYANCY_KGF) |>
  dplyr::summarise(
    BOTTOM_DEPTH_FM = mean(BOTTOM_DEPTH_FM, na.rm = TRUE),
    DOOR_ROLL_MAX = mean(DOOR_ROLL_MAX, na.rm = TRUE),
    BOTTOM_DEPTH = mean(BOTTOM_DEPTH, na.rm = TRUE)
  )


ggplot() +
  geom_point(
    data = depth_vs_door_setting,
    mapping = aes(x = HAUL, y = BOTTOM_DEPTH, shape = DOOR_SETTINGS, color = DOOR_BALE_POSITION),
    size = 2.5
  ) +
  scale_x_continuous(name = "Haul") +
  scale_y_continuous(name = "Bottom depth (m)", limits = c(0, NA)) +
  scale_color_colorblind(name = "Door bale position") +
  scale_shape(name = "Towing point") +
  facet_wrap(~factor(CODEND, levels = c("Open", "Closed"), labels = c("Codend open", "Codend closed")), scales = "free_x") +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold"))

ggplot() +
  geom_point(
    data = depth_vs_door_setting,
    mapping = aes(x = HAUL, y = DOOR_ROLL_MAX, shape = DOOR_SETTINGS, color = DOOR_BALE_POSITION),
    size = 2.5
  ) +
  geom_hline(yintercept = c(0, 30), linetype = 2) +
  scale_x_continuous(name = "Haul") +
  scale_y_continuous(name = "Door roll (\u00B0)") +
  scale_color_colorblind(name = "Door bale position") +
  scale_shape(name = "Towing point") +
  facet_wrap(~factor(CODEND, levels = c("Open", "Closed"), labels = c("Codend open", "Codend closed")), scales = "free_x") +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold"))


ggplot() +
  geom_point(
    data = trawl_data_by_haul,
    mapping = aes(x = DOOR_PITCH_MEAN, y = DOOR_ROLL_MAX)
  ) +
  geom_hline(yintercept = c(0, 30), linetype = 2) +
  scale_x_continuous(name = "Door pitch (\u00B0)") +
  scale_y_continuous(name = "Door roll (\u00B0)") +
  theme_bw()
