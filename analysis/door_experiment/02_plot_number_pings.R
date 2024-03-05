library(trawlmetrics)
library(mgcv)
library(ggthemes)
library(ggrepel)


load(here::here("analysis", "door_experiment", "data", "spread_height_data.rda"))

# Plot usable spread pings ----
haul_spread_comparison <- usable_width_treatments |>
  dplyr::inner_join(
    dplyr::select(hauls, VESSEL, CRUISE, HAUL, HAUL_ID, BOTTOM_DEPTH, STATIONID)
  ) |> dplyr::rename(`Net spread` = NET_SPREAD,
                     `Net spread standard deviation` = NET_SPREAD_STANDARD_DEVIATION,
                     `Bottom depth` = BOTTOM_DEPTH,
                     `Scope` = WIRE_OUT,
                     STATION = STATIONID) |>
  dplyr::mutate(type = "New doors") |>
  dplyr::bind_rows(standard_hauls |> 
                     dplyr::rename(`Net spread` = NET_SPREAD,
                                   `Net spread standard deviation` = NET_SPREAD_STANDARD_DEVIATION,
                                   `Bottom depth` = BOTTOM_DEPTH,
                                   `Scope` = WIRE_OUT) |>
                     dplyr::mutate(type = "Standard tows")
  ) |>
  tidyr::pivot_longer(cols = c("Net spread", "Net spread standard deviation", "Bottom depth", "Scope")) |>
  dplyr::arrange(type)


plot_usable_spread_pings <- ggplot() +
  geom_point(data = dplyr::filter(usable_spread_pings, HAUL < 15),
             mapping = aes(x = TIME_ELAPSED, y = NET_WIDTH, color = factor(treatment), alpha = USE)) +
  geom_segment(data = dplyr::filter(ping_events, HAUL < 15),
               mapping = aes(x = TIME_ELAPSED,
                             xend = TIME_ELAPSED,
                             y = 9, yend = 25),
               color = "red") +
  scale_alpha_manual(values = c('TRUE' = 1, 'FALSE' = 0.1), guide = "none") +
  scale_color_colorblind(name = "Treatment", na.translate = TRUE, na.value = "grey50") +
  scale_y_continuous(name = "Wing spread (m)") +
  scale_x_continuous(name = "Time elapsed (min)") +
  facet_wrap(~HAUL, scales = "free") +
  theme_bw() +
  theme(legend.position = "bottom")


png(filename = here::here("analysis", "door_experiment", "plots", "usable_83112_spread_pings_2023.png"), 
    width = 240, 
    height = 180, 
    units = "mm", 
    res = 300)
print(plot_usable_spread_pings +
        theme(legend.text = element_text(size = 14),
              legend.title = element_text(size = 14),
              axis.title = element_text(size = 18),
              axis.text = element_text(size = 16),
              strip.text = element_text(size = 14)))
dev.off()


# Plot usable height pings ----
haul_height_comparison <- usable_height_treatments |>
  dplyr::inner_join(
    dplyr::select(hauls, VESSEL, CRUISE, HAUL, HAUL_ID, BOTTOM_DEPTH, STATIONID)
  ) |> dplyr::rename(`Net height` = NET_HEIGHT,
                     `Net height standard deviation` = NET_HEIGHT_STANDARD_DEVIATION,
                     `Bottom depth` = BOTTOM_DEPTH,
                     `Scope` = WIRE_OUT,
                     STATION = STATIONID) |>
  dplyr::mutate(type = "New doors") |>
  dplyr::bind_rows(standard_hauls |> 
                     dplyr::rename(`Net height` = NET_HEIGHT,
                                   `Net height standard deviation` = NET_HEIGHT_STANDARD_DEVIATION,
                                   `Bottom depth` = BOTTOM_DEPTH,
                                   `Scope` = WIRE_OUT) |>
                     dplyr::mutate(type = "Standard tows")
  ) |>
  tidyr::pivot_longer(cols = c("Net height", "Net height standard deviation", "Bottom depth", "Scope")) |>
  dplyr::arrange(type)


plot_usable_height_pings <- ggplot() +
  geom_point(data = dplyr::filter(usable_height_pings, HAUL < 15),
             mapping = aes(x = TIME_ELAPSED, y = NET_HEIGHT, color = factor(treatment), alpha = USE)) +
  geom_segment(data = dplyr::filter(ping_events, HAUL < 15),
               mapping = aes(x = TIME_ELAPSED,
                             xend = TIME_ELAPSED,
                             y = 0, yend = 6),
               color = "red") +
  scale_alpha_manual(values = c('TRUE' = 1, 'FALSE' = 0.1), guide = "none") +
  scale_color_colorblind(name = "Treatment", na.translate = TRUE, na.value = "grey50") +
  scale_y_continuous(name = "Net height (m)") +
  scale_x_continuous(name = "Time elapsed (min)") +
  facet_wrap(~HAUL, scales = "free") +
  theme_bw() +
  theme(legend.position = "bottom")


png(filename = here::here("analysis", "door_experiment", "plots", "usable_83112_height_pings_2023.png"), 
    width = 240, 
    height = 180, 
    units = "mm", 
    res = 300)
print(plot_usable_height_pings +
        theme(legend.text = element_text(size = 14),
              legend.title = element_text(size = 14),
              axis.title = element_text(size = 18),
              axis.text = element_text(size = 16),
              strip.text = element_text(size = 14)))
dev.off()


plot_treatments_2023 <- ggplot() +
  geom_point(data = standard_hauls,
              mapping = aes(x = BOTTOM_DEPTH, y = SCOPE_TO_DEPTH, color = "Standard 83-112"),
              alpha = 0.04) +
  geom_path(data = dplyr::filter(usable_height_treatments, HAUL < 15),
            mapping = aes(x = BOTTOM_DEPTH, y = SCOPE_TO_DEPTH, group = HAUL, color = "4.5-m doors")) +
  geom_point(data = dplyr::filter(usable_height_treatments, HAUL < 15),
             mapping = aes(x = BOTTOM_DEPTH, y = SCOPE_TO_DEPTH, color = "4.5-m doors", fill = factor(treatment)),
             shape = 21, size = rel(3.3)) +
  geom_text_repel(data = dplyr::filter(usable_height_treatments, HAUL < 15) |>
                    dplyr::group_by(BOTTOM_DEPTH, HAUL) |>
                    dplyr::summarise(SCOPE_TO_DEPTH = max(SCOPE_TO_DEPTH)),
                  mapping = aes(x = BOTTOM_DEPTH, y = SCOPE_TO_DEPTH, label = HAUL, color = "4.5-m doors"),
                  size = rel(5.5)) +
  scale_y_continuous(name = "Scope/depth") +
  scale_x_log10(name = "Bottom Depth (m)", breaks = c(25, 50, 100, 150, 200)) +
  scale_color_manual(name = "Haul type", values = c("4.5-m doors" = "red", "Standard 83-112" = "grey50")) +
  scale_fill_colorblind(name = "Treatment") +
  theme_bw() +
  theme(legend.position = c(0.82, 0.80))


png(filename = here::here("analysis", "door_experiment", "plots", "treatments_by_haul2023.png"), 
    width = 180, 
    height = 180, 
    units = "mm", 
    res = 300)
print(plot_treatments_2023 +
        theme(legend.text = element_text(size = 14),
              legend.title = element_text(size = 16),
              axis.title = element_text(size = 18),
              axis.text = element_text(size = 16),
              strip.text = element_text(size = 14)))
dev.off()

