library(trawlmetrics)

bridle_data_2001 <- 
  readxl::read_xlsx(path = here::here("data", "somerton_munro_2001_bridles.xlsx")) |>
  dplyr::mutate(
    BRIDLE_ANGLE_DEG = 
      trawlmetrics::calc_bridle_angle(
        door_spread_m = DOOR_SPREAD,
        wing_spread_m = NET_WIDTH,
        total_bridle_length_m = BRIDLE_LENGTH_M + TAILCHAIN_LENGTH_M
      )
  )

bridle_treatments <- 
  bridle_data_2001 |>
  dplyr::select(
    VESSEL, CRUISE, HAUL, 
    WIRE_OUT_FM, TRIPLET, BRIDLES, 
    STRATUM, BRIDLE_LENGTH_M, TAILCHAIN_LENGTH_M) |>
  unique() |>
  dplyr::inner_join(
    bridle_data_2001 |>
      dplyr::filter(NET_HEIGHT > 0.7 & NET_HEIGHT < 6) |>
      dplyr::group_by(
        HAUL
      ) |>
      dplyr::summarise(
        NET_HEIGHT = mean(NET_HEIGHT)
      )
  ) |>
  dplyr::inner_join(
    bridle_data_2001 |>
      dplyr::filter(NET_WIDTH > 10 & NET_WIDTH < 23) |>
      dplyr::group_by(
        HAUL
      ) |>
      dplyr::summarise(
        NET_WIDTH = mean(NET_WIDTH)
      )
  ) |>
  dplyr::inner_join(
    bridle_data_2001 |>
      dplyr::filter(DOOR_SPREAD > 20 & DOOR_SPREAD < 100) |>
      dplyr::group_by(
        HAUL
      ) |>
      dplyr::summarise(
        DOOR_SPREAD = mean(DOOR_SPREAD)
      )
  ) |>
  dplyr::inner_join(
    bridle_data_2001 |>
      dplyr::filter(DOOR_SPREAD > 20 & DOOR_SPREAD < 100 & NET_WIDTH > 10 & NET_WIDTH < 23) |>
      dplyr::group_by(
        HAUL
      ) |>
      dplyr::summarise(
        BRIDLE_ANGLE_DEG = mean(BRIDLE_ANGLE_DEG)
      )
  ) |>
  dplyr::inner_join(
    bridle_data_2001 |>
      dplyr::filter(NET_HEIGHT > 0.7 & NET_HEIGHT < 6) |>
      dplyr::group_by(
        HAUL
      ) |>
      dplyr::summarise(
        BOTTOM_DEPTH_M = mean(BOTTOM_DEPTH_M, na.rm = TRUE)
      )
  )


bridle_treatments_standard <- 
  dplyr::filter(bridle_treatments,
                BRIDLE_LENGTH_M == 54.6)


ggplot() +
  geom_histogram(
    data = bridle_treatments,
    mapping = aes(x = BRIDLE_ANGLE_DEG)
  )

ggplot() +
  geom_histogram(
    data = bridle_treatments,
    mapping = aes(x = BRIDLE_ANGLE_DEG)
  ) +
  facet_wrap(~BRIDLE_LENGTH_M)

ggplot() +
  geom_histogram(
    data = bridle_treatments,
    mapping = aes(x = BOTTOM_DEPTH_M)
  ) +
  facet_wrap(~BRIDLE_LENGTH_M)

ggplot() +
  geom_histogram(
    data = bridle_treatments,
    mapping = aes(x = DOOR_SPREAD)
  ) +
  facet_wrap(~BRIDLE_LENGTH_M)

ggplot() +
  geom_histogram(
    data = bridle_treatments,
    mapping = aes(x = NET_WIDTH)
  ) +
  facet_wrap(~BRIDLE_LENGTH_M)

bridle_treatments_long <- 
  bridle_treatments_standard |>
  dplyr::select(BOTTOM_DEPTH_M, BRIDLE_ANGLE_DEG, NET_WIDTH, DOOR_SPREAD) |>
  tidyr::pivot_longer(
    cols = c(BRIDLE_ANGLE_DEG, NET_WIDTH, DOOR_SPREAD)
  ) |>
  dplyr::inner_join(
    data.frame(
      name = c("BRIDLE_ANGLE_DEG", "NET_WIDTH", "DOOR_SPREAD"),
      label = c("Bridle angle of attack (\u00B0)", "Upper wing spread (m)", "Door spread (m)")
    )
  )

ggplot(
  data = bridle_treatments_long,
           mapping = aes(
             x = BOTTOM_DEPTH_M, 
             y = value
           )) +
  geom_point() +
  geom_smooth(method = 'lm') +
  ggtitle(label = "EBS data from Somerton and Munro (2001)") +
  facet_wrap(~label, scales = "free_y", nrow = 4) +
  scale_y_continuous(name = "Value") +
  scale_x_continuous(name = "Bottom Depth (m)") + 
  theme_bw()


ggplot(
  data = trawlmetrics::bts_geom |>
    dplyr::filter(NET_MEASURED == TRUE, GEAR_NAME == "83-112"),
  mapping = aes(
    x = DEPTH_M, 
    y = NET_WIDTH_M
  )) +
  geom_point(size = 0.2, color = "grey50", alpha = 0.2) +
  geom_smooth(method = 'gam') +
  ggtitle(label = "EBS spread data from measured hauls") +
  # facet_wrap(~label, scales = "free_y", nrow = 4) +
  scale_y_continuous(name = "Upper wing spread (m)", limits = c(12, 23)) +
  scale_x_continuous(name = "Bottom Depth (m)") + 
  theme_bw()


net_spread_wide <- 
  bridle_treatments_standard |>
  dplyr::select(NET_WIDTH, BRIDLE_ANGLE_DEG, DOOR_SPREAD) |>
  tidyr::pivot_longer(
    cols = c(BRIDLE_ANGLE_DEG, DOOR_SPREAD)
  ) |>
  dplyr::inner_join(
    data.frame(
      name = c("BRIDLE_ANGLE_DEG", "NET_WIDTH", "DOOR_SPREAD"),
      label = c("Bridle angle of attack (\u00B0)", "Upper wing spread (m)", "Door spread (m)")
    )
  )

ggplot() +
  geom_point(
    data = net_spread_wide,
    mapping = aes(
      x = NET_WIDTH, y = value
    )
  ) +
  facet_wrap(~label, scales = "free_y") +
  scale_x_continuous(name = "Upper wing spread (m)") +
  scale_y_continuous(name = "Value") +
  theme_bw()


p_eff_area_swept <- 
  ggplot() +
  geom_point(
    data = bridle_treatments_standard,
    mapping = aes(
      x = NET_WIDTH, 
      y = 2*25*sin(BRIDLE_ANGLE_DEG*pi/180) + NET_WIDTH,
      color = "25 m"
      ),
  ) +
  geom_point(
    data = bridle_treatments_standard,
    mapping = aes(
      x = NET_WIDTH, 
      y = 2*32.8*sin(BRIDLE_ANGLE_DEG*pi/180) + NET_WIDTH,
      color = "32.8 m")
  ) +
  scale_x_continuous(name = "Upper wing spread (m)") +
  scale_y_continuous(name = "Effective area swept by bridle + footrope (m)") +
  scale_color_manual("Bridle contact", values = c("red", "black")) +
  theme_bw()

p_eff_area_swept_prop <- 
  ggplot() +
  geom_point(
    data = bridle_treatments_standard,
    mapping = aes(
      x = NET_WIDTH, 
      y = (2*25*sin(BRIDLE_ANGLE_DEG*pi/180) + NET_WIDTH)/NET_WIDTH,
      color = "25 m"
    ),
  ) +
  geom_point(
    data = bridle_treatments_standard,
    mapping = aes(
      x = NET_WIDTH, 
      y = (2*32.8*sin(BRIDLE_ANGLE_DEG*pi/180) + NET_WIDTH)/NET_WIDTH,
      color = "32.8 m")
  ) +
  scale_x_continuous(name = "Upper wing spread (m)") +
  scale_y_continuous(name = expression(over('Effective area swept', 'Upper wing spread'))) +
  scale_color_manual("Bridle contact", values = c("red", "black")) +
  theme_bw()

png(filename = here::here("plots", "design_considerations", "herding_proportion_83112.png"), width = 8, height = 4, units = "in", res = 300)
print(
cowplot::plot_grid(
  p_eff_area_swept + 
    theme(legend.position = "inside",
          legend.position.inside = c(0.8, 0.15),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 12),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 12)),
  p_eff_area_swept_prop + 
    theme(legend.position = "none",
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 12),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 12)),
  nrow = 1
)
)
dev.off()



# Map of spread

library(akgfmaps)
ebs_layers <- 
  akgfmaps::get_base_layers(
    select.region = "ebs",
    set.crs = "EPSG:3338"
  )

spread_by_station <- 
  dplyr::inner_join(ebs_layers$survey.grid,
                    bts_geom |>
                      dplyr::filter(GEAR_NAME == "83-112") |>
                      dplyr::group_by(STATION) |>
                      dplyr::summarise(
                        NET_WIDTH_M = mean(NET_WIDTH_M, na.rm = TRUE)
                      ) )

ggplot() +
  geom_sf(
    data = spread_by_station,
    mapping = aes(fill = NET_WIDTH_M)
  ) +
  geom_sf(data = ebs_layers$akland) +
  scale_fill_viridis_c(
    name = "Mean upper\nwing spread (m)",
    option = "F", 
    direction = -1) +
  scale_x_continuous(
    limits = ebs_layers$plot.boundary$x,
    breaks = ebs_layers$lon.breaks
                     ) +
  scale_y_continuous(
    limits = ebs_layers$plot.boundary$y,
    breaks = ebs_layers$lat.breaks
    ) +
  theme_bw()
