# Compare flume tank with real

library(trawlmetrics)
library(xlsx)
library(ggthemes)
library(ggrepel)

# ggthemes::colorblind_pal()(8)
# "#000000" "#E69F00" "#56B4E9" "#009E73" "#F0E442" "#0072B2" "#D55E00" "#CC79A7"

# Load flume tank data
sheet_flume_xlsx <- "data"

path_flume_xlsx <- here::here("analysis", "flume_tank", "data", "flume_tank_data.xlsx")

flume_only_data <- xlsx::read.xlsx(file = path_flume_xlsx,
                                   sheetName = sheet_flume_xlsx) |>
  dplyr::filter(!is.na(bridles), 
                pulling_point_elevation_mm < 300,
                additional_floatation_kg == 0, 
                is.na(bcs_unit),
                trial > 0,
                catch == "empty",
                bridles != "2-weighted") |>
  dplyr::mutate(type = "Flume tank",
                fac_trial = factor(trial),
                type = paste0("Flume ", trawl, ", ", bridles))


ggplot(data = flume_only_data,
       mapping = aes(x = spread_u_wing_m,
                     y = opening_headline_m,
                     color = factor(towing_speed_kn),
                     shape = footrope)) +
  geom_point() +
  geom_smooth(method = 'lm')


ggplot(
  data = flume_only_data,
  mapping = aes(x = towing_speed_kn,
                y = opening_headline_m,
                color = factor(spread_treatment),
                shape = trawl)
  ) +
  geom_point() +
  geom_smooth(method = 'lm') +
  scale_color_colorblind(name = "Spread trt. (m)") +
  facet_wrap(~trawl) +
  theme_bw()



ggplot() +
  geom_point(data = dplyr::filter(flume_only_data, 
                                  trawl %in% c("RACE", "PNE"),
                                  !(footrope %in% c("GOA/AI_v2", "GOA/AI_v3")),
                                  towing_speed_kn == 4, spread_treatment >= 16),
             mapping = aes(x = spread_u_wing_m,
                           y = opening_headline_m,
                           color = footrope),
             size = 4) +
  geom_text_repel(data = dplyr::filter(flume_only_data, 
                                       trawl %in% c("RACE", "PNE"),
                                       !(footrope %in% c("GOA/AI_v2", "GOA/AI_v3")),
                                       towing_speed_kn == 4, spread_treatment >= 16),
                  mapping = aes(x = spread_u_wing_m,
                                y = opening_headline_m,
                                color = footrope,
                                label = trial),
                  size = 4) +
  scale_x_continuous(name = "Upper wing spread (m)") +
  scale_y_continuous(name = "Opening headline height (m)") +
  scale_color_colorblind() +
  theme_bw()



# Speed x spread versus height
ggplot(data = dplyr::filter(flume_only_data, 
                            footrope %in% c("EBS+65", "GOA/AI_v5"),
                            !is.na(opening_headline_m)),
       mapping = aes(x = towing_speed_kn,
                     y = opening_headline_m,
                     label = trial,
                     color = factor(spread_treatment),
                     linetype = footrope,
                     shape = footrope),
       size = 4) +
  geom_smooth(method = 'lm') +
  geom_point(size = 3) +
  geom_text_repel() +
  scale_x_continuous(name = "Towing speed (kn)") +
  scale_y_continuous(name = "Opening headline height (m)") +
  scale_color_viridis_d(name = "Spread (m)", option = "C") +
  scale_shape(name = "Trawl") +
  scale_linetype(name = "Trawl") +
  theme_bw()


