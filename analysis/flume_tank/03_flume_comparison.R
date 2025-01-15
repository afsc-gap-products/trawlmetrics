# Compare flume tank with real

library(trawlmetrics)
library(xlsx)
library(ggthemes)

# ggthemes::colorblind_pal()(8)
#colors
"#000000" "#E69F00" "#56B4E9" "#009E73" "#F0E442" "#0072B2" "#D55E00" "#CC79A7"

# Load flume tank data
sheet_flume_xlsx <- "data"

path_flume_xlsx <- here::here("analysis", "flume_tank", "data", "flume_tank_data.xlsx")

flume_only_data <- xlsx::read.xlsx(file = path_flume_xlsx,
                                   sheetName = sheet_flume_xlsx) |>
  dplyr::filter(!is.na(bridles), 
                pulling_point_elevation_mm < 300,
                additional_floatation_kg == 0, 
                is.na(bcs_unit),
                treatment > 0,
                bridles != "2-weighted") |>
  dplyr::mutate(type = "Flume tank",
                fac_trial = factor(trial),
                type = paste0("Flume ", trawl, ", ", bridles))


ggplot(data = flume_only_data,
       mapping = aes(x = spread_u_wing_m,
                     y = opening_headline_m,
                     color = factor(towing_speed_kn),
                     shape = trawl)) +
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
