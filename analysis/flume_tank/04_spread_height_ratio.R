library(trawlmetrics)
library(xlsx)
library(ggthemes)
library(ggrepel)
library(tidyr)
library(dplyr)

# EBS v0
# EBS v1 = EBS_v0 + 65 kg
# EBS v2 = 

# ggthemes::colorblind_pal()(8)
# "#000000" "#E69F00" "#56B4E9" "#009E73" "#F0E442" "#0072B2" "#D55E00" "#CC79A7"

# Load flume tank data
sheet_flume_xlsx <- "data"

path_flume_xlsx <- here::here("analysis", "flume_tank", "data", "flume_tank_data.xlsx")

spread_height_ratio_flume <- xlsx::read.xlsx(file = path_flume_xlsx,
                sheetName = sheet_flume_xlsx) |>
  dplyr::filter(!is.na(bridles), 
                pulling_point_elevation_mm < 300,
                additional_floatation_kg == 0, 
                # is.na(bcs_unit),
                trial > 0,
                catch == "empty",
                bridles != "2-weighted") |>
  dplyr::mutate(type = "Flume tank",
                fac_trial = factor(trial),
                type = paste0("Flume ", trawl, ", ", bridles),
                spread_to_height_ratio = spread_u_wing_m/opening_headline_m) |>
  dplyr::inner_join(data.frame(trawl = c("83-112", "PNE", "RACE", "RACE", "RACE", "RACE"),
                               bridles = c("standard", "standard", "2", "2", "2", "2"),
                               footrope = c("83-112", "PNE", "EBS_v1", "GOA/AI_v5", "GOA/AI_v6", "EBS_v2"),
                               catch = "empty",
                               survey = c("EBS", "GOA", "EBS", "GOA", "GOA", "EBS")
  ))


pne_v_race <- spread_height_ratio_flume |>
  dplyr:::filter(survey == "GOA")

ggplot() +
  scale_y_continuous(name = "Spread:height ratio") +
  scale_x_continuous(name = "Towing speed (kn)") +
  scale_color_colorblind(name = "Trawl") +
  geom_smooth(data = spread_height_ratio_flume,
              mapping = aes(x = towing_speed_kn,
                            y = spread_to_height_ratio, 
                            color = footrope), 
              method = 'lm') +
  geom_point(data = spread_height_ratio_flume,
             mapping = aes(x = towing_speed_kn,
                           y = spread_to_height_ratio, 
                           color = footrope,
                           shape = bridle_length)) +
  geom_text_repel(data = spread_height_ratio_flume,
             mapping = aes(x = towing_speed_kn,
                           y = spread_to_height_ratio, 
                           color = footrope,
                           label = trial)) +
  theme_bw()


n_spread <- length(unique(spread_height_ratio_flume$spread_treatment))
pal <- scales::viridis_pal(option = "H")(n_spread+1)[1:(n_spread)]

ggplot() +
  scale_y_continuous(name = "Spread:height ratio") +
  scale_x_continuous(name = "Towing speed (kn)") +
  geom_point(data = spread_height_ratio_flume,
             mapping = aes(x = towing_speed_kn,
                           y = spread_to_height_ratio, 
                           color = factor(spread_treatment),
                           shape = bridle_length),
             size = 3.5) +
  geom_text_repel(data = spread_height_ratio_flume,
                  mapping = aes(x = towing_speed_kn,
                                y = spread_to_height_ratio, 
                                color = factor(spread_treatment),
                                label = trial)) +
  scale_color_manual(name = "Spread (m)", values = pal) +
  theme_bw() +
  facet_wrap(~footrope)


goa_vs_pne <- dplyr::filter(spread_height_ratio_flume, survey == "GOA") |>
  dplyr::filter(!(spread_treatment == 16 & bridle_length == "short")) |>
  dplyr::select(footrope, towing_speed_kn, spread_treatment, opening_headline_m) |>
  # dplyr::filter(spread_treatment == 16) |>
  tidyr::pivot_wider(values_from = "opening_headline_m",
                     names_from = "footrope",
                     values_fn = function(x) {mean(x, na.rm = TRUE)}) |>
  tidyr::pivot_longer(cols = c("GOA/AI_v5", "GOA/AI_v6"))


cowplot::plot_grid(
  ggplot() +
    geom_point(data = goa_vs_pne,
               mapping = aes(x = PNE, y = value, color = factor(spread_treatment)),
               size = 3,
               alpha = 0.7) +
    # geom_smooth(data = goa_vs_pne,
    #             mapping = aes(x = PNE, y = value, color = factor(spread_treatment)),
    #             alpha = 0.3,
    #             method = 'lm') +
    geom_abline(slope = 1, intercept = 0, linetype = 2) +
    scale_color_viridis_d(name = "Spread (m)") +
    scale_x_continuous(name = "PNE height (m)") +
    scale_y_continuous(name = "RACE height (m)") +
    facet_wrap(~name) +
    theme_bw(),
  ggplot() +
    # geom_smooth(data = goa_vs_pne,
    #             mapping = aes(x = PNE, y = value, color = factor(towing_speed_kn)),
    #             alpha = 0.3,
    #             method = 'lm') +
    geom_point(data = goa_vs_pne,
               mapping = aes(x = PNE, y = value, color = factor(towing_speed_kn)),
               size = 3,
               alpha = 0.7) +
    geom_abline(slope = 1, intercept = 0, linetype = 2) +
    scale_color_viridis_d(name = "Towing speed (kn)", option = "B") +
    scale_x_continuous(name = "PNE height (m)") +
    scale_y_continuous(name = "RACE height (m)") +
    facet_wrap(~name) +
    theme_bw(),
  nrow = 2,
  align = "hv"
)

ebs_vs_pne <- dplyr::filter(spread_height_ratio_flume, survey == "EBS") |>
  dplyr::filter(!(spread_treatment == 16 & bridle_length == "short")) |>
  dplyr::select(footrope, towing_speed_kn, spread_treatment, opening_headline_m) |>
  tidyr::pivot_wider(values_from = "opening_headline_m",
                     names_from = "footrope",
                     values_fn = function(x) {mean(x, na.rm = TRUE)}) |>
  tidyr::pivot_longer(cols = c("EBS_v1", "EBS_v2"))

ggplot() +
  geom_point(data = ebs_vs_pne,
             mapping = aes(x = `83-112`, y = value, color = factor(spread_treatment)),
             size = 3,
             alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  scale_color_viridis_d(name = "Spread (m)") +
  scale_x_continuous(name = "83-112 height (m)") +
  scale_y_continuous(name = "RACE height (m)") +
  facet_wrap(~name) +
  theme_bw()
