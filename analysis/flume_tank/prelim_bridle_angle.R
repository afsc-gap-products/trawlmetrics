library(trawlmetrics)
library(ggthemes)
library(ggrepel)
library(scales)
library(gapindex)

survey_abbv <- data.frame(SURVEY_ABBV = c("AI", "GOA", "EBS", "NBS", "BSS"),
                          SURVEY_DEFINITION_ID = c(52, 47, 98, 143, 78))

dat <- trawlmetrics::flume_tank |> 
  dplyr::filter(footrope %in% c("83-112", "PNE", "EBS_v2", "GOA/AI_v6"), 
                catch == "empty",
                pulling_point_elevation_mm <= 200,
                additional_floatation_kg == 0, 
                !(bridles == 2 & trawl == "83-112"),
                !(bridles == 2 & trawl == "PNE"),
                !(bridles == "2-weighted" & trawl == "PNE"),
                trial > 0,
                !benthic_bag)

ggplot() +
  geom_smooth(data = dat,
              mapping = aes(x = spread_u_wing_m, 
                            y = bridle_angle_deg,
                            color = paste0(trawl, " (", footrope, ")")),
              method = 'gam', formula = y ~ s(x, bs = "cs", k = 5)) +
  geom_point(data = dat,
             mapping = aes(x = spread_u_wing_m, 
                           y = bridle_angle_deg,
                           color = paste0(trawl, " (", footrope, ")")),
             size = rel(2.5),
              shape = 1) +
  geom_hline(yintercept = c(18, 21), linetype  = 2) +
  scale_color_colorblind(name = "Spread (m)") +
  # scale_color_manual(name = "Spread (m)", values = colors_trawl_footrope) +
  scale_x_continuous(name = "Spread (m)") +
  scale_y_continuous(name = expression('Bridle angle ('*degree*')')) +
  theme_bw()

ggplot() +
  geom_smooth(data = dat,
              mapping = aes(x = spread_u_wing_m, 
                            y = bridle_angle_deg),
              method = 'gam', formula = y ~ s(x, bs = "cs", k = 5)) +
  geom_point(data = dat,
             mapping = aes(x = spread_u_wing_m, 
                           y = bridle_angle_deg,
                           color = bridle_length),
             size = rel(2.5)) +
  geom_hline(yintercept = c(18, 21), linetype  = 2) +
  scale_color_colorblind(name = "Spread (m)") +
  # scale_color_manual(name = "Spread (m)", values = colors_trawl_footrope) +
  scale_shape(solid = FALSE) +
  scale_x_continuous(name = "Spread (m)") +
  scale_y_continuous(name = expression('Bridle angle ('*degree*')')) +
  theme_bw() +
  facet_wrap(~paste0(trawl, " (", footrope, ")"))



colors_trawl_footrope <- c(
  "PNE (PNE)" = "#000000",
  "83-112 (83-112)" = "#E69F00",
  "RACE (EBS_v2)" = "#56B4E9",
  "RACE (GOA/AI_v6)" = "#009E73")
