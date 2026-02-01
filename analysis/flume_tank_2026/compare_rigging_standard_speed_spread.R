library(trawlmetrics)
library(ggthemes)
library(ggrepel)

# Compare rigging options


flume_2026 <- 
  read.csv(file = here::here("analysis", "flume_tank_2026", "data", "flume_tank_data_2026_all.csv")) |> 
  dplyr::filter(towing_speed_kn == 3, spread_treatment == 16, bridles != "Standard") |>
  dplyr::mutate(
    bridles = paste0("2026-", bridles),
    trawl = paste0("New ", trawl)
  )


ggplot() +
  geom_text(
    data = flume_2026,
    mapping = aes(x = spread_u_wing_m, y = opening_headline_m, color = rig, label = rig)
  ) +
  facet_wrap(~trawl)


ggplot() +
  geom_text(
    data = flume_2026,
    mapping = aes(x = factor(rig), y = opening_headline_m, color = bridles, label = trial)
  ) +
  scale_color_colorblind(name = "Bridles") +
  scale_x_discrete(name = "Rig") +
  scale_y_continuous(name = "Headline (m)") +
  ggtitle(label = "Effects of rigging on height at 3 knots and 16 m upper wing-tip spread; text = trial") +
  facet_wrap(~trawl, scales = "free_x") +
  theme_bw()
