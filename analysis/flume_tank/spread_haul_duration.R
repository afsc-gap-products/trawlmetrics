library(sratio)
library(brms)

net_spread <- sratio::data_1530$haul |>
  dplyr::select(CRUISE, NET_WIDTH, MATCHUP, TREATMENT) |>
  tidyr::pivot_wider(values_from = c("NET_WIDTH"), names_from = "TREATMENT", names_prefix = "DUR_")

spread_gam <- mgcv::gam(formula = DUR_15 ~ s(DUR_30, bs = "tp"), data = net_spread)


ggplot(data = net_spread, mapping = aes(x = DUR_30, y = DUR_15)) +
  geom_point() +
  geom_smooth(method = 'gam') +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  scale_x_continuous(name = "30 min. upper wing (m)") +
  scale_y_continuous(name = "15 min. upper wing (m)") +
  theme_bw()

ggplot() +
  geom_histogram(data = net_spread,
                 mapping = aes(DUR_15/DUR_30))

spread_ratio_brm <- brm(formula = DUR_15/DUR_30 ~ 1, data = net_spread)

plot(spread_ratio_brm )