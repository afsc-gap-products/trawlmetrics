# Spread time series
library(trawlmetrics)

net_measured <- 
  trawlmetrics::bts_geom |>
  dplyr::filter(NET_MEASURED == TRUE,
                SURVEY_ABBV != "BSS") |>
  dplyr::mutate(SURVEY_GROUP = ifelse(SURVEY_ABBV %in% c("GOA", "AI"), "GOA/AI", "EBS/NBS"))

ggplot() +
  geom_boxplot(
    data = net_measured,
    mapping = aes(x = YEAR, y = NET_WIDTH_M, group = interaction(YEAR, VESSEL_ID))
               ) +
  geom_vline(xintercept = c(2012.5, 2023.5), linetype = 2) +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Upper wingtip spread (m)") +
  facet_wrap(~SURVEY_GROUP, ncol = 1, scales = "free_y") +
  theme_bw()

ggplot() +
  geom_boxplot(
    data = net_measured,
    mapping = aes(x = YEAR, y = NET_HEIGHT_M, group = interaction(YEAR, VESSEL_ID))
  ) +
  geom_vline(xintercept = c(2012.5, 2023.5), linetype = 2) +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Net height (m)") +
  facet_wrap(~SURVEY_GROUP, ncol = 1, scales = "free_y") +
  theme_bw()


ggplot() +
  geom_boxplot(
    data = net_measured,
    mapping = aes(x = YEAR, y = NET_WIDTH_M/NET_HEIGHT_M, group = interaction(YEAR, VESSEL_ID))
  ) +
  geom_vline(xintercept = c(2012.5, 2023.5), linetype = 2) +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Spread/Height") +
  facet_wrap(~SURVEY_GROUP, ncol = 1, scales = "free_y") +
  theme_bw()


ggplot() +
  geom_boxplot(
    data = net_measured,
    mapping = aes(x = YEAR, y = NET_WIDTH_M, group = YEAR)
  ) +
  geom_vline(xintercept = c(2012.5, 2023.5), linetype = 2) +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Upper wingtip spread (m)") +
  facet_wrap(~SURVEY_GROUP, ncol = 1, scales = "free_y") +
  theme_bw()


ggplot() +
  geom_boxplot(
    data = net_measured,
    mapping = aes(x = YEAR, y = NET_HEIGHT_M, group = YEAR)
  ) +
  geom_vline(xintercept = c(2012.5, 2023.5), linetype = 2) +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Net height (m)") +
  facet_wrap(~SURVEY_GROUP, ncol = 1, scales = "free_y") +
  theme_bw()


ggplot() +
  geom_boxplot(
    data = net_measured,
    mapping = aes(x = YEAR, y = NET_WIDTH_M/NET_HEIGHT_M, group = YEAR)
  ) +
  geom_vline(xintercept = c(2012.5, 2023.5), linetype = 2) +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Spread/Height") +
  facet_wrap(~SURVEY_GROUP, ncol = 1, scales = "free_y") +
  theme_bw()


net_measured_corr <- 
  trawlmetrics::bts_geom |>
  dplyr::filter(NET_MEASURED == TRUE,
                SURVEY_ABBV != "BSS") |>
  dplyr::mutate(
    SURVEY_GROUP = ifelse(SURVEY_ABBV %in% c("GOA", "AI"), "GOA/AI", "EBS/NBS"),
    CORR_NET_WIDTH_M = ifelse(SURVEY_GROUP == "EBS/NBS" & YEAR > 2023, (NET_WIDTH_M-0.40046503)/0.935684155, NET_WIDTH_M)
  )

net_ebs_corrections <-
  net_measured_corr |>
  tidyr::pivot_longer(cols = c("NET_WIDTH_M", "CORR_NET_WIDTH_M")) |>
  dplyr::filter(SURVEY_GROUP == "EBS/NBS") |>
  dplyr::inner_join(
    data.frame(
      name = c("NET_WIDTH_M", "CORR_NET_WIDTH_M"),
      label = c("Current EBS/NBS", "EBS/NBS without M2N 2024-2025")
    )
  )

ggplot() +
  geom_boxplot(
    data = net_ebs_corrections,
    mapping = aes(x = YEAR, y = value, group = interaction(YEAR, VESSEL_ID))
  ) +
  geom_vline(xintercept = c(2012.5, 2023.5), linetype = 2) +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Upper wingtip spread (m)") +
  facet_wrap(~label, ncol = 1, scales = "free_y") +
  theme_bw() +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 16),
        strip.text = element_text(size = 18))
