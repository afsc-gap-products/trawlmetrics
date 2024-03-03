library(trawlmetrics)
library(mgcv)
library(ggthemes)
library(ggrepel)

load(here::here("analysis", "door_experiment", "data", "spread_height_data.rda"))

# 83-112 models ----

# Spread model for 83-112 hauls
mod_spread_83112 <- gam(formula = NET_SPREAD ~ s(BOTTOM_DEPTH, SCOPE_TO_DEPTH, bs = "tp", k = 100) + s(STATIONID, bs = "re"), data = standard_hauls)

fit_spread_83112 <- dplyr::filter(usable_width_treatments, GEAR == 44) |>
  dplyr::mutate(type = "4.5-m doors")

standard_hauls$fit_spread <- predict(mod_spread_83112)
standard_hauls$type <- "Standard 83-112"

fit_spread_83112$fit_spread <- predict(mod_spread_83112, 
                                       newdata = fit_spread_83112)
plot_spread_gam <- ggplot() +
  geom_point(data = standard_hauls,
             mapping = aes(x = fit_spread, y = NET_SPREAD, color = type),
             alpha = 0.2) +
  geom_path(data = fit_spread_83112,
            mapping = aes(x = fit_spread, y = NET_SPREAD, group = HAUL),
            color = "red") +
  geom_point(data = fit_spread_83112,
             mapping = aes(x = fit_spread, y = NET_SPREAD, color = type, fill = factor(treatment), group = HAUL),
             size = rel(4), shape = 21) +
  geom_abline(mapping = aes(intercept = 0, slope = 1),
              linetype = 2, linewidth = 1.5) +
  geom_text_repel(data = dplyr::filter(fit_spread_83112) |>
                    dplyr::filter(treatment == 1),
                  mapping = aes(x = fit_spread, y = NET_SPREAD, label = HAUL, color = "4.5-m doors")) +
  scale_color_manual(name = "Haul type", values = c("4.5-m doors" = "red", "Standard 83-112" = "grey50")) +
  scale_fill_colorblind(name = "Treatment", na.translate = TRUE, na.value = "grey50") +
  scale_x_continuous(name = "Predicted wing spread (m)") +
  scale_y_continuous(name = "Observed wing spread (m)") +
  theme_bw() +
  theme(legend.position = c(0.82, 0.18),
        legend.box.background = element_blank(),
        legend.background = element_blank())

png(filename = here::here("analysis", "door_experiment", "plots", "gam_obs_vs_predicted_spread.png"), height = 180, width = 180, units = "mm", res = 300)
print(plot_spread_gam +
        theme(legend.text = element_text(size = 12),
              legend.title = element_text(size = 14),
              axis.title = element_text(size = 20),
              axis.text = element_text(size = 18)))
dev.off()

# Height model for 83-112 hauls
mod_height_83112 <- gam(formula = NET_HEIGHT ~ s(BOTTOM_DEPTH, SCOPE_TO_DEPTH, bs = "tp", k = 100) + s(STATIONID, bs = "re"), data = standard_hauls)

fit_height_83112 <- dplyr::filter(usable_height_treatments, GEAR == 44) |>
  dplyr::mutate(type = "4.5-m doors")

standard_hauls$fit_height <- predict(mod_height_83112)
standard_hauls$type <- "Standard 83-112"

fit_height_83112$fit_height <- predict(mod_height_83112, 
                                       newdata = fit_height_83112)

plot_height_gam <- ggplot() +
  geom_point(data = standard_hauls,
             mapping = aes(x = fit_height, y = NET_HEIGHT, color = type),
             alpha = 0.2) +
  geom_path(data = fit_height_83112,
            mapping = aes(x = fit_height, y = NET_HEIGHT, group = HAUL),
            color = "red") +
  geom_point(data = fit_height_83112,
             mapping = aes(x = fit_height, y = NET_HEIGHT, color = type, fill = factor(treatment), group = HAUL),
             size = rel(4),
             shape = 21) +
  geom_abline(mapping = aes(intercept = 0, slope = 1),
              linetype = 2, linewidth = 1.5) +
  geom_text_repel(data = dplyr::filter(fit_height_83112) |>
                    dplyr::filter(treatment == 1),
                  mapping = aes(x = fit_height, y = NET_HEIGHT, label = HAUL, color = "4.5-m doors")) +
  scale_color_manual(name = "Haul type", values = c("4.5-m doors" = "red", "Standard 83-112" = "grey50")) +
  scale_fill_colorblind(name = "Treatment", na.translate = TRUE, na.value = "grey50") +
  scale_x_continuous(name = "Predicted height (m)") +
  scale_y_continuous(name = "Observed height (m)") +
  theme_bw() +
  theme(legend.position = c(0.8, 0.12),
        legend.box.background = element_blank(),
        legend.background = element_blank())


png(filename = here::here("analysis", "door_experiment", "plots", "gam_obs_vs_predicted_height.png"), height = 180, width = 180, units = "mm", res = 300)
print(plot_height_gam +
        theme(legend.text = element_text(size = 12),
              legend.title = element_text(size = 14),
              axis.title = element_text(size = 20),
              axis.text = element_text(size = 18),
              legend.position = c(0.15, 0.82)))
dev.off()


# PNE models ----

# Spread model for PNE hauls
mod_spread_pne <- gam(formula = NET_SPREAD ~ s(BOTTOM_DEPTH, SCOPE_TO_DEPTH, bs = "tp", k = 100), data = slope_hauls)

fit_spread_pne <- dplyr::filter(usable_width_treatments, GEAR == 172) |>
  dplyr::mutate(type = "4.5-m doors")

slope_hauls$fit_spread <- predict(mod_spread_pne)
slope_hauls$type <- "Standard PNE"

fit_spread_pne$fit_spread <- predict(mod_spread_pne, 
                                     newdata = fit_spread_pne)

plot_spread_gam_pne <- ggplot() +
  geom_point(data = slope_hauls,
             mapping = aes(x = fit_spread, y = NET_SPREAD, color = type),
             alpha = 0.2) +
  geom_path(data = fit_spread_pne,
            mapping = aes(x = fit_spread, y = NET_SPREAD, group = HAUL),
            color = "red") +
  geom_point(data = fit_spread_pne,
             mapping = aes(x = fit_spread, y = NET_SPREAD, color = type, fill = factor(treatment), group = HAUL),
             size = rel(4), shape = 21) +
  geom_abline(mapping = aes(intercept = 0, slope = 1),
              linetype = 2, linewidth = 1.5) +
  geom_text_repel(data = dplyr::filter(fit_spread_pne) |>
                    dplyr::filter(treatment == 1),
                  mapping = aes(x = fit_spread, y = NET_SPREAD, label = HAUL, color = "4.5-m doors")) +
  scale_color_manual(name = "Haul type", values = c("4.5-m doors" = "red", "Standard PNE" = "grey50")) +
  scale_fill_colorblind(name = "Treatment", na.translate = TRUE, na.value = "grey50") +
  scale_x_continuous(name = "Predicted wing spread (m)") +
  scale_y_continuous(name = "Observed wing spread (m)") +
  theme_bw() +
  theme(legend.position = c(0.3, 0.12),
        legend.box.background = element_blank(),
        legend.background = element_blank())

png(filename = here::here("analysis", "door_experiment", "plots", "gam_obs_vs_predicted_spread_pne.png"), height = 180, width = 180, units = "mm", res = 300)
print(plot_spread_gam_pne +
        theme(legend.text = element_text(size = 12),
              legend.title = element_text(size = 14),
              axis.title = element_text(size = 20),
              axis.text = element_text(size = 18),
              legend.direction = "horizontal"))
dev.off()



# Height model for PNE hauls
mod_height_pne <- gam(formula = NET_HEIGHT ~ s(BOTTOM_DEPTH, SCOPE_TO_DEPTH, bs = "tp", k = 100), data = slope_hauls)

fit_height_pne <- dplyr::filter(usable_height_treatments, GEAR == 172) |>
  dplyr::mutate(type = "4.5-m doors")

slope_hauls$fit_height <- predict(mod_height_pne)
slope_hauls$type <- "Standard PNE"

fit_height_pne$fit_height <- predict(mod_height_pne, 
                                     newdata = fit_height_pne)

plot_height_gam_pne <- ggplot() +
  geom_point(data = slope_hauls,
             mapping = aes(x = fit_height, y = NET_HEIGHT, color = type),
             alpha = 0.2) +
  geom_path(data = fit_height_pne,
            mapping = aes(x = fit_height, y = NET_HEIGHT, group = HAUL),
            color = "red") +
  geom_point(data = fit_height_pne,
             mapping = aes(x = fit_height, y = NET_HEIGHT, color = type, fill = factor(treatment), group = HAUL),
             size = rel(4),
             shape = 21) +
  geom_abline(mapping = aes(intercept = 0, slope = 1),
              linetype = 2, linewidth = 1.5) +
  geom_text_repel(data = dplyr::filter(fit_height_pne) |>
                    dplyr::filter(treatment == 1),
                  mapping = aes(x = fit_height, y = NET_HEIGHT, label = HAUL, color = "4.5-m doors")) +
  scale_color_manual(name = "Haul type", values = c("4.5-m doors" = "red", "Standard PNE" = "grey50")) +
  scale_fill_colorblind(name = "Treatment", na.translate = TRUE, na.value = "grey50") +
  scale_x_continuous(name = "Predicted height (m)") +
  scale_y_continuous(name = "Observed height (m)", limits = c(3,9)) +
  theme_bw() +
  theme(legend.position = c(0.3, 0.12),
        legend.box.background = element_blank(),
        legend.background = element_blank())


png(filename = here::here("analysis", "door_experiment", "plots", "gam_obs_vs_predicted_height_pne.png"), height = 180, width = 180, units = "mm", res = 300)
print(plot_height_gam_pne +
        theme(legend.text = element_text(size = 12),
              legend.title = element_text(size = 14),
              axis.title = element_text(size = 20),
              axis.text = element_text(size = 18),
              legend.direction = "horizontal"))
dev.off()
