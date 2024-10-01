library(trawlmetrics)
library(dplyr)
library(mgcv)

channel <- trawlmetrics::get_connected(schema = "AFSC")

ai_dat <- get_trawlmetrics(survey = "AI", 
                           year = 2024,
                           select_haul_types = 3,
                           select_gear_code = 172,
                           channel = channel
)

ai_dat$trawl_data |>
  dplyr::filter(CRUISE == 202401) |>
  dplyr::group_by(NET_NUMBER, VESSEL) |>
  dplyr::summarise(n = n())

# ebs_dat <- get_trawlmetrics(survey = "EBS", 
#                             year = 2024, 
#                             select_haul_types = 3,
#                             select_gear_code = 44,
#                             channel = channel)
# 
# ebs_dat$trawl_data |>
#   dplyr::filter(CRUISE == 202401) |>
#   dplyr::group_by(NET_NUMBER, GEAR) |>
#   dplyr::summarise(n = n())

ai_dat$trawl_data


calc_tow_speed <- function(trawl_data) {
  
  
}

trawl_data <- ai_dat$trawl_data

mod_data <- dplyr::filter(trawl_data, 
                         NET_SPREAD_METHOD %in% c(1, 6, 7, 8),
                         PERFORMANCE >= 0) |>
  dplyr::mutate(TRAWL_ID = factor(TRAWL_ID),
                dummy_var = 1)


tow_speed_gam <- mgcv::gam(NET_WIDTH ~ s(TOW_SPEED_KN) + s(TRAWL_ID, bs = "re", by = dummy_var), 
                           data = mod_data)

plot(tow_speed_gam)


speed_fit <- data.frame(TOW_SPEED_KN = seq(2.8, 3.2, 0.01),
                        TRAWL_ID = mod_data$TRAWL_ID[1],
                        dummy_var = 0)

speed_fit <- dplyr::bind_cols(speed_fit, 
                              as.data.frame(
                                predict(tow_speed_gam, newdata = newdata, se.fit = TRUE)
                                )
                              )

mod_data$fit_no_re <- predict(tow_speed_gam,
                              newdata = data.frame(TOW_SPEED_KN = mod_data$TOW_SPEED_KN,
                                                   TRAWL_ID = mod_data$TRAWL_ID[1],
                                                   dummy_var = 0))

mod_data |>
  dplyr::group_by(VESSEL, YEAR) |>
  dplyr::summarise(EST_MEAN_NET_WIDTH = mean(fit_no_re) - mean(mod_data$NET_WIDTH))


ggplot() +
  geom_point(data = mod_data,
             mapping = aes(x = TOW_SPEED_KN,
                           y = NET_WIDTH),
             size = 0.1) +
  geom_ribbon(data = speed_fit,
              mapping = aes(x = TOW_SPEED_KN,
                            ymin = fit - se.fit,
                            ymax = fit + se.fit),
              alpha = 0.5) +
  geom_path(data = speed_fit, 
            mapping = aes(x = TOW_SPEED_KN, 
                          y = fit)) +
  scale_x_continuous(name = "Tow speed (knots)", limits = c(2.8, 3.2)) +
  scale_y_continuous(name = "Spread (m)", limits = c(12.5, 22)) +
  theme_bw()



plot(tow_speed_gam)

mod_dat$TOW_SPEED_KN
mod_dat$TRAWL_ID



ggplot() +
  geom_hline(yintercept = 3) +
  geom_hline(yintercept = c(2.8, 3.2), linetype = 2) +
  geom_boxplot(data = mod_dat,
               mapping = aes(x = YEAR, 
                             y = TOW_SPEED_KN, 
                             color = factor(VESSEL), 
                             group = interaction(VESSEL, YEAR))) +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Tow Speed (kn)") +
  scale_color_discrete(name = "Vessel ID") +
    theme_bw()


mod_dat |>
  dplyr::group_by(VESSEL, YEAR) |>
  dplyr::summarise(MEAN_TOW_SPEED_KN = mean(TOW_SPEED_KN),
                   SD_TOW_SPEED_KN = sd(TOW_SPEED_KN))
