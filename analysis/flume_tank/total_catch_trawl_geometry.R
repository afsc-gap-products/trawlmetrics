# Retrieve and plot total catch weight statistics by survey
# Sean Rohan
# January 3, 2025

library(trawlmetrics)
library(ggthemes)

channel <- trawlmetrics::get_connected(schema = "AFSC")

survey_abbv <- data.frame(SURVEY_ABBV = c("AI", "GOA", "EBS", "NBS", "BSS"),
                          SURVEY_DEFINITION_ID = c(52, 47, 98, 143, 78))

gp_catch <- RODBC::sqlQuery(
  channel = channel,
  query = 
    "SELECT 
      C.HAULJOIN,
      CR.VESSEL_ID AS VESSEL,
      CR.SURVEY_DEFINITION_ID,
      CR.SURVEY_NAME,
      C.SPECIES_CODE, 
      C.WEIGHT_KG,
      H.NET_MEASURED,
      H.HAUL,
      H.DEPTH_GEAR_M,
      H.DEPTH_M,
      H.NET_WIDTH_M,
      H.NET_HEIGHT_M,
      H.DISTANCE_FISHED_KM,
      H.DURATION_HR,
      H.STATION,
      H.WIRE_LENGTH_M,
      H.GEAR,
      H.ACCESSORIES
    FROM 
      GAP_PRODUCTS.AKFIN_CATCH C,
      GAP_PRODUCTS.AKFIN_HAUL H,
      GAP_PRODUCTS.AKFIN_CRUISE CR
    WHERE
      H.HAULJOIN = C.HAULJOIN
      AND CR.CRUISEJOIN = H.CRUISEJOIN
        "
) |>
  dplyr::inner_join(survey_abbv, by = 'SURVEY_DEFINITION_ID')


total_catch <- gp_catch |>
  dplyr::group_by(HAULJOIN, 
                  VESSEL, 
                  SURVEY_DEFINITION_ID, 
                  SURVEY_NAME,
                  SURVEY_ABBV,
                  HAUL, 
                  DEPTH_GEAR_M, 
                  DEPTH_M, 
                  NET_MEASURED,
                  NET_WIDTH_M, 
                  NET_HEIGHT_M, 
                  DISTANCE_FISHED_KM, 
                  DURATION_HR, 
                  WIRE_LENGTH_M, 
                  GEAR,
                  ACCESSORIES) |>
  dplyr::summarise(TOTAL_WEIGHT_KG = sum(WEIGHT_KG),
                   .groups = 'keep')

catch_quantiles <- total_catch |>
  dplyr::ungroup() |>
  dplyr::group_by(SURVEY_ABBV) |>
  dplyr::summarise(`0` = round(quantile(TOTAL_WEIGHT_KG, 0), 1),
                   `1` = round(quantile(TOTAL_WEIGHT_KG, 0.01), 1),
                   `10` = round(quantile(TOTAL_WEIGHT_KG, 0.1), 1),
                   `25` = round(quantile(TOTAL_WEIGHT_KG, 0.25), 1),
                   `50` = round(quantile(TOTAL_WEIGHT_KG, 0.5), 1),
                   `75` = round(quantile(TOTAL_WEIGHT_KG, 0.75), 1),
                   `90` = round(quantile(TOTAL_WEIGHT_KG, 0.9), 1),
                   `99` = round(quantile(TOTAL_WEIGHT_KG, 0.99), 1),
                   `100` = round(quantile(TOTAL_WEIGHT_KG, 1), 1))

write.csv(catch_quantiles, 
          file = here::here("analysis", "flume_tank", "total_catch_weight_quantiles_by_survey.csv"),
          row.names = FALSE)

# Plot trawl geometry by catch weight ----
p_spread_gam <- ggplot(data = dplyr::filter(total_catch, NET_MEASURED == 1),
                       mapping = aes(x = TOTAL_WEIGHT_KG,
                                     y = NET_WIDTH_M)) +
  geom_point(size = rel(0.1)) +
  geom_smooth(formula = y ~ s(x, bs = "tp"),
              method = 'gam') +
  scale_x_log10(name = "Total Catch (kg)") +
  scale_y_continuous(name = "Net spread (m)") +
  facet_wrap(~factor(SURVEY_ABBV, levels = survey_abbv$SURVEY_ABBV)) +
  theme_bw()

ragg::agg_png(filename = here::here("analysis", "flume_tank", "p_spread_gam.png"), 
              width = 6, 
              height = 4, 
              units = "in", 
              res = 300)
print(p_spread_gam)
dev.off()

p_height_gam <- ggplot(data = dplyr::filter(total_catch, NET_MEASURED == 1),
                       mapping = aes(x = TOTAL_WEIGHT_KG,
                                     y = NET_HEIGHT_M)) +
  geom_point(size = rel(0.1)) +
  geom_smooth(formula = y ~ s(x, bs = "tp"),
              method = 'gam') +
  scale_x_log10(name = "Total Catch (kg)") +
  scale_y_continuous(name = "Net height (m)") +
  facet_wrap(~factor(SURVEY_ABBV, levels = survey_abbv$SURVEY_ABBV), 
             scales = 'free_y') +
  theme_bw()

ragg::agg_png(filename = here::here("analysis", "flume_tank", "p_height_gam.png"),
              width = 6, 
              height = 4, 
              units = "in", 
              res = 300)
print(p_height_gam)
dev.off()

p_cumulative_catch <- ggplot() +
  stat_ecdf(data = total_catch,
            mapping = aes(x = TOTAL_WEIGHT_KG,
                          color = SURVEY_ABBV),
            linewidth = 1.1) +
  scale_x_log10(name = "Total Catch (kg)") +
  scale_color_colorblind(name = "Survey") +
  theme_bw() +
  theme(legend.position = c(0.1, 0.75))

ragg::agg_png(filename = here::here("analysis", "flume_tank", "p_ecdf_catch.png"), 
              width = 6, 
              height = 4, 
              units = "in", 
              res = 300)
print(p_cumulative_catch)
dev.off()


ggplot(data = dplyr::filter(total_catch, NET_MEASURED == 1),
       mapping = aes(x = DEPTH_M,
                     y = TOTAL_WEIGHT_KG)) +
  geom_point(size = rel(0.1)) +
  geom_smooth(formula = y ~ s(x, bs = "tp"),
              method = 'gam') +
  scale_x_continuous(name = "Bottom depth (m)") +
  scale_y_log10(name = "Total Catch (kg)") +
  facet_wrap(~factor(SURVEY_ABBV, levels = survey_abbv$SURVEY_ABBV), 
             scales = 'free') +
  theme_bw()
