# Evaluating effect of a change in wing tip spread 

library(trawlmetrics)
library(gapindex)
library(crabpack)
library(akgfmaps)
library(ggthemes)
library(cowplot)

sebs_layers <- akgfmaps::get_base_layers(select.region = "sebs", set.crs = "EPSG:3338")

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

lm_wing_83112 <- 
  lm(
    spread_l_wing_m ~ spread_u_wing_m, 
    data = dplyr::filter(dat, trawl == "83-112")
  )

fhs <- gapindex::get_data(survey_set = "EBS", 
                          year_set = 1982:2024,
                          spp_codes = data.frame(SPECIES_CODE = c(10129, 10130, 10140),
                                                 GROUP_CODE = "Flathead sole and Bering flounder"),
                          pull_lengths = TRUE)

fhs_cpue <- gapindex::calc_cpue(fhs)
fhs_alk <- gapindex::calc_alk(fhs)
fhs_stratum <- gapindex::calc_biomass_stratum(gapdata = fhs, cpue = fhs_cpue)
fhs_region <- gapindex::calc_biomass_subarea(gapdata = fhs, biomass_stratum = fhs_stratum)
fhs_region$SPREAD <- "Upper wing"

fhs_adj <- fhs
fhs_adj$haul$NET_WIDTH <- predict(lm_wing_83112,
                                  data.frame(spread_u_wing_m = fhs_adj$haul$NET_WIDTH))

fhs_adj_cpue <- gapindex::calc_cpue(fhs_adj)
fhs_adj_alk <- gapindex::calc_alk(fhs_adj)
fhs_adj_stratum <- gapindex::calc_biomass_stratum(gapdata = fhs_adj, cpue = fhs_adj_cpue)
fhs_adj_region <- gapindex::calc_biomass_subarea(gapdata = fhs_adj, biomass_stratum = fhs_adj_stratum)
fhs_adj_region$SPREAD <- "Lower wing"

fhs_comparison <- 
  dplyr::bind_rows(fhs_region,
                   fhs_adj_region) |>
  dplyr::select(SPECIES_CODE, BIOMASS_MT, BIOMASS_VAR, YEAR, SPREAD, AREA_ID) |> 
  dplyr::filter(AREA_ID == 99900) |>
  dplyr::arrange(YEAR) |>
  dplyr::mutate(GROUP = YEAR < 2020)

p1_fhs <- ggplot(data = fhs_comparison,
                 mapping = aes(x = YEAR, y = BIOMASS_MT, color = SPREAD)) +
  geom_path() +
  geom_point() +
  facet_wrap(~"EBS shelf flathead sole and Bering flounder") +
  scale_color_colorblind(name = "Spread") +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Mean biomass (mt)") +
   theme_bw() +
  theme(legend.position = "bottom")

fhs_pct_diff <- fhs_comparison |>
  dplyr::select(-BIOMASS_VAR) |>
tidyr::pivot_wider(id_cols = c("YEAR", "AREA_ID"),
                   names_from = "SPREAD", values_from = "BIOMASS_MT") |>
  dplyr::mutate(PCT_DIFF  = (`Upper wing`-`Lower wing`)/`Upper wing`*100)

p2_fhs <- ggplot(data = fhs_pct_diff,
                mapping = aes(x = YEAR, y = PCT_DIFF)) +
  geom_path() +
  geom_point() +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = expression(100%*%over(B[upper]-B[lower],B[upper])~('%'))) +
  facet_wrap(~"EBS shelf flathead sole and Bering flounder") +
  theme_bw()

p3_fhs <- ggplot(data = fhs_comparison,
                 mapping = aes(x = YEAR, y = sqrt(BIOMASS_VAR)/BIOMASS_MT, color = SPREAD)) +
  geom_path() +
  geom_point() +
  facet_wrap(~"EBS shelf flathead sole and Bering flounder") +
  scale_color_colorblind(name = "Spread") +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Biomass CV") +
  theme_bw() +
  theme(legend.position = "bottom")
  

cowplot::plot_grid(p1_fhs, p2_fhs, p3_fhs, nrow = 1, align = "hv")

# Shortraker rockfish ----
lm_wing_pne <- 
  lm(
    spread_l_wing_m ~ spread_u_wing_m, 
    data = dplyr::filter(dat, trawl == "PNE")
  )

sr <- gapindex::get_data(survey_set = "GOA", 
                          year_set = c(1990:2000, 2002:2023),
                          spp_codes = data.frame(SPECIES_CODE = 30576,
                                                 GROUP_CODE = "shortraker rockfish"),
                          pull_lengths = TRUE)

sr_cpue <- gapindex::calc_cpue(sr)
sr_alk <- gapindex::calc_alk(sr)
sr_stratum <- gapindex::calc_biomass_stratum(gapdata = sr, cpue = sr_cpue)
sr_region <- gapindex::calc_biomass_subarea(gapdata = sr, biomass_stratum = sr_stratum)
sr_region$SPREAD <- "Upper wing"

sr_adj <- sr
sr_adj$haul$NET_WIDTH <- predict(lm_wing_pne,
                                  data.frame(spread_u_wing_m = sr_adj$haul$NET_WIDTH))

sr_adj_cpue <- gapindex::calc_cpue(sr_adj)
sr_adj_alk <- gapindex::calc_alk(sr_adj)
sr_adj_stratum <- gapindex::calc_biomass_stratum(gapdata = sr_adj, cpue = sr_adj_cpue)
sr_adj_region <- gapindex::calc_biomass_subarea(gapdata = sr_adj, biomass_stratum = sr_adj_stratum)
sr_adj_region$SPREAD <- "Lower wing"

sr_comparison <- 
  dplyr::bind_rows(sr_region,
                   sr_adj_region) |>
  dplyr::select(SPECIES_CODE, BIOMASS_MT, BIOMASS_VAR, YEAR, SPREAD, AREA_ID) |> 
  dplyr::filter(AREA_ID == 99903) |>
  dplyr::arrange(YEAR)

p1_sr <- ggplot(data = sr_comparison,
                mapping = aes(x = YEAR, y = BIOMASS_MT, color = SPREAD)) +
  geom_path() +
  geom_point() +
  facet_wrap(~"GOA shortraker rockfish") +
  scale_color_colorblind(name = "Spread") +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Mean biomass (mt)") +
  theme_bw() +
  theme(legend.position = "bottom")

sr_pct_diff <- sr_comparison |>
  dplyr::select(-BIOMASS_VAR) |>
  tidyr::pivot_wider(id_cols = c("YEAR", "AREA_ID"),
                     names_from = "SPREAD", values_from = "BIOMASS_MT") |>
  dplyr::mutate(PCT_DIFF  = (`Upper wing`-`Lower wing`)/`Upper wing`*100)

p2_sr <-ggplot(data = sr_pct_diff,
               mapping = aes(x = YEAR, y = PCT_DIFF)) +
  geom_path() +
  geom_point() +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = expression(100%*%over(B[upper]-B[lower],B[upper])~('%'))) +
  facet_wrap(~"GOA shortraker rockfish") +
  theme_bw()

p3_sr <- ggplot(data = sr_comparison,
                 mapping = aes(x = YEAR, y = sqrt(BIOMASS_VAR)/BIOMASS_MT, color = SPREAD)) +
  geom_path() +
  geom_point() +
  facet_wrap(~"EBS shelf flathead sole and Bering flounder") +
  scale_color_colorblind(name = "Spread") +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Biomass CV") +
  theme_bw() +
  theme(legend.position = "bottom")


cowplot::plot_grid(p1_sr, p2_sr, p3_sr, nrow = 1, align = "hv")


# species <- "RKC"
# specimen_data <- crabpack::get_specimen_data(species = "RKC",
#                                              region = "EBS",
#                                              years = c(1982:2024),
#                                              channel = "API")
# 
# 
# 
# specimen_data_adj <- specimen_data
# 
# specimen_data_adj$haul$NET_WIDTH <- 
#   predict(
#     object = lm_wing_83112,
#     newdata = data.frame(spread_u_wing_m = specimen_data_adj$haul$NET_WIDTH)
#   )
# 
# specimen_data_adj$haul$AREA_SWEPT <- specimen_data_adj$haul$NET_WIDTH * specimen_data_adj$haul$DISTANCE_FISHED/(1000*1.852^2)
# 
# specimen_data_adj$specimen <- specimen_data_adj$specimen |>
#   dplyr::select(-AREA_SWEPT) |>
#   dplyr::inner_join(dplyr::select(specimen_data_adj$haul, HAULJOIN, AREA_SWEPT))
# 
# matfem_bbrkc <- 
#   crabpack::calc_bioabund(crab_data = specimen_data,
#                           species = "RKC",
#                           region = "EBS",
#                           district = "BB",
#                           crab_category = "mature_female",
#                           female_maturity = "cutline")
# 
# matfem_bbrkc_adj <- 
#   crabpack::calc_bioabund(crab_data = specimen_data_adj,
#                           species = "RKC",
#                           region = "EBS",
#                           district = "BB",
#                           crab_category = "mature_female",
#                           female_maturity = "cutline")
# 
# biomass_comparison <- 
#   dplyr::select(
#   matfem_bbrkc_adj, 
#   SPECIES, 
#   YEAR, 
#   REGION, 
#   DISTRICT, 
#   ADJ_BIOMASS_MT = BIOMASS_MT) |>
#   dplyr::inner_join(
#     dplyr::select(
#       matfem_bbrkc, 
#       SPECIES, 
#       YEAR, 
#       REGION, 
#       DISTRICT, 
#       BIOMASS_MT)
#   ) |> tidyr::pivot_longer(
#   cols = c("ADJ_BIOMASS_MT", "BIOMASS_MT")
# )
# 
# 
# ggplot() +
#   geom_point(data = biomass_comparison,
#              mapping = aes(x = YEAR, y = value, color = name)) +
#   scale_color_colorblind()
