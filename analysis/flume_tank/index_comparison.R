# Evaluating effect of a change in wing tip spread 

library(trawlmetrics)
library(gapindex)
library(crabpack)
library(akgfmaps)
library(ggthemes)

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



species <- "RKC"
specimen_data <- crabpack::get_specimen_data(species = "RKC",
                                             region = "EBS",
                                             years = c(1982:2024),
                                             channel = "API")

dat <- gapindex::get_data()

specimen_data_adj <- specimen_data

specimen_data_adj$haul$NET_WIDTH <- 
  predict(
    object = lm_wing_83112,
    newdata = data.frame(spread_u_wing_m = specimen_data_adj$haul$NET_WIDTH)
  )

specimen_data_adj$haul$AREA_SWEPT <- specimen_data_adj$haul$NET_WIDTH * specimen_data_adj$haul$DISTANCE_FISHED/(1000*1.852^2)

specimen_data_adj$specimen <- specimen_data_adj$specimen |>
  dplyr::select(-AREA_SWEPT) |>
  dplyr::inner_join(dplyr::select(specimen_data_adj$haul, HAULJOIN, AREA_SWEPT))

matfem_bbrkc <- 
  crabpack::calc_bioabund(crab_data = specimen_data,
                          species = "RKC",
                          region = "EBS",
                          district = "BB",
                          crab_category = "mature_female",
                          female_maturity = "cutline")

matfem_bbrkc_adj <- 
  crabpack::calc_bioabund(crab_data = specimen_data_adj,
                          species = "RKC",
                          region = "EBS",
                          district = "BB",
                          crab_category = "mature_female",
                          female_maturity = "cutline")

biomass_comparison <- 
  dplyr::select(
  matfem_bbrkc_adj, 
  SPECIES, 
  YEAR, 
  REGION, 
  DISTRICT, 
  ADJ_BIOMASS_MT = BIOMASS_MT) |>
  dplyr::inner_join(
    dplyr::select(
      matfem_bbrkc, 
      SPECIES, 
      YEAR, 
      REGION, 
      DISTRICT, 
      BIOMASS_MT)
  ) |> tidyr::pivot_longer(
  cols = c("ADJ_BIOMASS_MT", "BIOMASS_MT")
)


ggplot() +
  geom_point(data = biomass_comparison,
             mapping = aes(x = YEAR, y = value, color = name)) +
  scale_color_colorblind()
