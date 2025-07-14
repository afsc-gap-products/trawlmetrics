library(ggplot2)
library(ggridges)
library(rayshader)

survey_total_catch <- trawlmetrics::bts_geom |>
  dplyr::filter(!(SURVEY_ABBV %in% "BSS"))

p_total_catch <-  ggplot() +
  ggridges::stat_density_ridges(
    data = survey_total_catch,
    mapping = aes(x = TOTAL_WEIGHT_KG,
                  y = factor(SURVEY_ABBV, levels = c("GOA", "AI", "NBS", "EBS")),
                  fill = SURVEY_ABBV),
    quantile_lines = TRUE,
    alpha = 0.7
  ) +
  scale_x_log10(name = "Total weight (kg)") +
  scale_fill_viridis_d() +
  theme_minimal() +
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 20))

dplyr::group_by(survey_total_catch, SURVEY_ABBV) |>
  dplyr::summarise(q25 = quantile(TOTAL_COUNT, 0.25),
                   q50 = quantile(TOTAL_COUNT, 0.5),
                   q75 = quantile(TOTAL_COUNT, 0.75),
                   mean_count = mean(TOTAL_COUNT),
                   median(TOTAL_WEIGHT_KG))

ggplot() +
  ggridges::stat_density_ridges(
    data = survey_total_catch,
    mapping = aes(x = TOTAL_COUNT,
                  y = factor(SURVEY_ABBV, levels = c("GOA", "AI", "NBS", "EBS")),
                  fill = SURVEY_ABBV),
    quantile_lines = TRUE,
    alpha = 0.7
  ) +
  scale_x_log10(name = "Total count") +
  scale_fill_viridis_d() +
  theme_minimal() +
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 20))

ggplot() +
  ggridges::stat_density_ridges(
    data = survey_total_catch,
    mapping = aes(x = TOTAL_WEIGHT_KG/TOTAL_COUNT,
                  y = factor(SURVEY_ABBV, levels = c("GOA", "AI", "NBS", "EBS")),
                  fill = SURVEY_ABBV),
    quantile_lines = TRUE,
    alpha = 0.7
  ) +
  scale_x_continuous(limits = c(0, 1)) +
  scale_fill_viridis_d() +
  theme_minimal() +
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 20))


# Get length comps
channel <- gapindex::get_connected()

size_comps <- 
  RODBC::sqlQuery(
    channel = channel,
    query = "SELECT * FROM GAP_PRODUCTS.AKFIN_SIZECOMP 
    WHERE SPECIES_CODE IN (10261, 21740, 30060) AND YEAR > 2001 AND AREA_ID IN (99900, 99902, 99903, 99904)"
    ) |>
  dplyr::group_by(SURVEY_DEFINITION_ID, YEAR, AREA_ID, SPECIES_CODE, LENGTH_MM) |>
  dplyr::summarise(POPULATION_COUNT = sum(POPULATION_COUNT))

pollock_lengths <- 
  ggplot() +
  geom_tile(data = dplyr::filter(size_comps, SURVEY_DEFINITION_ID == 98, SPECIES_CODE == 21740), 
            mapping = aes(x = LENGTH_MM/10, y = YEAR, fill = POPULATION_COUNT^0.5)) +
  scale_fill_viridis_c(option = "F", direction = -1) +
  scale_y_reverse() +
  scale_x_continuous(name = "Fork length (cm)") +
  theme_few() +
  theme(legend.position = "none",
        axis.title.y = element_blank()) 

rayshader::plot_gg(test)
