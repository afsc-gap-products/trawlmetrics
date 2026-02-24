library(gapindex)
library(dplyr)
library(crabpack)
library(ggpp)
library(ggthemes)

taxa <- 
  data.frame(
    SPECIES_CODE = c(21740, 21720, 10110, 10112, 10115, 10210, 10261, 10130, 471, 10285),
    GROUP_CODE = c("walleye pollock", "Pacific cod", "arrowtooth flounder", "Kamchatka flounder",
                   "Greenland turbot", "yellowfin sole", "northern rock sole", "flathead sole", "Alaska skate", "Alaska plaice"),
    MIN_SIZE_CM = c(NA, 10, 20, 15, 10, 10, NA, NA, NA, NA),
    MIN_AGE = c(NA, 1, 1, 1, 1, 1, NA, NA, NA, NA)
  )

taxa_slope <- 
  data.frame(
    SPECIES_CODE = c(21740, 21720, 10110, 10112, 10115, 10130),
    GROUP_CODE = c("walleye pollock", "Pacific cod", "arrowtooth flounder", "Kamchatka flounder",
                   "Greenland turbot",  "flathead sole"),
    MIN_SIZE_CM = c(NA, 10, 20, 21, 10, NA),
    MIN_AGE = c(NA, 1, 2, 2, 3, NA)
  )

channel <- gapindex::get_connected(check_access = FALSE)

gapdata <- 
  gapindex::get_data(
    survey_set = "EBS",
    year_set = c(2010:2019, 2021:2025), 
    spp_codes = taxa,
    pull_lengths = TRUE,
    taxonomic_source = "GAP_PRODUCTS.TAXONOMIC_CLASSIFICATION",
    channel = channel
  )

fish_size <- 
  gapdata$size |>
  dplyr::mutate(LENGTH_CM = floor(LENGTH/10)) |>
  dplyr::group_by(SPECIES_CODE, LENGTH_CM, SEX) |>
  dplyr::summarise(TOTAL = sum(FREQUENCY, na.rm = TRUE))

fish_specimen <- 
  gapdata$specimen |>
  dplyr::group_by(SPECIES_CODE, AGE, SEX) |>
  dplyr::summarise(N = n())


p_fish_size_thresholds <- 
  ggplot() +
  geom_bar(
    data = fish_size,
    mapping = aes(x = LENGTH_CM, y = TOTAL, fill = factor(SEX, levels = c(2,1,3), labels = c("Female", "Male", "Unsexed"))),
    stat = "identity",
    position = "stack",
    width = 1
  ) +
  geom_vline(data = dplyr::mutate(taxa, SPECIES_CODE = GROUP_CODE),
             mapping = aes(xintercept = MIN_SIZE_CM, linetype = "Assessment\nMin. size (shelf)")) +
  geom_vline(data = dplyr::mutate(taxa_slope, SPECIES_CODE = GROUP_CODE),
             mapping = aes(xintercept = MIN_SIZE_CM, linetype = "Assessment\nMin. size (slope)")) +
  scale_x_continuous(name = "Length (cm)", expand = c(0,0)) +
  scale_y_continuous(name = "Frequency (#)",
                     expand = expansion(mult = c(0, 0.1))) +
  scale_fill_tableau(name = "Sex") +
  scale_linetype_manual(name = NULL, values = c(2, 3)) +
  expand_limits(x = 0) +
  facet_wrap(~SPECIES_CODE, scales = "free") +
  theme_bw() +
  theme(strip.background = element_blank())

p_fish_age_thresholds <- 
  ggplot() +
  geom_bar(
    data = fish_specimen,
    mapping = aes(x = AGE, y = N, fill = factor(SEX, levels = c(2,1,3), labels = c("Female", "Male", "Unsexed"))),
    stat = "identity",
    position = "stack",
    width = 1
  ) +
  geom_vline(data = dplyr::mutate(taxa, SPECIES_CODE = GROUP_CODE),
             mapping = aes(xintercept = MIN_AGE, linetype = "Assessment\nMin. age (shelf)")) +
  geom_vline(data = dplyr::mutate(taxa_slope, SPECIES_CODE = GROUP_CODE),
             mapping = aes(xintercept = MIN_AGE, linetype = "Assessment\nMin. age (slope)")) +
  scale_x_continuous(name = "Age (years)", expand = c(0,0)) +
  scale_y_continuous(name = "Frequency (#)",
                     expand = expansion(mult = c(0, 0.1))) +
  expand_limits(x = -0.5) +
  facet_wrap(~SPECIES_CODE, scales = "free") +
  scale_fill_tableau(name = "Sex") +
  scale_linetype_manual(name = NULL, values = c(2, 3)) +
  theme_bw() +
  theme(strip.background = element_blank())

png(filename = here::here("analysis", "flume_tank_2026", "plots", "fish_size_thresholds.png"), width = 14, height = 7, res = 300, units = "in")
print(p_fish_size_thresholds + 
        theme(
          axis.text = element_text(size = 14),
          axis.title = element_text(size = 16),
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 14),
          strip.text = element_text(size = 14)))
dev.off()

png(filename = here::here("analysis", "flume_tank_2026", "plots", "fish_age_thresholds.png"), width = 14, height = 7, res = 300, units = "in")
print(p_fish_age_thresholds + 
        theme(
          axis.text = element_text(size = 14),
          axis.title = element_text(size = 16),
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 14),
          strip.text = element_text(size = 14)))
dev.off()


crab_taxa <- c("RKC", "BKC", "TANNER", "SNOW")

crab_data <- vector(mode = "list", length = length(crab_taxa))

for(ii in 1:length(crab_taxa)) {
  crab_data[[ii]] <- 
    crabpack::get_specimen_data(
      species = crab_taxa[ii], 
      region = "EBS", 
      year = 2010:2025, 
      channel = "API"
    )  
}


crab_sizes <- 
  do.call(
    what = rbind,
    lapply(crab_data, FUN = function(x) x$specimen)
  ) |>
  dplyr::group_by(SIZE_1MM, SEX, SPECIES) |>
  dplyr::summarise(N = n()) |>
  dplyr::inner_join(
    data.frame(
      SPECIES = c("BKC", "RKC", "SNOW", "TANNER"),
      COMMON_NAME = c("blue king crab", "red king crab", "snow crab", "Tanner crab")
    )
  )

crab_thresholds <- 
  data.frame(
    COMMON_NAME = c("blue king crab", "blue king crab", "red king crab", "red king crab", "red king crab", "snow crab", "Tanner crab"),
    SIZE = c(30, 90, 65, 35, 63.5, 25, 25),
    UNIT = c("PI", "SM", "BB", "PI", "NS", "All", "All")
  )


p_crab_thresholds <- 
  ggplot() +
  geom_bar(
    data = crab_sizes,
    mapping = aes(x = SIZE_1MM, y = N, fill = factor(SEX, levels = c(2,1,3), labels = c("Female", "Male", "Unsexed"))),
    stat = "identity", 
    position = "stack",
    width = 1
  ) +
  geom_vline(
    data = crab_thresholds, 
    mapping = aes(xintercept = SIZE, linetype = "Assessment\nMin. size (shelf/NBS)"),
  ) +
  scale_x_continuous(name = "Size (mm)", expand = c(0,0)) +
  scale_y_continuous(name = "Frequency (#)",
                     expand = expansion(mult = c(0, 0.1))) +
  expand_limits(x = 0) +
  scale_linetype_manual(name = NULL, values = c(2)) +
  scale_fill_tableau(name = "Sex", na.value = "grey") +
  facet_wrap(~COMMON_NAME, scales = "free", nrow = 1) +
  theme_bw() +
  theme(strip.background = element_blank())

png(filename = here::here("analysis", "flume_tank_2026", "plots", "crab_size_thresholds.png"), width = 14, height = 5, res = 300, units = "in")
print(p_crab_thresholds + 
        theme(
          axis.text = element_text(size = 16),
          axis.title = element_text(size = 18),
          legend.text = element_text(size = 16),
          legend.title = element_text(size = 16),
          strip.text = element_text(size = 18)))
dev.off()
