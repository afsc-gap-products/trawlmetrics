# Catch comparison results

library(trawlmetrics)
library(akgfmaps)
library(ggthemes)
library(shadowtext)
library(dplyr)
library(ggpp)

# Matched catch treatments
catch_treatments <- 
  readxl::read_xlsx(
    path = here::here("data", "2026_gear_testing_haul_log.xlsx"),
    sheet = "catch_treatments"
  )

# Species taxonomy
cc_species_codes <- readRDS(here::here("data", "analysis_species_taxonomy.rds"))

catch_records <- readRDS(here::here("data", "06_catch_data", "catch_records.rds"))
catch_data <- readRDS(here::here("data", "06_catch_data", "catch_gapindex.rds"))


# Biomass/numbers: Focal species calculations ------------------------------------------------------

cpue_focal <- readRDS(here::here("data", "cpue_focal.rds"))

cpue_comparison_target <- cpue_focal |>
  dplyr::select(SPECIES_CODE, COMMON_NAME, REPORT_NAME_SCIENTIFIC, FAMILY, GROUP_NAME, FISH_CRAB, GEAR_NAME, BLOCK, CPUE_KGKM2, CPUE_NOKM2, AREA_SWEPT_KM2, NET_WIDTH, NET_HEIGHT, DISTANCE_FISHED, DURATION) |>
  tidyr::pivot_wider(
    names_from = GEAR_NAME,
    values_from = c(CPUE_KGKM2, CPUE_NOKM2, AREA_SWEPT_KM2, NET_WIDTH, NET_HEIGHT, DISTANCE_FISHED, DURATION),
    values_fill = 0
  ) |>
  dplyr::filter( !(CPUE_NOKM2_RRT == 0 & `CPUE_NOKM2_83-112` == 0) ) |>
  dplyr::mutate( 
    CCR_CPUE_KGKM2 = CPUE_KGKM2_RRT/(CPUE_KGKM2_RRT+`CPUE_KGKM2_83-112`),
    CCR_CPUE_NOKM2 = CPUE_NOKM2_RRT/(CPUE_NOKM2_RRT+`CPUE_NOKM2_83-112`)
    )

cpue_comparison_target_summary <-
  cpue_comparison_target |>
  dplyr::group_by(REPORT_NAME_SCIENTIFIC, COMMON_NAME, FAMILY, GROUP_NAME, FISH_CRAB, SPECIES_CODE) |>
  dplyr::summarise(
    MEAN_CCR_CPUE_KGKM2 = mean(CCR_CPUE_KGKM2),
    MEAN_CCR_CPUE_NOKM2 = mean(CCR_CPUE_NOKM2)
  ) |>
  dplyr::mutate(
    MEAN_RELEFF_CPUE_KGKM2 = MEAN_CCR_CPUE_KGKM2/(1-MEAN_CCR_CPUE_KGKM2),
    MEAN_RELEFF_CPUE_NOKM2 = MEAN_CCR_CPUE_NOKM2/(1-MEAN_CCR_CPUE_NOKM2)
  )

# Biomass/numbers: Other species calculation -------------------------------------------------------
# Calculate CPUE, catch comparison rate, and relative efficiency jellyfish, misc. fish, and misc. inverts

cpue_misc  <- 
  catch_records |>
  dplyr::filter(!(SPECIES_CODE %in% cc_species_codes$SPECIES_CODE)) |>
  dplyr::mutate(
    COMMON_NAME = ifelse(SPECIES_CODE < 40000, "Other fish", "Other invertebrates"),
    COMMON_NAME = ifelse(SPECIES_CODE >= 40500 & SPECIES_CODE <= 40599, "jellyfish", COMMON_NAME)
  ) |>
  dplyr::group_by(COMMON_NAME, HAULJOIN, VESSEL, CRUISE, HAUL, AREA_SWEPT_KM2, DISTANCE_FISHED, NET_WIDTH, NET_HEIGHT, DURATION) |>
  dplyr::summarise(
    WEIGHT_KG = sum(WEIGHT),
    N_SPECIES = dplyr::n()
  ) |>
  dplyr::ungroup() |>
  dplyr::mutate(CPUE_KGKM2 = WEIGHT_KG/AREA_SWEPT_KM2) |>
  dplyr::inner_join(catch_treatments) |>
  dplyr::inner_join(cc_species_codes)

cpue_misc$COMMON_NAME = factor(cpue_misc$COMMON_NAME, levels = cc_species_codes$COMMON_NAME)

cpue_comparison_misc <- cpue_misc |>
  dplyr::select(COMMON_NAME, FAMILY, GROUP_NAME, FISH_CRAB, GEAR_NAME, BLOCK, CPUE_KGKM2, AREA_SWEPT_KM2, NET_WIDTH, NET_HEIGHT, DISTANCE_FISHED, DURATION) |>
  tidyr::pivot_wider(
    names_from = GEAR_NAME,
    values_from = c(CPUE_KGKM2, AREA_SWEPT_KM2, NET_WIDTH, NET_HEIGHT, DISTANCE_FISHED, DURATION),
    values_fill = 0
  ) |>
  dplyr::mutate( 
    CCR_CPUE_KGKM2 = CPUE_KGKM2_RRT/(CPUE_KGKM2_RRT+`CPUE_KGKM2_83-112`)
  )

cpue_comparison_misc_summary <-
  cpue_comparison_misc |>
  dplyr::group_by(COMMON_NAME, FAMILY, GROUP_NAME, FISH_CRAB) |>
  dplyr::summarise(
    MEAN_CCR_CPUE_KGKM2 = mean(CCR_CPUE_KGKM2)
  ) |>
  dplyr::mutate(
    MEAN_RELEFF_CPUE_KGKM2 = MEAN_CCR_CPUE_KGKM2/(1-MEAN_CCR_CPUE_KGKM2)
  )


# Biomass/numbers: Catch comparison rate -----------------------------------------------------------
cpue_all <- dplyr::bind_rows(cpue_focal, cpue_misc)
cpue_comparison_all <- dplyr::bind_rows(cpue_comparison_target, cpue_comparison_misc)
cpue_comparison_all_summary <- dplyr::bind_rows(cpue_comparison_target_summary, cpue_comparison_misc_summary)

cpue_all$COMMON_NAME <- factor(cpue_all$COMMON_NAME, levels = cc_species_codes$COMMON_NAME)
cpue_comparison_all$COMMON_NAME <- factor(cpue_comparison_all$COMMON_NAME, levels = cc_species_codes$COMMON_NAME)
cpue_comparison_all_summary$COMMON_NAME <- factor(cpue_comparison_all_summary$COMMON_NAME, levels = cc_species_codes$COMMON_NAME)


# Plot catch results

family_palette <- 
  c(
    "Gadidae" = "#009E73" , 
    "Pleuronectidae" =  "#56B4E9", 
    "Scyphozoa/Hydrozoa" = "#E69F00", 
    "Oregoniidae" = "#D55E00", 
    "Rajidae" = "#0072B2", 
    "Various" = "#000000"
  )

p_biomass_ccr <- 
  ggplot() +
  geom_abline(slope = 1, intercept = 2, linetype = 2, color = "grey60") +
  geom_point(
    data = cpue_comparison_all,
    mapping = aes(x = `CPUE_KGKM2_83-112`, y = CPUE_KGKM2_RRT, color = FAMILY),
    size = rel(2.2),
    alpha = 0.5
  ) +
  geom_point(
    data = cpue_all |>
      dplyr::group_by(COMMON_NAME) |>
      dplyr::summarise(MAX_CPUE_KGKM2 = max(CPUE_KGKM2)*1.02,
                       MAX_CPUE_NOKM2 = max(CPUE_NOKM2)*1.02),
    mapping = aes(x = MAX_CPUE_KGKM2, y = MAX_CPUE_KGKM2) ,
    color = NA
  ) +
  geom_text_npc(
    data = cpue_comparison_all_summary,
    mapping = 
      aes(
        label = paste0("CCR: ", format(round(MEAN_CCR_CPUE_KGKM2, 2), nsmall = 2), "\n", 
                       "RCE:", format(round(MEAN_RELEFF_CPUE_KGKM2, 2), nsmall = 2)), 
        npcx = 0.02, 
        npcy = 0.98),
    hjust = 0, 
    size = 2.2
  ) +
  scale_x_continuous(
    name = expression(CPUE[83-112]*' '*(kg%.%km^-2)), limits = c(0, NA)
    ) +
  scale_y_continuous(name = expression(CPUE[RRT]*' '*(kg%.%km^-2)), limits = c(0, NA)) +
  scale_color_manual(name = "Classification", values = family_palette) +
  facet_wrap(~COMMON_NAME, scales = "free", ncol = 3) +
  theme_bw() +
  theme(strip.text = element_text(face = "bold", size = 8),
        strip.background = element_blank(),
        legend.position = "bottom",
        axis.text = element_text(size = 7),
        axis.title = element_text(size = 9),
        panel.spacing = unit(0.5, units = "mm"),
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 9))

ggsave(
  p_biomass_ccr, 
  filename = here::here("plots", "catch_comparison", "cpue_biomass_scatter.png"), 
  width = 169, 
  height = 200,
  units = "mm",
  dpi = 300
)

p_numeric_ccr <- 
  ggplot() +
  geom_abline(slope = 1, intercept = 2, linetype = 2, color = "grey60") +
  geom_point(
    data = dplyr::filter(cpue_comparison_all, SPECIES_CODE > 0),
    mapping = aes(x = `CPUE_NOKM2_83-112`, y = CPUE_NOKM2_RRT, color = FAMILY),
    size = rel(2.2),
    alpha = 0.5
  ) +
  geom_point(
    data = dplyr::filter(cpue_all, SPECIES_CODE > 0) |>
      dplyr::group_by(COMMON_NAME) |>
      dplyr::summarise(MAX_CPUE_KGKM2 = max(CPUE_KGKM2)*1.02,
                       MAX_CPUE_NOKM2 = max(CPUE_NOKM2)*1.02),
    mapping = aes(x = MAX_CPUE_NOKM2, y = MAX_CPUE_NOKM2) ,
    color = NA
  ) +
  geom_text_npc(
    data = dplyr::filter(cpue_comparison_all_summary, SPECIES_CODE > 0),
    mapping = 
      aes(
        label = paste0("CCR: ", format(round(MEAN_CCR_CPUE_NOKM2, 2), nsmall = 2), "\n", 
                       "RE:", format(round(MEAN_RELEFF_CPUE_NOKM2, 2), nsmall = 2)), 
        npcx = 0.02, 
        npcy = 0.98),
    hjust = 0, 
    size = 2.2
  ) +
  scale_x_continuous(name = expression(CPUE[83-112]*' '*('#'%.%km^-2)), limits = c(0, NA)) +
  scale_y_continuous(name = expression(CPUE[RRT]*' '*('#'%.%km^-2)), limits = c(0, NA)) +
  scale_color_manual(name = "Classification", values = family_palette) +
  facet_wrap(~COMMON_NAME, scales = "free", ncol = 3) +
  theme_bw() +
  theme(strip.text = element_text(face = "bold"),
        strip.background = element_blank(),
        legend.position = "bottom") +
  theme(strip.text = element_text(face = "bold", size = 8),
        strip.background = element_blank(),
        legend.position = "bottom",
        axis.text = element_text(size = 7),
        axis.title = element_text(size = 9),
        panel.spacing = unit(0.5, units = "mm"),
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 9))

ggsave(
  p_numeric_ccr, 
  filename = here::here("plots", "catch_comparison", "cpue_numeric_scatter.png"), 
  width = 169, 
  height = 200,
  units = "mm",
  dpi = 300
)

p_ccr_boxplot <- 
  ggplot() +
  geom_vline(xintercept = 0.5, linetype = 2) +
  geom_boxplot(
    data = cpue_comparison_all,
    mapping = aes(x = CCR_CPUE_KGKM2, y = COMMON_NAME, fill = FAMILY, color = FAMILY),
    alpha = 0.3
  ) +
  geom_text(
    data = cpue_comparison_all_summary,
    mapping =
      aes(
        label = paste0("RCE: ", format(round(MEAN_RELEFF_CPUE_KGKM2, 2), nsmall = 2)),
        x = -0.3,
        y = COMMON_NAME),
    hjust = 0,
    size = 3.5
  ) +
  scale_x_continuous(
    name = "Catch comparison rate (biomass)", 
    expand = c(0,0), 
    breaks = seq(0, 1, 0.25),
    limits = c(-0.35, 1.02)
  ) +
  scale_y_discrete(limits = rev) +
  scale_fill_manual(name = "Classification", values = family_palette) +
  scale_color_manual(name = "Classification", values = family_palette) +
  theme_bw() +
  theme(
    strip.background = element_blank(),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 8),
        axis.title.x = element_text(size = 9),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line()
  )

ggsave(
  p_ccr_boxplot, 
  filename = here::here("plots", "catch_comparison", "ccr_boxplot.png"), 
  width = 169, 
  height = 200,
  units = "mm",
  dpi = 300
)

# Prep size data -----------------------------------------------------------------------------------

crab_size_freq <- readRDS(here::here("data", "06_catch_data", "size_freq_crab.rds")) |>
  dplyr::filter(SEX == 3)

crab_size_data <- readRDS(here::here("data", "06_catch_data", "crab_size_data.rds"))

fish_length_data <- readRDS(here::here("data", "06_catch_data", "fish_size_data.rds"))

fish_length_freq <- readRDS(here::here("data", "06_catch_data", "size_freq_fish.rds"))

# Summary table for size-frequency composition ----

crab_size_table_by_sex <- 
  dplyr::select(catch_data$haul, HAULJOIN, VESSEL, CRUISE, HAUL) |>
  dplyr::inner_join(catch_treatments) |>
  dplyr::inner_join(crab_size_data) |>
  dplyr::group_by(SPECIES_CODE, SEX, GEAR_NAME) |>
  dplyr::summarise(
    MEAN_SIZE = mean(SIZE_1MM, w = SAMPLING_FACTOR),
    MEDIAN_SIZE = median(SIZE_1MM, w = SAMPLING_FACTOR),
    MIN_SIZE = min(SIZE_1MM),
    MAX_SIZE = max(SIZE_1MM),
    N = n()
  )

crab_size_table <- 
  dplyr::select(catch_data$haul, HAULJOIN, VESSEL, CRUISE, HAUL) |>
  dplyr::inner_join(catch_treatments) |>
  dplyr::inner_join(crab_size_data) |>
  dplyr::group_by(SPECIES_CODE, GEAR_NAME) |>
  dplyr::summarise(
    MEAN_SIZE = mean(SIZE_1MM, w = SAMPLING_FACTOR),
    MEDIAN_SIZE = median(SIZE_1MM, w = SAMPLING_FACTOR),
    MIN_SIZE = min(SIZE_1MM),
    MAX_SIZE = max(SIZE_1MM),
    N = n()
  )

fish_size_table <- 
  fish_length_data |>
  dplyr::group_by(SPECIES_CODE, GEAR_NAME) |>
  dplyr::summarise(
    N = sum(FREQUENCY),
    MEAN_SIZE = mean(LENGTH_CM, w = SAMPLING_FACTOR),
    MEDIAN_SIZE = median(LENGTH_CM, w = SAMPLING_FACTOR),
    MIN_SIZE = min(LENGTH_CM),
    MAX_SIZE = max(LENGTH_CM)
  )

size_summary <-
  dplyr::bind_rows(
    crab_size_table,
    fish_size_table,
    crab_size_table_by_sex
  ) |>
  dplyr::inner_join(
    cc_species_codes
  ) |>
  dplyr::mutate(
    SEX = case_when(
      SEX == 1 ~ "M",
      SEX == 2 ~ "F",
      SEX == 4 ~ "I",
      is.na(SEX) ~"All"
    )
  ) |>
  dplyr::ungroup() |>
  dplyr::mutate(MEAN_SIZE = sprintf("%.1f", MEAN_SIZE),
                RANGE_SIZE =  sprintf("%.0f-%.0f", MIN_SIZE, MAX_SIZE)) |>
  dplyr::mutate(COMMON_NAME = factor(COMMON_NAME, cc_species_codes$COMMON_NAME))

size_table <- size_summary |>
  dplyr::select(COMMON_NAME, SEX, GEAR_NAME, MEAN_SIZE, N, RANGE_SIZE) |>
  tidyr::pivot_wider(values_from = c(N, MEAN_SIZE, RANGE_SIZE), names_from = "GEAR_NAME") |>
  dplyr::filter(!is.na(SEX)) |>
  dplyr::arrange(COMMON_NAME)

write.csv(size_table, 
          file = here::here("plots", "catch_comparison", "size_summary_table.csv"), 
          row.names = FALSE)

# Numerical CPUE for 40-60 mm crab ----

crab_40_60_freq <- 
  crab_size_freq |>
  dplyr::filter(SIZE <= 60 & SIZE >=40) |>
  dplyr::group_by(HAULJOIN, DISTANCE_FISHED, NET_WIDTH, VESSEL, CRUISE, HAUL, GEAR_NAME, BLOCK) |>
  dplyr::summarise(CPUE_NOKM2 = sum(CPUE_NOKM2), .groups = "drop")

crab_40_60_stats <- 
  crab_40_60_freq |>
  dplyr::group_by(GEAR_NAME) |>
  dplyr::summarise(
    MEAN_CPUE_NOKM2 = mean(CPUE_NOKM2),
    SD_CPUE_NOKM2 = sd(CPUE_NOKM2),
    N = n()
  ) |>
  dplyr::mutate(SE_CPUE_NOKM2 = SD_CPUE_NOKM2/sqrt(N))

crab_40_60_paired <- 
  crab_40_60_freq |>
  dplyr::select(BLOCK, CPUE_NOKM2, GEAR_NAME) |>
  tidyr::pivot_wider(
    values_from = CPUE_NOKM2,
    names_from = GEAR_NAME,
    names_prefix = "CPUE_NOKM2_",
    values_fill = 0)

p_crab_40_60 <- 
  ggplot() +
  geom_abline(slope = 1, intercept = 0, linetype = 2, color = "grey60") +
  geom_point(
    data = crab_40_60_paired,
    mapping = aes(
      x = `CPUE_NOKM2_83-112`,
      y = CPUE_NOKM2_RRT),
    size = 1.1
  ) +
  scale_x_continuous(name = expression(CPUE[83-112]*' '*('#'%.%km^-2)), limits = c(0, max(crab_40_60_freq$CPUE_NOKM2))) +
  scale_y_continuous(name = expression(CPUE[RRT]*' '*('#'%.%km^-2)), limits = c(0, max(crab_40_60_freq$CPUE_NOKM2))) +
  facet_wrap(~"Chinoecetes spp. (40-60 mm)") +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 9, face = "bold"),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 8),
        plot.margin = ggplot2::unit(c(2, 5, 2, 2), units = "mm"))

ggsave(
  p_crab_40_60, 
  filename = here::here("plots", "catch_comparison", "cpue_chinoecetes_40_60.png"), 
  width = 80, 
  height = 80, 
  units = "mm", 
  dpi = 300
)

# Total CPUE 

agg_cpue_crab <-
  crab_size_freq |>
  dplyr::group_by(SIZE, SPECIES_CODE, REPORT_NAME_SCIENTIFIC, COMMON_NAME, FAMILY, GROUP_NAME, FISH_CRAB, GEAR_NAME) |>
  dplyr::summarise(
    TOTAL_CPUE_NOKM2 = sum(FREQUENCY)/sum(AREA_SWEPT_KM2)
  )

agg_cpue_fish <- fish_length_freq |>
  dplyr::group_by(SIZE, SPECIES_CODE, REPORT_NAME_SCIENTIFIC, COMMON_NAME, FAMILY, GROUP_NAME, FISH_CRAB, GEAR_NAME) |>
  dplyr::summarise(
    TOTAL_CPUE_NOKM2 = mean(TOTAL_FREQUENCY/AREA_SWEPT_KM2)
  )

agg_cpue_focal_filled  <- 
  dplyr::bind_rows(agg_cpue_fish, agg_cpue_crab) |> 
  tidyr::pivot_wider(
    values_from = TOTAL_CPUE_NOKM2,
    names_from = GEAR_NAME,
    values_fill = 1e-3) |>
  tidyr::pivot_longer(
    cols = c(RRT, `83-112`),
    names_to = "GEAR_NAME",
    values_to = "TOTAL_CPUE_NOKM2"
  ) |>
  dplyr::mutate(COMMON_NAME = factor(COMMON_NAME, levels = cc_species_codes$COMMON_NAME))


p_agg_size_comp <-
  ggplot() +
  geom_bar(
    data = dplyr::mutate(
      agg_cpue_focal_filled, 
      FLIPPED_CPUE_NOKM2 = ifelse(GEAR_NAME == "83-112", TOTAL_CPUE_NOKM2*-1, TOTAL_CPUE_NOKM2)),
    mapping = aes(x = SIZE, y = FLIPPED_CPUE_NOKM2, fill = GEAR_NAME),
    stat = "identity",
    width = 1,
    alpha = 0.7
  ) +
  geom_point(data = dplyr::group_by(agg_cpue_focal_filled, COMMON_NAME) |>
               dplyr::summarise(MAX_CPUE_NOKM2 = max(TOTAL_CPUE_NOKM2),
                                MIN_SIZE = min(SIZE)),
             mapping = aes(x = MIN_SIZE, y = MAX_CPUE_NOKM2*1.25),
             color = NA) +
  geom_point(data = dplyr::group_by(agg_cpue_focal_filled, COMMON_NAME) |>
               dplyr::summarise(MAX_CPUE_NOKM2 = max(TOTAL_CPUE_NOKM2),
                                MIN_SIZE = min(SIZE)),
             mapping = aes(x = MIN_SIZE, y = -1*MAX_CPUE_NOKM2*1.25),
             color = NA) +
  geom_text_npc(
    data = dplyr::filter(size_summary, GEAR_NAME == "83-112" & SEX == "All"),
    mapping = aes(
      npcx = 0.02, 
      npcy = 0.04, 
      label = paste0(MEAN_SIZE," (", MIN_SIZE, "-", MAX_SIZE, ")"),
      color = GEAR_NAME),
    size = 2.4,
    fontface = "bold") +
  geom_text_npc(
    data = dplyr::filter(size_summary, GEAR_NAME == "RRT" & SEX == "All"),
    mapping = aes(
      npcx = 0.02, 
      npcy = 0.96, 
      label = paste0(MEAN_SIZE," (", MIN_SIZE, "-", MAX_SIZE, ")"),
      color = GEAR_NAME),
    size = 2.4,
    fontface = "bold") +
  geom_hline(yintercept = 1, linewidth = 0.1) +
  scale_fill_tableau(name = "Gear") +
  scale_color_tableau(name = "Gear") +
  scale_x_continuous(name = "Size") +
  scale_y_continuous(name = expression('Mean CPUE '*' '*('#'%.%km^-2)), labels = abs, expand = c(0,0)) +
  facet_wrap(~COMMON_NAME, ncol = 3, scales = "free") +
  theme_bw() + 
  theme(strip.background = element_blank(),
        panel.spacing = unit(1, unit = "mm"),
        strip.text = element_text(size = 9, face = "bold"),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 8),
        panel.grid = element_line(linewidth = 0.1, color = "grey85"))

ggsave(
  p_agg_size_comp, 
  filename = here::here("plots", "catch_comparison", "agg_size_comp.png"), 
  width = 169, 
  height = 169, 
  units = "mm", 
  dpi = 300
)


# Weighted ECDF

weighted_ecdf <- 
  data.frame(
    SIZE = 
      rep(agg_cpue_focal_filled$SIZE, agg_cpue_focal_filled$TOTAL_CPUE_NOKM2),
    SPECIES_CODE =
      rep(agg_cpue_focal_filled$SPECIES_CODE, agg_cpue_focal_filled$TOTAL_CPUE_NOKM2),
    COMMON_NAME = 
      rep(agg_cpue_focal_filled$COMMON_NAME, agg_cpue_focal_filled$TOTAL_CPUE_NOKM2),
    GEAR_NAME = 
      rep(agg_cpue_focal_filled$GEAR_NAME, agg_cpue_focal_filled$TOTAL_CPUE_NOKM2)
  ) |>
  dplyr::mutate(
    COMMON_NAME = factor(COMMON_NAME, levels = cc_species_codes$COMMON_NAME)
  )

p_ecdf_size_by_gear <- 
  ggplot() +
  stat_ecdf(
    data = weighted_ecdf,
    mapping = aes(x = SIZE, color = GEAR_NAME),
    geom = "step"
  ) +
  scale_x_continuous(name = "Size") +
  scale_color_tableau(name = "Gear") +
  scale_y_continuous(name = "Cumulative proportion") + 
  facet_wrap(~COMMON_NAME, ncol = 3, scales = "free") +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 9, face = "bold"),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 8))

size_quantiles <- 
  weighted_ecdf |>
  dplyr::group_by(GEAR_NAME, COMMON_NAME) |>
  dplyr::summarise(
    MEDIAN_SIZE = median(SIZE),
    MEAN_SIZE = mean(SIZE),
    MIN_SIZE = min(SIZE),
    MAX_SIZE = max(SIZE),
    Q_25 = quantile(SIZE, 0.25),
    Q_75 = quantile(SIZE, 0.75),
    Q_025 = quantile(SIZE, 0.025),
    Q_975 = quantile(SIZE, 0.975)
  ) |>
  dplyr::inner_join(
    cc_species_codes
  ) |>
  dplyr::mutate(
    COMMON_NAME = factor(COMMON_NAME, levels = cc_species_codes$COMMON_NAME)
  )

ggsave(
  p_ecdf_size_by_gear, 
  filename = here::here("plots", "catch_comparison", "ecdf_size_by_gear.png"), 
  width = 169, 
  height = 169, 
  units = "mm", 
  dpi = 300
)

