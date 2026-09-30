# Catch comparison results

library(trawlmetrics)
library(akgfmaps)
library(ggthemes)
library(shadowtext)
library(dplyr)
library(ggpp)

# Configuration ------------------------------------------------------------------------------------
# Focal species
cc_species_codes <- 
  data.frame(
    SPECIES_CODE = c(68560, 68580, 68590, 21740, 21720, 10110, 10112, 10210, 10261, 10130, 10285, 10200, 10120, 435, 471, -1, -2, -3),
    COMMON_NAME = 
      c(
        "Tanner crab",
        "snow crab",
        "Snow/Tanner hybrid",
        "walleye pollock", 
        "Pacific cod", 
        "arrowtooth flounder",
        "Kamchatka flounder", 
        "yellowfin sole", 
        "northern rock sole",
        "flathead sole",
        "Alaska plaice",
        "rex sole",
        "Pacific halibut",
        "Bering skate",
        "Alaska skate",
        "jellyfish",
        "Other fish", 
        "Other invertebrates"),
    FAMILY = 
      c(
        rep("Oregoniidae", 3),
        rep("Gadidae", 2),
        rep("Pleuronectidae", 8),
        rep("Rajidae", 2),
        "Scyphozoa/Hydrozoa",
        "Various",
        "Various"
      ),
    GROUP_NAME = 
      c(
        rep("Chinoecetes spp.", 3),
        "walleye pollock", 
        "Pacific cod", 
        rep("Atheresthes spp.", 2),
        "yellowfin sole", 
        "northern rock sole",
        "flathead sole",
        "Alaska plaice",
        "rex sole",
        "Pacific halibut",
        rep("Bathyraja spp.", 2),
        "jellyfish",
        "Other fish",
        "Other invertebrates"
      ),
    FISH_CRAB = c(
      rep("Crab", 3),
      rep("Fish", 12),
      "jellyfish",
      "Fish",
      "Other invertebrates")
  )

# Data prep ----------------------------------------------------------------------------------------

# Matched catch treatments
catch_treatments <- 
  readxl::read_xlsx(
    path = here::here("data", "2026_gear_testing_haul_log.xlsx"),
    sheet = "catch_treatments"
  )

# Review door trials and assign to hauls
channel <- trawlmetrics::get_connected(schema = "AFSC")

catch_records <- RODBC::sqlQuery(
  channel = channel,
  query = "SELECT C.*, H.DISTANCE_FISHED, H.NET_WIDTH, H.NET_HEIGHT, H.DURATION FROM RACEBASE.HAUL H, RACEBASE.CATCH C 
    WHERE C.CRUISE = 202601 
  AND H.HAUL_TYPE = 23
  AND H.PERFORMANCE >= 0
  AND H.HAULJOIN = C.HAULJOIN"
) |>
  dplyr::mutate(AREA_SWEPT_KM2 = DISTANCE_FISHED * NET_WIDTH/1000)

# Get focal species data with gapindex
# fish_codes <- unique(catch_records$SPECIES_CODE[catch_records$SPECIES_CODE < 40000])

catch_data <- 
  gapindex::get_data(
    year_set = 2026,
    spp_codes = cc_species_codes$SPECIES_CODE,
    haul_type = 23,
    abundance_haul = "N",
    pull_lengths = TRUE,
    survey_set = "EBS",
    channel = channel
  )

cc_species_codes <- 
  dplyr::left_join(cc_species_codes, catch_data$species)

cpue_target <- gapindex::calc_cpue(catch_data)

cpue_target$COMMON_NAME <- factor(cpue_target$SPECIES_CODE, levels = cc_species_codes$SPECIES_CODE, labels = cc_species_codes$COMMON_NAME)


# Calculate total catch

total_catch <- 
  catch_records |>
  dplyr::mutate(FISH_CRAB = ifelse(SPECIES_CODE < 40000, "Fish", "Other invertebrates"),
                FISH_CRAB = ifelse(SPECIES_CODE %in% c(68560, 68580, 68590), "Chinoecetes spp.", FISH_CRAB),
                FISH_CRAB = ifelse(SPECIES_CODE >= 40500 & SPECIES_CODE <= 40599, "jellyfish", FISH_CRAB)) |>
  dplyr::mutate() |>
  dplyr::group_by(FISH_CRAB, HAULJOIN, VESSEL, CRUISE, HAUL, AREA_SWEPT_KM2, DISTANCE_FISHED, NET_WIDTH, NET_HEIGHT, DURATION) |>
  dplyr::summarise(
    TOTAL_CATCH_WEIGHT_KG = sum(WEIGHT),
    N_SPECIES = dplyr::n()
  ) |>
  dplyr::ungroup() |>
  dplyr::mutate(CPUE_KGKM2 = TOTAL_CATCH_WEIGHT_KG/AREA_SWEPT_KM2) |>
  dplyr::inner_join(catch_treatments)

total_catch_wide <- 
  total_catch |>
  dplyr::select(
    FISH_CRAB, BLOCK, GEAR_NAME, CPUE_KGKM2, AREA_SWEPT_KM2, NET_WIDTH, NET_HEIGHT, DISTANCE_FISHED, DURATION) |> 
  tidyr::pivot_wider(
    names_from = GEAR_NAME,
    values_from = c(CPUE_KGKM2, AREA_SWEPT_KM2, NET_WIDTH, NET_HEIGHT, DISTANCE_FISHED, DURATION),
    values_fill = 0) |>
  dplyr::mutate( CCR_CPUE_KGKM2 =  CPUE_KGKM2_RRT/(CPUE_KGKM2_RRT+`CPUE_KGKM2_83-112`))

p_mirror_plot <- 
  ggplot() + 
  geom_bar(
    data = dplyr::mutate(
      total_catch, 
      CPUE_KGKM2 = ifelse(GEAR_NAME == "RRT", CPUE_KGKM2, CPUE_KGKM2*-1)
    ),  
    mapping = aes(x = factor(as.numeric(gsub("[^0-9]", "", BLOCK))), y = CPUE_KGKM2, fill = FISH_CRAB), 
    position = "stack", 
    stat = "identity") +
  geom_hline(yintercept = 0) +
  scale_y_continuous(
    name = expression(CPUE*' '*(kg%.%km^1)),
    breaks = seq(-1e5, 1e5, 5e4),
    labels = abs(seq(-1e5, 1e5, 5e4))
  ) +
  geom_text_npc(mapping = aes(label = c("83-112", "RRT"), npcx = c(0.02, 0.02), npcy = c(0.02, 0.98))) +
  scale_fill_brewer(direction = -1, palette = "BrBG") +
  scale_x_discrete(name = "Haul pair") +
  theme_bw() +
  theme(legend.title = element_blank())

png(filename = here::here("plots", "catch_comparison", "cpue_stacked_coarse_taxa.png"), 
    width = 169, height = 80, units = "mm", res = 300)
print(p_mirror_plot)
dev.off()

p_cpue_coarse <- 
  ggplot() + 
  geom_abline(slope = 1, intercept = 0, linetype = 2, color = "grey70") +
  geom_point(
    data = total_catch_wide,
    mapping = aes(
      x = `CPUE_KGKM2_83-112`,
      y = CPUE_KGKM2_RRT,
      color = FISH_CRAB
    )
  ) +
  geom_point(
    data = total_catch |>
      dplyr::group_by(FISH_CRAB) |>
      dplyr::summarise(MAX_CPUE_KGKM2 = max(CPUE_KGKM2)*1.02),
    mapping = aes(x = MAX_CPUE_KGKM2, y = MAX_CPUE_KGKM2),
    color = NA
  ) +
  facet_wrap(~FISH_CRAB, scales = "free") +
  scale_color_brewer(direction = -1, palette = "BrBG") +
  scale_x_continuous(name = expression(CPUE[83-112]*' '*(kg%.%km^-2)), limits = c(0, NA)) +
  scale_y_continuous(name = expression(CPUE[RRT]*' '*(kg%.%km^-2)), limits = c(0, NA)) +
  theme_bw() +
  theme(strip.background = element_blank(),
        legend.title = element_blank(),
        legend.position = "none")

png(filename = here::here("plots", "catch_comparison", "cpue_scatter_coarse_taxa.png"), 
    width = 120, height = 120, units = "mm", res = 300)
print(p_cpue_coarse)
dev.off()


# Biomass/numbers: Focal species calculations ------------------------------------------------------

cpue_target <- 
  dplyr::select(catch_data$haul, VESSEL, HAULJOIN, HAUL, NET_WIDTH, NET_HEIGHT, DISTANCE_FISHED, DURATION) |>
    dplyr::inner_join(cpue_target) |>
    dplyr::inner_join(catch_data$species) |>
    dplyr::inner_join(
      dplyr::select(cc_species_codes, SPECIES_CODE, COMMON_NAME, FAMILY, GROUP_NAME, FISH_CRAB, REPORT_NAME_SCIENTIFIC)) |>
  dplyr::inner_join(catch_treatments)

cpue_comparison_target <- cpue_target |>
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
cpue_all <- dplyr::bind_rows(cpue_target, cpue_misc)
cpue_comparison_all <- dplyr::bind_rows(cpue_comparison_target, cpue_comparison_misc)
cpue_comparison_all_summary <- dplyr::bind_rows(cpue_comparison_target_summary, cpue_comparison_misc_summary)

cpue_all$COMMON_NAME <- factor(cpue_all$COMMON_NAME, levels = cc_species_codes$COMMON_NAME)
cpue_comparison_all$COMMON_NAME <- factor(cpue_comparison_all$COMMON_NAME, levels = cc_species_codes$COMMON_NAME)
cpue_comparison_all_summary$COMMON_NAME <- factor(cpue_comparison_all_summary$COMMON_NAME, levels = cc_species_codes$COMMON_NAME)


# Plot catch results

family_palette <- 
  c("Gadidae" = "#009E73" , "Pleuronectidae" =  "#56B4E9", "Scyphozoa/Hydrozoa" = "#E69F00", "Oregoniidae" = "#D55E00", "Rajidae" = "#0072B2", "Various" = "#000000")

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


png(here::here("plots", "catch_comparison", "cpue_biomass_scatter.png"), 
    width = 169, 
    height = 200,
    units = "mm",
    res = 300)
print(p_biomass_ccr)
dev.off()

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

png(here::here("plots", "catch_comparison", "cpue_numeric_scatter.png"), 
    width = 169, 
    height = 169,
    units = "mm",
    res = 300)
print(p_numeric_ccr)
dev.off()

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

png(here::here("plots", "catch_comparison", "ccr_boxplot.png"), width = 169, height = 169, units = "mm",
    res = 300)
print(p_ccr_boxplot)
dev.off()

# ggplot() +
#   stat_ecdf(
#     data = cpue_comparison_target,
#     mapping = aes(x = CCR_CPUE_NOKM2, group = COMMON_NAME),
#     color = "grey70"
#   ) +
#   stat_ecdf(
#     data = cpue_comparison_target,
#     mapping = aes(x = CCR_CPUE_NOKM2)
#   ) +
#   geom_vline(xintercept = 0.5, linetype = 2) +
#   geom_hline(yintercept = 0.5, linetype = 2) +
#   scale_x_continuous(name = expression('Numeric '*over(CPUE[RRT], CPUE[RRT]+CPUE[83-112])), limits = c(0,1), expand = c(0,0)) +
#   scale_y_continuous(name = "Cumulative proportion of hauls", limits = c(0, 1), expand = c(0,0)) +
#   facet_wrap(~GROUP_NAME, scales = "free") +
#   theme_bw()

# Prep size data

crab_size_data <- read.csv(here::here("data", "06_catch_data", "crab_specimen_2026mod.csv")) |>
  dplyr::mutate(SIZE_5MM = plyr::round_any(SIZE, accuracy = 5, f = floor))

crab_size_freq <- 
  crab_size_data |>
  dplyr::group_by(SIZE_1MM, SPECIES_CODE, HAULJOIN) |>
  dplyr::summarize(FREQUENCY = sum(SAMPLING_FACTOR)) |>
  dplyr::mutate(
    COMMON_NAME = factor(
      SPECIES_CODE, 
      levels = cc_species_codes$SPECIES_CODE, 
      labels = cc_species_codes$COMMON_NAME
    )
  ) |>
  dplyr::inner_join(
    dplyr::select(catch_data$haul, HAULJOIN, DISTANCE_FISHED, NET_WIDTH, VESSEL, CRUISE, HAUL) |>
      dplyr::mutate(
        AREA_SWEPT_KM2 = DISTANCE_FISHED * NET_WIDTH / 1000)
  ) |>
  dplyr::inner_join(catch_treatments) |>
  dplyr::mutate(CPUE_NOKM2 = FREQUENCY/AREA_SWEPT_KM2) |>
  dplyr::inner_join(dplyr::select(cc_species_codes, SPECIES_CODE, REPORT_NAME_SCIENTIFIC, COMMON_NAME, FAMILY, GROUP_NAME, FISH_CRAB)) |>
  dplyr::mutate(
    COMMON_NAME = factor(COMMON_NAME, levels = cc_species_codes$COMMON_NAME),
    SIZE = SIZE_1MM)

agg_cpue_crab <- 
  crab_size_freq |>
  dplyr::group_by(SIZE, SPECIES_CODE, REPORT_NAME_SCIENTIFIC, COMMON_NAME, FAMILY, GROUP_NAME, FISH_CRAB, GEAR_NAME) |>
  dplyr::summarise(
    TOTAL_CPUE_NOKM2 = sum(FREQUENCY)/sum(AREA_SWEPT_KM2)
  )

haul_length_freq <- 
  # Calculate raising factor
  catch_data$size |>
  dplyr::group_by(SPECIES_CODE, HAULJOIN) |>
  dplyr::summarise(N_LENGTHS = sum(FREQUENCY)) |>
  dplyr::inner_join(catch_data$catch) |>
  dplyr::mutate(
    RAISING_FACTOR = NUMBER_FISH/N_LENGTHS) |>
  dplyr::ungroup() |>
  dplyr::select(SPECIES_CODE, HAULJOIN, RAISING_FACTOR) |>
  # Apply raising factor to lengths
  dplyr::inner_join(catch_data$size) |>
  dplyr::mutate(
    TOTAL_FREQUENCY = FREQUENCY * RAISING_FACTOR,
    LENGTH_CM = LENGTH /10,
    SIZE = LENGTH_CM
  ) |>
  # Use haul data to calculate length-based CPUE
  dplyr::inner_join(
    dplyr::select(catch_data$haul, HAULJOIN, DISTANCE_FISHED, NET_WIDTH, VESSEL, CRUISE, HAUL) |>
      dplyr::mutate(AREA_SWEPT_KM2 = DISTANCE_FISHED * NET_WIDTH / 1000)
  ) |>
  dplyr::inner_join(catch_treatments) |>
  dplyr::mutate(CPUE_NOKM2 = TOTAL_FREQUENCY/AREA_SWEPT_KM2) |>
  dplyr::inner_join(dplyr::select(cc_species_codes, SPECIES_CODE, REPORT_NAME_SCIENTIFIC, COMMON_NAME, FAMILY, GROUP_NAME, FISH_CRAB)) |>
  dplyr::mutate(
    COMMON_NAME = factor(
      COMMON_NAME, 
      levels = cc_species_codes$COMMON_NAME)
  )

agg_cpue_fish <- haul_length_freq |>
  dplyr::group_by(SIZE, SPECIES_CODE, REPORT_NAME_SCIENTIFIC, COMMON_NAME, FAMILY, GROUP_NAME, FISH_CRAB, GEAR_NAME) |>
  dplyr::summarise(
    TOTAL_CPUE_NOKM2 = sum(TOTAL_FREQUENCY)/sum(AREA_SWEPT_KM2)
  )


agg_cpue_target  <- 
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
      agg_cpue_target, 
      FLIPPED_CPUE_NOKM2 = ifelse(GEAR_NAME == "83-112", TOTAL_CPUE_NOKM2*-1, TOTAL_CPUE_NOKM2)),
    mapping = aes(x = SIZE, y = FLIPPED_CPUE_NOKM2, fill = GEAR_NAME),
    stat = "identity",
    width = rel(1)
  ) +
  geom_point(data = dplyr::group_by(agg_cpue_target, COMMON_NAME) |>
               dplyr::summarise(MAX_CPUE_NOKM2 = max(TOTAL_CPUE_NOKM2),
                                MIN_SIZE = min(SIZE)),
             mapping = aes(x = MIN_SIZE, y = MAX_CPUE_NOKM2*1.04),
             color = NA) +
  geom_point(data = dplyr::group_by(agg_cpue_target, COMMON_NAME) |>
               dplyr::summarise(MAX_CPUE_NOKM2 = max(TOTAL_CPUE_NOKM2),
                                MIN_SIZE = min(SIZE)),
             mapping = aes(x = MIN_SIZE, y = -1*MAX_CPUE_NOKM2*1.04),
             color = NA) +
  geom_hline(yintercept = 1) +
  scale_fill_tableau(name = "Gear") +
  scale_color_tableau(name = "Gear") +
  scale_x_continuous(name = "Size") +
  scale_y_continuous(name = expression('Aggregate CPUE '*' '*('#'%.%km^-2)), labels = abs, expand = c(0,0)) +
  facet_wrap(~COMMON_NAME, ncol = 3, scales = "free") +
  theme_bw() + 
  theme(strip.background = element_blank(),
        panel.spacing = unit(1, unit = "mm"),
        strip.text = element_text(size = 9, face = "bold"),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 8))

png(here::here("plots", "catch_comparison", "agg_size_comp.png"), width = 169, height = 169, units = "mm",
    res = 300)
print(p_agg_size_comp)
dev.off()


# Weighted ECDF

weighted_ecdf <- 
  data.frame(
    SIZE = 
      rep(agg_cpue_target$SIZE, agg_cpue_target$TOTAL_CPUE_NOKM2),
    SPECIES_CODE =
      rep(agg_cpue_target$SPECIES_CODE, agg_cpue_target$TOTAL_CPUE_NOKM2),
    COMMON_NAME = 
      rep(agg_cpue_target$COMMON_NAME, agg_cpue_target$TOTAL_CPUE_NOKM2),
    GEAR_NAME = 
      rep(agg_cpue_target$GEAR_NAME, agg_cpue_target$TOTAL_CPUE_NOKM2)
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

png(here::here("plots", "catch_comparison", "ecdf_size_by_gear.png"), 
    width = 169, 
    height = 169, 
    units = "mm",
    res = 300)
print(p_ecdf_size_by_gear)
dev.off()


# Crab sub 60

crab_40_60_freq <- 
  crab_size_data |>
  dplyr::filter(SIZE <= 60 & SIZE >=40) |>
  dplyr::group_by(HAULJOIN) |>
  dplyr::summarize(FREQUENCY = sum(SAMPLING_FACTOR)) |>
  dplyr::inner_join(
    dplyr::select(catch_data$haul, HAULJOIN, DISTANCE_FISHED, NET_WIDTH, VESSEL, CRUISE, HAUL) |>
      dplyr::mutate(
        AREA_SWEPT_KM2 = DISTANCE_FISHED * NET_WIDTH / 1000)
  ) |>
  dplyr::inner_join(catch_treatments) |>
  dplyr::mutate(CPUE_NOKM2 = FREQUENCY/AREA_SWEPT_KM2)

crab_40_60_stats <- 
  crab_40_60_freq |>
  dplyr::group_by(GEAR_NAME) |>
  dplyr::summarise(
    MEAN_CPUE_NOKM2 = mean(CPUE_NOKM2),
    SD_CPUE_NOKM2 = sd(CPUE_NOKM2),
    N = n()
  ) |>
  dplyr::mutate(SE_CPUE_NOKM2 = SD_CPUE_NOKM2/sqrt(N))

t.test(
  crab_40_60_freq$CPUE_NOKM2[crab_40_60_freq$GEAR_NAME == "RRT"],
  crab_40_60_freq$CPUE_NOKM2[crab_40_60_freq$GEAR_NAME == "83-112"],
  paired = TRUE,
  var.equal = FALSE
)

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

png(filename = here::here("plots", "catch_comparison", "cpue_chinoecetes_40_60.png"), width = 80, height = 80, units = "mm", res = 300)
print(p_crab_40_60)
dev.off()


# Catch attributed to bridle herding (Somerton and Munro, 2001)


data.frame(
  SPECIES_CODE = c(10261, 10210, 10130, 10200),
  h = c(0.84, 0.58, 0.51, 0.502),
  w_d = 58.7,
  w_n = 17.3,
  w_off = 21.8
) |>
  dplyr::mutate(
    w_on = w_d - w_n - w_off,
    p_bridles = 1+(h*w_on)/(w_n + h*w_on)
  )





# Extra plots

ggplot() +
  stat_ecdf(
    data = cpue_comparison_target,
    mapping = aes(x = CCR_CPUE_KGKM2)
  ) +
  geom_rug(
    data = cpue_comparison_target,
    mapping = aes(x = CCR_CPUE_KGKM2)
  ) +
  geom_vline(xintercept = 0.5, linetype = 2) +
  geom_hline(yintercept = 0.5, linetype = 2) +
  scale_x_continuous(name = expression(over(CPUE[RRT], CPUE[RRT]+CPUE[83-112])), limits = c(0,1), expand = c(0,0)) +
  scale_y_continuous(name = "Cumulative proportion", limits = c(0, 1)) +
  facet_wrap(~COMMON_NAME, scales = "free") +
  theme_bw()

ggplot() +
  geom_histogram(
    data = cpue_comparison_target,
    mapping = aes(x = CCR_CPUE_KGKM2, fill = FAMILY),
    breaks = seq(0,1,0.1)
  ) +
  geom_text_npc(
    data = cpue_comparison_target_summary,
    mapping = 
      aes(
        label = paste0("CCR: ", format(round(MEAN_CCR_CPUE_KGKM2, 2), nsmall = 2), "\n", 
                       "RE:", format(round(MEAN_RELEFF_CPUE_KGKM2, 2), nsmall = 2)), 
        npcx = 0.98, 
        npcy = 0.98),
    hjust = 1
  ) +
  geom_vline(xintercept = 0.5, linetype = 2) +
  scale_x_continuous(name = expression('Biomass catch comparison rate, '*over(CPUE[RRT], CPUE[RRT]+CPUE[83-112])), limits = c(0,1), expand = c(0,0)) +
  scale_y_continuous(name = "Frequency (hauls)") +
  scale_fill_manual(name = "Family", values = colorblind_pal()(5)[2:5]) +
  facet_wrap(~COMMON_NAME, scales = "free", ncol = 3) +
  theme_bw() +
  theme(strip.background = element_blank())