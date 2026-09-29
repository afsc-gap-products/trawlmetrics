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

# Calculate total catch

total_catch <- 
  catch_records |>
  dplyr::mutate(FISH = ifelse(SPECIES_CODE < 40000, "Fish", "Other invertebrate"),
                FISH = ifelse(SPECIES_CODE %in% c(68560, 68580, 68590), "Chinoecetes spp.", FISH),
                FISH = ifelse(SPECIES_CODE >= 40500 & SPECIES_CODE <= 40599, "jellyfish", FISH)) |>
  dplyr::mutate() |>
  dplyr::group_by(FISH, HAULJOIN, VESSEL, CRUISE, HAUL, AREA_SWEPT_KM2, DISTANCE_FISHED, NET_WIDTH, NET_HEIGHT, DURATION) |>
  dplyr::summarise(
    TOTAL_CATCH_WEIGHT_KG = sum(WEIGHT),
    N_SPECIES = dplyr::n()
  ) |>
  dplyr::ungroup() |>
  dplyr::mutate(TOTAL_CPUE_KGKM2 = TOTAL_CATCH_WEIGHT_KG/AREA_SWEPT_KM2) |>
  dplyr::inner_join(catch_treatments)

ggplot() + 
  geom_bar(
    data = dplyr::mutate(
      total_catch, 
      TOTAL_CPUE_KGKM2 = ifelse(GEAR_NAME == "RRT", TOTAL_CPUE_KGKM2, TOTAL_CPUE_KGKM2*-1)
    ),  
    mapping = aes(x = factor(as.numeric(gsub("[^0-9]", "", BLOCK))), y = TOTAL_CPUE_KGKM2, fill = FISH), 
    position = "stack", 
    stat = "identity") +
  geom_hline(yintercept = 0) +
  scale_y_continuous(
    name = expression(CPUE*' '*(kg%.%km^1)),
    breaks = seq(-1e5, 1e5, 5e4),
    labels = abs(seq(-1e5, 1e5, 5e4))
  ) +
  geom_text_npc(mapping = aes(label = c("83-112", "RRT"), npcx = c(0.02, 0.02), npcy = c(0.02, 0.98))) +
  scale_fill_brewer(direction = -1) +
  scale_x_discrete(name = "Haul pair") +
  theme_bw() +
  theme(legend.title = element_blank())

ggplot() + 
  geom_bar(
    data = total_catch, 
    mapping = aes(x = factor(as.numeric(gsub("[^0-9]", "", BLOCK))), y = TOTAL_CPUE_KGKM2, fill = GEAR_NAME), 
    position = "dodge",
    stat = "identity") +
  scale_fill_brewer(direction = -1) +
  scale_x_discrete(name = "Haul pair") +
  theme_bw() +
  facet_wrap(~FISH, nrow = 3, scales = "free") +
  theme(legend.title = element_blank())

total_catch_wide <- 
  total_catch |>
  dplyr::select(
    FISH, BLOCK, GEAR_NAME, TOTAL_CPUE_KGKM2, AREA_SWEPT_KM2, NET_WIDTH, NET_HEIGHT, DISTANCE_FISHED, DURATION) |> 
  tidyr::pivot_wider(
    names_from = GEAR_NAME,
    values_from = c(TOTAL_CPUE_KGKM2, AREA_SWEPT_KM2, NET_WIDTH, NET_HEIGHT, DISTANCE_FISHED, DURATION),
    values_fill = 0) |>
  dplyr::mutate( CCR_CPUE_KGKM2 =  TOTAL_CPUE_KGKM2_RRT/(TOTAL_CPUE_KGKM2_RRT+`TOTAL_CPUE_KGKM2_83-112`))


ggplot() +
  stat_ecdf(
    data = total_catch_wide,
    mapping = aes(x = CCR_CPUE_KGKM2, color = FISH),
  ) +
  geom_text_npc(mapping = aes(label = c("83-112 Higher"), npcx = c(0.45), npcy = 0.45),
                hjust = 1) +
  geom_text_npc(mapping = aes(label = c("RRT Higher"), npcx = c(0.55), npcy = 0.45),
                hjust = 0) +
  geom_vline(xintercept = 0.5, linetype = 2) +
  geom_hline(yintercept = 0.5, linetype = 2) +
  scale_x_continuous(name = expression('Biomass '*over(CPUE[RRT], CPUE[RRT]+CPUE[83-112])), limits = c(0,1), expand = c(0,0)) +
  scale_y_continuous(name = "Cumulative proportion of hauls", limits = c(0, 1), expand = c(0,0)) +
  theme_bw()

ggplot() +
  geom_point(
    data = total_catch_wide,
    mapping = aes(x = `TOTAL_CPUE_KGKM2_83-112`, y = TOTAL_CPUE_KGKM2_RRT)) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  scale_x_log10(name = expression(CPUE[83-112]*' '*(kg%.%km^-2))) +
  scale_y_log10(name = expression(CPUE[RRT]*' '*(kg%.%km^-2))) +
  facet_wrap(~FISH, scales = "free") +
  theme_bw()

ggplot() +
  geom_histogram(
    data = total_catch_wide,
    mapping = aes(x = `TOTAL_CPUE_KGKM2_83-112`/TOTAL_CPUE_KGKM2_RRT),
    breaks = seq(0, 5, 0.25)) +
  geom_vline(xintercept = 1, linetype = 2) +
  scale_x_continuous(name = expression(CPUE[83-112]/CPUE[RRT])) +
  scale_y_continuous(name = "Frequency (hauls)") +
  facet_wrap(~FISH, scales = "free_y") +
  theme_bw()

# Check fish codes
# fish_codes <- unique(catch_records$SPECIES_CODE[catch_records$SPECIES_CODE < 40000])
# fish_codes

# Focal species calculations -----

# Manually set focal species codes
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
        "Other",
        "Other"
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


catch_data <- 
  gapindex::get_data(
    year_set = 2026,
    spp_codes = cc_species_codes$SPECIES_CODE,
    haul_type = 23,
    abundance_haul = "N",
    pull_lengths = TRUE,
    survey_set = "EBS"
  )

cc_species_codes <- 
  dplyr::left_join(cc_species_codes, catch_data$species)

cpue_target <- gapindex::calc_cpue(catch_data)

cpue_target <- 
  dplyr::select(catch_data$haul, VESSEL, HAULJOIN, HAUL, NET_WIDTH, NET_HEIGHT, DISTANCE_FISHED, DURATION) |>
    dplyr::inner_join(cpue_target) |>
    dplyr::inner_join(catch_data$species) |>
    dplyr::inner_join(dplyr::select(cc_species_codes, SPECIES_CODE, COMMON_NAME, FAMILY, GROUP_NAME, FISH_CRAB, REPORT_NAME_SCIENTIFIC)) |>
  dplyr::inner_join(catch_treatments)

cpue$COMMON_NAME <- factor(cpue$SPECIES_CODE, levels = cc_species_codes$SPECIES_CODE, labels = cc_species_codes$COMMON_NAME)

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

# Other species calculations ----

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


# Combine CPUE data sets
cpue_all <- dplyr::bind_rows(cpue_target, cpue_misc)
cpue_comparison_all <- dplyr::bind_rows(cpue_comparison_target, cpue_comparison_misc)
cpue_comparison_all_summary <- dplyr::bind_rows(cpue_comparison_target_summary, cpue_comparison_misc_summary)

cpue_all$COMMON_NAME <- factor(cpue_all$COMMON_NAME, levels = cc_species_codes$COMMON_NAME)
cpue_comparison_all$COMMON_NAME <- factor(cpue_comparison_all$COMMON_NAME, levels = cc_species_codes$COMMON_NAME)
cpue_comparison_all_summary$COMMON_NAME <- factor(cpue_comparison_all_summary$COMMON_NAME, levels = cc_species_codes$COMMON_NAME)


# Plot catch results

anchor_points <- 
  cpue_all |>
  dplyr::group_by(COMMON_NAME) |>
  dplyr::summarise(MAX_CPUE_KGKM2 = max(CPUE_KGKM2)*1.02,
                   MAX_CPUE_NOKM2 = max(CPUE_NOKM2)*1.02)

ggplot() +
  # geom_abline(data = cpue_comparison_all_summary,
  #             mapping = aes(intercept = 0, slope = MEAN_RELEFF_CPUE_KGKM2, color = FAMILY)) +
  geom_abline(slope = 1, intercept = 2, linetype = 1, color = "grey50") +
  geom_point(
    data = cpue_comparison_all,
    mapping = aes(x = `CPUE_KGKM2_83-112`, y = CPUE_KGKM2_RRT, color = FAMILY),
    size = rel(2.2),
    alpha = 0.5
  ) +
  # geom_text(
  #   data = cpue_comparison_all,
  #   mapping = aes(x = `CPUE_KGKM2_83-112`, y = CPUE_KGKM2_RRT, color = FAMILY, label = as.numeric(gsub("[^0-9]", "", BLOCK)))
  #   # size = rel(2.2),
  #   # alpha = 0.5
  # ) +
  geom_point(
    data = anchor_points,
    mapping = aes(x = MAX_CPUE_KGKM2, y = MAX_CPUE_KGKM2) ,
    color = NA
  ) +
  # geom_text_npc(
  #   data = cpue_comparison_target_summary,
  #   mapping = 
  #     aes(
  #       label = COMMON_NAME, 
  #       npcx = 0.02, 
  #       npcy = 0.98),
  #   hjust = 0,
  #   fontface = "bold"
  # ) +
  geom_text_npc(
    data = cpue_comparison_all_summary,
    mapping = 
      aes(
        label = paste0("CCR: ", format(round(MEAN_CCR_CPUE_KGKM2, 2), nsmall = 2), "\n", 
                       "RE:", format(round(MEAN_RELEFF_CPUE_KGKM2, 2), nsmall = 2)), 
        npcx = 0.02, 
        npcy = 0.98),
    hjust = 0, size = 2.8
  ) +
  scale_x_continuous(name = expression(CPUE[83-112]*' '*(kg%.%km^-2)), limits = c(0, NA)) +
  scale_y_continuous(name = expression(CPUE[RRT]*' '*(kg%.%km^-2)), limits = c(0, NA)) +
  scale_color_manual(name = "Family", values = colorblind_pal()(7)[2:7]) +
  facet_wrap(~COMMON_NAME, scales = "free", ncol = 3) +
  theme_bw() +
  theme(strip.text = element_text(face = "bold"),
        strip.background = element_blank())

# ggplot() +
#   geom_point(
#     data = cpue_comparison_target,
#     mapping = aes(x = `CPUE_NOKM2_83-112`, y = CPUE_NOKM2_RRT, color = FAMILY),
#     size = rel(3)
#   ) +
#   geom_point(
#     data = anchor_points,
#     mapping = aes(x = MAX_CPUE_NOKM2, y = MAX_CPUE_NOKM2) ,
#     color = NA
#   ) +
#   scale_x_continuous(name = expression(CPUE[83-112]*' '*('#'%.%km^-2)), limits = c(0, NA)) +
#   scale_y_continuous(name = expression(CPUE[RRT]*' '*('#'%.%km^-2)), limits = c(0, NA)) +
#   scale_color_manual(name = "Family", values = colorblind_pal()(5)[2:5]) +
#   geom_abline(slope = 1, intercept = 2, linetype = 2) +
#   facet_wrap(~COMMON_NAME, scales = "free") +
#   theme_bw()


# ggplot() +
#   geom_area(
#     data = dplyr::filter(cpue, FAMILY == "Oregoniidae"),
#     mapping = aes(x = as.numeric(gsub("[^0-9]", "", BLOCK)), y = CPUE_KGKM2, fill = COMMON_NAME)
#   ) +
#   scale_fill_colorblind() +
#   facet_wrap(~GEAR_NAME, nrow = 2)

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

ggplot() +
  geom_vline(xintercept = 0.5, linetype = 2) +
  geom_boxplot(
    data = cpue_comparison_target,
    mapping = aes(x = CCR_CPUE_KGKM2, y = COMMON_NAME, fill = FAMILY, color = FAMILY),
    alpha = 0.3
  ) +
  geom_text(
    data = cpue_comparison_target_summary,
    mapping =
      aes(
        label = paste0("RE: ", format(round(MEAN_RELEFF_CPUE_KGKM2, 2), nsmall = 2)),
        x = -0.1,
        y = COMMON_NAME),
    hjust = 0
  ) +
  scale_x_continuous(name = "Catch comparison rate (biomass)", expand = c(0,0), limits = c(-0.15, 1.02)) +
  scale_y_discrete() +
  scale_fill_manual(name = "Family", values = colorblind_pal()(5)[2:5]) +
  scale_color_manual(name = "Family", values = colorblind_pal()(5)[2:5]) +
  theme_bw() +
  theme(strip.background = element_blank(),
        axis.title.y = element_blank())


# ggplot() +
#   stat_ecdf(
#     data = cpue_comparison_target,
#     mapping = aes(x = CCR_CPUE_KGKM2, color = COMMON_NAME)
#   ) +
#   stat_ecdf(
#     data = cpue_comparison_target,
#     mapping = aes(x = CCR_CPUE_KGKM2)
#   ) +
#   geom_vline(xintercept = 0.5, linetype = 2) +
#   geom_hline(yintercept = 0.5, linetype = 2) +
#   scale_x_continuous(name = expression('Biomass '*over(CPUE[RRT], CPUE[RRT]+CPUE[83-112])), limits = c(0,1), expand = c(0,0)) +
#   scale_y_continuous(name = "Cumulative proportion of hauls", limits = c(0, 1), expand = c(0,0)) +
#   facet_wrap(~FAMILY, scales = "free") +
#   theme_bw()



ggplot() +
  stat_ecdf(
    data = cpue_comparison_target,
    mapping = aes(x = CCR_CPUE_NOKM2, group = COMMON_NAME),
    color = "grey70"
  ) +
  stat_ecdf(
    data = cpue_comparison_target,
    mapping = aes(x = CCR_CPUE_NOKM2)
  ) +
  geom_vline(xintercept = 0.5, linetype = 2) +
  geom_hline(yintercept = 0.5, linetype = 2) +
  scale_x_continuous(name = expression('Numeric '*over(CPUE[RRT], CPUE[RRT]+CPUE[83-112])), limits = c(0,1), expand = c(0,0)) +
  scale_y_continuous(name = "Cumulative proportion of hauls", limits = c(0, 1), expand = c(0,0)) +
  facet_wrap(~GROUP_NAME, scales = "free") +
  theme_bw()



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
    LENGTH_CM = LENGTH /10
    ) |>
  # Use haul data to calculate length-based CPUE
  dplyr::inner_join(
    dplyr::select(catch_data$haul, HAULJOIN, DISTANCE_FISHED, NET_WIDTH, VESSEL, CRUISE, HAUL) |>
      dplyr::mutate(AREA_SWEPT_KM2 = DISTANCE_FISHED * NET_WIDTH / 1000)
  ) |>
  dplyr::inner_join(catch_treatments) |>
  dplyr::mutate(CPUE_NOKM2 = TOTAL_FREQUENCY/AREA_SWEPT_KM2) |>
  dplyr::inner_join(dplyr::select(cc_species_codes, SPECIES_CODE, REPORT_NAME_SCIENTIFIC, COMMON_NAME, FAMILY, GROUP_NAME, FISH_CRAB)) |>
  dplyr::mutate(COMMON_NAME = factor(COMMON_NAME, levels = cc_species_codes$COMMON_NAME))


agg_cpue <- haul_length_freq |>
  dplyr::group_by(LENGTH_CM, SPECIES_CODE, REPORT_NAME_SCIENTIFIC, COMMON_NAME, FAMILY, GROUP_NAME, FISH_CRAB, GEAR_NAME) |>
  dplyr::summarise(
    TOTAL_CPUE_NOKM2 = sum(TOTAL_FREQUENCY)/sum(AREA_SWEPT_KM2)
  )

ggplot() +
  geom_bar(
    data = agg_cpue,
    mapping = aes(x = LENGTH_CM, y = TOTAL_CPUE_NOKM2, fill = GEAR_NAME),
    stat = "identity",
    position = "dodge", width = 0.5
  ) +
  scale_fill_tableau() +
  scale_x_continuous(name = "Size (mm)") +
  scale_y_continuous(name = expression('Mean CPUE '*' '*('#'%.%km^-2))) + 
  facet_wrap(~COMMON_NAME, ncol = 3, scales = "free") +
  theme_bw()


# Weighted ECDF

weighted_ecdf <- 
  data.frame(
    LENGTH_CM = rep(agg_cpue$LENGTH_CM, agg_cpue$TOTAL_CPUE_NOKM2),
    SPECIES_CODE = rep(agg_cpue$SPECIES_CODE, agg_cpue$TOTAL_CPUE_NOKM2),
    REPORT_NAME_SCIENTIFIC = rep(agg_cpue$REPORT_NAME_SCIENTIFIC, agg_cpue$TOTAL_CPUE_NOKM2),
    GEAR_NAME = rep(agg_cpue$GEAR_NAME, agg_cpue$TOTAL_CPUE_NOKM2)
  )


ggplot() +
  stat_ecdf(
    data = agg_cpue,
    mapping = aes(x = LENGTH_CM, color = GEAR_NAME),
    geom = "step"
  ) +
  geom_rug(
    data = agg_cpue,
    mapping = aes(x = LENGTH_CM, color = GEAR_NAME)
  ) +
  scale_fill_tableau() +
  scale_x_continuous(name = "Length (cm)") +
  # scale_y_continuous(name = expression('Mean CPUE '*' '*('#'%.%km^-2))) + 
  facet_wrap(~COMMON_NAME, ncol = 3, scales = "free") +
  theme_bw()

