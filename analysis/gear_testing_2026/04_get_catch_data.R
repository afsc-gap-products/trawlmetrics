library(trawlmetrics)

# Matched catch treatments
catch_treatments <- 
  readxl::read_xlsx(
    path = here::here("data", "2026_gear_testing_haul_log.xlsx"),
    sheet = "catch_treatments"
  )

gear_config <- 
  readxl::read_xlsx(path = here::here("data", "2026_gear_config.xlsx")) |>
  dplyr::mutate(
    TOTAL_GEAR_LENGTH_M = (total_bridle_length_ft + door_leg_tail_chain_length_ft + bridle_chain_length_ft)/3.281
  ) |>
  dplyr::rename_with(toupper)|>
  dplyr::mutate(DOOR_SETTINGS =  factor(
    DOOR_SETTINGS, 
    levels = c("B-D", "B-E"), 
    labels = c("30.25\u00B0 (B-D)", "32.60\u00B0 (B-E)"),
  )) |>
  dplyr::mutate(DOOR_SIZE_M2 = factor(DOOR_SIZE_M2, levels = c(4, 4.5), labels = c("4.0", "4.5")),
                BRIDLE_OPTION = factor(BRIDLE_OPTION))

# Focal species

analysis_species <- read.csv(here::here("data", "analysis_species.csv"))

# Get catch and effort data from closed codend tows
channel <- trawlmetrics::get_connected(schema = "AFSC")

catch_records <- RODBC::sqlQuery(
  channel = channel,
  query = "SELECT
  C.SPECIES_CODE,
  C.WEIGHT,
  C.NUMBER_FISH,
  C.VESSEL,
  C.CRUISE,
  C.HAUL,
  RDH.DOOR_SPREAD,
  H.BOTTOM_DEPTH,
  H.HAULJOIN,
  H.NET_WIDTH,
  H.DISTANCE_FISHED,
  H.NET_HEIGHT,
  H.DURATION,
  H.WIRE_LENGTH,
  H.START_LONGITUDE,
  H.START_LATITUDE,
  H.START_TIME
  FROM RACEBASE.HAUL H,
  RACEBASE.CATCH C,
  RACE_DATA.HAULS RDH,
  RACE_DATA.CRUISES RDC
    WHERE C.CRUISE = 202601
  AND H.HAUL_TYPE = 23
  AND H.PERFORMANCE >= 0
  AND H.HAULJOIN = C.HAULJOIN
  AND H.HAUL = RDH.HAUL
  AND RDH.CRUISE_ID = RDC.CRUISE_ID
  AND H.VESSEL = RDC.VESSEL_ID
  AND RDC.CRUISE = C.CRUISE"
) |>
  dplyr::mutate(
    AREA_SWEPT_KM2 = DISTANCE_FISHED * NET_WIDTH/1000
  ) |>
  dplyr::left_join(
    gear_config  |>
      dplyr::select(
        HAUL, 
        DOOR_SETTINGS,
        DOOR_SIZE_M2,
        BRIDLE_OPTION,
        TOTAL_GEAR_LENGTH_M
      ) |>
      dplyr::mutate(
        VESSEL = 134,
        CRUISE = 202601
      )
  ) |>
  dplyr::mutate(
    BRIDLE_ANGLE =
      trawlmetrics::calc_bridle_angle(door_spread_m = DOOR_SPREAD, wing_spread_m = NET_WIDTH, total_bridle_length_m = TOTAL_GEAR_LENGTH_M
  )
)

saveRDS(catch_records, here::here("data", "06_catch_data", "catch_records.rds"))

# Get focal species data with gapindex

catch_data <- 
  gapindex::get_data(
    year_set = 2026,
    spp_codes = analysis_species$SPECIES_CODE,
    haul_type = 23,
    abundance_haul = "N",
    pull_lengths = TRUE,
    survey_set = "EBS",
    channel = channel
  )

saveRDS(catch_data, here::here("data", "06_catch_data", "catch_gapindex.rds"))

cc_species_codes <- 
  dplyr::left_join(analysis_species, catch_data$species)

saveRDS(cc_species_codes, here::here("data", "analysis_species_taxonomy.rds"))


# CPUE for focal species
cpue_focal <- gapindex::calc_cpue(catch_data)

cpue_focal$COMMON_NAME <- 
  factor(
    cpue_focal$SPECIES_CODE, 
    levels = cc_species_codes$SPECIES_CODE, 
    labels = cc_species_codes$COMMON_NAME
  )

cpue_focal <- 
  dplyr::select(catch_data$haul, VESSEL, HAULJOIN, HAUL, NET_WIDTH, NET_HEIGHT, DISTANCE_FISHED, DURATION) |>
  dplyr::inner_join(cpue_focal) |>
  dplyr::inner_join(catch_data$species) |>
  dplyr::inner_join(
    dplyr::select(cc_species_codes, SPECIES_CODE, COMMON_NAME, FAMILY, GROUP_NAME, FISH_CRAB, REPORT_NAME_SCIENTIFIC)) |>
  dplyr::inner_join(catch_treatments)

saveRDS(cpue_focal, here::here("data", "cpue_focal.rds"))

# Prep size data -----------------------------------------------------------------------------------

crab_size_data <- read.csv(here::here("data", "06_catch_data", "crab_specimen_2026mod.csv"))  |>
  dplyr::mutate(SIZE_5MM = plyr::round_any(SIZE, accuracy = 5, f = floor))

saveRDS(crab_size_data, here::here("data", "06_catch_data", "crab_size_data.rds"))

crab_size_freq_no_sex <- 
  crab_size_data |>
  dplyr::group_by(SIZE_1MM, SPECIES_CODE, HAULJOIN) |>
  dplyr::summarize(FREQUENCY = sum(SAMPLING_FACTOR), .groups = "drop") |>
  dplyr::inner_join(
    dplyr::select(catch_data$haul, HAULJOIN, DISTANCE_FISHED, NET_WIDTH, VESSEL, CRUISE, HAUL) |>
      dplyr::mutate(
        AREA_SWEPT_KM2 = DISTANCE_FISHED * NET_WIDTH / 1000)
  ) |>
  dplyr::inner_join(catch_treatments) |>
  dplyr::inner_join(dplyr::select(cc_species_codes, SPECIES_CODE, REPORT_NAME_SCIENTIFIC, COMMON_NAME, FAMILY, GROUP_NAME, FISH_CRAB)) |>
  dplyr::mutate(CPUE_NOKM2 = FREQUENCY/AREA_SWEPT_KM2) |>
  dplyr::mutate(
    SEX = 3,
    COMMON_NAME = factor(COMMON_NAME, levels = cc_species_codes$COMMON_NAME),
    SIZE = SIZE_1MM)

crab_size_sex_freq <- 
  crab_size_data |>
  dplyr::group_by(SIZE_5MM, SPECIES_CODE, SEX, HAULJOIN) |>
  dplyr::summarize(FREQUENCY = sum(SAMPLING_FACTOR), .groups = "drop") |>
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
    SIZE = SIZE_5MM)

crab_size_freq <- 
  dplyr::bind_rows(crab_size_sex_freq,
                   crab_size_freq_no_sex)

saveRDS(crab_size_freq , here::here("data", "06_catch_data", "size_freq_crab.rds"))


fish_length_data <- 
  # Calculate raising factor
  catch_data$size |>
  dplyr::group_by(SPECIES_CODE, HAULJOIN) |>
  dplyr::summarise(N_LENGTHS = sum(FREQUENCY), .groups = "drop") |>
  dplyr::inner_join(catch_data$catch) |>
  dplyr::mutate(
    SAMPLING_FACTOR = NUMBER_FISH/N_LENGTHS) |>
  dplyr::select(SPECIES_CODE, HAULJOIN, SAMPLING_FACTOR) |>
  # Apply raising factor to lengths
  dplyr::inner_join(catch_data$size) |>
  dplyr::mutate(
    TOTAL_FREQUENCY = FREQUENCY * SAMPLING_FACTOR,
    LENGTH_CM = LENGTH /10,
    SIZE = LENGTH_CM
  ) |>
  # Use haul data to calculate length-based CPUE
  dplyr::inner_join(
    dplyr::select(catch_data$haul, HAULJOIN, DISTANCE_FISHED, NET_WIDTH, VESSEL, CRUISE, HAUL) |>
      dplyr::mutate(AREA_SWEPT_KM2 = DISTANCE_FISHED * NET_WIDTH / 1000)
  ) |>
  dplyr::inner_join(catch_treatments)

saveRDS(fish_length_data, here::here("data", "06_catch_data", "fish_size_data.rds"))

fish_length_freq <-
  fish_length_data |>
  dplyr::mutate(CPUE_NOKM2 = TOTAL_FREQUENCY/AREA_SWEPT_KM2) |>
  dplyr::inner_join(dplyr::select(cc_species_codes, SPECIES_CODE, REPORT_NAME_SCIENTIFIC, COMMON_NAME, FAMILY, GROUP_NAME, FISH_CRAB)) |>
  dplyr::mutate(
    COMMON_NAME = factor(
      COMMON_NAME, 
      levels = cc_species_codes$COMMON_NAME)
  )

saveRDS(fish_length_freq, here::here("data", "06_catch_data", "size_freq_fish.rds"))
