# Bridle comparison

bridle_treatments <- 
  readxl::read_xlsx(path = here::here("data", "2026_gear_testing_haul_log.xlsx"),
                                       sheet = "bridle_blocks") |>
  dplyr::select(-DOOR_SIZE_M2)

trawl_data_by_haul <- readRDS(here::here("data", "trawl_data_by_haul.rds"))

# Compare effects of bridle rigging
bridle_settings_data <- 
  trawl_data_by_haul |>
  dplyr::inner_join(
    bridle_treatments
  )
