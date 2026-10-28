library(DBI)
library(RSQLite)

marport_pings <- readRDS( here::here("data", "marport_pings.rds")) |>
  dplyr::select(DT, HAUL, NET_HEIGHT, NET_WIDTH, DOOR_SPREAD, WIRE_LENGTH_FM, BRIDLE_ANGLE_DEG)

btd_data <- readRDS(here::here("output", "btd_data.rds")) |>
  dplyr::select(-pass, -scope, -haul) |>
  dplyr::rename_with(toupper) |>
  dplyr::mutate(GEAR_DEPTH_M = DEPTH)

sqlite_file <- here::here("output", "2026_gear_testing_trawl_data.sqlite")
table_name <- "trawl_data"

bcs_pings <- readRDS(here::here("data", "bcs_pings.rds")) |>
  dplyr::mutate(var_name = paste0("BCS_", DISTANCE, "_", SIDE)) |>
  dplyr::filter(DISTANCE != 18) |>
  dplyr::select(DT, HAUL, WIRE_LENGTH_FM, var_name, HEIGHT_FIT) |>
  unique() |>
  tidyr::pivot_wider(values_from = HEIGHT_FIT, names_from = var_name, values_fn = mean)

df <- dplyr::full_join(marport_pings, bcs_pings) |>
  dplyr::full_join(btd_data)


# 3. Connect to the SQLite database
# If the file does not exist, dbConnect will create it automatically.
con <- dbConnect(RSQLite::SQLite(), dbname = sqlite_file)

# 4. Write the data frame to a table
dbWriteTable(
  conn = con,
  name = table_name,
  value = df,
  overwrite = TRUE  # Overwrites the table if it already exists
)

# 5. Always disconnect when finished
dbDisconnect(con)
