# Extract door spread from SCS xml files
library(xml2)
library(ggthemes)
library(dplyr)
library(ggplot2)
library(glmmTMB)
library(ggrepel)


extract_and_rename_xml <- function(zip_path, exdir = dirname(zip_path)) {
  # Verify input file exists
  if (!file.exists(zip_path)) {
    stop("The specified zip file does not exist: ", zip_path)
  }
  
  # List contents of the zip file without extracting yet
  zip_contents <- unzip(zip_path, list = TRUE)
  
  # Find the XML file inside the zip archive
  xml_files <- zip_contents$Name[grep("\\.xml$", zip_contents$Name, ignore.case = TRUE)]
  
  if (length(xml_files) == 0) {
    stop("No .xml file was found inside the zip archive.")
  }
  
  # Select the first XML file found
  target_xml_relative <- xml_files[1]
  
  # Unzip the file to the destination directory
  unzip(zip_path, files = target_xml_relative, exdir = exdir)
  
  # Construct full paths for extracted file and target file name
  extracted_xml_path <- file.path(exdir, target_xml_relative)
  zip_basename <- tools::file_path_sans_ext(basename(zip_path))
  new_xml_path <- file.path(exdir, paste0(zip_basename, ".xml"))
  
  # Rename the file if the name differs
  if (extracted_xml_path != new_xml_path) {
    # If the target file already exists, remove it to avoid rename failure
    if (file.exists(new_xml_path)) {
      file.remove(new_xml_path)
    }
    file.rename(from = extracted_xml_path, to = new_xml_path)
  }
  
  return(new_xml_path)
}

scs_zip <- list.files(here::here("data", "04_scs_data"), full.names = TRUE, pattern = ".zip")

vapply(scs_zip, extract_and_rename_xml, FUN.VALUE = character(1))

# Parse xml files to retrieve net height, net spread, and door spread

library(xml2)

parse_nmea_xml <- function(xml_file, door_range_m = c(20, 60), wing_range_m = c(8,23), height_range_m = c(1,10)) {
  # 1. Extract numeric haul number from the filename (e.g., "haul0521.xml" -> 521)
  file_name <- basename(xml_file)
  haul_num <- as.integer(gsub("[^0-9]", "", file_name))
  
  # Helper function to return empty data frame when no valid tags are found
  # empty_df <- function() {
  #   data.frame(
  #     HAUL = integer(0),
  #     timestamp = as.POSIXct(character(0), tz = "UTC"),
  #     sensor = character(0),
  #     value = numeric(0),
  #     unit = character(0),
  #     stringsAsFactors = FALSE
  #   )
  # }
  
  # 2. Read XML file
  doc <- read_xml(xml_file)
  
  # 3. Find all <DataItem> nodes
  nodes <- xml_find_all(doc, "//DataItem")
  if (length(nodes) == 0) return(empty_df())
  
  # 4. Extract raw text strings and timestamp attributes
  payloads <- xml_text(nodes)
  timestamps_raw <- xml_attr(nodes, "timestamp")
  
  # 5. Regex matching ONLY the 3 target NMEA sentence prefixes
  pattern <- "^(HR,10,DTB|PW,18,XST|PD,23,XST),([^,]+),([-+]?[0-9]*\\.?[0-9]+)"
  
  # Filter out non-matching NMEA tags
  keep_idx <- grepl(pattern, payloads)
  if (!any(keep_idx)) return(NULL)
  
  payloads <- payloads[keep_idx]
  timestamps_raw <- timestamps_raw[keep_idx]
  
  # 6. Extract matched values and units
  prefix <- sub(paste0(pattern, ".*$"), "\\1", payloads)
  # units  <- sub(paste0(pattern, ".*$"), "\\2", payloads)
  values <- as.numeric(sub(paste0(pattern, ".*$"), "\\3", payloads))
  
  # 7. Convert timestamps to POSIXct (UTC)
  timestamps <- as.POSIXct(timestamps_raw, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
  
  # 8. Map exact prefixes to sensor names
  sensor_map <- c(
    "HR,10,DTB" = "NET_HEIGHT_M",
    "PW,18,XST" = "NET_SPREAD_M",
    "PD,23,XST" = "DOOR_SPREAD_M"
  )
  sensors <- unname(sensor_map[prefix])
  
  # 9. Return structured data frame
  values <- 
    data.frame(
    haul = haul_num,
    dt = lubridate::with_tz(timestamps, "America/Anchorage"),
    name = sensors,
    value = values,
    stringsAsFactors = FALSE
  )|>
    tidyr::pivot_wider(
      names_from = "name",
      values_from = "value"
    )
  
  values$DOOR_SPREAD_M[values$DOOR_SPREAD_M < door_range_m[1] | values$DOOR_SPREAD_M > door_range_m[2]] <- NA
  values$NET_SPREAD_M[values$NET_SPREAD_M < wing_range_m[1] | values$NET_SPREAD_M > wing_range_m[2]] <- NA
  values$NET_HEIGHT_M[values$NET_HEIGHT_M < height_range_m[1] | values$NET_HEIGHT_M > height_range_m[2]] <- NA
  
  return(values)
  
  
}

scope_tables <- 
  read_xlsx(path = here::here("data", "shelf_slope_table.xlsx")) |>
  dplyr::mutate(mean_depth_fm = (min_depth_fm+max_depth_fm)/2,
                scope_to_depth = wire_out_fm/mean_depth_fm) |>
  dplyr::inner_join(data.frame(table = c("GOA/AI", "EBS shelf", "EBS slope"), gear = c("PNE", "83-112", "PNE-S")))

scs_xml <- list.files(here::here("data", "04_scs_data"), full.names = TRUE, pattern = ".xml")

# Load gear configuration data

gear_config <- readxl::read_xlsx(path = here::here("data", "2026_gear_testing_gear_log.xlsx"))

trawl_measurements <- 
  lapply(X = scs_xml, FUN = parse_nmea_xml) |>
  do.call(what = dplyr::bind_rows) |> 
  isolate_treatments() |>
  dplyr::filter(!is.na(scope)) |>
  dplyr::select(haul, dt, NET_HEIGHT_M, NET_SPREAD_M, DOOR_SPREAD_M, pass, scope)

trawl_measurement_summary <- 
  trawl_measurements |>
  dplyr::group_by(
    haul, scope
  ) |>
  dplyr::summarise(
    MEAN_NET_SPREAD = mean(NET_SPREAD_M, na.rm = TRUE),
    SD_NET_SPREAD = sd(NET_SPREAD_M, na.rm = TRUE),
    MEAN_NET_HEIGHT = mean(NET_HEIGHT_M, na.rm = TRUE),
    SD_NET_HEIGHT = sd(NET_HEIGHT_M, na.rm = TRUE),
    MEAN_DOOR_SPREAD = mean(DOOR_SPREAD_M, na.rm = TRUE),
    SD_DOOR_SPREAD = sd(DOOR_SPREAD_M, na.rm = TRUE),
    MIN_DT = min(dt),
    MAX_DT = max(dt),
    MEAN_DT = mean(dt)
  ) |>
  dplyr::mutate(
    CV_NET_SPREAD = SD_NET_SPREAD/MEAN_NET_SPREAD,
    CV_NET_HEIGHT = SD_NET_HEIGHT/MEAN_NET_HEIGHT,
    CV_DOOR_SPREAD = SD_DOOR_SPREAD/MEAN_DOOR_SPREAD
  )

# Parse BT and calculate averages for each treatment

btd_path <- list.files(here::here("data", "05_btd_data"), pattern = ".BTD", full.names = TRUE)

btd_data <- lapply(X = btd_path, FUN = read.csv) |>
  do.call(what = dplyr::bind_rows) |>
  dplyr::mutate(dt = as.POSIXct(DATE_TIME, tz = "America/Anchorage", format = "%m/%d/%Y %H:%M:%S")) |>
  dplyr::select(dt, HAUL, DEPTH)

names(btd_data) <- tolower(names(btd_data))



btd_summary <- isolate_treatments(btd_data) |>
  dplyr::filter(!is.na(scope)) |>
  dplyr::group_by(haul, pass, scope) |>
  dplyr::summarise(
    BT_DEPTH_M = mean(depth, na.rm = TRUE),
    BT_DEPTH_FM = BT_DEPTH_M/1.8288
  ) |>
  dplyr::inner_join(
    trawl_measurement_summary |>
      dplyr::select(haul, scope, MEAN_NET_HEIGHT)) |>
  dplyr::mutate(
    BOTTOM_DEPTH_FM = BT_DEPTH_FM + MEAN_NET_HEIGHT/1.8288,
    SCOPE_TO_DEPTH = scope/BOTTOM_DEPTH_FM 
  )

unique_scs_hauls <- unique(trawl_measurements$haul)

for(ii in 1:length(unique_scs_hauls)) {
  
  haul_pings <- trawl_measurements |>
    dplyr::filter(haul == unique_scs_hauls[ii])
  
  min_time <- min(trawl_measurements$dt)
  max_time <- max(trawl_measurements$dt)
  
  haul_summary <- trawl_measurement_summary |>
    dplyr::filter(haul == unique_scs_hauls[ii])
  
  btd_haul <- btd_summary |>
    dplyr::filter(haul == unique_scs_hauls[ii])
  
  p_net_spread <- 
    ggplot() +
    geom_point(
      data = haul_pings,
      mapping = aes(x = dt, y = NET_SPREAD_M, color = factor(scope))
    ) +
    geom_segment(
      data = haul_summary,
      mapping = aes(x = MIN_DT, xend = MAX_DT, y = MEAN_NET_SPREAD, color = factor(scope)),
      linewidth = 1.1
    ) +
    geom_text(
      data = haul_summary,
      mapping = aes(x = MEAN_DT, y = 11, 
                    label = paste0(format(MEAN_NET_SPREAD, nsmall = 1, digits = 3), " (", format(SD_NET_SPREAD, nsmall = 1, digits = 1), ")"))
    ) +
    scale_color_viridis_d(name = "Scope (fm)", direction = -1) +
    scale_x_datetime(name = "Date/time (AKDT)") +
    scale_y_continuous(name = "Net Spread (m)", limits = c(10, 23), breaks = seq(10,23,2)) +
    theme_bw()
  
  p_door_spread <- 
    ggplot() +
    geom_point(
      data = haul_pings,
      mapping = aes(x = dt, y = DOOR_SPREAD_M, color = factor(scope))
    ) +
    geom_segment(
      data = haul_summary,
      mapping = aes(x = MIN_DT, xend = MAX_DT, y = MEAN_DOOR_SPREAD, color = factor(scope)),
      linewidth = 1.1
    ) +
    geom_text(
      data = haul_summary,
      mapping = aes(x = MEAN_DT, y = 23, 
                    label = paste0(format(MEAN_DOOR_SPREAD, nsmall = 1, digits = 3), " (", format(SD_DOOR_SPREAD, nsmall = 1, digits = 2), ")"))
    ) +
    scale_color_viridis_d(name = "Scope (fm)", direction = -1) +
    scale_x_datetime(name = "Date/time (AKDT)") +
    scale_y_continuous(name = "Door Spread (m)", limits = c(20, 62), breaks = seq(20,60,10)) +
    theme_bw()
  
  p_net_height <- 
    ggplot() +
    geom_point(
      data = haul_pings,
      mapping = aes(x = dt, y = NET_HEIGHT_M, color = factor(scope))
    ) +
    geom_segment(
      data = haul_summary,
      mapping = aes(x = MIN_DT, xend = MAX_DT, y = MEAN_NET_HEIGHT, color = factor(scope)),
      linewidth = 1.1
    ) +
    geom_text(
      data = haul_summary,
      mapping = aes(x = MEAN_DT, y = 3.5, 
                    label = paste0(format(MEAN_NET_HEIGHT, nsmall = 1, digits = 1), " (", format(SD_NET_HEIGHT, nsmall = 1, digits = 2), ")"))
    ) +
    ggtitle(paste0("Net/Door/Scope/Depth, Haul: ", haul_pings$haul[1])) +
    scale_color_viridis_d(name = "Scope (fm)", direction = -1) +
    scale_x_datetime(name = "Date/time (AKDT)") +
    scale_y_continuous(name = "Net Height (m)", limits = c(3, 10.5), breaks = seq(3,10,1)) +
    theme_bw()
  
  p_sdr <- ggplot() +
    geom_point(
      data = btd_haul, 
      mapping = aes(x = BT_DEPTH_FM, y = SCOPE_TO_DEPTH, color = factor(scope)),
      size = rel(2.2)) +
    geom_text_repel(
      data = btd_haul,
      mapping = aes(x = BT_DEPTH_FM, y = SCOPE_TO_DEPTH, label = format(SCOPE_TO_DEPTH, digits = 2, nsmall = 1))
                    ) +
    geom_line(
      data = scope_tables,
      mapping = aes(x = mean_depth_fm, y = scope_to_depth, linetype = gear)
      ) +
    scale_color_viridis_d(name = "Scope (ftm)", direction = -1) +
    scale_x_continuous(name = "Bottom depth (fathoms)", limits = c(0, 300/1.8288), oob = scales::oob_keep) +
    scale_y_continuous(name = "Scope/depth", limits = c(0, 9), breaks = seq(1,9,1)) +
    scale_linetype(name = "Survey") +
    theme_bw()
  
  p_net_door <- 
    cowplot::plot_grid(
      p_net_height + 
        theme(
          legend.position = "none", 
          axis.title.x = element_blank(),
          plot.margin = unit(c(5,5,0,5), units = "mm")),
      p_door_spread + 
        theme(
          legend.position = "none", 
          axis.title.x = element_blank(),
          plot.margin = unit(c(0,5,0,5), units = "mm")),
      p_net_spread +
        theme(
          legend.position = "none", 
          plot.margin = unit(c(0,5,0,5), units = "mm")
        ),
      p_sdr  +
        theme(
          legend.position = "bottom", 
          plot.margin = unit(c(0,5,0,5), units = "mm")
        ),
      rel_heights = c(1,1,1,1.2),
      nrow = 4,
      align = "v"
    )
  
  png(filename = here::here("plots", "nm_dist", paste0(haul_pings$haul[1], "_net_door.png")), width = 6, height = 8, units = "in", res = 300)
  print(p_net_door)
  dev.off()
  
}

treatment_net_results <-
  dplyr::left_join(trawl_measurement_summary, btd_summary, by = c("haul", "scope")) |>
  dplyr::mutate(BOTTOM_DEPTH_M = MEAN_NET_HEIGHT + BT_DEPTH_M,
                SCOPE_RATIO = scope/BOTTOM_DEPTH_M)

# No difference in spread varaibility between doors?
# model_door_sd0 <- 
#   glmmTMB(formula = SD_DOOR_SPREAD ~ 1 + (1|depth_treatment) + (1|haul),
#           data = treatment_net_results)
# 
# model_door_sd1 <- 
#   glmmTMB(formula = SD_DOOR_SPREAD ~ factor(door_size_m2) + (1|depth_treatment) + (1|haul),
#         data = treatment_net_results)
# 
# summary(model_door_sd1)
# 
# AIC(model_door_sd0, model_door_sd1)
