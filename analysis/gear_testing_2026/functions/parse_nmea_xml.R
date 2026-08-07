parse_nmea_xml <- function(xml_file, door_range_m = c(20, 60), wing_range_m = c(8,23), height_range_m = c(1,10)) {
  # 1. Extract numeric haul number from the filename (e.g., "haul0521.xml" -> 521)
  file_name <- basename(xml_file)
  haul_num <- as.integer(gsub("[^0-9]", "", file_name))
  
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