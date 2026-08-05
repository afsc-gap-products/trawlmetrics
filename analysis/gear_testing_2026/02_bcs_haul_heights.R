# Analyze bottom contact sensor data for each haul

library(trawlmetrics)
library(readxl)
library(here)
library(lubridate)
library(cowplot)
library(mgcv)

bcs_gam_2026 <- readRDS(file = here::here("output", "01_bcs_output", "bcs_calibration_gams_2026.rds"))

bcs_haul_data <- 
  data.frame(
    path = list.files(path = here::here("data", "01_bcs_data", "haul_data"), full.names = TRUE, recursive = TRUE, pattern = ".csv")
  )


parse_bcs_paths <- function(x) {
  x_file <- basename(x)
  x_basename <- gsub(x_file, pattern = ".csv", replacement = "")
  x_metadata <- unlist(strsplit(x_basename, split = "_"))
  
  return(data.frame(haul = as.numeric(x_metadata[1]), bcs_id = x_metadata[2], position = x_metadata[3]))
  
}


bcs_haul_data <- 
  cbind(
    bcs_haul_data,
    lapply(X = bcs_haul_data$path, FUN = parse_bcs_paths) |>
      do.call(what = rbind)
  )

bcs_bc_data <- vector(mode = "list", length = nrow(bcs_haul_data))

for(ii in 1:nrow(bcs_haul_data)) {
  
  bc <- read.csv(bcs_haul_data$path[ii], skip = 1, fileEncoding = "latin1")
  
  if(ncol(bc) > 11) {
    bc <- bc[, 2:8]
    colnames(bc) <- c("dt", "x_g", "y_g", "z_g", "x_tilt", "y_tilt", "z_tilt")
  } else{
    bc <- bc[, 2:5]
    colnames(bc) <- c("dt", "x_g", "y_g", "z_g")
  }
  
  
 
  
  bc$haul <- bcs_haul_data$haul[ii]
  bc$position <- bcs_haul_data$position[ii]
  bc$distance <-  as.numeric(gsub("\\D", "", bc$position))
  bc$side <- gsub("[0-9]", "", bc$position)
  bc$dt <- as.POSIXct(bc$dt, format = "%m/%d/%y %H:%M:%S", tz = "America/Anchorage")
  
  # values
  
  bc$height_fit <- predict(object = bcs_gam_2026[[bcs_haul_data$bcs_id[ii]]]$model, bc)
  
  bcs_bc_data[[ii]] <- bc
  
}

bcs_heights <- lapply(
  X = bcs_bc_data, FUN = 
         function(x) {
           x[c("dt", "haul", "position", "distance", "side", "height_fit", "x_g")]
         }
  ) |>
  do.call(what = dplyr::bind_rows) |>
  dplyr::filter(height_fit > -0.1)


ggplot() +
  geom_smooth(
    data = bcs_heights,
    mapping = aes(x = dt, y = height_fit, color = factor(distance), linetype = side),
    se = FALSE,
    method = "loess",
    span = 0.01
  ) +
  scale_y_continuous(limits = c(0, 10), name = "Height (cm)") +
  scale_color_viridis_d(name = "Distance from center (m)") +
  scale_linetype(name = "Side") +
  facet_wrap(~haul, scales = "free") +
  theme_bw()

ggplot() +
  geom_smooth(
    data = bcs_heights,
    mapping = aes(x = dt, y = height_fit,  group = haul),
    se = FALSE,
    method = "loess",
    span = 0.01
  ) +
  scale_y_continuous(limits = c(0, 10), name = "Height (cm)") +
  scale_color_viridis_d(name = "Distance from center (m)") +
  scale_linetype(name = "Side") +
  facet_wrap(~position, scales = "free") +
  theme_bw()


ggplot() +
  geom_smooth(
    data = bcs_heights,
    mapping = aes(x = dt, y = x_g, color = factor(distance), linetype = side),
    se = FALSE,
    method = "loess",
    span = 0.01
  ) +
  scale_y_continuous(name = "X Acceleration (g)") +
  scale_color_viridis_d(name = "Distance from center (m)") +
  scale_linetype(name = "Side") +
  facet_wrap(~haul, scales = "free") +
  theme_bw()
