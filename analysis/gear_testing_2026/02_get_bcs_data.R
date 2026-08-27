library(trawlmetrics)
library(readxl)
library(here)
library(lubridate)
library(cowplot)
library(mgcv)
library(dplyr)

source("./functions/parse_bcs_paths.R")
source("./functions/multi_pass_kalman.R")
source("./functions/isolate_treatments.R")


# BCS paths
bcs_paths <- list.files(
  path = here::here("data", "01_bcs_data", "haul_data"), 
  full.names = TRUE, recursive = TRUE, pattern = ".csv"
)

# Load bottom contact sensor height generalized additive models
bcs_gam_2026 <- readRDS(file = here::here("output", "01_bcs_output", "bcs_calibration_gams_2026.rds"))


bcs_data <- 
  lapply(
    X = bcs_paths, 
    bcs_gams = bcs_gam_2026 ,
    FUN = 
      function(x, bcs_gams) {
        
        metadata <- parse_bcs_paths(x)[c("bcs_id", "position")]
        
        bc <- read.csv(x, skip = 1, fileEncoding = "latin1")
        
        if(ncol(bc) > 11) {
          bc <- bc[, 2:8]
          colnames(bc) <- c("dt", "x_g", "y_g", "z_g", "x_tilt", "y_tilt", "z_tilt")
        } else {
          bc <- bc[, 2:5]
          colnames(bc) <- c("dt", "x_g", "y_g", "z_g")
        }
        
        bc <- bc[c("dt", "x_g", "y_g", "z_g")]
        
        bc$bcs_id <- metadata$bcs_id
        bc$position <- metadata$position
        bc$distance <-  as.numeric(gsub("\\D", "", metadata$position))
        bc$side <- gsub("[0-9]", "", metadata$position)
        bc$dt <- as.POSIXct(bc$dt, format = "%m/%d/%y %H:%M:%S", tz = "America/Anchorage")
        
        # Kalman filter on x-axis acceleration
        bc$x_g_original <- bc$x_g
        bc$x_g <- multi_pass_kalman(bc$x_g, n_passes = 1, mode = "lowpass", q = 0.01)
        
        bc$height_fit <- 
          predict(
            object = bcs_gams[[metadata$bcs_id]]$model, 
            newdata = bc
          )
        
        return(bc)
        
      }
    
  )

bcs_data <- 
  bcs_data |>
  do.call(what = dplyr::bind_rows) |>
  dplyr::group_by(dt, bcs_id, position, distance, side) |>
  dplyr::summarise(
    x_g = mean(x_g), 
    y_g = mean(y_g),
    z_g = mean(z_g),
    x_g_original = mean(x_g_original),
    height_fit = mean(height_fit)
  ) |>
  dplyr::ungroup()

# Isolate BCS data by treatment

bcs_segments <- 
  isolate_treatments(
    data_to_split = bcs_data, 
    haul_log_path = here::here("data", "2026_gear_testing_haul_log.xlsx"), 
    buffer_eq_s = 50, 
    buffer_scope_change_s = 10, 
    buffer_hb_s = 10
    )

# Haul-level summary of BCS height data
bcs_height_summary <- 
  bcs_segments |>
  dplyr::filter(!is.na(scope)) |>
  dplyr::group_by(haul, position, distance, side, pass, scope) |>
  dplyr::summarise(median_height = median(height_fit, na.rm = TRUE),
                   mean_height = mean(height_fit, na.rm = TRUE),
                   sd_height = sd(height_fit, na.rm = TRUE)) |>
  dplyr::ungroup()

saveRDS(bcs_segments, file = here::here("output", "bcs_segments.rds"))
saveRDS(bcs_height_summary, file = here::here("output", "bcs_height_summary.rds"))

# unique_haul_scope <- 
#   bcs_height_summary |>
#   dplyr::ungroup() |>
#   dplyr::select(haul, scope) |>
#   unique()

unique_hauls <- unique(bcs_height_summary$haul)

for(vv in 1:length(unique_hauls)) {
  
  sel_bcs <- dplyr::filter(bcs_height_summary, haul == unique_hauls[vv])
  
  sel_segments <- dplyr::filter(bcs_segments, haul == unique_hauls[vv]) 
  
  p_bcs_median <- 
    ggplot() + 
    geom_point(
      data = sel_bcs,
      mapping = aes(
        x = ifelse(side == "P", distance*-1, distance),
        y = median_height,
        color = factor(scope)), 
      size = rel(2.5),
      alpha = 0.8
    ) +
    ggtitle(paste0("BCS Height, Haul: ", sel_bcs$haul[1])) +
    geom_vline(xintercept = 0, linetype = 2) +
    scale_x_continuous(name = "Distance from center (m)") +
    scale_y_continuous(name = "Distance off bottom (cm)", limits = c(-1, 40), expand = c(0,0), oob = scales::oob_squish) +
    scale_color_viridis_d(name = "Scope (fm)", direction = -1) +
    theme_bw()
  
  panel_labels <- 
    sel_bcs |>
    dplyr::select(haul, distance) |>
    unique()
  
  p_bcs_timeseries <- 
    ggplot() +
    geom_path(
      data = sel_segments,
      mapping = aes(x = dt, y = height_fit, color = factor(scope), linetype = side, group = interaction(scope, pass, side)),
      linewidth = 1.05
    ) +
    ggpp::geom_text_npc(
      data = panel_labels, 
      mapping = aes(npcx = "left", npcy = "top", label = paste0(distance, " m"))
    ) +
    scale_x_datetime(name = "Date/time (AKDT)") +
    scale_y_continuous(name = "Distance off bottom (cm)", limits = c(-3, 40), expand = c(0,0), oob = scales::oob_squish) +
    scale_color_viridis_d(name = "Scope (fm)", direction = -1) +
    scale_linetype(name = "Side") +
    ggtitle("Time series") +
    facet_wrap(~distance , ncol = 1) +
    theme_bw() +
    theme(strip.text = element_blank(),
          strip.background = element_blank())
  
  p_bcs_panels <-
    cowplot::plot_grid(
      p_bcs_median + theme(legend.position = "none"),
      p_bcs_timeseries,
      ncol = 2,
      rel_widths = c(0.4,0.6)
    )
  
  png(filename = here::here("plots", "bcs_dtb", paste0(panel_labels$haul[1], "_dist_to_bottom", ".png")),
      height = 6, width = 8, units = "in", res = 300)
  print(p_bcs_panels)
  dev.off()
  
}
