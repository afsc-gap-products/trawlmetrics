library(lubridate)
library(dplyr)
library(tidyr)
library(ggpp)

split_hauls <-
  function(data_to_split, haul_log_path, buffer_eq_s, buffer_scope_change_s, buffer_hb_s) {
    
    data_to_split <- bcs_heights
    
    buffer_eq_s = 30
    buffer_scope_change_s = 30
    buffer_hb_s = 30
    
    # Load haul data and configure date/times
    haul_log_path <- here::here("data", "2026_gear_testing_haul_log.xlsx")
    
    haul_events <- readxl::read_xlsx(haul_log_path) |>
      dplyr::filter(Event %in% c("Brake set", "EQ", "Scope change", "Haulback")) |>
      dplyr::mutate(start_end = ifelse(Event %in% c("EQ", "Brake set"), "start", "end"))
    
    haul_events$dt <- haul_events$Time_AKDT
    lubridate::date(haul_events$dt) <- lubridate::date(haul_events$Date_AKDT)
    haul_events$dt <- lubridate::force_tz(haul_events$dt, "America/Anchorage") 
    
    names(haul_events) <- tolower(names(haul_events))
    
    # Add buffers
    haul_events$dt[haul_events$event == "Brake set"] <- 
      haul_events$dt[haul_events$event == "Brake set"] + buffer_eq_s
    
    haul_events$dt[haul_events$event == "EQ"] <- 
      haul_events$dt[haul_events$event == "EQ"] + buffer_eq_s
    
    haul_events$dt[haul_events$event == "Scope change"] <- 
      haul_events$dt[haul_events$event == "Scope change"] - buffer_scope_change_s
    
    haul_events$dt[haul_events$event == "Haulback"] <- 
      haul_events$dt[haul_events$event == "Haulback"] - buffer_hb_s
    
    # QA/QC check
    check_duplicates <-
    haul_events |>
      dplyr::group_by(haul, pass, scope, start_end) |>
      dplyr::summarise(n = n()) |>
      dplyr::filter(n > 1) |> nrow()
    
    stopifnot(check_duplicates == 0)
    
    # Assign scope
    scope_haul <-
    haul_events |>
      dplyr::select(haul, pass, scope, start_end, dt) |> 
      tidyr::pivot_wider(names_from = c("start_end"), values_from = "dt")
    
    # Assign tension, door size, phase, and data flag 
    additional_fields <- 
      dplyr::select(haul_events, -time_akdt, -date_akdt) |>
      dplyr::filter(!is.na(tension_port))
    
    result_dplyr <- data_to_split |>
      dplyr::left_join(
        scope_haul,
        by = dplyr::join_by(dplyr::between(dt, start, end), haul)
      ) |>
      dplyr::left_join(
        additional_fields
      )
    
  }


height_summary <- 
  result_dplyr |>
  dplyr::filter(!is.na(scope)) |>
  dplyr::group_by(haul, position, distance, side, pass, scope) |>
  dplyr::summarise(median_height = median(height_fit, na.rm = TRUE),
                   mean_height = mean(height_fit, na.rm = TRUE),
                   sd_height = sd(height_fit, na.rm = TRUE))

unique_haul_scope <- 
  height_summary |>
  dplyr::ungroup() |>
  dplyr::select(haul, scope)

dir.create(here::here("plots", "dtb_haul_scope"), recursive = TRUE)

for(ii in 1:nrow(unique_haul_scope_pass)) {
  
  sel_hsp <- unique_haul_scope[ii, ]
  
  p_trt <- ggplot() + 
    geom_point(
      data = height_summary |>
        dplyr::inner_join(sel_hsp, by = c("haul", "scope")),
      mapping = aes(
        x = ifelse(side == "P", distance*-1, distance),
        y = median_height)
    ) +
    ggtitle(paste0("Haul: ", sel_hsp$haul, ", Scope: ", sel_hsp$scope, " fm")) +
    geom_vline(xintercept = 0, linetype = 2) +
    scale_x_continuous(name = "Distance from center (m)") +
    scale_y_continuous(name = "Distance off bottom (cm)", limits = c(-0.1, 60), expand = c(0,0)) +
    theme_bw()
  
  png(filename = here::here("plots", "dtb_haul_scope", paste0("dist_to_bottom_", paste(sel_hsp[1, ], collapse = "_"), ".png")),
      width = 4, height = 4, units = "in", res = 300)
  print(p_trt)
  dev.off()
  
  
}

p_dtb_all <- 
  ggplot() + 
  geom_point(
    data = height_summary,
    mapping = aes(
      x = ifelse(side == "P", distance*-1, distance),
      y = median_height)
             ) +
  geom_vline(xintercept = 0, linetype = 2) +
  scale_x_continuous(name = "Distance from center (m)") +
  scale_y_continuous(name = "Distance off bottom (cm)", limits = c(-0.1, 60), expand = c(0,0)) +
  theme_bw()

png(filename = here::here("plots", "dtb_haul_scope", paste0("dist_to_bottom_all_hauls.png")),
    width = 4, height = 4, units = "in", res = 300)
print(p_trt)
dev.off()
