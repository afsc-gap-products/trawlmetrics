#' Fill missing height and spread
#'
#' @param height_paths Path(S) to height data files.
#' @param spread_paths Path(s) to spread data files.
#' @param rds_dir Path to haul rds files.
#' @return Reads in measurement data from _sor.rds files from rds_dir and writes corrected results to _final.rds files in rds_dir.
#' @export

sor_fill_missing <- function(height_paths, spread_paths, rds_dir) {
  
  # Check that input files exist
  stopifnot("sor_fill_missing: One or more height_path files does not exist." = all(file.exists(height_paths)))
  stopifnot("sor_fill_missing: One or more spread_path files does not exist." = all(file.exists(spread_paths)))
  
  # Read-in height and spread data
  height_df <- data.frame()
  spread_df <- data.frame()
  
  for(ii in 1:length(height_paths)) {
    
    height_df <- dplyr::bind_rows(height_df, readRDS(height_paths[ii]))
    
  }
  
  for(jj in 1:length(spread_paths)) {
    spread_df <- dplyr::bind_rows(spread_df, readRDS(spread_paths[jj]))
  }
  
  # Create a data.frame of height and spread for model-fitting
  hs_df <- dplyr::inner_join(height_df, spread_df, by = c("vessel", "cruise", "haul")) %>%
    tidyr::drop_na(edit_net_height, mean) # drop NA
  
  # Summarise height based on scope
  scope_height_df <- height_df %>% 
    dplyr::group_by(edit_wire_out_FM, vessel) %>%
    dplyr::summarise(mean_height = mean(edit_net_height, na.rm = TRUE))
    
  
  message("sor_fill_missing: Fitting generalized linear model for estimating missing spread.")
  width_glm <- glm(mean ~ invscope + edit_net_height + invscope*edit_net_height, 
                    data = hs_df, family = "gaussian")
  
  rds_paths <-list.files(rds_dir, full.names = TRUE, pattern = "sor.rds")
  
  message("sor_fill_missing: Found ", length(rds_paths), " files to read in.")
  
  for(mm in 1:length(rds_paths)) {
    
    message("sor_fill_missing: Processing ", rds_paths[mm])
    
    sel_dat <- readRDS(file = rds_paths[mm])
    # sel_dat <- readRDS(file = "C:/Users/sean.rohan/Work/afsc/postsurvey_hauldata_processing/output/EBS/202202/162/ping_files_NBS_2022/202202_162_10_sor.rds")

    est_height <- FALSE
    est_width <- FALSE
    
    if(is.null(sel_dat[['height']])) { 
      warning("sor_fill_missing: Skipping ", rds_paths[mm], " because no height data were found.")
      next 
      }
    
    # Estimate height if height is NA or there are less than 150 height measurements
    if(is.na(sel_dat[['height']]$net_height_pings) | sel_dat[['height']]$net_height_pings < 150) {
      
      est_height <- TRUE
      
    }

    if(est_height) {
      final_height <- data.frame(edit_net_height = sel_dat[['height']]$edit_net_height,
                                 net_height_method = 4,
                                 net_height_pings = sel_dat[['height']]$net_height_pings,
                                 net_height_standard_deviation = sel_dat[['height']]$net_height_standard_deviation)
    } else{
      final_height <- data.frame(edit_net_height = scope_height_df$mean_height[scope_height_df$edit_wire_out_FM == sel_dat$height$edit_wire_out_FM],
                                 net_height_method = 6,
                                 net_height_pings = sel_dat[['height']]$net_height_pings,
                                 net_height_standard_deviation = sel_dat[['height']]$net_height_standard_deviation)
    }
    

    # Estimate width if missing
    est_spread <- FALSE
    accept_spread <- NA
    
    # Case where there is no spread data
    if(is.null(sel_dat$spread)) {
      
      message("sor_fill_missing: No spread data avilable. Spread will be estimated.")
      est_spread <- TRUE
      
    }
    
    # Case where there was spread data but not enough to conduct sequential outlier rejection
    if(!is.null(sel_dat$spread) & is.null(sel_dat[['sor_results']]$mean)) {
      
      # Review spread
      mean_spread <- round(mean(sel_dat[['spread']]$measurement_value), 1)
      
      sd_spread <- sd(sel_dat[['spread']]$measurement_value)
      
      n_pings <- length(sel_dat[['spread']]$measurement_value)
      
      on_off_events <- sel_dat$events[which(sel_dat$events$event %in% c(3,4,7)), ]
      
      min_date_time <- on_off_events$date_time[on_off_events$event == 4]
      
      spread_plot <- ggplot()+
        geom_point(data = sel_dat[['spread']],
                   mapping = aes(x = date_time,
                                 y = measurement_value),
                   shape = 1,
                   size = 2.5) +
        geom_segment(data = on_off_events,
                  mapping = aes(x = date_time, xend = date_time, y = 8, yend = 22, color = factor(event))) +
        geom_hline(yintercept = mean_spread, 
                   linetype = 2, 
                   color = "red") +
        geom_text(mapping = aes(x = min_date_time,
                  y = mean_spread + 0.5,
                  label = paste0("Mean: ", mean_spread, " (n = ", n_pings, ")")),
                  color = "red", hjust = -0.1) +
        scale_color_manual(name = "Event", values = c("#009E73", "#0072B2", "#D55E00", "#CC79A7")) +
        scale_y_continuous(name = "Spread (m)",
                           limits=c(8, 22), 
                           expand = c(0, 0)) +
        scale_x_datetime(name = "Time") +
        theme_bw() +
        labs(title = "Without SOR - Accept (y/n)?", subtitle = paste("Vessel", sel_dat$haul$vessel, "Haul", sel_dat$haul$haul))
      
      print(spread_plot)
      
      while(!(accept_spread %in% c("y", "n"))) {
        
        accept_spread <- readline(prompt = "Insufficient data for sequential outlier rejection. Accept mean spread from available points (y or n)?")
        accept_spread <- tolower(accept_spread)
        
        if(!(accept_spread %in% c("y", "n"))) {
          message("Invalid selection. Must enter y or n.")
        }
        
        if(accept_spread == "y") {
          
          message("sor_fill_missing: Accepting mean spread.")
          sel_dat$sor_results <- list(mean = mean_spread,
                                      sd = sd_spread,
                                      n_pings = n_pings)
          
        } else {
          
          message("sor_fill_missing: Rejecting mean spread. Spread will be estimated.")
          
          est_spread <- TRUE
        }
        
      }

    } 
    
    
    if(est_spread) {
      final_spread <- data.frame(edit_net_spread = predict.glm(object = width_glm, 
                                  newdata = data.frame(edit_net_height = sel_dat[['height']]$edit_net_height, 
                                                       invscope = sel_dat[['height']]$invscope)),
        net_spread_pings = 0,
        net_spread_method = 4,
        net_spread_standard_deviation = 0)
    } else {
      final_spread <- data.frame(edit_net_spread = sel_dat[['sor_results']]$mean,
                                 net_spread_pings = sel_dat[['sor_results']]$n_pings,
                                 net_spread_method = 7,
                                 net_spread_standard_deviation = sel_dat[['sor_results']]$sd)
      
    }
    
    final_cruise <- data.frame(cruise_id = sel_dat[['haul']]$cruise_idnum,
                               haul_id = sel_dat[['height']]$haul_id,
                               cruise = sel_dat[['haul']]$cruise,
                               vessel = sel_dat[['haul']]$vessel,
                               haul = sel_dat[['haul']]$haul)
    
    sel_dat$final <- dplyr::bind_cols(final_cruise,
                                      final_spread,
                                      final_height
    )

    saveRDS(object = sel_dat,
            file = gsub(x = rds_paths[mm], pattern = "sor.rds", replacement = "final.rds"))
    
  }
  
}
