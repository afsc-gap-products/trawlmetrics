#' Fill missing height and spread
#'
#' @param height_paths Path(s) to height data files.
#' @param spread_paths Path(s) to spread data files.
#' @param haul_path Path to haul data file (required when \code{fill_method = "goa"}).
#' @param rds_dir Path to haul rds files.
#' @param fill_method Method for filling missing height and spread data. Either "goa", which uses GAMs or "ebs", which uses GLMs. See details.
#' @param convert_marport_to_netmind Should Marport spread measurements be converted to Netmind spread using trawlmetric::marport_to_netmind()? 
#' @param min_height_pings Minimum number of height pings required for height data to not be filled.
#' @details
#' There are two options for estimating missing spread and height, which differ between regions. 
#' \describe{
#'     \item{Aleutian Islands and Gulf of Alaska}{\code{fill_method = "goa"}: Aleutian Islands and Gulf of Alaska surveys use generalized additive models that use net height, net spread, vessel speed, bottom depth, scope ratio, and total catch weight as covariates.}
#'     \item{Eastern Bering Sea and Northern Bering Sea}{\code{fill_method = "ebs"}: Eastern Bering Sea shelf and northern Bering Sea surveys uses mean height for a given scope to fill height and a generalized linear model with inverse scope and net height as covariates to fill missing spread.}
#'     }
#' @return Reads in measurement data from _sor.rds files from rds_dir and writes corrected results to _final.rds files in rds_dir.
#' @import ggplot2
#' @export

sor_fill_missing <- function(height_paths, 
                             spread_paths, 
                             haul_path = NULL, 
                             rds_dir, 
                             fill_method, 
                             convert_marport_to_netmind = TRUE,
                             min_height_pings = 150) {
  
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
  hs_df <- dplyr::inner_join(height_df, 
                             spread_df,
                             by = c("haul", "haul_id", "edit_wire_out", 
                                    "cruise", "vessel", "invscope")) |>
    dplyr::filter(!is.na(edit_net_height), 
                  !is.na(mean_spread), 
                  net_height_pings >= min_height_pings,
                  performance >= 0)
  
  # Summarize height based on scope
  scope_height_df <- height_df |>
    dplyr::group_by(edit_wire_out_FM, vessel) |>
    dplyr::summarise(mean_height = mean(edit_net_height, na.rm = TRUE),
                     .groups = "keep")
    
  cat("sor_fill_missing: Fitting models for estimating missing height and spread.\n")
  
  if(fill_method == "ebs") {
    
    models <- list(spread = glm(mean_spread ~ invscope + 
                                 edit_net_height + 
                                 invscope*edit_net_height, 
                               data = hs_df, 
                               family = "gaussian"),
                   height = scope_height_df)
  }
  
  if(fill_method == "goa") {
    
    haul_df <- readRDS(file = haul_path)
    
    hs_df <- suppressMessages(
      dplyr::inner_join(hs_df, 
                        haul_df,
                        by = names(hs_df)[which(names(hs_df) %in% names(haul_df))])
    )
    
    height_df <- suppressMessages(
      dplyr::full_join(height_df, 
                       hs_df) |>
        dplyr::full_join(spread_df)
    )
    
    spread_df <- dplyr::full_join(hs_df, 
                                  spread_df,
                                  by = c("haul", "cruise", "vessel", "region", "survey", 
                                         "cruise_idnum", "n_pings", "mean_spread", "sd_spread"))
    
    n_vessels <- length(unique(hs_df$vessel))
    
    models <- list(
      spread = mgcv::gam(
        switch(n_vessels, 
               '1' = mean_spread ~ factor(net_number) + 
                 s(bottom_depth) + 
                 s(speed) + 
                 s(scope_ratio) + 
                 s(total_weight) +
                 s(edit_net_height),
               '2' = mean_spread ~ factor(net_number) + 
                 factor(vessel) +
                 s(bottom_depth) + 
                 s(speed) + 
                 s(scope_ratio) + 
                 s(total_weight) +
                 s(edit_net_height)
        ), 
        data = hs_df, 
        family = "gaussian"),
      spread_no_height = mgcv::gam(
        switch(n_vessels, 
               '1' = mean_spread ~ factor(net_number) + 
                 s(bottom_depth) + 
                 s(speed) + 
                 s(scope_ratio) + 
                 s(total_weight),
               '2' = mean_spread ~ factor(net_number) + 
                 factor(vessel) +
                 s(bottom_depth) + 
                 s(speed) + 
                 s(scope_ratio) + 
                 s(total_weight)
        ), 
        data = hs_df, 
        family = "gaussian"),
      spread_no_net = mgcv::gam(
        switch(n_vessels, 
               '1' = mean_spread ~ s(bottom_depth) + 
                 s(speed) + 
                 s(scope_ratio) + 
                 s(total_weight) +
                 s(edit_net_height),
               '2' = mean_spread ~ factor(vessel) +
                 s(bottom_depth) + 
                 s(speed) + 
                 s(scope_ratio) + 
                 s(total_weight) +
                 s(edit_net_height)
        ), 
        data = hs_df, 
        family = "gaussian"),
      spread_no_height_net = mgcv::gam(
        switch(n_vessels, 
               '1' = mean_spread ~
                 s(bottom_depth) + 
                 s(speed) + 
                 s(scope_ratio) + 
                 s(total_weight),
               '2' = mean_spread ~ 
                 factor(vessel) +
                 s(bottom_depth) + 
                 s(speed) + 
                 s(scope_ratio) + 
                 s(total_weight)
        ), 
        data = hs_df, 
        family = "gaussian"),
      height = mgcv::gam(
        switch(n_vessels, 
               '1' = mean_spread ~ factor(net_number) + 
                 s(bottom_depth) + 
                 s(speed) + 
                 s(scope_ratio) + 
                 s(total_weight) +
                 s(mean_spread),
               '2' = mean_spread ~ factor(net_number) + 
                 factor(vessel) +
                 s(bottom_depth) + 
                 s(speed) + 
                 s(scope_ratio) + 
                 s(total_weight) +
                 s(mean_spread)
        ), 
        data = hs_df, 
        family = "gaussian"),
      height_no_spread = mgcv::gam(
        switch(n_vessels, 
               '1' = mean_spread ~ factor(net_number) + 
                 s(bottom_depth) + 
                 s(speed) + 
                 s(scope_ratio) + 
                 s(total_weight),
               '2' = mean_spread ~ factor(net_number) + 
                 factor(vessel) +
                 s(bottom_depth) + 
                 s(speed) + 
                 s(scope_ratio) + 
                 s(total_weight)
        ), 
        data = hs_df, 
        family = "gaussian"),
      height_no_net = mgcv::gam(
        switch(n_vessels, 
               '1' = mean_spread ~ s(bottom_depth) + 
                 s(speed) + 
                 s(scope_ratio) + 
                 s(total_weight) +
                 s(mean_spread),
               '2' = mean_spread ~ factor(vessel) +
                 s(bottom_depth) + 
                 s(speed) + 
                 s(scope_ratio) + 
                 s(total_weight) +
                 s(mean_spread)
        ),
        data = hs_df, 
        family = "gaussian"),
      height_no_spread_net = mgcv::gam(
        switch(n_vessels, 
               '1' = mean_spread ~ s(bottom_depth) + 
                 s(speed) + 
                 s(scope_ratio) + 
                 s(total_weight) +
                 s(mean_spread),
               '2' = mean_spread ~ factor(vessel) +
                 s(bottom_depth) + 
                 s(speed) + 
                 s(scope_ratio) + 
                 s(total_weight)
        ), 
        data = hs_df, 
        family = "gaussian") 
    )
    
  }
  
  .fill_missing_models <- function(models, 
                                   fill_method, 
                                   type, 
                                   est_height,
                                   est_spread,
                                   edit_wire_out_FM,
                                   edit_net_height,
                                   edit_net_spread,
                                   mean_spread,
                                   net_number, 
                                   speed,
                                   total_weight,
                                   scope_ratio,
                                   bottom_depth,
                                   invscope,
                                   performance,
                                   vessel) {
    
    if(type == "spread") {
      
      if(fill_method == "ebs") {
        mod_name <- "spread"
      }
      
      if(fill_method == "goa" & performance < 0 & !is.na(edit_net_spread)) {
        return(edit_net_spread)
      }
      
      if(fill_method == "goa" & !est_height & !is.na(net_number)) {
        mod_name <- "spread"
      }
      
      if(fill_method == "goa" & est_height & !is.na(net_number)) {
        mod_name <- "spread_no_height"
      }
      
      if(fill_method == "goa" & !est_height & is.na(net_number)) {
        mod_name <- "spread_no_net"
      }
      
      if(fill_method == "goa" & est_height & is.na(net_number)) {
        mod_name <- "spread_no_height_net"
      }
      
      cat("fill_missing_models: Estimating spread using ", mod_name, "\n")
      
    }
    
    if(type == "height") {
      
      if(fill_method == "ebs") {
        mod_name <- "height"
      }
      
      if(fill_method == "goa" & performance < 0 & !is.na(edit_net_height)) {
        return(edit_net_height)
      }
      
      if(fill_method == "goa" & !est_spread & !is.na(net_number)) {
        mod_name <- "height"
      }
      
      if(fill_method == "goa" & est_spread & !is.na(net_number)) {
        mod_name <- "height_no_spread"
      }
      
      if(fill_method == "goa" & !est_spread & is.na(net_number)) {
        mod_name <- "height_no_net"
      }
      
      if(fill_method == "goa" & est_spread & is.na(net_number)) {
        mod_name <- "height_no_spread_net"
      }
      
      cat("fill_missing_models: Estimating height using ", mod_name, " method.\n")
      
    }
    
    if(any(c("gam", "glm", "lm") %in% class(models[[mod_name]])[1])) {
      
      newdata <- data.frame(net_number = net_number,
                            edit_net_height = edit_net_height,
                            mean_spread = mean_spread,
                            speed = speed,
                            total_weight = total_weight,
                            scope_ratio = scope_ratio,
                            bottom_depth = bottom_depth,
                            invscope = invscope,
                            vessel = vessel)
      
      fit <- predict(models[[mod_name]], newdata = newdata)
      
    }
    
    if("data.frame" %in% class(models[[mod_name]])) {
      
      fit <- models[[mod_name]]$mean_height[models[[mod_name]]$edit_wire_out_FM == edit_wire_out_FM]
      
    }
    
    return(fit)
    
  }
  
  rds_paths <- list.files(rds_dir, full.names = TRUE, pattern = "sor.rds")
  
  cat("sor_fill_missing: Found ", length(rds_paths), " files to read in.\n")
  
  for(mm in 1:length(rds_paths)) {
    
    cat("sor_fill_missing: Processing ", rds_paths[mm], "\n")
    
    sel_dat <- readRDS(file = rds_paths[mm])

    est_height <- FALSE
    est_spread <- FALSE
    
    if(is.null(sel_dat[['height']])) { 
      warning("sor_fill_missing: Skipping ", rds_paths[mm], " because no height data were found.")
      next 
      }
    
    # Estimate height if height is NA or there are less than threshold value; estimate spread if missing
    if(is.na(sel_dat[['height']]$net_height_pings) | 
       sel_dat[['height']]$net_height_pings < min_height_pings) {
      est_height <- TRUE
    }
    
    # Case where there is no spread data
    if(is.null(sel_dat$spread) | is.null(sel_dat$sor_results)) {
      
      cat("sor_fill_missing: No spread data available. Spread will be estimated.\n")
      est_spread <- TRUE
      
    }
    
    # Case where there was spread data but not enough to conduct sequential outlier rejection
    accept_spread <- NA
    
    if(!is.null(sel_dat$spread) & is.null(sel_dat[['sor_results']]$mean_spread)) {
      
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
                  mapping = aes(x = date_time, 
                                xend = date_time, 
                                y = 8, yend = 22, 
                                color = factor(event))) +
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
        labs(title = "Without SOR - Accept (y/n)?", 
             subtitle = paste("Vessel", sel_dat$haul$vessel, "Haul", sel_dat$haul$haul))
      
      print(spread_plot)
      
      while(!(accept_spread %in% c("y", "n"))) {
        
        accept_spread <- readline(prompt = "Insufficient data for sequential outlier rejection. Accept mean spread from available points (y or n)?")
        accept_spread <- tolower(accept_spread)
        
        if(!(accept_spread %in% c("y", "n"))) {
          cat("Invalid selection. Must enter y or n.\n")
        }
        
        if(accept_spread == "y") {
          
          cat("sor_fill_missing: Accepting mean spread.\n")
          sel_dat$sor_results <- list(mean_spread = mean_spread,
                                      sd_spread = sd_spread,
                                      n_pings = n_pings)
          
          est_spread <- FALSE
          
        } else {
          
          cat("sor_fill_missing: Rejecting mean spread. Spread will be estimated.\n")
          
          est_spread <- TRUE
        }
        
      }

    } 
    
    # Fill height
    if(est_height) {
      
      final_height <- data.frame(
        edit_net_height = 
          .fill_missing_models(models = models, 
                               fill_method = fill_method, 
                               type = "height", 
                               edit_wire_out_FM = sel_dat$haul$edit_wire_out,
                               edit_net_height = sel_dat[['height']]$edit_net_height,
                               edit_net_spread = sel_dat$haul$edit_net_spread,
                               est_spread = est_spread,
                               est_height = est_height,
                               mean_spread = switch(as.character(est_spread), 
                                             "TRUE" = NA, 
                                             "FALSE" = sel_dat$sor_results$mean_spread),
                               net_number = sel_dat$haul$net_number, 
                               speed = sel_dat$haul$speed,
                               total_weight = sel_dat$haul$total_weight,
                               scope_ratio = sel_dat$haul$scope_ratio,
                               bottom_depth = sel_dat$haul$bottom_depth,
                               invscope = sel_dat$haul$invscope,
                               performance = sel_dat$haul$performance,
                               vessel = sel_dat$haul$vessel),
        net_height_method = 4,
        net_height_pings = sel_dat[['height']]$net_height_pings,
        net_height_standard_deviation = sel_dat[['height']]$net_height_standard_deviation)
      
    } else {
      
      final_height <- data.frame(edit_net_height = sel_dat[['height']]$edit_net_height,
                                 net_height_method = 6,
                                 net_height_pings = sel_dat[['height']]$net_height_pings,
                                 net_height_standard_deviation = sel_dat[['height']]$net_height_standard_deviation)
      
    }
    
    
    # Fill spread
    if(est_spread) {
      
      final_spread <- data.frame(edit_net_spread = 
                                   .fill_missing_models(models = models, 
                                                        fill_method = fill_method, 
                                                        type = "spread", 
                                                        edit_wire_out_FM = sel_dat$haul$edit_wire_out,
                                                        edit_net_height = final_height$edit_net_height,
                                                        edit_net_spread = sel_dat$haul$edit_net_spread,
                                                        est_spread = est_spread,
                                                        est_height = est_height,
                                                        mean_spread = NA,
                                                        net_number = sel_dat$haul$net_number, 
                                                        speed = sel_dat$haul$speed,
                                                        total_weight = sel_dat$haul$total_weight,
                                                        scope_ratio = sel_dat$haul$scope_ratio,
                                                        bottom_depth = sel_dat$haul$bottom_depth,
                                                        invscope = sel_dat$haul$invscope,
                                                        performance = sel_dat$haul$performance,
                                                        vessel = sel_dat$haul$vessel),
                                 net_spread_pings = 0,
                                 net_spread_method = 4,
                                 net_spread_standard_deviation = 0)
      
    } else {
      
      final_spread <- data.frame(edit_net_spread = sel_dat[['sor_results']]$mean_spread,
                                 net_spread_pings = sel_dat[['sor_results']]$n_pings,
                                 net_spread_method = 7,
                                 net_spread_standard_deviation = sel_dat[['sor_results']]$sd_spread)
      
      
    }
    
    if(convert_marport_to_netmind) {
      
      # Convert Marport spread to Netmind spread
      final_spread$edit_net_spread <- marport_to_netmind(final_spread$edit_net_spread)
      
    }
    
    final_cruise <- data.frame(cruise_id = sel_dat[['haul']]$cruise_idnum,
                               haul_id = sel_dat[['height']]$haul_id,
                               cruise = sel_dat[['haul']]$cruise,
                               vessel = sel_dat[['haul']]$vessel,
                               haul = sel_dat[['haul']]$haul)
    
    sel_dat$final <- dplyr::bind_cols(final_cruise,
                                      final_spread,
                                      final_height)
    
    saveRDS(object = sel_dat,
            file = gsub(x = rds_paths[mm], pattern = "sor.rds", replacement = "final.rds"))
    
  }
  
}
