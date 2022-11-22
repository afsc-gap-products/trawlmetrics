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
  
  rds_paths <- character() 
  
  for(kk in 1:length(rds_dir)) {
    rds_paths <- c(rds_paths, list.files(rds_dir[kk], full.names = TRUE, pattern = "sor.rds"))
  }
  
  message("sor_fill_missing: Found ", length(rds_paths), " files to read-in.")
  
  
  for(mm in 1:length(rds_paths)) {
    
    message("sor_fill_missing: Processing ", rds_paths[mm])
    
    sel_dat <- readRDS(file = rds_paths[mm])
    # sel_dat <- readRDS(file = "C:/Users/sean.rohan/Work/afsc/postsurvey_hauldata_processing/output/EBS/202202/162/ping_files_NBS_2022/202202_162_10_sor.rds")

    est_height <- FALSE
    est_width <- FALSE
    
    # Estimate height if height is NA or there are less than 150 height measurements
    if(is.na(sel_dat[['height']]$net_height_pings)) {
      est_height <- TRUE
    } else {
      if(sel_dat[['height']]$net_height_pings < 150) {
        est_height <- TRUE
      }
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
    if(is.null(sel_dat$spread)) {
      est_spread <- TRUE
    } 
    
    if(est_spread) {
      final_spread <- data.frame(edit_net_spread = predict.glm(object = width_glm, 
                                  newdata = data.frame(edit_net_height = sel_dat[['height']]$edit_net_height, 
                                                       invscope = sel_dat[['height']]$invscope)),
        net_spread_pings = 0,
        net_spread_method = 4,
        net_spread_standard_deviation = 0)
    } else{
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
