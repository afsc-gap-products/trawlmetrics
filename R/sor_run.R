#' Function to run sequential outlier rejection on data
#' 
#' Runs sequential outlier rejections on hauls that have at least 50 spread measurements.
#' 
#' @param region Survey region as a 1L character vector (EBS or NBS). Must provide rds_dir or all of region, vessel, cruise, survey.
#' @param cruise Cruise number as a numeric vector (e.g. 202202). Must provide rds_dir or all of region, vessel, cruise, survey.
#' @param vessel vessel ID number as a numeric vector (e.g. 162 for Alaska Knight. Must provide rds_dir or all of region, vessel, cruise, survey.
#' @param survey Survey name prefix to use in filename (e.g. NBS_2022). Must provide rds_dir or all of region, vessel, cruise, survey.
#' @return Reads in measurement data from _ping.rds files from rds_dir and writes corrected results to _sor.rds files in rds_dir.
#' @export

sor_run <- function(vessel = NULL, cruise = NULL, region = NULL, survey = NULL) {
  
    region <- toupper(region)
    stopifnot("run_sor: Region must be 'EBS' or 'NBS'" = region %in% c("EBS", "NBS"))  
    
    rds_dir <- here::here("output", region, cruise, vessel, paste0("ping_files_", survey))
    stopifnot("run_sor: Directory from rds_dir does not exist." = dir.exists(rds_dir))
  
  rds_path <- list.files(rds_dir, full.names = TRUE, pattern = "pings.rds")
  
  stopifnot("run_sor: No output files found in rds_dir." = length(rds_path) > 0)
  output_path <- gsub(rds_path, pattern = "pings.rds", replacement = "sor.rds")
  
  mean_spread_df <- data.frame()
  
  for(ii in 1:length(rds_path)) {

    if(!file.exists(output_path[ii])) {
      sel_dat <- readRDS(file = rds_path[ii])
      message("run_sor: Reading ", rds_path[ii])
      sel_dat <- readRDS(file = rds_path[ii])
      
      sel_dat[['sor_ping_ranks']] <- NULL
      sel_dat[['sor_results']] <- NULL
      sel_dat[['sor_rmse']] <- NULL
      
      if(is.null(sel_dat$spread)) {
        message("run_sor: No measurement data in ",  rds_path[ii], " Writing output without correcting pings.")
      } else {
        if(nrow(sel_dat$spread) < 50) {
          message("run_sor: Skipping SOR on ",  rds_path[ii], ". Less than 50 spread pings.")
        } else {
          message("run_sor: Running SOR on ",  rds_path[ii])
          sor_pings <- sel_dat$spread %>%  
            sequentialOR( 
              method = 'ss', #smooth spline
              # formula = data.sub$response_var~data.sub$predictor.var, #or formula = response_var~predictor.var 
              formula = measurement_value ~ date_time,
              n.reject = 1, 
              n.stop = 0.5, 
              threshold.stop = TRUE, 
              tail = "both")
          
          sel_dat[['sor_ping_ranks']] <- sor_pings$obs_rank
          sel_dat[['sor_results']] <- sor_pings$results
          sel_dat[['sor_rmse']] <- sor_pings$rmse
          
          
        }
      }
    } else {
      sel_dat <- readRDS(output_path[ii])
      message("run_sor: Skipping SOR on ", rds_path[ii], ". sor.rds file already exists.")
    }
    
    if(!is.null(sel_dat[['sor_results']])) {
      # Combine mean spread with haul data
      mean_spread_df <- mean_spread_df %>% 
        dplyr::bind_rows(
          dplyr::bind_cols(sel_dat[['haul']], 
                           sel_dat[['sor_results']])
        )
    }
    
    saveRDS(object = sel_dat, file = output_path[ii])
  }
  
  # Write corrected mean spread to a file that can be used to estimate spread for missing data
  saveRDS(object = mean_spread_df,
          file = here::here("output", region, cruise, vessel, 
                            paste0("SPREAD_AFTER_SOR_", region, "_", cruise, "_", vessel, ".rds")))
}
