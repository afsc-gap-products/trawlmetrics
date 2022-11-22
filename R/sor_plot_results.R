#' Make plots of SOR results
#'
#' @param region Survey region as a 1L character vector (EBS or NBS). Must provide rds_dir or all of region, vessel, cruise, survey.
#' @param cruise Cruise number as a numeric vector (e.g. 202202). Must provide rds_dir or all of region, vessel, cruise, survey.
#' @param vessel vessel ID number as a numeric vector (e.g. 162 for Alaska Knight. Must provide rds_dir or all of region, vessel, cruise, survey.
#' @param survey Survey name prefix to use in filename (e.g. NBS_2022). Must provide rds_dir or all of region, vessel, cruise, survey.
#' @return Reads in measurement data from _ping.rds files from rds_dir and writes corrected results to _sor.rds files in rds_dir.
#' @export

sor_plot_results <- function(vessel = NULL, cruise = NULL, region = NULL, survey = NULL) {
  
  region <- toupper(region)
  stopifnot("run_sor: Region must be 'EBS' or 'NBS'" = region %in% c("EBS", "NBS"))  
  
  # Setup file paths to input (rds_dir) and output (output_dir) directories
  rds_dir <- here::here("output", region, cruise, vessel, paste0("ping_files_", survey))
  stopifnot("run_sor: Directory from rds_dir does not exist." = dir.exists(rds_dir))
  
  rds_path <- list.files(rds_dir, full.names = TRUE, pattern = "sor.rds")
  stopifnot("run_sor: No output files found in rds_dir." = length(rds_path) > 0)
  
  output_dir <- here::here("output", region, cruise, vessel, paste0("SOR_graphics_", survey))
  stopifnot("run_sor: Directory from rds_dir does not exist." = dir.exists(output_dir))
  
  
  for(ii in 1:length(rds_path)) {
    
    sel_dat <- readRDS(file = rds_path[ii])
    
    if(is.null(sel_dat$spread)) { next }
      # pings
      not_rejected <- sel_dat$sor_ping_ranks %>% 
        dplyr::filter(is.na(SOR_RANK))
      rejected <- sel_dat$sor_ping_ranks %>% 
        dplyr::filter(!is.na(SOR_RANK))
      
      # CIA: add n pings, mean, sd info to before and after plots
      
      # initial data
      p_init <- sel_dat$sor_ping_ranks %>%
        ggplot()+
        geom_point(aes(x = date_time, 
                       y = measurement_value), 
                   shape = 1, 
                   size = 2.5) +
        scale_y_continuous(limits=c(10, 22), 
                           expand = c(0, 0)) +
        theme_bw() +
        labs(x = "time", y = "spread", title = "Before SOR",
             subtitle = paste("Vessel", sel_dat$haul$vessel, "Haul", sel_dat$haul$haul))
      # after sor
      p_post <- not_rejected %>%
        ggplot()+
        geom_point(aes(x = date_time, 
                       y = measurement_value), 
                   shape = 1, 
                   size = 2.5) +
        scale_y_continuous(limits=c(10, 22), expand = c(0, 0)) +
        theme_bw() +
        labs(x = "time", 
             y = "spread", 
             title = "After SOR",
             subtitle = paste("Vessel", sel_dat$haul$vessel, "Haul", sel_dat$haul$haul))
      
      p_both <- ggplot()+
        geom_point(data = not_rejected, 
                   aes(x = date_time, 
                       y = measurement_value), 
                   shape = 16, 
                   size = 2.5, 
                   alpha = 0.35) +
        geom_point(data = rejected, 
                   aes(x = date_time, 
                       y = measurement_value), 
                   shape = 16, 
                   size = 2.5, 
                   col = "red", 
                   alpha = 0.5) +
        geom_hline(data = sel_dat$sor_results, 
                   aes(yintercept=mean), 
                   col = "blue", 
                   cex = 1) +
        scale_y_continuous(limits=c(10, 22), 
                           expand = c(0, 0)) +
        theme_bw() +
        theme(plot.caption = element_text(hjust = 0)) +
        labs(x = "time", y = "spread", title = "All pings ",
             subtitle = paste(#"Vessel", unique(data_sub$vessel), "Haul", unique(data_sub$haul), 
               "n_pings =", sel_dat$sor_results$n_pings, " mean = ", round(sel_dat$sor_results$mean, 2), 
               " sd =", round(sel_dat$sor_results$sd, 2)),
             caption = "red = rejected by SOR, blue = mean")
      
      # rmse
      p_rmse <- sel_dat$sor_rmse %>%
        ggplot()+
        geom_point(aes(x = N, 
                       y = RMSE), shape = 17, size = 2.5) +
        theme_bw() +
        labs(x = "iteration number", y = "rmse", title = "RMSE",
             subtitle = paste()) #"Vessel", unique(rmse_sub$vessel), "Haul", unique(rmse_sub$haul)))
      
      p_full <- cowplot::plot_grid(p_init, p_post, p_both, p_rmse) # blank plot:, ggplot() + theme_bw() + theme(panel.border = element_blank()))
      
      ggsave(p_full, 
             filename = paste0("SOR_", sel_dat$haul$cruise, "_", sel_dat$haul$vessel, "_", sel_dat$haul$haul, ".png"),
             path = output_dir, 
             width = 10, 
             height = 6)
    
  }
  
}