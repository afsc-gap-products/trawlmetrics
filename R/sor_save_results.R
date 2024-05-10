#' Setup directory
#' 
#' Function to retrieve data for sequential outlier rejection from RACEBASE and split data by haul.
#'
#' @param final_dir Path to one or more directories containing outputs of sequential outlier rejection process. Filenames will end in 'final.rds'
#' @param create_user Oracle user making the update.
#' @param cruise_idnum One or more cruise ID numbers as a numeric vector (e.g. 757). Must be the same length as final_dir.
#' @param survey Survey name prefix to use in output filename (e.g. NBS_2022)
#' @param channel An RODBC channel. Will prompt user to get connected if NULL.
#' @import RODBC
#' @export

sor_save_results <- function(final_dir, create_user = "", survey, cruise_idnum, channel = NULL) {
  
  stopifnot("sor_save_results: cruise_idnum and final_dir must be the same length." = length(cruise_idnum) == length(final_dir))
  
  channel <- get_connected(channel = channel, 
                           schema = "AFSC")
  
  fpath <- character(length = 0L)
  
  for(jj in final_dir) {
    fpath <- c(fpath, list.files(jj, 
                                 pattern = "final.rds",
                                 full.names = TRUE))
  }

  
  final_values <- data.frame()
  
  for(ii in 1:length(fpath)) {
    
    dat <- readRDS(fpath[ii])
    
    final_values <- rbind(final_values, dat$final)
    
  }
  
  # Convert all values to character
  final_values <- as.data.frame(apply(X = final_values, MARGIN = c(1,2), FUN = as.character))
  
  # Fill NA values
  final_values[is.na(final_values)] <- ""
  
  # Convert names to upper case
  names(final_values) <- toupper(names(final_values))
  
  final_values$CREATE_DATE <- Sys.time()
  final_values$CREATE_USER <- toupper(create_user)

  # Write output to .csv
  csv_path <- here::here("output", paste0("race_data_edit_hauls_table_", survey[1], ".csv"))
  
  message(paste0("sor_save_results: Writing results to ", csv_path))
  
  write.csv(final_values, 
            file = csv_path, 
            row.names = FALSE)
  
  
  # Clear existing data from the table
  delete_existing <- readline(paste0("Any existing data must be deleted from RACE_DATA.EDIT_HAUL_IMPORT_SOR_UPDATES before adding new results. Should data for cruise(s) ", paste(cruise_idnum, collapse = ", "), " be deleted (y or n)?"))
  
  delete_existing <- tolower(delete_existing)
  
  stopifnot("sor_save_results: Execution halted. User opted not to delete existing data from " = delete_existing == "y")

  message("sor_save_results: Removing existing cruise data from RACE_DATA.EDIT_HAUL_IMPORT_SOR_UPDATES")
  
  RODBC::sqlQuery(channel = channel, 
                  query = paste0(" DELETE FROM RACE_DATA.EDIT_HAUL_IMPORT_SOR_UPDATES WHERE CRUISE_ID IN (", paste(cruise_idnum, collapse = ", "), ");"))
  
  # Append data to table  
  message("sor_save_results: Appending new data to RACE_DATA.EDIT_HAUL_IMPORT_SOR_UPDATES")

  RODBC::sqlSave(channel = channel, 
                 dat = final_values, 
                 tablename = "RACE_DATA.EDIT_HAUL_IMPORT_SOR_UPDATES",
                 append = TRUE, # need append = TRUE because of access permissions
                 rownames = FALSE, 
                 colnames = FALSE, 
                 verbose = FALSE,
                 safer = FALSE, 
                 addPK = FALSE, 
                 # typeInfo, 
                 # varTypes,
                 fast = TRUE, 
                 test = FALSE, 
                 nastring = NULL)
  
  return(final_values)
  
}