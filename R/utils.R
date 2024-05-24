#' Create a database connection using RODBC
#'
#' A function that accepts a data source name, username, and password to establish returns an Oracle DataBase Connection (ODBC) as an RODBC class in R.
#'
#' @param schema Data source name (DSN) as a character vector.
#' @param channel An open RODBC channel; used within functions.
#' @return An RODBC class ODBC connection.
#' @import getPass RODBC
#' @export

get_connected <- function(channel = NULL, schema = NA){
  if(is.null(channel)) {
    (echo = FALSE)
    if(is.na(schema)) {
      schema <- getPass::getPass(msg = "Enter ORACLE schema: ")
    }
    username <- getPass::getPass(msg = "Enter your ORACLE Username: ")
    password <- getPass::getPass(msg = "Enter your ORACLE Password: ")
    channel  <- RODBC::odbcConnect(dsn = paste(schema),
                                   uid = paste(username),
                                   pwd = paste(password),
                                   believeNRows = FALSE)
  }
  return(channel)
}



#' Filter pings based on date/time
#' 
#' @param data data.frame containing events, measurements, and date_times for events.
#' @param start_event_code Start event code (e.g. 3 for on-bottom in the EBS/NBS, 4 for Equilibrium Time in the GOA and AI)
#' @param end_event_code Stop event code (e.g. 7 for off-bottom in the EBS, NBS, GOA, and AI )
#' @export

get_pings2 <- function(data, start_event_code, end_event_code) {
  data_sub <- data |> 
    dplyr::select(-datum_code, -cabinet_sensor_flag, -measurement_value) |> 
    dplyr::distinct() 
  start_t <- data_sub |> dplyr::filter(event == start_event_code) |> 
    dplyr::select(date_time)
  end_t <- data_sub |> dplyr::filter(event == end_event_code) |> 
    dplyr::select(date_time)
  data_new <- data |>
    dplyr::mutate(start = start_t$date_time, end = end_t$date_time) |>
    dplyr::filter(date_time >= start & date_time <= end) |>
    dplyr::filter(!is.na(measurement_value)) 
  
  return(data_new)
}


#' Convert Marport spread to NetMind spread
#' 
#' @param x Numeric vector of spread values
#' @references Lauth, R. R., and S. Kotwicki. 2014. A calibration function for correcting mean net spread values obtained from Marport spread sensors used in conjunction with the Marport MK II receiver. AFSC Processed Rep. 2014-02, 26 p. Alaska Fish. Sci. Cent., NOAA, Natl. Mar. Fish. Serv., 7600 Sand Point Way NE, Seattle WA 98115. https://apps-afsc.fisheries.noaa.gov/Publications/ProcRpt/PR2014-02.pdf
#' @export

marport_to_netmind <- function(x) {
  # Correction parameters from Lauth and Kotwicki (2014)
  return(0.935684155 * x + 0.400465037)
}