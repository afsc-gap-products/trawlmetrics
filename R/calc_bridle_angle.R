#' Calculate the Bridle Angle of Attack
#'
#' Calculates the angle (in degrees) of a trawl bridle based on the door spread, 
#' wing spread, and the total length of the bridle.
#'
#' @details 
#' The function assumes a symmetric triangular geometry where the "opposite" 
#' side of the triangle is half the difference between the door spread and 
#' the wing spread. The bridle length is treated as the hypotenuse. 
#' 
#' The angle is calculated using the sine inverse:
#' \deqn{angle = \arcsin\left(\frac{0.5 \times (door\_spread - wing\_spread)}{total\_bridle\_length}\right)}
#'
#' @param door_spread_m Numeric. The distance between the trawl doors (meters).
#' @param wing_spread_m Numeric. The distance between the wing ends of the net (meters).
#' @param total_bridle_length_m Numeric. The total length of the bridle cable (meters).
#'
#' @return A numeric value representing the bridle angle in degrees.
#' @export
#'
#' @examples
#' calc_bridle_angle(door_spread_m = 50, wing_spread_m = 20, total_bridle_length_m = 100)
calc_bridle_angle <- function(door_spread_m, wing_spread_m, total_bridle_length_m) {
  
  x <- 0.5 * (door_spread_m - wing_spread_m)
  
  angle_rad <- sin(x / total_bridle_length_m)
  
  angle_deg <- angle_rad * 180 / pi
  
  return(angle_deg)
}


calc_bridle_angle <- function(door_spread_m, wing_spread_m, total_bridle_length_m) {
  
  x <- 0.5*(door_spread_m-wing_spread_m)
  y <- total_bridle_length_m
  angle_rad <- sin(x/y)
  angle_deg <- angle_rad * 180/pi
  
  return(angle_deg)
  
}