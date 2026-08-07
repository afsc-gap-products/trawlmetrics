bridle_angle_wes <- function(door_spread, bridle_length = 180/3.281, wing_spread ) {
  
  O <- door_spread/2-wing_spread/2
  
  H <- bridle_length
  
  angle_radians <- asin(O/H)
  
  angle_degrees <- angle_radians * (180 / pi)
  
  return(angle_degrees)
  
}
