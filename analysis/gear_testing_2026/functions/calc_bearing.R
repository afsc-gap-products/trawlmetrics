calc_bearing <- function(lat1, lon1, lat2, lon2) {
  rad <- pi / 180
  phi1 <- lat1 * rad
  phi2 <- lat2 * rad
  dlam <- (lon2 - lon1) * rad
  
  y <- sin(dlam) * cos(phi2)
  x <- cos(phi1) * sin(phi2) - sin(phi1) * cos(phi2) * cos(dlam)
  (atan2(y, x) * 180 / pi + 360) %% 360
}

bearing_to_compass16 <- function(bearing) {
  dirs <- c("N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE",
            "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW")
  idx <- floor(((bearing %% 360) + 11.25) / 22.5) %% 16 + 1
  dirs[idx]
}