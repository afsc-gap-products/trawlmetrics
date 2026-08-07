
# Multi-pass Kalman or high-pass filter ----
multi_pass_kalman <- function(x, n_passes = 1, mode = c("highpass", "lowpass"), q = 0.01) {
  mode <- match.arg(mode)
  
  if (n_passes < 1 || n_passes != as.integer(n_passes)) {
    stop("`n_passes` must be a positive integer.")
  }
  
  current_signal <- as.numeric(x)
  
  for (i in seq_len(n_passes)) {
    # Build state-space model list matching base R's internal C interface requirement:
    mod <- list(
      T = matrix(1, 1, 1),               # State transition matrix
      Z = matrix(1, 1, 1),               # Observation matrix
      h = 1,                             # Observation noise variance (scalar)
      V = matrix(q, 1, 1),               # Process noise covariance (RQR')
      a = as.numeric(current_signal[1]), # Initial state estimate vector
      P = matrix(1e6, 1, 1),             # Initial state uncertainty matrix P
      Pn = matrix(1e6, 1, 1)            # Prior state uncertainty matrix Pn
    )
    
    # Run Kalman Smoother
    smoothed_trend <- stats::KalmanSmooth(current_signal, mod)$smooth[, 1]
    
    if (mode == "lowpass") {
      current_signal <- smoothed_trend
    } else {
      current_signal <- current_signal - smoothed_trend
    }
  }
  
  return(current_signal)
}