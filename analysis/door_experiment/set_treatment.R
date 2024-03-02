# Function to assign treatment values to spread measurements ----
set_treatment <- function(wd, trt) {
  
  unique_trt <- unique(trt$treatment)
  
  wd$treatment <- NA
  
  for(ii in 1:length(unique_trt)) {
    
    wd$treatment[wd$DATE_TIME >= trt$start[trt$treatment == unique_trt[ii]] & wd$DATE_TIME <= trt$end[trt$treatment == unique_trt[ii]]] <- unique_trt[ii]
    
  }
  
  return(wd)
  
}