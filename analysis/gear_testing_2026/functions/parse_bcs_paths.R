parse_bcs_paths <- function(x) {
  x_file <- basename(x)
  x_basename <- gsub(x_file, pattern = ".csv", replacement = "")
  x_metadata <- unlist(strsplit(x_basename, split = "_"))
  
  return(data.frame(haul = as.numeric(x_metadata[1]), bcs_id = x_metadata[2], position = x_metadata[3]))
  
}