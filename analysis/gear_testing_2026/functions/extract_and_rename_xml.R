extract_and_rename_xml <- function(zip_path, exdir = dirname(zip_path)) {
  # Verify input file exists
  if (!file.exists(zip_path)) {
    stop("The specified zip file does not exist: ", zip_path)
  }
  
  # List contents of the zip file without extracting yet
  zip_contents <- unzip(zip_path, list = TRUE)
  
  # Find the XML file inside the zip archive
  xml_files <- zip_contents$Name[grep("\\.xml$", zip_contents$Name, ignore.case = TRUE)]
  
  if (length(xml_files) == 0) {
    stop("No .xml file was found inside the zip archive.")
  }
  
  # Select the first XML file found
  target_xml_relative <- xml_files[1]
  
  # Unzip the file to the destination directory
  unzip(zip_path, files = target_xml_relative, exdir = exdir)
  
  # Construct full paths for extracted file and target file name
  extracted_xml_path <- file.path(exdir, target_xml_relative)
  zip_basename <- tools::file_path_sans_ext(basename(zip_path))
  new_xml_path <- file.path(exdir, paste0(zip_basename, ".xml"))
  
  # Rename the file if the name differs
  if (extracted_xml_path != new_xml_path) {
    # If the target file already exists, remove it to avoid rename failure
    if (file.exists(new_xml_path)) {
      file.remove(new_xml_path)
    }
    file.rename(from = extracted_xml_path, to = new_xml_path)
  }
  
  return(new_xml_path)
}