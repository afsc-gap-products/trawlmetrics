#' Bottom trawl survey net height and spread
#' 
#' Net height and spread data from good performance index station hauls from NOAA Fisheries Alaska Fisheries Science Center bottom trawl surveys in the eastern Bering Sea shelf, northern Bering Sea, eastern Bering Sea slope, Gulf of Alaska, and Aleutian Islands. Codes are described in the RACE Groundfish Asessment Program [species and data code manuals](https://www.fisheries.noaa.gov/resource/document/groundfish-survey-species-code-manual-and-data-codes-manual).
#' 
#' @format A data frame
#' \describe{
#'      \item{SURVEY_DEFINITION_ID}{The survey definition ID key code is an integer that uniquely identifies a survey region/survey design.}
#'      \item{CRUISE}{This is a six-digit integer identifying the cruise number of the form: YYYY99 (where YYYY = year of the cruise; 99 = 2-digit number and is sequential; 01 denotes the first cruise that vessel made in this year, 02 is the second, etc.).}
#'      \item{YEAR}{Year the observation (survey) was collected.}
#'      \item{VESSEL_ID}{ID number of the vessel used to collect data for that haul.}
#'      \item{SURVEY_NAME}{Long name of the survey conducted}
#'      \item{VESSEL_NAME}{Name of the vessel used to collect data for that haul.}
#'      \item{DEPTH_M}{Bottom depth (meters).}
#'      \item{WIRE_LENGTH_M}{Length of wire deployed during a given haul in meters.}
#'      \item{GEAR}{Bottom trawl survey gear code.}
#'      \item{ACCESSORIES}{Type of accessories used on net.}
#'      \item{HAUL}{This number uniquely identifies a sampling event (haul) within a cruise. It is a sequential number, in chronological order of occurrence.}
#'      \item{DISTANCE_FISHED_KM}{Distance the net fished (thousands of kilometers).}
#'      \item{NET_WIDTH_M}{Measured or estimated distance (meters) between wingtips of the trawl.}
#'      \item{NET_HEIGHT_M}{Measured or estimated distance (meters) between footrope and headrope of the trawl.}
#'      \item{DURATION_HR}{This is the elapsed time between start and end of a haul (decimal hours).}
#'      \item{GEAR_NAME}{Gear name as a character vector.}
#' }
#' @source \url{https://www.fisheries.noaa.gov/contact/groundfish-assessment-program}
#' @export
"bts_geom"

#' Flume tank experimental data
#' 
#' Net measurement data from flume tank experiments conducted at the Centre for Sustainable Aquatic Resources (CSAR), Memorial University of Newfoundland, St. John's, Newfoundland and Labrador from January 13-17, 2025. CURRENTLY EXAMPLE DATA.
#' 
#' @format A data frame
#' \describe{
#'      \item{date_time_nst}{Date and time in Newfoundland Standard Time (UTC-3.5).}
#'      \item{trial}{Trial number.}
#'      \item{trawl}{Trawl model (e.g., "83-112", "PNE", "RACE").}
#'      \item{bridles}{Type of bridles used (e.g., "standard", "bridle 1", "bridle 2").}
#'      \item{footrope}{Footrope used on the trawl (e.g., "EBS" or "GOA").}
#'      \item{pulling_point_elevation_m}{Elevation of the pulling point on the trawl mast in meters.}
#'      \item{backstrap_length_m}{Backstrap length in meters.}
#'      \item{bridle_u_length_m}{Upper bridle length in meters.}
#'      \item{bridle_l_length_m}{Lower bridle length in meters.}
#'      \item{sweep_length_m}{Sweep length in meters.}
#'      \item{u_bridle_extension_m}{Upper bridle extension length in meters.}
#'      \item{towing_speed_kn}{Towing speed in knots.}
#'      \item{spread_door_m}{Door spread in meters.}
#'      \item{spread_u_wing_m}{Upper wing spread in meters.}
#'      \item{spread_l_wing_m}{Lower wing spread in meters.}
#'      \item{spread_mean_we_m}{Mean wing spread in meters.}
#'      \item{opening_wing_m}{Wing opening height in meters.}
#'      \item{opening_headline_m}{Headline opening height in meters.}
#'      \item{height_headline_m}{Headline opening height from the bottom in meters.}
#'      \item{bridle_tension_port_t}{Port bridle tension in metric tons.}
#'      \item{bridle_tension_stbd_t}{Starboard bridle tension in metric tons.}
#'      \item{bridle_tension_total_t}{Total bridle tension in metric tons.}
#'      \item{mouth_area_m2}{Mouth opening area in square meters.}
#'      \item{mouth_drag_kgf_m2}{Mouth drag in kilogram-force per square meter.}
#'      \item{bridle_angle_deg}{Bridle angle of attack in degrees.}
#' }
#' @source \url{https://www.fisheries.noaa.gov/contact/groundfish-assessment-program}
#' @export
"flume_tank"