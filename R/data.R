#' Bottom trawl survey net height and spread
#' 
#' Net height and spread data from good performance index station hauls from NOAA Fisheries Alaska Fisheries Science Center bottom trawl surveys in the eastern Bering Sea shelf, northern Bering Sea, eastern Bering Sea slope, Gulf of Alaska, and Aleutian Islands. Codes are described in the RACE Groundfish Asessment Program [species and data code manuals](https://www.fisheries.noaa.gov/resource/document/groundfish-survey-species-code-manual-and-data-codes-manual).
#' 
#' @format A data frame
#' \describe{
#'      \item{HAULJOIN}{Unique ID given to a haul}
#'      \item{SURVEY_DEFINITION_ID}{The survey definition ID key code is an integer that uniquely identifies a survey region/survey design.}
#'      \item{SURVEY_ABBV}{Abbreviated survey name}
#'      \item{YEAR}{Year the observation (survey) was collected.}
#'      \item{CRUISE}{This is a six-digit integer identifying the cruise number of the form: YYYY99 (where YYYY = year of the cruise; 99 = 2-digit number and is sequential; 01 denotes the first cruise that vessel made in this year, 02 is the second, etc.).}
#'      \item{VESSEL_ID}{ID number of the vessel used to collect data for that haul.}
#'      \item{HAUL}{This number uniquely identifies a sampling event (haul) within a cruise. It is a sequential number, in chronological order of occurrence.}
#'      \item{STATION}{Station name/ID}
#'      \item{START_LONGITUDE}{Vessel longitude at the start of the haul in decimal degrees}
#'      \item{START_LATITUDE}{Vessel latitude at the start of the haul in decimal degrees}
#'      \item{END_LONGITUDE}{Vessel longitude at the start of the haul in decimal degrees}
#'      \item{END_LATITUDE}{Vessel latitude at the start of the haul in decimal degrees}
#'      \item{GEAR_NAME}{Abbreviated name of the gear, 83-112 (83-112 eastern otter trawl )or PNE (Poly nor'eastern)}
#'      \item{GEAR}{Bottom trawl survey gear code.}
#'      \item{ACCESSORIES}{Acessory code for footrope, benthic bag, etc.}
#'      \item{WIRE_LENGTH_M}{Length of wire deployed during a given haul in meters.}
#'      \item{NET_MEASURED}{Were upper wing tip spread spread and opening headline height measurements available from acoustic sensors? TRUE or FALSE}
#'      \item{NET_WIDTH_M}{Measured or estimated distance (meters) between the upper wingtips of the trawl.}
#'      \item{NET_HEIGHT_M}{Measured or estimated distance (meters) between headline and sea floor at the center of the headline.}
#'      \item{DISTANCE_FISHED_KM}{Distance the net fished between start and end position in kilometers.}
#'      \item{DURATION_HR}{Time elapsed between start and end positions in hours.}
#'      \item{DEPTH_M}{Bottom depth (meters).}
#'      \item{TOTAL_WEIGHT_KG}{Total weight of the catch in kilograms}
#' }
#' @source \url{https://www.fisheries.noaa.gov/contact/groundfish-assessment-program}
"bts_geom"

#' Flume tank experimental data
#' 
#' Net measurement data from flume tank experiments conducted at the Centre for Sustainable Aquatic Resources (CSAR), Marine Institute, Memorial University of Newfoundland, St. John's, Newfoundland and Labrador from January 13-17, 2025. CURRENTLY EXAMPLE DATA.
#' 
#' @format A data frame
#' \describe{
#'      \item{trial}{Trial number.}
#'      \item{trawl}{Trawl model (e.g., "83-112", "PNE", "RACE").}
#'      \item{bridles}{Type of bridles used (e.g., "standard", "bridle 1", "bridle 2").}
#'      \item{bridle_length}{Bridle length as character vector.}
#'      \item{footrope}{Footrope used on the trawl.}
#'      \item{benthic_bag}{Was a model benthic bag attached to the footrope?}
#'      \item{marport}{Were model Marport spread and height sensors attached to the trawl?}
#'      \item{ctd}{Was a model CTD attached to the trawl?}
#'      \item{floats_n}{Number of floats attached to the headline of the model.}
#'      \item{total_buoyancy_kgf}{Total buoyancy in kilograms of force}
#'      \item{additional_floatation_kg}{Additional flotation attached to the headline, in kilograms of force, relative to the base configuration.}
#'      \item{bcs_unit}{Was a full size bottom contact sensor unit attached to the footrope? If NA, no unit was deployed.}
#'      \item{catch}{Catch treatment. Either Empty or catch level 1,2, or 3.}
#'      \item{pulling_point_elevation_mm}{Elevation of the pulling point on the trawl mast in meters.}
#'      \item{bridle_u_length_m}{Upper bridle length in meters.}
#'      \item{bridle_l_length_m}{Lower bridle length in meters.}
#'      \item{sweep_length_m}{Sweep length in meters.}
#'      \item{u_bridle_extension_m}{Upper bridle extension length in meters.}
#'      \item{towing_speed_kn}{Towing speed in knots.}
#'      \item{door_m}{Door spread in meters.}
#'      \item{spread_treatment}{Upper wing tip spread treatment.}
#'      \item{spread_u_wing_m}{Upper wing spread in meters.}
#'      \item{spread_m_wing_m}{Middle wing spread in meters.}
#'      \item{spread_l_wing_m}{Lower wing spread in meters.}
#'      \item{spread_mean_we_m}{Mean wing spread in meters.}
#'      \item{opening_wing_m}{Wing opening height in meters.}
#'      \item{opening_headline_m}{Headline opening height in meters.}
#'      \item{upper_wingend_height_m}{Upper wing tip end distance from bottom in meters.}
#'      \item{lower_wingend_height_m}{Lower wing tip end distance from bottom in meters. NA = 0}
#'      \item{fishing_line_height_m}{Distance between the fishing line and bottom, in meters.}
#'      \item{bridle_tension_port_t}{Port bridle/sweep tension in metric tons.}
#'      \item{bridle_tension_stbd_t}{Starboard bridle/sweep tension in metric tons.}
#'      \item{bridle_tension_total_t}{Sum of port and starboard bridle/sweep tension}
#'      \item{mouth_area_m2}{Mouth opening area in square meters.}
#'      \item{mouth_drag_kgf_m2}{Mouth drag in kilogram-force per square meter.}
#'      \item{bridle_angle_deg}{Bridle angle of attack in degrees.}
#' }
#' @source \url{https://www.fisheries.noaa.gov/contact/groundfish-assessment-program}
"flume_tank"