# Post-survey haul data processing
# Created by: Caitlin Allen Akselrud and Liz Dawson
# Contact: caitlin.allen_akselrud@noaa.gov
# Created: 2022-03-04
# Modified: 2022-03-04


# libraries ---------------------------------------------------------------
library(tidyverse)
library(RODBC)
library(here)
library(janitor)

functions <- list.files(here::here("functions"))
purrr::walk(functions, ~ source(here::here("functions", .x)))


# annually-dependent fixed values -----------------------------------------

# USER SPECIFIED
# annual cruise_id
cruise_idnum <- c(726, 731)


# connect to oracle -------------------------------------------------------

# make sure you are connected to VPN first (and have Oracle database login)
channel <- odbcConnect(dsn = "AFSC", 
                       uid = rstudioapi::showPrompt(title = "Username", 
                                                    message = "Oracle Username", default = ""), 
                       pwd = rstudioapi::askForPassword("enter password"),
                       believeNRows = FALSE)

## checks to see if connection has been established
odbcGetInfo(channel)

# data --------------------------------------------------------------------
# call from oracle

query_command <- paste0(" select * from race_data.edit_hauls where performance >= 0;")
haul_data_raw <- sqlQuery(channel, query_command)

# data cleaning -----------------------------------------------------------
haul_data <- haul_data_raw %>% 
  as_tibble() %>% 
  clean_names %>% 
  dplyr::filter(haul_type %in% c(3, 17),  # haul_type 3 = standard; 17 = crab re-survey tow (standard)
                cruise_id %in% cruise_idnum)

special_projects_hauls <- haul_data_raw %>% 
  clean_names %>%
  dplyr::filter(haul_type %in% c(7),
                cruise_id %in% cruise_idnum)


# here you can create a data set and filter by other haul types as needed

# sequential outlier rejection (SOR) --------------------------------------

# notes: from code stored in shared drive:
# # do_sor_table.R sources functions in sor_table.R and means_sor.R
# # written by S. Kotwicki 

# events=read.events()
events <- read.csv(here('data', 'AlaskaKnight_202101_Events.csv'))
# haul.data=read.hauls()
haul.data <- read.csv(here('data', 'AlaskaKnight_202101_Spread.csv'))
sor(haul.data,events,flag=12) #graphics output change
output_sor_table()
dev.off()
sor.means <- estimate.means(events)  #graphics output change
write.csv(sor.means, 'sor_means.csv')

# notes: change graphics displays to save output
# which model type does stand use in SOR? predict: linear model?

# data prep: filter pings for on/off bottom time
on_bottom <- events %>% 
  as_tibble() %>% 
  dplyr::filter(EVENT == 3) %>% 
  rename(on_bottom_time = DTIME)
off_bottom <- events %>% 
  as_tibble() %>% 
  dplyr::filter(EVENT == 7)%>% 
  rename(off_bottom_time = DTIME)

data_prep <- #full_join(haul.data, events) %>% 
  haul.data %>% 
  as_tibble() %>% 
  full_join(on_bottom) %>%
  full_join(off_bottom, by = c("VESSEL", "CRUISE", "HAUL")) %>% 
  # if haul data date_time is within events for matching cruise/haul, keep
  dplyr::filter(DATE_TIME >= on_bottom_time) %>%
  dplyr::filter(DATE_TIME <= off_bottom_time)
# on.bottom = time in events that = 3; select hauls with time >= time at at 3 FOR EACH HAUL
# off.bottom

formula_sor <- MEASUREMENT_VALUE~DATE_TIME #measurement value ~ date_time for all pings filtered within on/off bottom time
# then put data into fxn
sor_sr <- sequentialOR(data = data_prep, method = 'lm', formula = formula_sor, n.reject = 1, n.stop = 0.001, threshold.stop = NULL, 
                         tail = "both", plot = T, progress.plot = F)
# CIA: you are here- error in creating 'mod' in SOR function. Ask Sean about input setup. 

# need to return: haul, corrected number of pings, returns corrected average spread, and SD 

# marport-netmind correction ----------------------------------------------
# Note: this is for EBS- does GOA have diff correction?

# missing data ------------------------------------------------------------


# export corrected haul data ----------------------------------------------

# make col header names all caps and match input names
