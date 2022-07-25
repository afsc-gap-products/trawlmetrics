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
cruise <- c(202201) #202201
vessel <- c(94, 162)
region <- "BS"

# for troublshooting problem data:
skip_haul <-  c(145)
skip_vessel <- c(94)

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

# query_command <- paste0(" select * from race_data.edit_hauls where performance >= 0;")
# haul_data_raw <- sqlQuery(channel, query_command)
# 
# query_command <- paste0(" select * from race_data.edit_events;")
# event_data_raw <- sqlQuery(channel, query_command)
# 
# query_command <- paste0(" select * from race_data.v_extract_edit_haul where cruise in (", cruise,") and region = '", region, "';")
# edit_haul <- sqlQuery(channel, query_command)

query_command <- paste0(" select * from race_data.v_extract_edit_sgp where cruise in (", cruise,") and region = '", region, "';")
edit_sgp <- sqlQuery(channel, query_command)
# VALUE renamed MEASUREMENT_VALUE
# this one is haul.data
# this is net SPREAD data

query_command <- paste0(" select * from race_data.v_extract_edit_sgt where cruise in (", cruise,") and region = '", region, "';")
edit_sgt <- sqlQuery(channel, query_command)
# TIME_FLAG = EVENT
# this one is events

# write_csv(edit_haul, path = here("edit_haul.csv"))
# write_csv(edit_sgp, path = here("edit_sgp.csv"))
# write_csv(edit_sgt, path = here("edit_sgt.csv"))

# data cleaning -----------------------------------------------------------

event_dat <- edit_sgt %>% 
  as_tibble() %>% 
  clean_names() %>% 
  dplyr::rename(event = time_flag) %>% 
  dplyr::select(cruise, vessel, haul, date_time, event)

haul_dat <- edit_sgp %>% 
  as_tibble() %>% 
  clean_names() %>% 
  dplyr::rename(measurement_value = value) %>% 
  dplyr::select(cruise, vessel, haul, date_time, cabinet_sensor_flag, measurement_value, datum_code)
# CIA: note, missing record_id number-- needed in stan's function

# sequential outlier rejection (SOR) --------------------------------------

# * Stan's method ---------------------------------------------------------

# events=read.events()
events <- read.csv(here('data', 'AlaskaKnight_202101_Events.csv'))
str(events)
# CRUISE VESSEL HAUL EVENT DTIME

# haul.data=read.hauls()
haul.data <- read.csv(here('data', 'AlaskaKnight_202101_Spread.csv'))
str(haul.data)
# RECORD_ID HAUL_ID CRUISE_ID VESSEL CRUISE HAUL DATE_TIME CABINET_SENSOR_FLAG MEASUREMENT_VALUE DATUM_CODE


# Call SOR function
sor(haul.data,events,flag=12) #graphics output change
output_sor_table()
dev.off()
sor.means <- estimate.means(events)  #graphics output change
write.csv(sor.means, 'sor_means.csv')

# notes: change graphics displays to save output
# which model type does stand use in SOR? predict: linear model?


# * SOR Rohan code ----------------------------------------------------------

# * * example ---------------------------------------------------------------

# example data:
data = data.frame(t = 1:100, val = rnorm(n = 100)+ 8*(rbinom(100, size = 1, prob = 0.05)))
plot(x = data$t, y = data$val)

sor_sr <- sequentialOR(data = data, method = 'ss', formula = val~t, 
                       n.reject = 1, n.stop = 0.5, threshold.stop = TRUE, 
                       tail = "both", plot = T, progress.plot = F)

sor_sr
sor_sr$obs_rank %>% arrange(SOR_RANK)
keep_data <- sor_sr$obs_rank %>% 
  dplyr::filter(is.na(SOR_RANK))

plot(keep_data$t, keep_data$val)


# * clean SOR data -------------------------------------------------------------------------
# event_dat <- read_csv(here('data', 'AlaskaKnight_202101_Events.csv')) %>% 
#   clean_names %>% 
#   mutate(date_time = lubridate::mdy_hms(dtime)) %>% 
#   dplyr::select(-dtime)
# haul_dat <- read_csv(here('data', 'AlaskaKnight_202101_Spread.csv')) %>% 
#   clean_names %>% 
#   mutate(date_time = lubridate::mdy_hms(date_time))


# Subset pings for only ones that are between on and off bottom time
sor_data <- full_join(haul_dat, event_dat) %>% 
  arrange(date_time) %>% 
  mutate(bad_data = if_else(haul %in% skip_haul & vessel %in% skip_vessel, TRUE, FALSE)) %>% 
  dplyr::filter(bad_data == FALSE) %>%
  add_column(start = NA, end = NA) 

# new_data <- NULL
  # mutate(start = NULL, end = NULL) %>% 
# # for EACH HAUL, extract rows between event 3 and 7 (there will be many event = NA rows between 3 and 7 that you want)
# # you need purrr for each haul, and time >= time at event = 3 and <= time at event = 7, save row, otherwise filter out (remove) row
# 1) time of event 3 at each haul (purrr for each haul, mutate START col with time of event 3 for that haul)
# 2) time of event 7 at each haul (purrr for each haul, mutate STOP col with time of event 7 for that haul)
# 3) for each haul, save rows between times of event 3 and 7 (purrr each haul, dplyr::filter times between START and STOP cols)

# functions: purrr, case_when, while, 

# for(i in unique(data$haul))
# {
#   data_sub <- data %>% 
#     dplyr::filter(haul == i) 
#   start_t <- data_sub %>% dplyr::filter(event == 3) %>% dplyr::select(date_time)
#   end_t <- data_sub %>% dplyr::filter(event == 7) %>% dplyr::select(date_time)
#   data_sub <- data_sub %>% 
#     mutate(start = start_t$date_time, end = end_t$date_time) %>% 
#     dplyr::filter(date_time >= start & date_time <= end)
#   new_data <- bind_rows(new_data, data_sub)
# 
# }

# sor_data_test = sor_data %>% dplyr::filter(haul == 1, vessel == vessel[1])
# get_pings(sor_data_test, hauln = 1, vesseln = vessel[1])
# get_pings2(sor_data_test)
# 
# 
# ping_data <- sor_data %>% 
#   group_by(vessel, haul) %>% 
#   dplyr::group_map(~get_pings(data = .x, 
#                               hauln = .x$haul, 
#                               vesseln = .x$vessel))

ping_data <- sor_data %>% 
  group_by(vessel, haul) %>% 
  dplyr::group_map(~get_pings2(data = .x)) 


final_pings <- do.call(rbind, ping_data) %>% 
  left_join(sor_data %>% dplyr::select(-start, -end)) %>% #by = c("cruise", "date_time", "cabinet_sensor_flag", "measurement_value", "datum_code", "event")) #add vessel and haul info back into selected rows
  filter(measurement_value >= 10, measurement_value <= 22)

final_pings

# ** manual method for checks ---------------------------------------------

# MANUAL METHOD BELOW: for testing and weeding out any problem tows causing errors
# n <-  1
# test_list <-  list()
# for(h in unique(sor_data$haul))
# {
#   for(v in unique(sor_data$vessel))
#   {
#     sor_data_sub <- sor_data %>% dplyr::filter(haul == h, vessel == v)
#     # if(sor_data_sub$event ) #detect if sor data is missing event 3 or 7
#     test_list[[n]] <- get_pings2(data = sor_data_sub)
#     n <- n+1
#   }
# }

# troubleshooting:
# n
# index_check <- rep(unique(sor_data$haul), times = 2) %>% 
#   sort() %>% 
#   cbind(unique(sor_data$vessel))
# index_check[n, 1:2]
# h = index_check[n, 1]
# v = index_check[n, 2]
# data = sor_data_sub

# final_sor_data <- do.call(rbind, test_list)


# * * do SOR --------------------------------------------------------------

# loop over hauls here (or parallel) AND VESSELS
#     for(i in haul1 to haulx)
# then put data into fxn


# * * * test --------------------------------------------------------------

ping_test <- final_pings %>% dplyr::filter(vessel == vessel[1], haul == 1) %>% 
  as.data.frame()
plot(x = ping_test$date_time, y = ping_test$measurement_value)

sor_test <- sequentialOR(data = ping_test, method = 'ss', 
                       # formula = data.sub$response_var~data.sub$predictor.var, #or formula = response_var~predictor.var 
                       formula = measurement_value ~ date_time,
                       n.reject = 1, n.stop = 0.5, threshold.stop = TRUE, 
                       tail = "both", plot = T, progress.plot = F)
# rmse
plot(x = sor_test$rmse$N, y = sor_test$rmse$RMSE, main = "RMSE")
# pings
not_rejected <- sor_test$obs_rank %>% dplyr::filter(is.na(SOR_RANK))
# initial data
plot(x = ping_test$date_time, ping_test$measurement_value, 
     ylim = c(10, 22), main = "original")
# after sor
plot(x = not_rejected$date_time, not_rejected$measurement_value, 
     ylim = c(10,22), main = "after sor")

start_time <- Sys.time()
n <-  1
test_list <-  list()
for(h in unique(final_pings$haul)[1:5])
{
  for(v in unique(final_pings$vessel))
  {
    sor_data_sub <- final_pings %>% dplyr::filter(haul == h, vessel == v)
    # if(sor_data_sub$event ) #detect if sor data is missing event 3 or 7
    test_list[[n]] <- sequentialOR(data = sor_data_sub, method = 'ss', 
                                   # formula = data.sub$response_var~data.sub$predictor.var, #or formula = response_var~predictor.var 
                                   formula = measurement_value ~ date_time,
                                   n.reject = 1, n.stop = 0.5, threshold.stop = TRUE, 
                                   tail = "both", plot = T, progress.plot = F)
    n <- n+1
  }
  stop_time <- Sys.time()
}



# * * * all SOR -----------------------------------------------------------

sor_all <- final_pings %>% 
  group_by(vessel, haul) %>% 
  dplyr::group_map(~sequentialOR(data = .x, #as.data.frame(final_pings), 
                                 method = 'ss', 
                                 # formula = data.sub$response_var~data.sub$predictor.var, #or formula = response_var~predictor.var 
                                 formula = measurement_value ~ date_time,
                                 n.reject = 1, n.stop = 0.5, threshold.stop = TRUE, 
                                 tail = "both", plot = T, progress.plot = F))

# then you want: do.call(rbind, sor_all) %>% left_join(sor_data, by = c(...))
sor_final <- do.call(rbind, sor_all) %>% left_join(final_pings)
# ca; want n.reject set 1 to reject one point at a time
# ca: I expect the predictor is time, and the response in measurement_value, but check
# ca: add stan's plots with updates (two plots for: rejected and not rejected; or one plot with colors for rejected vs not)
# ca: add flag for hauls to check? (track haul id for ones with issues)

# -------------------------------------------------------------------------
 # CIA: older section

# data prep: filter pings for on/off bottom time
on_bottom <- events %>% 
  as_tibble() %>% 
  dplyr::filter(EVENT == 3) %>% 
  dplyr::mutate(DTIME = as.numeric(as.POSIXct(DTIME, format = "%m/%d/%Y %H:%M:%S", tz = "America/Anchorage"))) %>%
  rename(on_bottom_time = DTIME)
off_bottom <- events %>% 
  as_tibble() %>% 
  dplyr::filter(EVENT == 7)%>% 
  dplyr::mutate(DTIME = as.numeric(as.POSIXct(DTIME, format = "%m/%d/%Y %H:%M:%S", tz = "America/Anchorage"))) %>%
  rename(off_bottom_time = DTIME)
# note that gulf does EQ to haulback time; want flex inside fxn

data_prep <- #full_join(haul.data, events) %>% 
  haul.data %>% 
  as_tibble() %>% 
  full_join(on_bottom) %>%
  full_join(off_bottom, by = c("VESSEL", "CRUISE", "HAUL")) %>% 
  # if haul data date_time is within events for matching cruise/haul, keep
  # dplyr::filter(HAUL_ID == 19865) %>%
  dplyr::mutate(DATE_TIME = as.numeric(as.POSIXct(DATE_TIME, format = "%m/%d/%Y %H:%M:%S", tz = "America/Anchorage"))) %>% 
  dplyr::filter(DATE_TIME >= on_bottom_time) %>%
  dplyr::filter(DATE_TIME <= off_bottom_time)
# on.bottom = time in events that = 3; select hauls with time >= time at at 3 FOR EACH HAUL
# off.bottom

plot(x = data_prep$MEASUREMENT_VALUE) #quick check

formula_sor <- MEASUREMENT_VALUE~DATE_TIME #measurement value ~ date_time for all pings filtered within on/off bottom time

# then put data into fxn
# loop over hauls here (or parallel)
# for(i in haul1 to haulx)
# # ca: add threshold calc for stopping for each haul, then give to threshold.stop in sor fxn
sor_sr <- sequentialOR(data = data_prep, method = 'ss', formula = data.sub$response_var~data.sub$predictor.var, 
                       n.reject = 1, n.stop = 0.5, threshold.stop = TRUE, 
                         tail = "both", plot = T, progress.plot = F)
# CIA: you are here- error in creating 'mod' in SOR function. Ask Sean about input setup. 
# ca: add method = "smooth.spline" to match Stan's 
# ca: need to mod threshold stop with stan's **
# ca; want n.reject set 1 to reject one point at a time
# ca: add stan's plots with updates (two plots for: rejected and not rejected; or one plot with colors for rejected vs not)
# ca: add flag for hauls to check? (track haul id for ones with issues)
# ca: future note- may be some changes once we have salinity data -> changes in speed of sound -> changes in pings (Stan assumes a salinity- 32 psu?)

# ca: sor_sr$obs_rank
# # the lowest SOR (in SOR_RANK) was the point rejected first; NAs were not looked at 
# # sor_sr$rmse = order of points correspond with ranks
plot(x = sor_sr$rmse$RMSE)


# with threshold
# sor_sr <- sequentialOR(data = data_prep, method = 'lm', formula = formula_sor, n.reject = 1, n.stop = 0.5, threshold.stop = 1, #stop when max residual = 1 
#                        tail = "both", plot = T, progress.plot = F)
# plot(x = sor_sr$rmse$RMSE)

# note: in Stan's code, when not enough pings, ouput to indicate that


# need to return: haul, corrected number of pings, returns corrected average spread, and SD 

# marport-netmind correction ----------------------------------------------
# Note: this is for EBS- does GOA have diff correction?


# missing data ------------------------------------------------------------


# export corrected haul data ----------------------------------------------

# make col header names all caps and match input names

# need to figure out which hauls to make method code # changes to in 
# # GIDES when we fill in missing data
