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
vessels <- c(94, 162)
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

# height data:
# pull mean height data after CALPYSO done

# write_csv(edit_haul, path = here("edit_haul.csv"))
# write_csv(edit_sgp, path = here("output ,"test_edit_sgp.csv"))
# write_csv(edit_sgt, path = here("output ,"test_edit_sgt.csv"))

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
# data = data.frame(t = 1:100, val = rnorm(n = 100)+ 8*(rbinom(100, size = 1, prob = 0.05)))
# plot(x = data$t, y = data$val)
# 
# sor_sr <- sequentialOR(data = data, method = 'ss', formula = val~t, 
#                        n.reject = 1, n.stop = 0.5, threshold.stop = TRUE, 
#                        tail = "both", plot = T, progress.plot = F)
# 
# sor_sr
# sor_sr$obs_rank %>% arrange(SOR_RANK)
# keep_data <- sor_sr$obs_rank %>% 
#   dplyr::filter(is.na(SOR_RANK))
# 
# plot(keep_data$t, keep_data$val)


# * clean SOR data -------------------------------------------------------------------------
# event_dat <- read_csv(here('data', 'AlaskaKnight_202101_Events.csv')) %>% 
#   clean_names %>% 
#   mutate(date_time = lubridate::mdy_hms(dtime)) %>% 
#   dplyr::select(-dtime)
# haul_dat <- read_csv(here('data', 'AlaskaKnight_202101_Spread.csv')) %>% 
#   clean_names %>% 
#   mutate(date_time = lubridate::mdy_hms(date_time))


# Subset pings for only ones that are between on and off bottom time
sor_data <- haul_dat %>% 
  filter(measurement_value >= 10, 
         measurement_value <= 22) %>% 
  full_join(event_dat) %>% 
  arrange(date_time) %>% 
  mutate(bad_data = if_else(haul %in% skip_haul & vessel %in% skip_vessel, TRUE, FALSE)) %>% 
  dplyr::filter(bad_data == FALSE) %>%
  add_column(start = NA, end = NA) 

# sor_data_test <- sor_data %>% dplyr::filter(haul == 51, vessel == vessel[1])
# test_pings <- get_pings2(data = sor_data_test)
# sor_data_test2 <- sor_data_test %>% arrange(event) %>% dplyr::filter(!is.na(event))
# test_pings <- get_pings2(data = sor_data_test2[2:5,])

ping_data <- sor_data %>% 
  group_by(vessel, haul) %>% 
  dplyr::group_map(~get_pings2(data = .x)) 

# final pings have pings between on and off bottom time; filter net spread between 10 and 22 m (measurement value)
final_pings <- do.call(rbind, ping_data) %>% 
  left_join(sor_data %>% dplyr::select(-start, -end)) #%>% #by = c("cruise", "date_time", "cabinet_sensor_flag", "measurement_value", "datum_code", "event")) #add vessel and haul info back into selected rows
  

# good ping hauls are hauls with > 4 pings; hauls of concern have <50 pings
# for SOR, check whether any hauls have fewer than 4 pings (will break code)
check_pings <- final_pings %>% 
  group_by(vessel, haul) %>% 
  summarise(n_pings = n()) %>% 
  dplyr::filter(n_pings < 5)
check_pings

good_ping_hauls <- final_pings %>% 
  anti_join(check_pings)

bad_ping_hauls <- final_pings %>% 
  inner_join(check_pings)

# flag hauls to check that have fewer than 50 pings
flag_pings <- final_pings %>% 
  group_by(vessel, haul) %>% 
  summarise(n_pings = n()) %>% 
  dplyr::filter(n_pings < 50)
flag_pings

flag_ping_hauls <- final_pings %>% 
  inner_join(flag_pings)

# * * manual method for checks ---------------------------------------------

# MANUAL METHOD BELOW: for testing and weeding out any problem tows causing errors
# n <-  1
# test_list1 <-  list()
# hauls_vessel1 <- good_ping_hauls %>% dplyr::filter(vessel == vessels[1])
# hauls_vessel2 <- good_ping_hauls %>% dplyr::filter(vessel == vessels[2])
# 
# # for(v in vessels[1])
# # {
#   for(h in unique(hauls_vessel1$haul))
#   {
#     # sor_data_sub <- good_ping_hauls %>% dplyr::filter(haul == h, vessel == v)
#     sor_data_sub <- good_ping_hauls %>% dplyr::filter(haul == h, vessel == vessels[1])
#     # if(sor_data_sub$event ) #detect if sor data is missing event 3 or 7
#     test_list1[[n]] <- get_pings2(data = sor_data_sub)
#     n <- n+1
#     print(paste("vessel:", v, "and haul:", h))
#     print(paste(" n =", n))
#   }
# # }
# 
# n <-  1
# test_list2 <-  list()
# for(v in vessels[2])
# {
#   for(h in unique(hauls_vessel2$haul))
#   {
#     sor_data_sub <- good_ping_hauls %>% dplyr::filter(haul == h, vessel == v)
#     # if(sor_data_sub$event ) #detect if sor data is missing event 3 or 7
#     test_list2[[n]] <- get_pings2(data = sor_data_sub)
#     n <- n+1
#     print(paste("vessel:", v, "and haul:", h))
#     print(paste(" n =", n))
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

# * * testing code --------------------------------------------------------

# ping_test <- good_ping_hauls %>% dplyr::filter(vessel == vessel[1], haul == 1) %>% 
#   as.data.frame()
# plot(x = ping_test$date_time, y = ping_test$measurement_value)
# 
# sor_test <- sequentialOR(data = ping_test, method = 'ss', 
#                          # formula = data.sub$response_var~data.sub$predictor.var, #or formula = response_var~predictor.var 
#                          formula = measurement_value ~ date_time,
#                          n.reject = 1, n.stop = 0.5, threshold.stop = TRUE, 
#                          tail = "both", plot = T, progress.plot = F)
# # rmse
# plot(x = sor_test$rmse$N, y = sor_test$rmse$RMSE, main = "RMSE")
# # pings
# not_rejected <- sor_test$obs_rank %>% dplyr::filter(is.na(SOR_RANK))
# # initial data
# plot(x = ping_test$date_time, ping_test$measurement_value, 
#      ylim = c(10, 22), main = "original")
# # after sor
# plot(x = not_rejected$date_time, not_rejected$measurement_value, 
#      ylim = c(10,22), main = "after sor")
# 
start_time <- Sys.time()
n <-  1
test_list <-  list()
for(h in unique(good_ping_hauls$haul))
{
  for(v in unique(good_ping_hauls$vessel))
  {
    sor_data_sub <- good_ping_hauls %>% dplyr::filter(haul == h, vessel == v)
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


# * * do SOR --------------------------------------------------------------
# test
sor_test <- good_ping_hauls %>% dplyr::filter(vessel == vessel[[1]], haul %in% unique(good_ping_hauls$haul)[31:40])

start_time <- Sys.time()
sor_vessel1 <- sor_test %>%  # good_ping_hauls %>% 
  group_by(vessel, haul) %>% 
  # as.data.frame() %>% 
  dplyr::group_map(~sequentialOR(data = .x, #as.data.frame(good_ping_hauls), 
                                 method = 'ss', 
                                 # formula = data.sub$response_var~data.sub$predictor.var, #or formula = response_var~predictor.var 
                                 formula = measurement_value ~ date_time,
                                 n.reject = 1, n.stop = 0.5, threshold.stop = TRUE, 
                                 tail = "both", plot = T, progress.plot = F))
stop_time <- Sys.time()

# get mean SOR ------------------------------------------------------------

sor_v1_dat <- list()
sor_v1_data <- tibble() 
sor_res <- list()
sor_v1_results <- tibble()
for(i in 1:length(sor_vessel1))
{
  sor_v1_dat[[i]] <- sor_vessel1[[i]]$obs_rank %>% inner_join(sor_test)
  sor_v1_data <- sor_v1_data %>% bind_rows(sor_v1_dat[[i]])
  sor_res[[i]] <- bind_cols(sor_vessel1[[i]]$results, vessel = unique(sor_v1_dat[[i]]$vessel), haul = unique(sor_v1_dat[[i]]$haul))
  sor_v1_results <- sor_v1_results %>% bind_rows(sor_res[[i]])
  
  # CIA: generate plots here, with title including vessel and haul number; before and after plots
}


# sor_final <- sor_test_final
# 
# # the following is breaking rstudio:
# sor_all <- good_ping_hauls %>% 
#   group_by(vessel, haul) %>% 
#   dplyr::group_map(~sequentialOR(data = .x, #as.data.frame(good_ping_hauls), 
#                                  method = 'ss', 
#                                  # formula = data.sub$response_var~data.sub$predictor.var, #or formula = response_var~predictor.var 
#                                  formula = measurement_value ~ date_time,
#                                  n.reject = 1, n.stop = 0.5, threshold.stop = TRUE, 
#                                  tail = "both", plot = T, progress.plot = F))
# 
# # then you want: do.call(rbind, sor_all) %>% left_join(sor_data, by = c(...))
# sor_final <- do.call(rbind, sor_all) %>% left_join(good_ping_hauls)
# ca; want n.reject set 1 to reject one point at a time
# ca: I expect the predictor is time, and the response in measurement_value, but check
# ca: add stan's plots with updates (two plots for: rejected and not rejected; or one plot with colors for rejected vs not)
# ca: add flag for hauls to check? (track haul id for ones with issues)

# generate SOR plots ------------------------------------------------------


# reject SOR --------------------------------------------------------------

# no pings or very few; fewer than 50 pings cutoff after SOR
# # if all pings are clustered in one time period of the tow, rather than spread across the whole time period

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
# # net height method: 4- "estimate from other hauls" for missing height hauls
# # net spread method: 7- "SOR method" UNLESS you are estimating the missing spread from other hauls

# # change method code in database/GIDES
# # THEN upload the mean, sd, and ping numbers (new net spread, new # pings, and new standard deviation)

# write to oracle
# # RACE_DATA:EDIT_HAULS
# # sqlSave(channel, dat, tablename = NULL, append = FALSE,
# rownames = TRUE, colnames = FALSE, verbose = FALSE,
# safer = TRUE, addPK = FALSE, typeInfo, varTypes,
# fast = TRUE, test = FALSE, nastring = NULL)

# 1) generate tables with mean, sd, pings for height and spread
# 2) re-create race_data:edit_haul table to replace ; CHECK carefully ; download this table prior as .csv and save
# 3) produce updated table and ask Heather how to merge