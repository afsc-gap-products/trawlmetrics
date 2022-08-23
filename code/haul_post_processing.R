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
library(cowplot)

functions <- list.files(here::here("functions"))
purrr::walk(functions, ~ source(here::here("functions", .x)))


# annually-dependent fixed values -----------------------------------------

# USER SPECIFIED
# annual cruise_id

# 22 NBS
cruise <- c(202202) #
# cruise id num 726 = vessel 94; cruise id 756 = vessel 162
cruise_idnum <- c(757, 758) # make sure cruise id num 1 = vessel 1 and cruise id num 2 = vessel 2
vessel <- c(94, 162)
vessels <- c(94, 162)
region <- "BS"

# 22 EBS
# cruise <- c(202201) #202201
# # cruise id num 726 = vessel 94; cruise id 756 = vessel 162
# cruise_idnum <- c(755, 756) # make sure cruise id num 1 = vessel 1 and cruise id num 2 = vessel 2
# vessel <- c(94, 162)
# vessels <- c(94, 162)
# region <- "BS"

# 21 comparison
# cruise <- c(202101) #202201
# # cruise id num 726 = vessel 94; cruise id 756 = vessel 162
# cruise_idnum <- c(752) # make sure cruise id num 1 = vessel 1 and cruise id num 2 = vessel 2
# vessel <- c(162)
# vessels <- c(162)
# region <- "BS"

# for troublshooting problem data:
skip_haul <-  c() #change to hauljoins?
skip_vessel <- c()

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

# check cruise id num
query_command <- paste0(" select * from race_data.v_cruises where year = 2021 and survey_definition_id = 98;") # 98 = BS survey ; NBS = 143 #(", cruise,") and region = '", region, "';")
cruise_id_info <- sqlQuery(channel, query_command)
# CIA: fix

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
query_command <- paste0(" select * from race_data.edit_hauls where cruise_id in (", cruise_idnum[1], ",", cruise_idnum[2], ");") # and region = '", region, "';")
edit_height <- sqlQuery(channel, query_command)
# this is also the raw RACE_DATA:EDIT_HAULS table for updating at the end

write_csv(edit_sgp, path = here("output" ,"test_edit_sgp.csv"))
write_csv(edit_sgt, path = here("output" ,"test_edit_sgt.csv"))
write_csv(edit_height, path = here("output" ,"test_edit_height.csv"))

# in case you need to save and read in data
edit_sgp <- read_csv(here("output" ,"test_edit_sgp.csv"))
edit_sgt <- read_csv(here("output" ,"test_edit_sgt.csv"))
edit_height <- read_csv(here("output" ,"test_edit_height.csv"))

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

unique(edit_height$WIRE_OUT_METHOD)
unique(edit_height$EDIT_WIRE_OUT_UNITS)

height_dat <- edit_height %>% 
  as_tibble() %>% 
  clean_names() %>% 
  dplyr::select(cruise_id, haul, haul_id, edit_net_height, edit_net_height_units, 
                net_height_method, net_height_pings, net_height_standard_deviation,
                edit_wire_out, edit_wire_out_units, wire_out_method) %>% 
  mutate(cruise = cruise,
         vessel = case_when(cruise_id == cruise_idnum[1] ~vessel[1],
                            cruise_id == cruise_idnum[2] ~vessel[2]),
         
         edit_wire_out_FM = round(if_else(edit_wire_out_units == "FT", edit_wire_out*0.166667, as.numeric(edit_wire_out)),0),
         edit_wire_out_units_FM = if_else(edit_wire_out_units == "FT", "FM", "FM")) %>% 
  mutate(invscope = 1/edit_wire_out)
# note: all wire out should be in intervals of 25

edit_hauls_table_raw <- edit_height %>% 
  as_tibble() %>% 
  clean_names()

# sequential outlier rejection (SOR) --------------------------------------

# * Stan's method ---------------------------------------------------------

# events=read.events()
# events <- read.csv(here('data', 'AlaskaKnight_202101_Events.csv'))
# str(events)
# # CRUISE VESSEL HAUL EVENT DTIME
# event_dat <- events %>% 
#   as_tibble() %>% 
#   clean_names() %>% 
#   mutate(date_time = lubridate::mdy_hms(dtime))
#   # rename(date_time = DTIME) %>% 
# 
# # haul.data=read.hauls()
# haul.data <- read.csv(here('data', 'AlaskaKnight_202101_Spread.csv'))
# str(haul.data)
# # RECORD_ID HAUL_ID CRUISE_ID VESSEL CRUISE HAUL DATE_TIME CABINET_SENSOR_FLAG MEASUREMENT_VALUE DATUM_CODE
# haul_dat <- haul.data %>% 
#   as_tibble() %>% 
#   clean_names() %>% 
#   mutate(date_time = lubridate::mdy_hms(date_time)) %>% 
#   dplyr::select(cruise, vessel, haul, date_time, cabinet_sensor_flag, measurement_value, datum_code)

# # Call SOR function
# sor(haul.data,events,flag=12) #graphics output change
# output_sor_table()
# dev.off()
# sor.means <- estimate.means(events)  #graphics output change
# write.csv(sor.means, 'sor_means.csv')

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

# Subset pings for only ones that are between on and off bottom time
sor_data <- haul_dat %>% 
  filter(measurement_value >= 10,       # this is per trawl scpoe: net spread should be btw 10-22 m
         measurement_value <= 22) %>% 
  full_join(event_dat) %>% 
  arrange(date_time) %>% 
  mutate(bad_data = if_else(haul %in% skip_haul & vessel %in% skip_vessel, TRUE, FALSE)) %>% # in case there is a bad haul causing problems, you can filter that here
  dplyr::filter(bad_data == FALSE) %>%
  add_column(start = NA, end = NA) 

# get pings that are between the on-bottom (event 3) and off-bottom (event 7) times
ping_data <- sor_data %>% 
  group_by(vessel, haul) %>% 
  dplyr::group_map(~get_pings2(data = .x)) 

# final pings have pings between on and off bottom time; filter net spread between 10 and 22 m (measurement value)
final_pings <- do.call(rbind, ping_data) %>% 
  left_join(sor_data %>% dplyr::select(-start, -end)) #%>% #by = c("cruise", "date_time", "cabinet_sensor_flag", "measurement_value", "datum_code", "event")) #add vessel and haul info back into selected rows

# check for hauls that have no data wi/in scope and add back in:
orig_hauls <- haul_dat %>% 
  distinct(vessel, haul) %>% 
  arrange(vessel, haul)
filtered_hauls <- final_pings %>% 
  distinct(vessel, haul) %>% 
  arrange(vessel, haul)
missing_hauls <- anti_join(orig_hauls, filtered_hauls)
# 2022: vest: 172, 163, 117, 118 <- no pings
missing_pings <- haul_dat %>% 
  dplyr::filter(haul %in% missing_hauls$haul,
                vessel%in% missing_hauls$vessel) %>%
  # mutate(date_time = lubridate::ymd(date_time)) %>% 
  distinct(cruise, vessel, haul) 

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
  inner_join(check_pings) %>% 
  distinct(cruise, vessel, haul) %>% 
  bind_rows(missing_pings) %>% 
  arrange(vessel, haul)

# flag hauls to check that have fewer than 50 pings
flag_pings <- final_pings %>% 
  group_by(cruise, vessel, haul) %>% 
  summarise(n_pings = n()) %>% 
  dplyr::filter(n_pings < 50) %>% 
  bind_rows(missing_pings) %>% 
  arrange(vessel, haul)
flag_pings

flag_ping_hauls <- final_pings %>% 
  inner_join(flag_pings)


# * * do SOR --------------------------------------------------------------

# test
# sor_test <- good_ping_hauls %>% dplyr::filter(vessel == vessel[[1]], haul %in% unique(good_ping_hauls$haul)[1:10])

# NOTES: I'm sorry this is clunky.
# Running all hauls though SOR at once crashes RStudio. So, in the for loop:
#  for(i in #) you need to fill in the set number for sets 1 to 8 to run in chunks
#  run this for loop, then run the following loop: for(j in 1:length(sor_set))
#   to save everything. 
# I find the SOR runs faster if you go Session -> Restart R at this point before 
#   running the next i iteration (just make sure you've run the j loop and saved everything first!).


# section method when more stremlined version break rstudio
max_v1 <- good_ping_hauls %>% dplyr::filter(vessel == vessels[[1]]) %>% distinct(haul) #%>% dim()
max_v2 <- good_ping_hauls %>% dplyr::filter(vessel == vessels[[2]]) %>% distinct(haul) %>% arrange() #%>% dim()

set <- list(bind_cols(hauls = max_v1$haul[1:50],    vess = vessels[[1]]),                   #1
            bind_cols(hauls = max_v1$haul[51:100],  vess = vessels[[1]]),                   #2
            bind_cols(hauls = max_v1$haul[101:150], vess = vessels[[1]]),                   #3
            bind_cols(hauls = max_v1$haul[151:max(max_v1$haul)],  vess = vessels[[1]]),     #4
            bind_cols(hauls = max_v2$haul[1:50],    vess = vessels[[2]]),                   #5
            bind_cols(hauls = max_v2$haul[51:100],  vess = vessels[[2]]),                   #6
            bind_cols(hauls = max_v2$haul[101:150], vess = vessels[[2]]),                   #7
            bind_cols(hauls = max_v2$haul[151: max(max_v2$haul)],   vess = vessels[[2]]))   #8

# set[99] <- list(bind_cols(hauls = max_v1$haul[1:10],    vess = vessel[[1]])) #testing set
# set[99] <- list(bind_cols(hauls = c(117, 118, 145, 163, 165, 167, 169, 172),    vess = vessels[[2]])) 
#  this set 1x, 2x, 3x, 4
for(i in 99) #:length(set))
{
  sor_test <- good_ping_hauls %>% 
    dplyr::filter(vessel == unique(set[[i]]$vess), haul %in% set[[i]]$hauls)
  
  start_time <- Sys.time()
  print(paste("Start time:", start_time))
  sor_set <- sor_test %>%  # good_ping_hauls %>% 
    group_by(vessel, haul) %>% 
    # as.data.frame() %>% 
    dplyr::group_map(~sequentialOR(data = .x, #as.data.frame(good_ping_hauls), 
                                   method = 'ss', #smooth spline
                                   # formula = data.sub$response_var~data.sub$predictor.var, #or formula = response_var~predictor.var 
                                   formula = measurement_value ~ date_time,
                                   n.reject = 1, n.stop = 0.5, threshold.stop = TRUE, 
                                   tail = "both", plot = T, progress.plot = F))
  current_set <- i
  
  stop_time <- Sys.time()
  print(paste("Stop time:", stop_time))
  gc()
}

for(j in 1:length(sor_set))
{
  this_vessel <- set[[current_set]]$vess[j]
  this_haul <- set[[current_set]]$hauls[j]
  
  sor_set[[j]]$results <- sor_set[[j]]$results %>% 
    mutate(vessel = this_vessel,
           haul = this_haul)
  
  sor_set[[j]]$rmse <- sor_set[[j]]$rmse %>% 
    mutate(vessel = this_vessel,
           haul = this_haul)
  
  write_csv(sor_set[[j]]$obs_rank %>% inner_join(sor_test), 
            here("output", "SOR_files", 
                 paste0("vessel-", this_vessel, "_haul-", this_haul, "_data.csv")))
  write_csv(sor_set[[j]]$results, here("output", "SOR_files", 
                                       paste0("vessel-", this_vessel, "_haul-", this_haul, "_results.csv")))
  write_csv(sor_set[[j]]$rmse, here("output", "SOR_files", 
                                       paste0("vessel-", this_vessel, "_haul-", this_haul, "_rmse.csv")))
}

# following loop version also breaks RStudio...?

# start_time <- Sys.time()
# n <-  1
# test_list <-  list()
# for(v in unique(good_ping_hauls$vessel))
# {
#   sor_data_sub1 <- good_ping_hauls %>% dplyr::filter(vessel == v)
#   for(h in unique(sor_data_sub1$haul))
#   {
#     sor_data_sub <- good_ping_hauls %>% dplyr::filter(haul == h, vessel == v)
#     # if(sor_data_sub$event ) #detect if sor data is missing event 3 or 7
#     test_list[[n]] <- sequentialOR(data = sor_data_sub, method = 'ss',
#                                    # formula = data.sub$response_var~data.sub$predictor.var, #or formula = response_var~predictor.var
#                                    formula = measurement_value ~ date_time,
#                                    n.reject = 1, n.stop = 0.5, threshold.stop = TRUE,
#                                    tail = "both", plot = T, progress.plot = F)
#     # ca; want n.reject set 1 to reject one point at a time
#     # ca: the predictor is time, and the response in measurement_value
#     n <- n+1
#   }
#   stop_time <- Sys.time()
# }


# parallel version crashes R
# 
# start_time <- Sys.time()
# sor_vessel1 <- sor_test %>%  # good_ping_hauls %>% 
#   group_by(vessel, haul) %>% 
#   # as.data.frame() %>% 
#   dplyr::group_map(~sequentialOR(data = .x, #as.data.frame(good_ping_hauls), 
#                                  method = 'ss', 
#                                  # formula = data.sub$response_var~data.sub$predictor.var, #or formula = response_var~predictor.var 
#                                  formula = measurement_value ~ date_time,
#                                  n.reject = 1, n.stop = 0.5, threshold.stop = TRUE, 
#                                  tail = "both", plot = T, progress.plot = F))
# stop_time <- Sys.time()

# if you need to run in chunks and read in .csvs:
sor_data <- 
  list.files(pattern = "*data.csv",
             path = here("output", "SOR_files"),
             full.names = T) %>% 
  map_df(~read_csv(.)) %>% 
  mutate(vessel_haul = as.integer(paste0(vessel, haul)))

# sd(sor_data$measurement_value)
sor_results <- 
  list.files(pattern = "*results.csv",
             path = here("output", "SOR_files"),
             full.names = T ) %>% 
  map_df(~read_csv(.)) %>% 
  mutate(vessel_haul = as.integer(paste0(vessel, haul)))

sor_rmse <- 
  list.files(pattern = "*rmse.csv",
             path = here("output", "SOR_files"),
             full.names = T ) %>% 
  map_df(~read_csv(.)) %>% 
  mutate(vessel_haul = as.integer(paste0(vessel, haul)))

# CIA: need to replace sor_vessel1[[]] below with data/results read in

# get SOR plots ------------------------------------------------------------

# sor_data
# sor_results
# sor_rmse

for(i in unique(sor_data$vessel_haul))
{
  data_sub <- sor_data %>% dplyr::filter(vessel_haul == i)
  results_sub <- sor_results %>% dplyr::filter(vessel_haul == i)
  rmse_sub <- sor_rmse %>% dplyr::filter(vessel_haul == i)
  
  # pings
  not_rejected <- data_sub %>% dplyr::filter(is.na(SOR_RANK))
  rejected <- data_sub %>% dplyr::filter(!is.na(SOR_RANK))
  
  # CIA: add n pings, mean, sd info to before and after plots
  
  # initial data
  p_init <- data_sub %>%
    ggplot()+
    geom_point(aes(x = date_time, y = measurement_value), shape = 1, size = 2.5) +
    scale_y_continuous(limits=c(10, 22), expand = c(0, 0)) +
    theme_bw() +
    labs(x = "time", y = "spread", title = "Before SOR",
         subtitle = paste("Vessel", unique(data_sub$vessel), "Haul", unique(data_sub$haul)))
  # after sor
  p_post <- not_rejected %>%
    ggplot()+
    geom_point(aes(x = date_time, y = measurement_value), shape = 1, size = 2.5) +
    scale_y_continuous(limits=c(10, 22), expand = c(0, 0)) +
    theme_bw() +
    labs(x = "time", y = "spread", title = "After SOR",
         subtitle = paste("Vessel", unique(data_sub$vessel), "Haul", unique(data_sub$haul)))
  
  p_both <- ggplot()+
    geom_point(data = not_rejected, aes(x = date_time, y = measurement_value), shape = 16, size = 2.5, alpha = 0.35) +
    geom_point(data = rejected, aes(x = date_time, y = measurement_value), shape = 16, size = 2.5, col = "red", alpha = 0.5) +
    geom_hline(data = results_sub, aes(yintercept=mean), col = "blue", cex = 1) +
    scale_y_continuous(limits=c(10, 22), expand = c(0, 0)) +
    theme_bw() +
    theme(plot.caption = element_text(hjust = 0)) +
    labs(x = "time", y = "spread", title = "All pings ",
         subtitle = paste(#"Vessel", unique(data_sub$vessel), "Haul", unique(data_sub$haul), 
                          "n_pings =",results_sub$n_pings, " mean = ", round(results_sub$mean, 2), 
                          " sd =", round(results_sub$sd, 2)),
         caption = "red = rejected by SOR, blue = mean")
  
  # rmse
  p_rmse <- rmse_sub %>%
    ggplot()+
    geom_point(aes(x = N, y = RMSE), shape = 17, size = 2.5) +
    theme_bw() +
    labs(x = "iteration number", y = "rmse", title = "RMSE",
         subtitle = paste()) #"Vessel", unique(rmse_sub$vessel), "Haul", unique(rmse_sub$haul)))
  p_full <- plot_grid(p_init, p_post, p_both, p_rmse) # blank plot:, ggplot() + theme_bw() + theme(panel.border = element_blank()))
  ggsave(p_full, filename = paste0("vessel-", unique(data_sub$vessel), "_haul-", unique(data_sub$haul), "_sor_plot.png"),
         path = here("output", "SOR_graphics"), width = 10, height = 6)
}

# reject SOR --------------------------------------------------------------

# no pings or very few; fewer than 50 pings cutoff after SOR
# # if all pings are clustered in one time period of the tow, rather than spread across the whole time period

review_hauls <- sor_results %>% 
  mutate(process = "SOR") %>% 
  dplyr::filter(n_pings <= 50) %>% 
  mutate(cruise = cruise) %>% 
  full_join(flag_pings) %>% 
  mutate(process = if_else(is.na(process), "preSOR", "SOR"))

review_hauls_final <- review_hauls #%>% View()
# CIA: add mechanism to review these hauls

# marport-netmind correction ----------------------------------------------
# Note: this is for EBS- does GOA have diff correction?
# Converting to Netmind: netmind spread = 0.935684155 * mean marport spread (after sequential outlier rejection) + 0.400465037

net_spread <- sor_results %>% 
  mutate(mean_spread_corr = 0.935684155 * mean + 0.400465037)

# missing data ------------------------------------------------------------


# * height ----------------------------------------------------------------
# calc height by taking average height of all hauls with same wire out amount

# SEPARATE BY VESSEL

fill_height <- height_dat %>% 
  # group_by() %>% 
  dplyr::filter(net_height_pings > 150) %>%
  group_by(edit_wire_out_FM, vessel) %>% 
  summarize(mean_ht = mean(edit_net_height))

missing_height <- height_dat %>% 
  dplyr::filter(net_height_pings <= 150 | is.na(net_height_pings)) %>% 
  left_join(fill_height) %>% 
  mutate(edit_net_height = mean_ht) %>% 
  dplyr::select(-mean_ht)%>% 
  mutate(net_height_method = 4)

all_height_corr <- height_dat %>% 
  dplyr::filter(net_height_pings > 150) %>%
  bind_rows(missing_height) %>% 
  arrange(vessel, haul) 


# spot check:
# height_dat %>% dplyr::filter(haul == 38, vessel ==94)
# missing_height %>% dplyr::filter(haul == 38, vessel ==94)
# all_height_corr %>% dplyr::filter(haul == 38, vessel ==94)

# extra notes:
# scope range: 0-6
# by filtering 0-3 we can filter out double-echo data

# * spread ----------------------------------------------------------------

# Net width ~ inverse scope + height + (inverse scope * height)
# USE: spread from SOR with correction, corrected heights
# Predict: missing spreads (anything that didn't go through SOR) -- flag_pings
#  # 2017 tech memo (note inverse scope = 1/wire out)

input_glm <- net_spread %>% #add wire out and inv scope, and net height
  left_join(all_height_corr) %>% 
  dplyr::filter(n_pings >= 50) %>% # drop hauls that have fewer than 50 pings- not enough data to get good estimates
  drop_na(mean_spread_corr, invscope, edit_net_height) %>% 
  mutate(net_spread_method = 7)

fill_width <- glm(mean_spread_corr ~ invscope + edit_net_height + invscope*edit_net_height, 
                 data=c(input_glm),family="gaussian")
summary(fill_width)
# plot(fill_width)
# confint(fill_width)
summary(fill_width)$coefficients

new_flag_pings <- net_spread %>% #some n_pings may have dropped below 50 after SOR
  dplyr::filter(n_pings < 50) %>% 
  mutate(cruise = cruise) %>% 
  dplyr::select(cruise, vessel, haul, n_pings) %>% 
  full_join(flag_pings %>% dplyr::select(-n_pings)) %>% 
  arrange(vessel, haul) 

fill_glm <- new_flag_pings %>% 
  left_join(all_height_corr)
  
predict_missing <- stats::predict.glm(object = fill_width, newdata = fill_glm)

#CIA: I don't like this! We are putting in the SD of the original pings, but not using those pings in any way to get the mean for this haul
old_sd_missing_spread <- flag_pings %>% 
  mutate(cruise_id = case_when(vessel == vessels[1] ~ cruise_idnum[1],
                               vessel == vessels[2] ~ cruise_idnum[2])) %>% 
  inner_join(edit_hauls_table_raw) %>% 
  dplyr::select(cruise, vessel, haul, sd = net_spread_standard_deviation)

final_filled_in <- bind_cols(mean_spread_corr = predict_missing, fill_glm) %>% 
  mutate(net_spread_method = 4) %>%
  full_join(old_sd_missing_spread) %>% 
  bind_rows(input_glm) %>% 
  mutate(n_pings2 = if_else(is.na(n_pings), 0, n_pings)) %>%
  mutate(net_height_pings = if_else(is.na(net_height_pings), 0, net_height_pings)) %>%
  rename(edit_net_spread = mean_spread_corr,
         net_spread_pings = n_pings2,
         net_spread_standard_deviation = sd)
  

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

# ToDO:

# pull RACE_DATA:EDIT_HAULS: edit_hauls_table_raw
# edit: edit_net_spread, net_spread_method, net_spread_pings, net_spread_standard_deviation, 
#       edit_net_height, net_height_method, net_height_pings, net_height_standard deviation
# switch col names to all uppercase janitor::clean_names(case = "all_caps")
# save table to sql

# set an audit note

race_data_edit_hauls <- final_filled_in %>% 
  dplyr::select(-mean, - vessel_haul) %>% 
  janitor::clean_names(case = "all_caps") %>% 
  dplyr::select(CRUISE_ID, #set specific column order for ORACLE
                HAUL_ID,
                CRUISE,
                VESSEL,
                HAUL,
                EDIT_NET_SPREAD,
                NET_SPREAD_PINGS,
                NET_SPREAD_METHOD,
                NET_SPREAD_STANDARD_DEVIATION,
                EDIT_NET_HEIGHT,
                # EDIT_NET_HEIGHT_UNITS,
                NET_HEIGHT_METHOD,
                NET_HEIGHT_PINGS,
                NET_HEIGHT_STANDARD_DEVIATION
                # EDIT_WIRE_OUT,
                # EDIT_WIRE_OUT_UNITS,
                # WIRE_OUT_METHOD,
                # EDIT_WIRE_OUT_FM,
                # EDIT_WIRE_OUT_UNITS_FM,
                # INVSCOPE
                ) %>%   
  mutate(across(everything(), as.character))

# convert NA to blanks for Oracle
race_data_edit_hauls[is.na(race_data_edit_hauls)] <- ""                     # Replace NA with blank
race_data_edit_hauls

write_csv(race_data_edit_hauls, file = here("output", "race_data_edit_hauls_table.csv"))

write_csv(sor_results, file = here("output", "sor_results_all.csv"))

write_csv(fill_glm,  file = here("output", "replace_net_spread.csv"))

# final data check section ------------------------------------------------

# compare to orig dat
edit_hauls_table_raw
anti_join(final_filled_in, edit_hauls_table_raw, by = c("cruise_id", "haul"))
anti_join(edit_hauls_table_raw, final_filled_in, by = c("cruise_id", "haul"))
# final_filled_in %>% dplyr::filter(cruise_id == 756, haul %in% bad_ping_hauls$haul) #these are missing

dup_check <- final_filled_in %>% dplyr::select(cruise_id, haul) %>% duplicated()
dupes <- final_filled_in %>% 
  dplyr::select(cruise_id, haul) %>% 
  bind_cols(dupes = dup_check) %>% 
  dplyr::filter(dupes == TRUE) %>% 
  arrange(cruise_id, haul)
final_filled_in %>% 
  dplyr::filter(haul %in% dupes$haul & cruise_id %in% dupes$cruise_id) %>% 
  arrange(cruise_id, haul) #%>% View()

# fill_glm %>% dplyr::filter(vessel == 94, haul == 51)
# input_glm %>% dplyr::filter(vessel == 94, haul == 51)

edit_hauls_table_raw %>% distinct(edit_net_height_units)
edit_hauls_table_raw %>% distinct(edit_net_spread_units)
edit_hauls_table_raw %>% dplyr::filter(cruise_id == 755, haul == 129) %>% dplyr::select(net_height_standard_deviation)
