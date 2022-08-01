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
cruise <- c(202201) #202201
# cruise id num 726 = vessel 94; cruise id 756 = vessel 162
cruise_idnum <- c(726, 756) # make sure cruise id num 1 = vessel 1 and cruise id num 2 = vessel 2
vessel <- c(94, 162)
vessels <- c(94, 162)
region <- "BS"

# for troublshooting problem data:
skip_haul <-  c()
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


# write_csv(edit_haul, path = here("edit_haul.csv"))
write_csv(edit_sgp, path = here("output" ,"test_edit_sgp.csv"))
write_csv(edit_sgt, path = here("output" ,"test_edit_sgt.csv"))

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

edit_hauls_table_raw <- edit_height %>% 
  as_tibble() %>% 
  clean_names()

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

# section method when more stremlined version break rstudio
max_v1 <- good_ping_hauls %>% dplyr::filter(vessel == vessel[[1]]) %>% distinct(haul) #%>% dim()
max_v2 <- good_ping_hauls %>% dplyr::filter(vessel == vessel[[2]]) %>% distinct(haul) #%>% dim()

set <- list(bind_cols(hauls = max_v1$haul[1:50],    vess = vessel[[1]]),
            bind_cols(hauls = max_v1$haul[51:100],  vess = vessel[[1]]),
            bind_cols(hauls = max_v1$haul[101:150], vess = vessel[[1]]),
            bind_cols(hauls = max_v1$haul[151:max(max_v1$haul)],   vess = vessel[[1]]),
            bind_cols(hauls = max_v2$haul[1:50],    vess = vessel[[2]]),
            bind_cols(hauls = max_v2$haul[51:100],  vess = vessel[[2]]),
            bind_cols(hauls = max_v2$haul[101:150], vess = vessel[[2]]),
            bind_cols(hauls = max_v2$haul[151: max(max_v2$haul)],   vess = vessel[[2]]))

for(i in 1) #:length(set))
{
  sor_test <- good_ping_hauls %>% 
    dplyr::filter(vessel == unique(set[[i]]$vess), haul %in% set[[i]]$hauls)
  
  start_time <- Sys.time()
  print(paste("Start time:", start_time))
  sor_set <- sor_test %>%  # good_ping_hauls %>% 
    group_by(vessel, haul) %>% 
    # as.data.frame() %>% 
    dplyr::group_map(~sequentialOR(data = .x, #as.data.frame(good_ping_hauls), 
                                   method = 'ss', 
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
  
  write_csv(sor_set[[j]]$obs_rank %>% inner_join(sor_test), 
            here("output", "SOR_files", 
                 paste0("vessel-", this_vessel, "_haul-", this_haul, "_data.csv")))
  write_csv(sor_set[[j]]$results, here("output", "SOR_files", 
                                       paste0("vessel-", this_vessel, "_haul-", this_haul, "_results.csv")))
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
             path = here("output", "SOR_files")) %>% 
  map_df(~read_csv(.))

sor_results <- 
  list.files(pattern = "*results.csv",
             path = ) %>% 
  map_df(~read_csv(.))

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
  
  # pings
  not_rejected <- sor_v1_dat[[i]] %>% dplyr::filter(is.na(SOR_RANK))
  rejected <- sor_v1_dat[[i]] %>% dplyr::filter(!is.na(SOR_RANK))
  
  # initial data
  # plot(x = sor_v1_dat[[i]]$date_time, sor_v1_dat[[i]]$measurement_value,
  #      ylim = c(10, 22), main = "original")
  p_init <- sor_v1_dat[[i]] %>%
    ggplot()+
    geom_point(aes(x = date_time, y = measurement_value), shape = 1, size = 2.5) +
    scale_y_continuous(limits=c(10, 22), expand = c(0, 0)) +
    theme_bw() +
    labs(x = "time", y = "spread", title = "Before SOR",
         subtitle = paste("Vessel", unique(sor_v1_dat[[i]]$vessel), "Haul", unique(sor_v1_dat[[i]]$haul)))
  # after sor
  # plot(x = not_rejected$date_time, not_rejected$measurement_value,
  #      ylim = c(10,22), main = "after sor")
  p_post <- not_rejected %>%
    ggplot()+
    geom_point(aes(x = date_time, y = measurement_value), shape = 1, size = 2.5) +
    scale_y_continuous(limits=c(10, 22), expand = c(0, 0)) +
    theme_bw() +
    labs(x = "time", y = "spread", title = "After SOR",
         subtitle = paste("Vessel", unique(sor_v1_dat[[i]]$vessel), "Haul", unique(sor_v1_dat[[i]]$haul)))
  
  p_both <- ggplot()+
    geom_point(data = not_rejected, aes(x = date_time, y = measurement_value), shape = 16, size = 2.5, alpha = 0.35) +
    geom_point(data = rejected, aes(x = date_time, y = measurement_value), shape = 16, size = 2.5, col = "red", alpha = 0.5) +
    scale_y_continuous(limits=c(10, 22), expand = c(0, 0)) +
    theme_bw() +
    labs(x = "time", y = "spread", title = "All pings (red = rejected by SOR)",
         subtitle = paste("Vessel", unique(sor_v1_dat[[i]]$vessel), "Haul", unique(sor_v1_dat[[i]]$haul)))
    
  # rmse
  # plot(x = sor_vessel1[[i]]$rmse$N, y = sor_vessel1[[i]]$rmse$RMSE, main = "RMSE")
  p_rmse <- sor_vessel1[[i]]$rmse %>%
    ggplot()+
    geom_point(aes(x = N, y = RMSE), shape = 17, size = 2.5) +
    theme_bw() +
    labs(x = "iteration number", y = "rmse", title = "RMSE",
         subtitle = paste("Vessel", unique(sor_v1_dat[[i]]$vessel), "Haul", unique(sor_v1_dat[[i]]$haul)))
 p_full <- plot_grid(p_init, p_post, p_both, p_rmse) # blank plot:, ggplot() + theme_bw() + theme(panel.border = element_blank()))
 ggsave(p_full, filename = paste0("vessel-", unique(sor_v1_dat[[i]]$vessel), "_haul-", unique(sor_v1_dat[[i]]$haul), "_sor_plot.png"),
        path = here("output", "SOR_graphics"), width = 10, height = 6)
}

# reject SOR --------------------------------------------------------------

# no pings or very few; fewer than 50 pings cutoff after SOR
# # if all pings are clustered in one time period of the tow, rather than spread across the whole time period

review_hauls <- sor_v1_results %>% 
  mutate(process = "SOR") %>% 
  dplyr::filter(n_pings <= 50) %>% 
  full_join(flag_pings) %>% 
  mutate(process = if_else(is.na(process), "preSOR", "SOR"))

review_hauls_final <- review_hauls 
# CIA: add mechanism to review these hauls

# marport-netmind correction ----------------------------------------------
# Note: this is for EBS- does GOA have diff correction?
# Converting to Netmind: netmind spread = 0.935684155 * mean marport spread (after sequential outlier rejection) + 0.400465037

net_spread <- sor_v1_results %>% 
  mutate(mean_spread_corr = 0.935684155 * mean + 0.400465037)

# missing data ------------------------------------------------------------


# * height ----------------------------------------------------------------
# calc height by taking average height of all hauls with same wire out amount

fill_height <- height_dat %>% 
  dplyr::filter(net_height_pings > 150) %>%
  group_by(edit_wire_out_FM) %>% 
  summarize(mean_ht = mean(edit_net_height))

missing_height <- height_dat %>% 
  dplyr::filter(net_height_pings <= 150 | is.na(net_height_pings)) %>% 
  left_join(fill_height) %>% 
  mutate(edit_net_height = mean_ht) %>% 
  dplyr::select(-mean_ht)

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

input_glm <- sor_v1_results %>% #add wire out and inv scope, and net height
  left_join(all_height_corr) %>% 
  drop_na(mean, invscope, edit_net_height)

fill_width <- glm(mean ~ invscope + edit_net_height + invscope*edit_net_height, 
                 data=c(input_glm),family="gaussian")
summary(fill_width)
plot(fill_width)
confint(fill_width)

fill_glm <- flag_pings %>% 
  left_join(all_height_corr)

predict_missing <- stats::predict.glm(object = fill_width, newdata = fill_glm)

final_filled_in <- bind_cols(mean = predict_missing, fill_glm) %>% 
  bind_rows(input_glm)

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
# edit: edit_net_spread, net_spread method, net_spread_pings, net_spread_standard_deviation, 
#       edit_net_height, net_height_method, net_height_pings, net_height_standard deviation
# switch col names to all uppercase janitor::clean_names(case = "all_caps")
# save table to sql
