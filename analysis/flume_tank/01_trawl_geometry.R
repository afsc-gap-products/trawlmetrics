# Flume tank analysis

library(trawlmetrics)
library(xlsx)

# Load flume tank data

path_flume_xlsx <- here::here("analysis", "flume_tank", "data", "test_data.xlsx")

flume_data <- xlsx::read.xlsx(file = path_flume_xlsx,
                              sheetName = 'data')                                     
    
# 

ggplot() +
  geom_point(data = flume_data,
             mapping = aes(x = towing_speed_kn,
                           y = spread_mean_we_m)) +
  facet_wrap(~round(door_m))

ggplot(data = flume_data,
       mapping = aes(x = bridle_angle_deg,
                     y = spread_mean_we_m,
                     color = factor(towing_speed_kn))) +
  geom_point() +
  geom_smooth()

ggplot(data = flume_data,
       mapping = aes(x = bridle_angle_deg,
                     y = spread_mean_we_m,
                     color = factor(towing_speed_kn))) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~towing_speed_kn)

ggplot(data = flume_data,
       mapping = aes(x = toowing,
                     y = spread_mean_we_m,
                     color = factor(towing_speed_kn))) +
  geom_point() +
  geom_smooth()
