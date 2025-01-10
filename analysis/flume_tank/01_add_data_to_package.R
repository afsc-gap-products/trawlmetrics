library(trawlmetrics)
library(xlsx)

# dir.create(here::here("data"))

# Historical geometry data from hauls where trawl geometry was measured
bts_geom <-  readRDS(file = here::here("analysis", "flume_tank", "data", "HEIGHT_SPREAD_EBS_NBS_GOA_AI.rds"))

# Load flume tank data
flume_tank <- read.xlsx(file = here::here("analysis", "flume_tank", "data", "flume_tank_data.xlsx"), 
                        sheetName = 'example')

save(bts_geom, flume_tank, file = here::here("R", "sysdata.rda"))
