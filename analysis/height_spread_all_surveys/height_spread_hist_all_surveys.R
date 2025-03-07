# Retrieve spread and height data from GAP_PRODUCTS tables
# Save data and summaries to xlsx files

library(trawlmetrics) # devtools::install_github("afsc-gap-products/trawlmetrics")
library(dplyr)
library(xlsx)
library(ggthemes)
library(here)

channel <- trawlmetrics::get_connected(schema = "AFSC")

# Query good performance index station hauls where the net was measured
net_dat <- 
  RODBC::sqlQuery(
    channel = channel,
    query = 
    "SELECT
      C.SURVEY_DEFINITION_ID,
      C.CRUISE,
      C.YEAR,
      C.VESSEL_ID,
      C.SURVEY_NAME,
      C.VESSEL_NAME,
      H.DEPTH_M, 
      H.WIRE_LENGTH_M, 
      H.GEAR, 
      H.ACCESSORIES, 
      H.HAUL, 
      H.DISTANCE_FISHED_KM, 
      H.NET_WIDTH_M, 
      H.NET_HEIGHT_M, 
      H.DURATION_HR
    FROM
      GAP_PRODUCTS.AKFIN_HAUL H, 
      GAP_PRODUCTS.AKFIN_CRUISE C
    WHERE 
      C.SURVEY_DEFINITION_ID IN (47, 52, 98, 143)
      AND H.NET_MEASURED = 1
      AND C.CRUISEJOIN = H.CRUISEJOIN")

xlsx::write.xlsx(net_dat, file = here::here("analysis", "height_spread_all_surveys", "NET_HEIGHT_OBS_EBS_NBS_GOA_AI.xlsx"))

survey_gear <- data.frame(SURVEY_DEFINITION_ID = c(47, 52, 98, 143),
                          GEAR_NAME = c("PNE", "PNE", "83-112", "83-112"))

net_dat <- dplyr::inner_join(net_dat, survey_gear)

saveRDS(object = net_dat, 
        file = here::here("analysis", "height_spread_all_surveys", "HEIGHT_SPREAD_EBS_NBS_GOA_AI.rds"))

ggplot(data = net_dat,
       mapping = aes(x = NET_WIDTH_M, y = NET_HEIGHT_M)) +
  geom_hex() +
  geom_smooth() +
  scale_x_continuous(name = "Net spread (m)") +
  scale_y_continuous(name = "Net height (m)") +
  facet_wrap(~GEAR_NAME, scales = "free")

ggplot(data = net_dat,
       mapping = aes(x = NET_WIDTH_M, y = NET_HEIGHT_M)) +
  geom_point() +
  geom_smooth() +
  scale_x_continuous(name = "Net spread (m)") +
  scale_y_continuous(name = "Net height (m)") +
  facet_wrap(~GEAR_NAME, scales = "free")

ggplot() +
  geom_boxplot(data = net_dat,
               mapping = aes(x = round(NET_WIDTH_M, 0.1), y = NET_HEIGHT_M, group = round(NET_WIDTH_M, 0.1))) +
  geom_smooth(data = net_dat,
              mapping = aes(x = NET_WIDTH_M, y = NET_HEIGHT_M)) +
  scale_x_continuous(name = "Net spread (m)") +
  scale_y_continuous(name = "Net height (m)") +
  facet_wrap(~GEAR_NAME, scales = "free")


net_summary <- net_dat |>
  dplyr::group_by(SURVEY_DEFINITION_ID, SURVEY_NAME) |>
  dplyr::summarise(q90_NET_HEIGHT_M = quantile(NET_HEIGHT_M, 0.9, na.rm = TRUE),
                   q10_NET_HEIGHT_M = quantile(NET_HEIGHT_M, 0.1, na.rm = TRUE),
                   q90_NET_WIDTH_M = quantile(NET_WIDTH_M, 0.9, na.rm = TRUE),
                   q10_NET_WIDTH_M = quantile(NET_WIDTH_M, 0.1, na.rm = TRUE),
                   mean_NET_HEIGHT_M = mean(NET_HEIGHT_M, na.rm = TRUE),
                   mean_NET_WIDTH_M = mean(NET_WIDTH_M, na.rm = TRUE)) |>
  as.data.frame()

xlsx::write.xlsx(x = net_summary, 
                 file = here::here("analysis", "height_spread_all_surveys", "NET_HEIGHT_SUMMARY_EBS_NBS_GOA_AI.xlsx"))


p_height <- ggplot() +
  geom_histogram(data = net_dat,
                 mapping = aes(x = NET_HEIGHT_M),
                 fill = "grey50") +
  geom_vline(data = net_summary,
             mapping = aes(xintercept = q90_NET_HEIGHT_M, color = "90% range"), linewidth = 1.1) +
  geom_vline(data = net_summary,
             mapping = aes(xintercept = q10_NET_HEIGHT_M, color = "90% range"), linewidth = 1.1) +
  geom_vline(data = net_summary,
             mapping = aes(xintercept = mean_NET_HEIGHT_M, color = "Mean"), linewidth = 1.1) +
  ggtitle(label = "Net Height (m)") +
  scale_y_continuous(name = "Frequency") +
  scale_x_continuous(name = "Net height (m)") +
  scale_color_colorblind(name = "Value") + 
  facet_wrap(~SURVEY_NAME, scales = "free") +
  theme_bw()

p_width <- ggplot() +
  geom_histogram(data = net_dat,
                 mapping = aes(x = NET_WIDTH_M),
                 fill = "grey50") +
  geom_vline(data = net_summary,
             mapping = aes(xintercept = q90_NET_WIDTH_M, color = "90% range"), linewidth = 1.1) +
  geom_vline(data = net_summary,
             mapping = aes(xintercept = q10_NET_WIDTH_M, color = "90% range"), linewidth = 1.1) +
  geom_vline(data = net_summary,
             mapping = aes(xintercept = mean_NET_WIDTH_M, color = "Mean"), linewidth = 1.1) +
  ggtitle(label = "Net Width (m)") +
  scale_y_continuous(name = "Frequency") +
  scale_x_continuous(name = "Net width (m)") +
  scale_color_colorblind(name = "Value") + 
  facet_wrap(~SURVEY_NAME, scales = "free") +
  theme_bw()


png(filename = here::here("analysis", "height_spread_all_surveys", "net_height_hist_EBS_NBS_GOA_AI.png"), 
    width = 6, height = 6, units = "in", res = 300)  
print(p_height)
dev.off()

png(filename = here::here("analysis", "height_spread_all_surveys", "net_width_hist_EBS_NBS_GOA_AI.png"), 
    width = 6, height = 6, units = "in", res = 300)  
print(p_width)
dev.off()
