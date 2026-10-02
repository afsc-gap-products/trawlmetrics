# Assign door size treatments to blocks

library(trawlmetrics)
library(akgfmaps)
library(ggthemes)
library(shadowtext)

# Review door trials and assign to hauls
channel <- trawlmetrics::get_connected(schema = "AFSC")

locs <- 
  RODBC::sqlQuery(
    query = "select * from racebase.haul where vessel = 134 and cruise = 202601 and haul_type in (7, 23)", 
    channel = channel) |>
  sf::st_as_sf(crs = "WGS84", coords = c("START_LONGITUDE", "START_LATITUDE")) |>
  sf::st_transform(crs = "EPSG:3338") |>
  dplyr::mutate(
    Phase = dplyr::case_when(HAUL_TYPE == 7 & HAUL < 531 ~ "Phase 1: Door sizing",
                             HAUL_TYPE == 7 & HAUL > 530 ~ "Phase 1: Scope table",
                             HAUL_TYPE == 23 ~ "Phase 2: Catch comparison")
  )

map_layers <- akgfmaps::get_base_layers(select.region = "sebs", set.crs = "EPSG:3338")

# Range of door trial depths (Hauls 500-526)
range(locs$BOTTOM_DEPTH[locs$HAUL < 527])

bathy_labels <- data.frame(
  x = c(-163, -165, -168),
  y = c(55.5, 55.8, 55.4),
  lab = c("50 m", "100 m", "200 m")
) |>
  akgfmaps::transform_data_frame_crs(out.crs = "EPSG:3338")


p_sample_map <- 
  ggplot() +
  geom_sf(data = map_layers$bathymetry, color = "grey40", linewidth = 0.2) +
  geom_sf(data = map_layers$akland, color = NA, fill = "grey85") +
  geom_sf(
    data = locs, 
    mapping = aes(color = Phase, shape = Phase), 
    size = rel(3)
  ) +
  geom_shadowtext(
    data = bathy_labels,
    mapping = aes(x = x, y = y, label = lab), color = "grey40", bg.color = "white") +
  scale_x_continuous(limits = map_layers$plot.boundary$x + c(2.5e5, -2e5)) + 
  scale_y_continuous(limits = map_layers$plot.boundary$y + c(-2e5, -5.5e5)) +
  scale_color_tableau() +
  scale_shape(solid = FALSE) +
  theme_bw() +
  theme(legend.position = "inside", 
        legend.position.inside = c(0.75, 0.08),
        legend.title = element_blank(),
        legend.background = element_blank(),
        axis.title = element_blank())

png(filename = here::here("plots", "map_sample_locations.png"), width = 120, height = 120, units = "mm", res = 300)
print(p_sample_map)
dev.off()


door_clusters <- readxl::read_xlsx(
  path = here::here("data", "2026_gear_testing_haul_log.xlsx"), sheet = "door_blocks"
)

names(door_clusters) <- toupper(names(door_clusters))

door_locs <- dplyr::inner_join(
  locs, door_clusters
)

ggplot() +
  geom_sf(
    data = dplyr::filter(locs, HAUL < 526), 
    mapping = aes(color = Phase, shape = Phase), 
    size = rel(3)
  ) +
  geom_sf(
    data = dplyr::filter(locs, HAUL < 530), 
    mapping = aes(color = Phase, shape = Phase), 
    size = rel(3)
  ) +
  scale_color_tableau() +
  scale_shape(solid = FALSE) +
  theme_bw() +
  theme(legend.position = "inside", 
        legend.position.inside = c(0.75, 0.08),
        legend.title = element_blank(),
        legend.background = element_blank(),
        axis.title = element_blank())


ggplot() +
  geom_sf_text(
    data = dplyr::filter(locs, HAUL < 529, HAUL > 503, PERFORMANCE >=0), 
    mapping = aes(color = BOTTOM_DEPTH, label = HAUL), 
    size = rel(5)
  ) +
  scale_color_viridis_c() +
  scale_shape(solid = FALSE) +
  theme_bw() +
  theme(legend.position = "inside", 
        legend.position.inside = c(0.75, 0.08),
        legend.title = element_blank(),
        legend.background = element_blank(),
        axis.title = element_blank())

ggplot() +
  geom_sf(
    data = door_locs, 
    mapping = aes(color = factor(DOOR_SIZE_M2))
  ) +
  scale_shape(solid = FALSE) +
  theme_bw() +
  theme(legend.position = "inside", 
        legend.position.inside = c(0.75, 0.08),
        legend.title = element_blank(),
        legend.background = element_blank(),
        axis.title = element_blank()) +
  facet_wrap(~BLOCK)

dplyr::filter(locs, HAUL %in% c(500:501, 529, 530), PERFORMANCE >=0)


# Review data from door trial blocks to determine which treatments to use for analysis

haul_log <- 
  readxl::read_xlsx(
    path = here::here("data", "2026_gear_testing_haul_log.xlsx"),
    sheet = "treatments"
  ) |>
  dplyr::select(Haul, Scope, Time_AKDT, Door_size_m2, Pass,
                Port_Tension, Port_Door_Depth, Port_Door_Pitch, Port_Door_Roll,
                Stbd_Tension, Stbd_Door_Depth, Stbd_Door_Pitch, Stbd_Door_Roll) |>
  dplyr::filter(!is.na(Port_Tension),
                Haul < 527)

door_blocks <-
  readxl::read_xlsx(
    path = here::here("data", "2026_gear_testing_haul_log.xlsx"),
    sheet = "door_blocks"
  ) 

door_blocks <- 
  dplyr::inner_join(
    door_blocks,
    haul_log
  ) |>
  dplyr::arrange(Block, Haul, Time_AKDT) |>
  dplyr::select(-Time_AKDT)

write.csv(door_blocks, file = here::here("data", "door_blocks_qa_qc.csv"), row.names = FALSE)

