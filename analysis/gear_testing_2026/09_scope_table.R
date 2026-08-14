scope_performance <- 
  read.csv(here::here("data", "scope_performance.csv")) |>
  dplyr::filter(good == TRUE)

btd_summary <- readRDS(here::here("output", "btd_summary.rds"))

btd_scope <- dplyr::inner_join(scope_performance, btd_summary)


scope_tables <- 
  read_xlsx(path = here::here("data", "shelf_slope_table.xlsx")) |>
  dplyr::mutate(mean_depth_fm = (min_depth_fm+max_depth_fm)/2,
                scope_to_depth = wire_out_fm/mean_depth_fm) |>
  dplyr::inner_join(data.frame(table = c("GOA/AI", "EBS shelf", "EBS slope"), gear = c("PNE", "83-112", "PNE-S")))

ggplot(data = btd_scope,
       mapping = aes(x=BOTTOM_DEPTH_FM, y = SCOPE_TO_DEPTH)) +
  geom_point() +
  geom_smooth() +
  geom_path(
    data = scope_tables,
    mapping = aes(x = mean_depth_fm, y = scope_to_depth, linetype = gear),
    color = "grey"
  ) +
  scale_x_continuous(name = "Bottom depth (fathoms)", limits = c(0, 130)) +
  scale_y_continuous(name = "Scope/Depth", breaks = seq(2,8,0.5), limits = c(2,8)) +
  theme_bw()

ggplot() +
  geom_line(
    data = scope_tables,
    mapping = aes(x = mean_depth_fm, y = scope_to_depth, linetype = gear),
    color = "grey"
  ) +
  geom_segment(
    data = scope_tables,
    mapping = aes(x = min_depth_fm, y = max_depth_fm = group = mean_depth_fm)
  ) +
  geom_point(
    data = scope_tables,
    mapping = aes(x = mean_depth_fm, y = scope_to_depth, linetype = gear),
    color = "black"
  ) +
  scale_x_continuous(name = "Bottom depth (fathoms)", limits = c(0, 300/1.8288), oob = scales::oob_keep) +
  scale_y_continuous(name = "Scope/depth", limits = c(0, 9), breaks = seq(1,9,1)) +
  scale_linetype(name = "Gear") +
  theme_bw()