library(trawlmetrics)

test <- read.csv(file = here::here("data", "door_blocks_qa_qc_skr.csv")) |>
  dplyr::filter(Use != "N") |>
  dplyr::group_by(Block, Door_size_m2, Scope) |>
  dplyr::summarise(
    Port_Tension = mean(Port_Tension, na.rm = TRUE),
    Port_Door_Pitch = mean(Port_Door_Pitch, na.rm = TRUE),
    Port_Door_Roll = mean(Port_Door_Roll, na.rm = TRUE),
    Stbd_Tension = mean(Stbd_Tension, na.rm = TRUE),
    Stbd_Door_Pitch = mean(Stbd_Door_Pitch, na.rm = TRUE),
    Stbd_Door_Roll = mean(Stbd_Door_Roll, na.rm = TRUE)
  )

ggplot() +
  geom_point(data = test,
             mapping = aes(x = Scope, y = Port_Door_Pitch, color = factor(Door_size_m2), shape = "Starboard")) +
  geom_point(data = test,
             mapping = aes(x = Scope, y = Stbd_Door_Pitch, color = factor(Door_size_m2), shape = "Port")) +
  facet_wrap(~Block, scales = "free_x") +
  scale_color_tableau(name = "Door size") +
  scale_x_continuous(name = "Scope") +
  scale_y_continuous(name = "Door pitch (degrees)") +
  scale_shape(name = "Side") +
  theme_bw()

ggplot() +
  geom_hline(yintercept = c(30,-30), linetype = 2) +
  geom_hline(yintercept = 0, linetype = 3) +
  geom_point(data = test,
             mapping = aes(x = Scope, y = Stbd_Door_Roll, color = factor(Door_size_m2), shape = "Starboard")) +
  geom_point(data = test,
             mapping = aes(x = Scope, y = Port_Door_Roll, color = factor(Door_size_m2), shape = "Port")) +
  facet_wrap(~Block, scales = "free_x") +
  scale_color_tableau(name = "Door size") +
  scale_shape(name = "Side") +
  scale_x_continuous(name = "Scope") +
  scale_y_continuous(name = "Door roll (degrees)", breaks = seq(-30,30,15)) +
  theme_bw()
