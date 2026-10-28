

load(here::here("analysis", "door_experiment", "data", "bottom_contact_2023.rda"))


bcs_summary$location <- factor(bcs_summary$location, levels = c("C", "SC", "PC", "SBWT", "PBWT", "SB40", "PB40", "SB50", "PB50"))


plot_bc <- ggplot() +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_point(data = dplyr::filter(bcs_summary, HAUL < 15),
             mapping = aes(x = location, y = MEAN_HEIGHT, fill = factor(treatment)),
             size = rel(3.3),
             alpha = 0.7,
             shape = 21) +
  scale_x_discrete(name = "BCS Unit") +
  scale_y_continuous(name = "Height (cm)") +
  scale_fill_colorblind(name = "Treatment") +
  facet_wrap(~HAUL) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))


plot_bc_sd <- ggplot() +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_point(data = dplyr::filter(bcs_summary, HAUL < 15),
             mapping = aes(x = location, y = SD_HEIGHT, fill = factor(treatment)),
             size = rel(3.3),
             alpha = 0.7,
             shape = 21) +
  scale_x_discrete(name = "BCS Unit") +
  scale_y_continuous(name = "SD(Height) (cm)") +
  scale_fill_colorblind(name = "Treatment") +
  facet_wrap(~HAUL) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

png(filename = here::here("analysis", "door_experiment", "plots", "bottom_contact_2023.png"), 
    width = 240, 
    height = 180, 
    units = "mm", 
    res = 300)
print(plot_bc +
        theme(legend.text = element_text(size = 14),
              legend.title = element_text(size = 14),
              axis.title = element_text(size = 18),
              axis.text = element_text(size = 14),
              strip.text = element_text(size = 14)))
dev.off()

png(filename = here::here("analysis", "door_experiment", "plots", "bottom_contact_sd_2023.png"), 
    width = 240, 
    height = 180, 
    units = "mm", 
    res = 300)
print(plot_bc_sd +
        theme(legend.text = element_text(size = 14),
              legend.title = element_text(size = 14),
              axis.title = element_text(size = 18),
              axis.text = element_text(size = 14),
              strip.text = element_text(size = 14)))
dev.off()




