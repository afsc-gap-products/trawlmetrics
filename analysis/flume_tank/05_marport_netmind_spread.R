library(ggplot2)
library(ggthemes)
library(cowplot)

dat <- data.frame(marport = seq(12,22,0.1))
dat$netmind <- trawlmetrics::marport_to_netmind(dat$marport)


p1 <- ggplot() +
  geom_path(data = dat,
             mapping = aes(x = marport,
                           y = netmind,
                           color = "EBS")) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  scale_x_continuous(name = "Measured spread (m)") +
  scale_y_continuous(name = "Official spread (m)") + 
  scale_color_manual(values = "red") +
  ggtitle(label = "EBS/NBS") +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = "none")

p2 <- ggplot() +
  geom_path(data = dat,
            mapping = aes(x = marport,
                          y = marport,
                          color = "Observed")) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  scale_x_continuous(name = "Measured spread (m)") +
  scale_y_continuous(name = "Official spread (m)") + 
  scale_color_manual(values = "red") +
  ggtitle(label = "GOA/AI") +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = "none")


cowplot::plot_grid(p1, p2)
