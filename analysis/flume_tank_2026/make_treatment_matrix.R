library(xlsx)
library(trawlmetrics)
library(stringr)
library(ggthemes)


treatment_matrix <- 
  dplyr::bind_rows(
    xlsx::read.xlsx(file = here::here("analysis", "flume_tank_2026", "Flume Tank Treatments_20260125.xlsx"), sheetIndex = 1),
    xlsx::read.xlsx(file = here::here("analysis", "flume_tank_2026", "Flume Tank Treatments_20260125.xlsx"), sheetIndex = 2),
    xlsx::read.xlsx(file = here::here("analysis", "flume_tank_2026", "Flume Tank Treatments_20260125.xlsx"), sheetIndex = 3)
  ) |>
  dplyr::filter(!is.na(Spread)) |>
  dplyr::mutate(
    pars = ifelse(stringr::str_detect(Parameters.to.Capture, "Partial Video"), "P", ifelse(stringr::str_detect(Parameters.to.Capture, "Full Video"), "F", NA))
  ) |>
  dplyr::select(Treatment.Number, Modified.Net, Bridle, Footrope, Speed, Spread, pars)


all_combinations <- 
  expand.grid(
    Modified.Net = unique(treatment_matrix$Modified.Net),
    Bridle = unique(treatment_matrix$Bridle),
    Footrope = unique(treatment_matrix$Footrope),
    Speed = unique(treatment_matrix$Speed),
    Spread = unique(treatment_matrix$Spread)
  ) |>
  dplyr::left_join(treatment_matrix) |>
  dplyr::mutate(obs = !is.na(Treatment.Number)) |>
  dplyr::mutate(Speed = factor(Speed),
                Spread = factor(Spread))

pne_treatments <- dplyr::filter(treatment_matrix, Modified.Net == "PNE") |>
  dplyr::mutate(
    pars = ifelse(is.na(pars), "", pars),
    Treatment.Number = ifelse(is.na(Treatment.Number), "", Treatment.Number)
    ) |>
  dplyr::mutate(Speed = factor(Speed),
                Spread = factor(Spread))

race_treatments <- dplyr::filter(treatment_matrix, Modified.Net == "RACE") |>
  dplyr::mutate(
    pars = ifelse(is.na(pars), "", pars),
    Treatment.Number = ifelse(is.na(Treatment.Number), "", Treatment.Number)
) |>
  dplyr::mutate(Speed = factor(Speed),
                Spread = factor(Spread))

p_pne_testing_matrix <- 
  ggplot() +
  geom_tile(
    data = all_combinations,
    mapping = aes(x = Speed, y = Spread, fill = obs),
    color = "black"
  ) +
  geom_text(data = pne_treatments,
            mapping = aes(x = Speed, y = Spread, label = paste0(Treatment.Number, "\n", pars))) +
  ggtitle(paste("PNE Treatments (n=", nrow(pne_treatments), ")")) +
  facet_grid(Bridle ~ Footrope) +
  scale_fill_manual(values = c("TRUE" = "lightblue", "FALSE" = NA)) +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.position = "none")

p_race_testing_matrix <- 
  ggplot() +
  geom_tile(
    data = all_combinations,
    mapping = aes(x = Speed, y = Spread, fill = as.character(obs)),
    color = "black"
  ) +
  geom_text(data = race_treatments,
            mapping = aes(x = Speed, y = Spread, label = paste0(Treatment.Number, "\n", pars))) +
  facet_grid(Bridle ~ Footrope) +
  ggtitle(paste("RACE Treatments (n=", nrow(race_treatments), ")")) +
  scale_fill_manual(values = c("TRUE" = "lightblue", "FALSE" = "white")) +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.position = "none")

png(filename = here::here("analysis", "flume_tank_2026", "plots", "testing_matrix_race_trawl.png"), width = 169, height = 169, units = "mm", res = 300)
print(p_race_testing_matrix)
dev.off()

png(filename = here::here("analysis", "flume_tank_2026", "plots", "testing_matrix_pne_trawl.png"), width = 169, height = 169, units = "mm", res = 300)
print(p_pne_testing_matrix)
dev.off()


