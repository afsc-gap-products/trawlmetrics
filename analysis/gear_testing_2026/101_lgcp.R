# LGCP (Thygesen et al., 2019) on size data ----

library(sratio)

crab_size_sex_freq <- readRDS(here::here("data", "06_catch_data", "size_freq_crab.rds")) |>
  dplyr::filter(SEX == !is.na(SIZE_5MM))

fish_length_freq <- readRDS(here::here("data", "06_catch_data", "size_freq_fish.rds"))

lgcp_taxa <- 
  data.frame(
    SPECIES_CODE = c(10110, 10112, 10130, 10210, 10261, 10285, 21720, 21740, 68580, 68560, 68580, 68560),
    SEX = c(rep(3, 8), 1, 1, 2, 2)
  )

# Make wide format size frequency data frame
size_freq_wide <- 
  dplyr::mutate(fish_length_freq, SEX = 3) |>
  dplyr::bind_rows(
    dplyr::mutate(crab_size_sex_freq, TOTAL_FREQUENCY = FREQUENCY)
  ) |>
  dplyr::select(SPECIES_CODE, COMMON_NAME, SEX, HAUL, BLOCK, GEAR_NAME, TOTAL_FREQUENCY, SIZE, AREA_SWEPT_KM2) |>
  dplyr::group_by(
    SPECIES_CODE, COMMON_NAME, SEX, HAUL, BLOCK, GEAR_NAME, SIZE, AREA_SWEPT_KM2
  ) |>
  dplyr::summarise(
    TOTAL_FREQUENCY = sum(TOTAL_FREQUENCY, na.rm = TRUE),
    .groups = "drop"
  )

lgcp_fits <- vector(mode = "list", length = nrow(lgcp_taxa))
lgcp_boot <- vector(mode = "list", length = nrow(lgcp_taxa))

for(ii in 1:nrow(lgcp_taxa)) {
  
  sel_species_code <- lgcp_taxa$SPECIES_CODE[ii]
  
  sel_sex <- lgcp_taxa$SEX[ii]
  
  sel_size_freq <- 
    size_freq_wide |>
    dplyr::filter(SPECIES_CODE == sel_species_code, SEX == sel_sex) |> 
    dplyr::mutate(
    ) |>
    tidyr::pivot_wider(
      names_from = SIZE, 
      values_from = TOTAL_FREQUENCY, 
      values_fill = 0,
      names_expand = TRUE,
      names_sort = TRUE
    ) |>
    dplyr::arrange(
      desc(GEAR_NAME)
    )
  
  d_input <- 
    list(
      N = as.matrix(sel_size_freq[, 8:ncol(sel_size_freq)]),
      SweptArea = sel_size_freq$AREA_SWEPT_KM2,
      group = factor(sel_size_freq$BLOCK),
      Gear = factor(sel_size_freq$GEAR_NAME),
      Lvec = as.numeric(names(sel_size_freq)[8:ncol(sel_size_freq)])
    )
  
  d_fit  <- 
    sratio::gearcalib_fit(
      d = d_input,
      model = "poisson"
    )
  
  lgcp_fits[[ii]] <- 
    data.frame(
      SPECIES_CODE = sel_species_code,
      SEX = sel_sex,
      SIZE = d_input$Lvec,
      fit = d_fit$est,
      se_fit = d_fit$sd,
      pdHess = d_fit$rep$pdHess,
      max_grad = max(d_fit$rep$gradient.fixed)
    )
  
  # Bootstrap estimates
  d_boot <-
    sratio::gearcalib_boot(
      d = d_input,
      nboot = 1000
    )
  
  n_quantiles <- nrow(d_boot$BootQuantiles)
  
  boot_quantiles <-
    d_boot$BootQuantiles |>
    t() |>
    as.data.frame() |>
    dplyr::mutate(
      SPECIES_CODE = sel_species_code,
      SEX = sel_sex,
      SIZE = d_input$Lvec
    ) |> 
    tidyr::pivot_longer(
      cols = 1:n_quantiles,
      values_to = "RCE",
      names_to = "quantile"
    )
  
  lgcp_boot[[ii]] <- boot_quantiles
  
}

lgcp_fits <- 
  lgcp_fits |>
  do.call(what = rbind) |>
  dplyr::inner_join(cc_species_codes)


lgcp_boot <- 
  lgcp_boot |>
  do.call(what = rbind) |>
  dplyr::inner_join(cc_species_codes)


p_lgcp <- 
  ggplot() +
  geom_ribbon(
    data = lgcp_fits,
    mapping = aes(
      x = SIZE, 
      ymin = exp(fit - 2*se_fit),
      ymax = exp(fit + 2*se_fit),
      fill = factor(SEX, levels = 3:1, labels = c("All", "Female", "Male"))
    ),
    alpha = 0.3,
    color = NA
  ) +
  geom_path(
    data = lgcp_fits,
    mapping = aes(
      x = SIZE, 
      y = exp(fit),
      color = factor(SEX, levels = 3:1, labels = c("All", "Female", "Male"))
    )
  ) +
  
  geom_point(
    data = dplyr::filter(lgcp_boot, quantile == "50%"),
    mapping = aes(
      x = SIZE, 
      y = RCE,
      color = factor(SEX, levels = 3:1, labels = c("All", "Female", "Male"))
    ),
    size = 0.8
  ) +
  geom_hline(yintercept = 1, linetype = 2) +
  scale_y_continuous(name = "Relative catch efficiency", limits = c(0, 4), oob = scales::squish, expand = c(0,0)) +
  scale_x_continuous(name = "Size") +
  scale_fill_manual(name = "Sex", values = c('All' = 'grey30', 'Female' = "#DFC27D", 'Male' = "#80CDC1")) +
  scale_color_manual(name = "Sex", values = c('All' = 'grey30', 'Female' = "#DFC27D", 'Male' = "#80CDC1")) +
  facet_wrap(~factor(COMMON_NAME, levels = cc_species_codes$COMMON_NAME), scales = "free", ncol = 2) +
  theme_bw() +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(hjust = 0, face = "bold", size = 8),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 9),
    legend.text = element_text(size = 8, face = "bold"),
    legend.title = element_text(size = 9),
    panel.spacing = unit(1, unit = "mm"),
    legend.position = "bottom",
    legend.direction = "horizontal"
  )


ggsave(
  p_lgcp, 
  filename = here::here("plots", "catch_comparison", "lgcp_rce_by_size.png"), 
  width = 169, 
  height = 169, 
  units = "mm", 
  dpi = 300
)
