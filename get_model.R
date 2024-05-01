#' Build a mixed model 
#'
#' A function.
#' 
#' @param BCS_data Bottom contact dataframe output of get_bottom_data()
#' @export

get_model <- function(BCS_data = BCS_data) {

# Scale numeric data only  
BCS_data <- BCS_data %>% filter(!NET_YEAR %in% c("2013-36", "2014-35", "2016-33", "2017-2", "2023-14"))
scaled_data <- BCS_data %>% mutate_at(vars(c(6:23)), scale)

# Create a mixed-effects model to estimate the median x and z values for the BCS data with station as a random effect
bcs_x <- lmer(median.x ~ 0 + NET_NUMBER:YEAR + (1|STATION), data = scaled_data)
bcs_z <- lmer(median.z ~ 0 + NET_NUMBER:YEAR + (1|STATION), data = scaled_data) 

# Create dataframe for fixed effects mean estimates
par_x <- fixef(bcs_x) |>
  as.data.frame() |>
  dplyr::rename(`Fit x` = `fixef(bcs_x)`)
par_x$YEAR <- as.numeric(gsub(pattern = "\\D+", 
                              replacement = "", 
                              x = gsub(".*:","", rownames(par_x))))
par_x$NET_NUMBER <- gsub(pattern = "\\D+", 
                         replacement = "", 
                         x = gsub(":.*","", rownames(par_x)))

par_z <- fixef(bcs_z) |>
  as.data.frame() |>
  dplyr::rename(`Fit z` = `fixef(bcs_z)`)
par_z$YEAR <- as.numeric(gsub(pattern = "\\D+", 
                              replacement = "", 
                              x = gsub(".*:","", rownames(par_z))))
par_z$NET_NUMBER <- gsub(pattern = "\\D+", 
                         replacement = "", 
                         x = gsub(":.*","", rownames(par_z)))

# Create a dataframe of each combination of year, net number
# and station to use the above model to predict x and z BCS values
fit <- dplyr::select(scaled_data, YEAR, NET_NUMBER, STATION) |> unique()
fit$NET_YEAR <- paste(fit$YEAR, fit$NET_NUMBER, sep = "-")
fit$fit_x <- predict(bcs_x, newdata = fit)
fit$fit_z <- predict(bcs_z, newdata = fit)

# Unscale fit predictions based on raw BCS data stats
scalefit <- BCS_data |> dplyr::summarise(MEAN_median.x = mean(median.x, na.rm = TRUE),
                                    SD_median.x = sd(median.x, na.rm = TRUE),
                                    MEAN_median.z = mean(median.z, na.rm = TRUE),
                                    SD_median.z = sd(median.z, na.rm = TRUE))
fit$fit_x <- fit$fit_x*scalefit$SD_median.x + scalefit$MEAN_median.x
fit$fit_z <- fit$fit_z*scalefit$SD_median.z + scalefit$MEAN_median.z

# Summarize bottom contact by net_number and year
bottomcontact_netyear <- BCS_data |>
  dplyr::group_by(YEAR, NET_NUMBER, NET_YEAR) |>
  dplyr::summarise(MEAN_median.x = mean(median.x, na.rm = TRUE),
                   MIN_median.x = min(median.x, na.rm = TRUE),
                   MAX_median.x = max(median.x, na.rm = TRUE),
                   Q025_median.x = quantile(median.x, probs = 0.025, na.rm = TRUE),
                   Q975_median.x = quantile(median.x, probs = 0.975, na.rm = TRUE),
                   Q25_median.x = quantile(median.x, probs = 0.25, na.rm = TRUE),
                   Q75_median.x = quantile(median.x, probs = 0.75, na.rm = TRUE),
                   SD_median.x = sd(median.x, na.rm = TRUE),
                   MEAN_median.z = mean(median.z, na.rm = TRUE),
                   MIN_median.z = min(median.z, na.rm = TRUE),
                   MAX_median.z = max(median.z, na.rm = TRUE),
                   Q025_median.z = quantile(median.z, probs = 0.025, na.rm = TRUE),
                   Q975_median.z = quantile(median.z, probs = 0.975, na.rm = TRUE),
                   Q25_median.z = quantile(median.z, probs = 0.25, na.rm = TRUE),
                   Q75_median.z = quantile(median.z, probs = 0.75, na.rm = TRUE),
                   SD_median.z = sd(median.z, na.rm = TRUE))

# Summarize model fit for bottom contact by net_number and year
bottomcontact_fit <- fit |>
  dplyr::group_by(YEAR, NET_NUMBER, NET_YEAR) |>
  dplyr::summarise(MEAN_FITX = mean(fit_x, na.rm = TRUE),
                   MIN_FITX = min(fit_x, na.rm = TRUE),
                   MAX_FITX = max(fit_x, na.rm = TRUE),
                   Q025_FITX = quantile(fit_x, probs = 0.025, na.rm = TRUE),
                   Q975_FITX = quantile(fit_x, probs = 0.975, na.rm = TRUE),
                   Q25_FITX = quantile(fit_x, probs = 0.25, na.rm = TRUE),
                   Q75_FITX = quantile(fit_x, probs = 0.75, na.rm = TRUE),
                   SD_FITX = sd(fit_x, na.rm = TRUE),
                   MEAN_FITZ = mean(fit_z, na.rm = TRUE),
                   MIN_FITZ = min(fit_z, na.rm = TRUE),
                   MAX_FITZ = max(fit_z, na.rm = TRUE),
                   Q025_FITZ = quantile(fit_z, probs = 0.025, na.rm = TRUE),
                   Q975_FITZ = quantile(fit_z, probs = 0.975, na.rm = TRUE),
                   Q25_FITZ = quantile(fit_z, probs = 0.25, na.rm = TRUE),
                   Q75_FITZ = quantile(fit_z, probs = 0.75, na.rm = TRUE),
                   SD_FITZ = sd(fit_z, na.rm = TRUE))
 
# Summarize all data combined, all net/years
all_average <- BCS_data |>
  dplyr::summarise(MEAN_median.x = mean(median.x, na.rm = TRUE),
                   MIN_median.x = min(median.x, na.rm = TRUE),
                   MAX_median.x = max(median.x, na.rm = TRUE),
                   Q025_median.x = quantile(median.x, probs = 0.025, na.rm = TRUE),
                   Q975_median.x = quantile(median.x, probs = 0.975, na.rm = TRUE),
                   Q25_median.x = quantile(median.x, probs = 0.25, na.rm = TRUE),
                   Q75_median.x = quantile(median.x, probs = 0.75, na.rm = TRUE),
                   SD_median.x = sd(median.x, na.rm = TRUE),
                   MEAN_median.z = mean(median.z, na.rm = TRUE),
                   MIN_median.z = min(median.z, na.rm = TRUE),
                   MAX_median.z = max(median.z, na.rm = TRUE),
                   Q025_median.z = quantile(median.z, probs = 0.025, na.rm = TRUE),
                   Q975_median.z = quantile(median.z, probs = 0.975, na.rm = TRUE),
                   Q25_median.z = quantile(median.z, probs = 0.25, na.rm = TRUE),
                   Q75_median.z = quantile(median.z, probs = 0.75, na.rm = TRUE),
                   SD_median.z = sd(median.z, na.rm = TRUE))

# Summarize model fit, all data combined
all_fit <- fit |>
  dplyr::summarise(MEAN_FITX = mean(fit_x, na.rm = TRUE),
                   MIN_FITX = min(fit_x, na.rm = TRUE),
                   MAX_FITX = max(fit_x, na.rm = TRUE),
                   Q025_FITX = quantile(fit_x, probs = 0.025, na.rm = TRUE),
                   Q975_FITX = quantile(fit_x, probs = 0.975, na.rm = TRUE),
                   Q25_FITX = quantile(fit_x, probs = 0.25, na.rm = TRUE),
                   Q75_FITX = quantile(fit_x, probs = 0.75, na.rm = TRUE),
                   SD_FITX = sd(fit_x, na.rm = TRUE),
                   MEAN_FITZ = mean(fit_z, na.rm = TRUE),
                   MIN_FITZ = min(fit_z, na.rm = TRUE),
                   MAX_FITZ = max(fit_z, na.rm = TRUE),
                   Q025_FITZ = quantile(fit_z, probs = 0.025, na.rm = TRUE),
                   Q975_FITZ = quantile(fit_z, probs = 0.975, na.rm = TRUE),
                   Q25_FITZ = quantile(fit_z, probs = 0.25, na.rm = TRUE),
                   Q75_FITZ = quantile(fit_z, probs = 0.75, na.rm = TRUE),
                   SD_FITZ = sd(fit_z, na.rm = TRUE))

# Create and label overall (all netnumbers/years) averages and overall (all netnumbers/years) model fits
plot_all <- merge(all_average, all_fit)
plot_all$YEAR <- "ALL"
plot_all$NET_NUMBER <- "ALL"
plot_all$NET_YEAR <- "ALL"

# Add time-series averages to dataset
plot_data <- merge(bottomcontact_netyear, bottomcontact_fit, by = 'NET_YEAR', all = TRUE)
plot_data = subset(plot_data, select = -c(NET_NUMBER.y, YEAR.y) )
plot_data <- plot_data |> dplyr::rename(NET_NUMBER = NET_NUMBER.x, YEAR = YEAR.x)
plotdata <- rbind(plot_data, plot_all)

# Output list including both models and plot dataset
output <- list(bcs_x = bcs_x,
               bcs_z = bcs_z,
               plotdata = plotdata,
               par_x = par_x,
               par_z = par_z)


return(output)

}

