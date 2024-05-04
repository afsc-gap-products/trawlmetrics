library(trawlmetrics)
library(lubridate)
library(dplyr)
library(lme4)
library(Matrix)
library(car)



# scaling
numeric_data <- BCS_data[, sapply(BCS_data, is.numeric)]
scaled_numeric_data <- scale(numeric_data)
scaled_data <- cbind(scaled_numeric_data, BCS_data[, !sapply(BCS_data, is.numeric)])


bcs_lmer = lmer(median.x ~ (1|STATION) + NET_NUMBER + NET_NUMBER*YEAR, data = scaled_data)
bcs_lmer_scaledZ <- lmer(median.z ~ NET_NUMBER + NET_NUMBER*YEAR + mean.x + YEAR  + (1|STATION), data = scaled_data)

summary(bcs_lmer_scaledZ)

plot(residuals(bcs_lmer_scaledZ))
qqnorm(residuals(bcs_lmer_scaledZ))
qqline(residuals(bcs_lmer_scaledZ))

plot(bcs_lmer_scaledZ)
vif(bcs_lmer_scaledZ)

