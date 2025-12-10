library(dplyr)
library(tidyr)
library(sf)
library(spdep)
library(INLA)        # install.packages("INLA", repos=c(getOption("repos"), INLA='https://inla.r-inla-download.org/R/stable'))
library(ggplot2)
library(tmap)        # for mapping
library(Metrics)

################################################################################

crime_weekly <- readRDS("crime_weekly_new.rds")

k=2
crime_weekly <- crime_weekly %>%
  mutate(time_id3= ((time_id - 1) %/% k) + 1L)

crime_weekly$time_id4 <- crime_weekly$time_id

g <- inla.read.graph("adm3.adj") #read the created graph

################################################################################

t1 <- y ~ 1 + f(time_id, model = "rw1")

m_t1 <- inla(
  t1,
  family = "poisson",
  data = crime_weekly,
  control.predictor = list(compute = TRUE),
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE, config = TRUE),
  # safer numerical settings:
  control.inla = list(
    strategy = "simplified.laplace"       # gaussian, simplified.laplace (default) or laplace
    #int.strategy = "eb",                   # ccd (default), grid or eb (empirical bayes)
    #h = 0.005,                            #smaller step size for derivatives
    #tolerance = 1e-4                      # stricter convergence criterion
  ),
  control.mode = list(restart = TRUE),
  verbose = TRUE
)
summary(m_t1)

crime_weekly$pred_meant1 <- m_t1$summary.fitted.values$mean

ggplot(crime_weekly, aes(x = y, y = pred_meant1)) +
  geom_point(alpha = 0.3, color = "steelblue") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(
    x = "Actual weekly crime count",
    y = "Predicted mean (INLA)",
    title = "Actual vs Predicted Crime Counts"
  ) +
  theme_minimal()

rmse_value <- rmse(crime_weekly$y, crime_weekly$pred_meant1)
mae_value  <- mae(crime_weekly$y, crime_weekly$pred_meant1)
r2_value   <- cor(crime_weekly$y, crime_weekly$pred_meant1)^2

c(RMSE = rmse_value, MAE = mae_value, R2 = r2_value)

cpo_obj_t1 <- m_t1$cpo
pit_vals_t1 <- cpo_obj_t1$pit

# Histogram of PIT values: should be ~Uniform(0,1)
hist(pit_vals_t1, breaks = 20, main = "PIT histogram", xlab = "PIT values")

################################################################################

t2 <- y ~ 1 + f(time_id, model = "ar1")

m_t2 <- inla(
  t2,
  family = "poisson",
  data = crime_weekly,
  control.predictor = list(compute = TRUE),
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE, config = TRUE),
  # safer numerical settings:
  control.inla = list(
    strategy = "simplified.laplace"       # gaussian, simplified.laplace (default) or laplace
    #int.strategy = "eb",                   # ccd (default), grid or eb (empirical bayes)
    #h = 0.005,                            #smaller step size for derivatives
    #tolerance = 1e-4                      # stricter convergence criterion
  ),
  control.mode = list(restart = TRUE),
  verbose = TRUE
)
summary(m_t2)

crime_weekly$pred_meant2 <- m_t2$summary.fitted.values$mean

ggplot(crime_weekly, aes(x = y, y = pred_meant2)) +
  geom_point(alpha = 0.3, color = "steelblue") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(
    x = "Actual weekly crime count",
    y = "Predicted mean (INLA)",
    title = "Actual vs Predicted Crime Counts"
  ) +
  theme_minimal()

rmse_value <- rmse(crime_weekly$y, crime_weekly$pred_meant2)
mae_value  <- mae(crime_weekly$y, crime_weekly$pred_meant2)
r2_value   <- cor(crime_weekly$y, crime_weekly$pred_meant2)^2

c(RMSE = rmse_value, MAE = mae_value, R2 = r2_value)

cpo_obj_t2 <- m_t2$cpo
pit_vals_t2 <- cpo_obj_t2$pit

# Histogram of PIT values: should be ~Uniform(0,1)
hist(pit_vals_t2, breaks = 20, main = "PIT histogram", xlab = "PIT values")













