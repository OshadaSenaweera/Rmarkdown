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

# model
sto_1 <- y ~ 1 +
  f(area_id2, model = "besag", graph = g, group = time_id2, 
    control.group = list(model = "rw1"))

m_sto1 <- inla(
  sto_1,
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
summary(m_sto1)

crime_weekly$pred_meansto1 <- m_sto1$summary.fitted.values$mean

ggplot(crime_weekly, aes(x = y, y = pred_meansto1)) +
  geom_point(alpha = 0.3, color = "steelblue") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(
    x = "Actual weekly crime count",
    y = "Predicted mean (INLA)",
    title = "Actual vs Predicted Crime Counts"
  ) +
  theme_minimal()

rmse_value <- rmse(crime_weekly$y, crime_weekly$pred_meansto1)
mae_value  <- mae(crime_weekly$y, crime_weekly$pred_meansto1)
r2_value   <- cor(crime_weekly$y, crime_weekly$pred_meansto1)^2

c(RMSE = rmse_value, MAE = mae_value, R2 = r2_value)

cpo_obj_sto1 <- m_sto1$cpo
pit_vals_sto1 <- cpo_obj_sto1$pit

# Histogram of PIT values: should be ~Uniform(0,1)
hist(pit_vals_sto1, breaks = 20, main = "PIT histogram", xlab = "PIT values")

################################################################################

# model
sto_2 <- y ~ 1 +
  f(area_id2, model = "besag", graph = g, group = time_id2, 
    control.group = list(model = "iid"))

m_sto2 <- inla(
  sto_2,
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
summary(m_sto2)

crime_weekly$pred_meansto2 <- m_sto2$summary.fitted.values$mean

ggplot(crime_weekly, aes(x = y, y = pred_meansto2)) +
  geom_point(alpha = 0.3, color = "steelblue") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(
    x = "Actual weekly crime count",
    y = "Predicted mean (INLA)",
    title = "Actual vs Predicted Crime Counts"
  ) +
  theme_minimal()

rmse_value <- rmse(crime_weekly$y, crime_weekly$pred_meansto2)
mae_value  <- mae(crime_weekly$y, crime_weekly$pred_meansto2)
r2_value   <- cor(crime_weekly$y, crime_weekly$pred_meansto2)^2

c(RMSE = rmse_value, MAE = mae_value, R2 = r2_value)

cpo_obj_sto2 <- m_sto2$cpo
pit_vals_sto2 <- cpo_obj_sto2$pit

# Histogram of PIT values: should be ~Uniform(0,1)
hist(pit_vals_sto2, breaks = 20, main = "PIT histogram", xlab = "PIT values")

##################################################################################

# model
sto_3 <- y ~ 1 +
  f(area_id2, model = "besag", graph = g, group = time_id2, 
    control.group = list(model = "ar1"))

m_sto3 <- inla(
  sto_3,
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
summary(m_sto3)

crime_weekly$pred_meansto3 <- m_sto3$summary.fitted.values$mean

ggplot(crime_weekly, aes(x = y, y = pred_meansto3)) +
  geom_point(alpha = 0.3, color = "steelblue") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(
    x = "Actual weekly crime count",
    y = "Predicted mean (INLA)",
    title = "Actual vs Predicted Crime Counts"
  ) +
  theme_minimal()

rmse_value <- rmse(crime_weekly$y, crime_weekly$pred_meansto3)
mae_value  <- mae(crime_weekly$y, crime_weekly$pred_meansto3)
r2_value   <- cor(crime_weekly$y, crime_weekly$pred_meansto3)^2

c(RMSE = rmse_value, MAE = mae_value, R2 = r2_value)

cpo_obj_sto3 <- m_sto3$cpo
pit_vals_sto3 <- cpo_obj_sto3$pit

# Histogram of PIT values: should be ~Uniform(0,1)
hist(pit_vals_sto3, breaks = 20, main = "PIT histogram", xlab = "PIT values")

################################################################################

# model
sto_4 <- y ~ 1 +
  f(area_id2, model = "besag", graph = g, group = time_id3, 
    control.group = list(model = "rw1"))

m_sto4 <- inla(
  sto_4,
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
summary(m_sto4)

crime_weekly$pred_meansto4 <- m_sto4$summary.fitted.values$mean

ggplot(crime_weekly, aes(x = y, y = pred_meansto4)) +
  geom_point(alpha = 0.3, color = "steelblue") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(
    x = "Actual weekly crime count",
    y = "Predicted mean (INLA)",
    title = "Actual vs Predicted Crime Counts"
  ) +
  theme_minimal()

rmse_value <- rmse(crime_weekly$y, crime_weekly$pred_meansto4)
mae_value  <- mae(crime_weekly$y, crime_weekly$pred_meansto4)
r2_value   <- cor(crime_weekly$y, crime_weekly$pred_meansto4)^2

c(RMSE = rmse_value, MAE = mae_value, R2 = r2_value)

cpo_obj_sto4 <- m_sto4$cpo
pit_vals_sto4 <- cpo_obj_sto4$pit

# Histogram of PIT values: should be ~Uniform(0,1)
hist(pit_vals_sto4, breaks = 20, main = "PIT histogram", xlab = "PIT values")


###############################################################################


# model
sto_5 <- y ~ 1 +
  f(area_id2, model = "besag", graph = g, group = time_id, 
    control.group = list(model = "iid"))

m_sto5 <- inla(
  sto_5,
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
summary(m_sto5)

crime_weekly$pred_meansto5 <- m_sto5$summary.fitted.values$mean

ggplot(crime_weekly, aes(x = y, y = pred_meansto5)) +
  geom_point(alpha = 0.3, color = "steelblue") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(
    x = "Actual weekly crime count",
    y = "Predicted mean (INLA)",
    title = "Actual vs Predicted Crime Counts"
  ) +
  theme_minimal()

rmse_value <- rmse(crime_weekly$y, crime_weekly$pred_meansto5)
mae_value  <- mae(crime_weekly$y, crime_weekly$pred_meansto5)
r2_value   <- cor(crime_weekly$y, crime_weekly$pred_meansto5)^2

c(RMSE = rmse_value, MAE = mae_value, R2 = r2_value)

cpo_obj_sto5 <- m_sto5$cpo
pit_vals_sto5 <- cpo_obj_sto5$pit

# Histogram of PIT values: should be ~Uniform(0,1)
hist(pit_vals_sto5, breaks = 20, main = "PIT histogram", xlab = "PIT values")

################################################################################

# model
sto_6 <- y ~ 1 +
  f(area_id2, model = "besag", graph = g, group = time_id, 
    control.group = list(model = "rw1"))

m_sto6 <- inla(
  sto_6,
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
summary(m_sto6)

crime_weekly$pred_meansto6 <- m_sto6$summary.fitted.values$mean

ggplot(crime_weekly, aes(x = y, y = pred_meansto6)) +
  geom_point(alpha = 0.3, color = "steelblue") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(
    x = "Actual weekly crime count",
    y = "Predicted mean (INLA)",
    title = "Actual vs Predicted Crime Counts"
  ) +
  theme_minimal()

rmse_value <- rmse(crime_weekly$y, crime_weekly$pred_meansto6)
mae_value  <- mae(crime_weekly$y, crime_weekly$pred_meansto6)
r2_value   <- cor(crime_weekly$y, crime_weekly$pred_meansto6)^2

c(RMSE = rmse_value, MAE = mae_value, R2 = r2_value)

cpo_obj_sto6 <- m_sto6$cpo
pit_vals_sto6 <- cpo_obj_sto6$pit

# Histogram of PIT values: should be ~Uniform(0,1)
hist(pit_vals_sto6, breaks = 20, main = "PIT histogram", xlab = "PIT values")

###############################################################################

# model
sto_7 <- y ~ 1 + spike_weeks + riot +
  f(area_id2, model = "besag", graph = g, group = time_id3, 
    control.group = list(model = "rw1"))

m_sto7 <- inla(
  sto_7,
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

summary(m_sto7)

crime_weekly$pred_meansto6 <- m_sto6$summary.fitted.values$mean

ggplot(crime_weekly, aes(x = y, y = pred_meansto6)) +
  geom_point(alpha = 0.3, color = "steelblue") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(
    x = "Actual weekly crime count",
    y = "Predicted mean (INLA)",
    title = "Actual vs Predicted Crime Counts"
  ) +
  theme_minimal()

rmse_value <- rmse(crime_weekly$y, crime_weekly$pred_meansto6)
mae_value  <- mae(crime_weekly$y, crime_weekly$pred_meansto6)
r2_value   <- cor(crime_weekly$y, crime_weekly$pred_meansto6)^2

c(RMSE = rmse_value, MAE = mae_value, R2 = r2_value)

cpo_obj_sto7 <- m_sto7$cpo
pit_vals_sto7 <- cpo_obj_sto7$pit

# Histogram of PIT values: should be ~Uniform(0,1)
hist(pit_vals_sto7, breaks = 20, main = "PIT histogram", xlab = "PIT values")












