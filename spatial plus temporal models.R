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

formula_spacetime_6 <- y ~ 1 +
  f(area_id, model = "besag", graph = g, scale.model = TRUE)+
  f(time_id, model = "rw1")

m6 <- inla(
  formula_spacetime_6,
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
summary(m6)

crime_weekly$pred_mean6 <- m6$summary.fitted.values$mean

ggplot(crime_weekly, aes(x = y, y = pred_mean6)) +
  geom_point(alpha = 0.3, color = "steelblue") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(
    x = "Actual weekly crime count",
    y = "Predicted mean (INLA)",
    title = "Actual vs Predicted Crime Counts"
  ) +
  theme_minimal()

rmse_value <- rmse(crime_weekly$y, crime_weekly$pred_mean6)
mae_value  <- mae(crime_weekly$y, crime_weekly$pred_mean6)
r2_value   <- cor(crime_weekly$y, crime_weekly$pred_mean6)^2

c(RMSE = rmse_value, MAE = mae_value, R2 = r2_value)

cpo_obj_6 <- m6$cpo
pit_vals_6 <- cpo_obj_6$pit

# Histogram of PIT values: should be ~Uniform(0,1)
hist(pit_vals_6, breaks = 20, main = "PIT histogram", xlab = "PIT values")


###############################################################################

formula_spacetime_7 <- y ~ 1 +
  f(area_id, model = "besag", graph = g, scale.model = TRUE)+
  f(time_id, model = "ar1")

m7 <- inla(
  formula_spacetime_7,
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
summary(m7)

crime_weekly$pred_mean7 <- m7$summary.fitted.values$mean

ggplot(crime_weekly, aes(x = y, y = pred_mean6)) +
  geom_point(alpha = 0.3, color = "steelblue") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(
    x = "Actual weekly crime count",
    y = "Predicted mean (INLA)",
    title = "Actual vs Predicted Crime Counts"
  ) +
  theme_minimal()

rmse_value <- rmse(crime_weekly$y, crime_weekly$pred_mean7)
mae_value  <- mae(crime_weekly$y, crime_weekly$pred_mean7)
r2_value   <- cor(crime_weekly$y, crime_weekly$pred_mean7)^2

c(RMSE = rmse_value, MAE = mae_value, R2 = r2_value)

cpo_obj_7 <- m7$cpo
pit_vals_7 <- cpo_obj_7$pit

# Histogram of PIT values: should be ~Uniform(0,1)
hist(pit_vals_7, breaks = 20, main = "PIT histogram", xlab = "PIT values")

###############################################################################

formula_spacetime_8 <- y ~ 1 +
  f(area_id, model = "besag", graph = g, scale.model = TRUE)+
  f(time_id, model = "iid")

m8 <- inla(
  formula_spacetime_8,
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
summary(m8)

crime_weekly$pred_mean8 <- m8$summary.fitted.values$mean

ggplot(crime_weekly, aes(x = y, y = pred_mean8)) +
  geom_point(alpha = 0.3, color = "steelblue") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(
    x = "Actual weekly crime count",
    y = "Predicted mean (INLA)",
    title = "Actual vs Predicted Crime Counts"
  ) +
  theme_minimal()

rmse_value <- rmse(crime_weekly$y, crime_weekly$pred_mean8)
mae_value  <- mae(crime_weekly$y, crime_weekly$pred_mean8)
r2_value   <- cor(crime_weekly$y, crime_weekly$pred_mean8)^2

c(RMSE = rmse_value, MAE = mae_value, R2 = r2_value)

cpo_obj_8 <- m8$cpo
pit_vals_8 <- cpo_obj_8$pit

# Histogram of PIT values: should be ~Uniform(0,1)
hist(pit_vals_8, breaks = 20, main = "PIT histogram", xlab = "PIT values")

################################################################################

formula_spacetime_9 <- y ~ 1 +
  f(area_id, model = "besag", graph = g, scale.model = TRUE)+
  f(time_id, model = "rw2")

m9 <- inla(
  formula_spacetime_9,
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
summary(m9)

crime_weekly$pred_mean9 <- m9$summary.fitted.values$mean

ggplot(crime_weekly, aes(x = y, y = pred_mean9)) +
  geom_point(alpha = 0.3, color = "steelblue") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(
    x = "Actual weekly crime count",
    y = "Predicted mean (INLA)",
    title = "Actual vs Predicted Crime Counts"
  ) +
  theme_minimal()

rmse_value <- rmse(crime_weekly$y, crime_weekly$pred_mean9)
mae_value  <- mae(crime_weekly$y, crime_weekly$pred_mean9)
r2_value   <- cor(crime_weekly$y, crime_weekly$pred_mean9)^2

c(RMSE = rmse_value, MAE = mae_value, R2 = r2_value)

cpo_obj_9 <- m9$cpo
pit_vals_9 <- cpo_obj_9$pit

# Histogram of PIT values: should be ~Uniform(0,1)
hist(pit_vals_9, breaks = 20, main = "PIT histogram", xlab = "PIT values")

################################################################################

formula_spacetime_10 <- y ~ 1 +
  f(area_id, model = "bym2", graph = g, scale.model = TRUE)+
  f(time_id, model = "rw1")

m10 <- inla(
  formula_spacetime_10,
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
summary(m10)

crime_weekly$pred_mean10 <- m10$summary.fitted.values$mean

ggplot(crime_weekly, aes(x = y, y = pred_mean9)) +
  geom_point(alpha = 0.3, color = "steelblue") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(
    x = "Actual weekly crime count",
    y = "Predicted mean (INLA)",
    title = "Actual vs Predicted Crime Counts"
  ) +
  theme_minimal()

rmse_value <- rmse(crime_weekly$y, crime_weekly$pred_mean10)
mae_value  <- mae(crime_weekly$y, crime_weekly$pred_mean10)
r2_value   <- cor(crime_weekly$y, crime_weekly$pred_mean10)^2

c(RMSE = rmse_value, MAE = mae_value, R2 = r2_value)

cpo_obj_9 <- m9$cpo
pit_vals_9 <- cpo_obj_9$pit

# Histogram of PIT values: should be ~Uniform(0,1)
hist(pit_vals_9, breaks = 20, main = "PIT histogram", xlab = "PIT values")

################################################################################

formula_spacetime_11 <- y ~ 1 +
  f(area_id, model = "bym2", graph = g, scale.model = TRUE)+
  f(time_id, model = "ar1")

m11 <- inla(
  formula_spacetime_11,
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
summary(m11)

crime_weekly$pred_mean11 <- m11$summary.fitted.values$mean

ggplot(crime_weekly, aes(x = y, y = pred_mean11)) +
  geom_point(alpha = 0.3, color = "steelblue") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(
    x = "Actual weekly crime count",
    y = "Predicted mean (INLA)",
    title = "Actual vs Predicted Crime Counts"
  ) +
  theme_minimal()

rmse_value <- rmse(crime_weekly$y, crime_weekly$pred_mean11)
mae_value  <- mae(crime_weekly$y, crime_weekly$pred_mean11)
r2_value   <- cor(crime_weekly$y, crime_weekly$pred_mean11)^2

c(RMSE = rmse_value, MAE = mae_value, R2 = r2_value)

cpo_obj_9 <- m9$cpo
pit_vals_9 <- cpo_obj_9$pit

# Histogram of PIT values: should be ~Uniform(0,1)
hist(pit_vals_9, breaks = 20, main = "PIT histogram", xlab = "PIT values")


################################################################################







