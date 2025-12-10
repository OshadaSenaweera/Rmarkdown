library(dplyr)
library(tidyr)
library(sf)
library(spdep)
library(INLA)        # install.packages("INLA", repos=c(getOption("repos"), INLA='https://inla.r-inla-download.org/R/stable'))
library(ggplot2)
library(tmap)        # for mapping


################################################################################

crime_weekly <- readRDS("crime_weekly_new.rds")

k=2
crime_weekly <- crime_weekly %>%
  mutate(time_id3= ((time_id - 1) %/% k) + 1L)

crime_weekly$time_id4 <- crime_weekly$time_id

g <- inla.read.graph("adm3.adj") #read the created graph

################################################################################

# Hyper-priors (penalized complexity) that work well in practice
hyper_bym2 <- list(
  phi  = list(prior = "pc", param = c(0.5, 2/3)),      # mixing BYM2 structured/unstructured
  prec = list(prior = "pc.prec", param = c(1, 0.01))   # base precision
)

#h.spec <- list(rho = list(prior = 'pc.cor1', param = c(0, 0.9)))
#hyper_iid = list(prec = list(prior = "pc.prec", param = c(1, 0.01)))


formula_spacetime <- y ~ 1 +
  f(area_id, model = "bym2", graph = g, scale.model = TRUE, hyper = hyper_bym2)

m1 <- inla(
  formula_spacetime,
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
summary(m1)

crime_weekly$pred_mean <- m1$summary.fitted.values$mean

ggplot(crime_weekly, aes(x = y, y = pred_mean)) +
  geom_point(alpha = 0.3, color = "steelblue") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(
    x = "Actual weekly crime count",
    y = "Predicted mean (INLA)",
    title = "Actual vs Predicted Crime Counts"
  ) +
  theme_minimal()

library(Metrics)
rmse_value <- rmse(crime_weekly$y, crime_weekly$pred_mean)
mae_value  <- mae(crime_weekly$y, crime_weekly$pred_mean)
r2_value   <- cor(crime_weekly$y, crime_weekly$pred_mean)^2

c(RMSE = rmse_value, MAE = mae_value, R2 = r2_value)

cpo_obj <- m1$cpo
pit_vals <- cpo_obj$pit

# Histogram of PIT values: should be ~Uniform(0,1)
hist(pit_vals, breaks = 20, main = "PIT histogram", xlab = "PIT values")

################################################################################

formula_spacetime <- y ~ 1 +
  f(area_id, model = "besag", graph = g, scale.model = TRUE)

m2 <- inla(
  formula_spacetime,
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

summary(m2)
