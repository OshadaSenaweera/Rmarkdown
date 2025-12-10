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

# Hyper-priors (penalized complexity) that work well in practice
hyper_bym2 <- list(
  phi  = list(prior = "pc", param = c(0.5, 2/3)),      # mixing BYM2 structured/unstructured
  prec = list(prior = "pc.prec", param = c(1, 0.01))   # base precision
)

#h.spec <- list(rho = list(prior = 'pc.cor1', param = c(0, 0.9)))
#hyper_iid = list(prec = list(prior = "pc.prec", param = c(1, 0.01)))

################################################################################

formula_spacetime <- y ~ 1 +
  f(area_id, model = "bym2", graph = g, scale.model = TRUE)

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

rmse_value <- rmse(crime_weekly$y, crime_weekly$pred_mean)
mae_value  <- mae(crime_weekly$y, crime_weekly$pred_mean)
r2_value   <- cor(crime_weekly$y, crime_weekly$pred_mean)^2

c(RMSE = rmse_value, MAE = mae_value, R2 = r2_value)

cpo_obj_1 <- m1$cpo
pit_vals_1 <- cpo_obj_1$pit

# Histogram of PIT values: should be ~Uniform(0,1)
hist(pit_vals_1, breaks = 20, main = "PIT histogram", xlab = "PIT values")

################################################################################

formula_spacetime_2 <- y ~ 1 +
  f(area_id, model = "besag", graph = g, scale.model = TRUE)

m2 <- inla(
  formula_spacetime_2,
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

crime_weekly$pred_mean_2 <- m2$summary.fitted.values$mean

ggplot(crime_weekly, aes(x = y, y = pred_mean_2)) +
  geom_point(alpha = 0.3, color = "steelblue") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(
    x = "Actual weekly crime count",
    y = "Predicted mean (INLA)",
    title = "Actual vs Predicted Crime Counts"
  ) +
  theme_minimal()


rmse_value <- rmse(crime_weekly$y, crime_weekly$pred_mean_2)
mae_value  <- mae(crime_weekly$y, crime_weekly$pred_mean_2)
r2_value   <- cor(crime_weekly$y, crime_weekly$pred_mean_2)^2

c(RMSE = rmse_value, MAE = mae_value, R2 = r2_value)

cpo_obj_2 <- m2$cpo
pit_vals_2 <- cpo_obj_2$pit

# Histogram of PIT values: should be ~Uniform(0,1)
hist(pit_vals_2, breaks = 20, main = "PIT histogram", xlab = "PIT values")

################################################################################

formula_spacetime_3 <- y ~ 1 +
  f(area_id, model = "besagproper", graph = g)

m3 <- inla(
  formula_spacetime_3,
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

summary(m3)

crime_weekly$pred_mean_3 <- m3$summary.fitted.values$mean

ggplot(crime_weekly, aes(x = y, y = pred_mean_3)) +
  geom_point(alpha = 0.3, color = "steelblue") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(
    x = "Actual weekly crime count",
    y = "Predicted mean (INLA)",
    title = "Actual vs Predicted Crime Counts"
  ) +
  theme_minimal()


rmse_value <- rmse(crime_weekly$y, crime_weekly$pred_mean_3)
mae_value  <- mae(crime_weekly$y, crime_weekly$pred_mean_3)
r2_value   <- cor(crime_weekly$y, crime_weekly$pred_mean_3)^2

c(RMSE = rmse_value, MAE = mae_value, R2 = r2_value)

cpo_obj_3 <- m3$cpo
pit_vals_3 <- cpo_obj_3$pit

# Histogram of PIT values: should be ~Uniform(0,1)
hist(pit_vals_3, breaks = 20, main = "PIT histogram", xlab = "PIT values")


################################################################################

formula_spacetime_4 <- y ~ 1 +
  f(area_id, model = "iid", graph = g)

m4 <- inla(
  formula_spacetime_4,
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

summary(m4)

crime_weekly$pred_mean_4 <- m4$summary.fitted.values$mean

ggplot(crime_weekly, aes(x = y, y = pred_mean_4)) +
  geom_point(alpha = 0.3, color = "steelblue") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(
    x = "Actual weekly crime count",
    y = "Predicted mean (INLA)",
    title = "Actual vs Predicted Crime Counts"
  ) +
  theme_minimal()


rmse_value <- rmse(crime_weekly$y, crime_weekly$pred_mean_4)
mae_value  <- mae(crime_weekly$y, crime_weekly$pred_mean_4)
r2_value   <- cor(crime_weekly$y, crime_weekly$pred_mean_4)^2

c(RMSE = rmse_value, MAE = mae_value, R2 = r2_value)

cpo_obj_4 <- m4$cpo
pit_vals_4 <- cpo_obj_4$pit

# Histogram of PIT values: should be ~Uniform(0,1)
hist(pit_vals_4, breaks = 20, main = "PIT histogram", xlab = "PIT values")

################################################################################
path <- "C:/Users/oshad/OneDrive/Documents/R/SriLanka_Admin_Boundaries"
adm3_sf <- st_read(file.path(path, "lka_admbnda_adm3_slsd_20220816.shp"))  # DS Division

adm3_sf <- adm3_sf %>% select(-ADM3_SI, -ADM3_TA,-ADM2_SI,-ADM2_TA, 
                              -ADM2_PCODE, -ADM1_SI,-ADM1_TA,-ADM1_PCODE,-ADM0_EN,
                              -ADM0_SI,-ADM0_TA,-ADM0_PCODE,-date,-validOn,-validTo
)

adm3_sf <- adm3_sf %>%
  rename(area = ADM3_EN)

# Ensure order and geometry validity
adm3_sf <- adm3_sf %>%
  st_make_valid() %>%
  arrange(area)

# Create neighbor list and graph
nb <- poly2nb(adm3_sf, queen = TRUE)

dym <- c(st_distance(adm3_sf[c(48),], adm3_sf))
closest = order(dym)[1:3]
nb_1 <- addlinks1(nb, from = closest[1], to = closest[2:3])

dym2 <- c(st_distance(adm3_sf[c(118),], adm3_sf))
closest = order(dym2)[1:3]
nb_2 <- addlinks1(nb_1, from = closest[1], to = closest[3])

dym3 <- c(st_distance(adm3_sf[c(66),], adm3_sf))
closest = order(dym3)[1:3]
nb_3 <- addlinks1(nb_2, from = closest[2], to = closest[3])


library(Matrix)

n <- length(nb_3)

row_id <- rep(1:n, sapply(nb_3, length))
col_id <- unlist(nb_3)

W <- sparseMatrix(i = row_id, j = col_id, x = 1, dims = c(n, n))

C <- Diagonal(x = 1, n = nrow(adm3_sf)) - W

formula_spacetime_5 <- y ~ 1 +
  f(area_id, model = "generic1", Cmatrix = C)

m5 <- inla(
  formula_spacetime_5,
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

summary(m5)









