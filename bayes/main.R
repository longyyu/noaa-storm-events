# Bayes model fitting
# Author: Yanyu Long
# Updated: Apr 18, 2021

# Set up ----------------------------------------------------------------------
run_level = 2 # 1 for trial run, 2 for official run
N_WARMUP = switch(run_level, 300, 1000)
N_ITER = switch(run_level, 1000, 3000)
N_CORE = switch(run_level, 1L, 12L)
EXPORT_FILE = switch(run_level, "fit_logit_rl1", "fit_logit_rl2")
EXPORT_FILE_HIER = switch(run_level, "fit_logit_hier_rl1", "fit_logit_hier_rl2")

suppressPackageStartupMessages({
  library(tidyverse)
  library(rstan)
  library(bayesplot)
})

compute_ber = function(yhat, ytrue) {
  # helper function to compute the balanced error rates
  yhat = (yhat > 0.5) * 1L
  TP = sum((yhat == 1) & (ytrue == 1))
  FP = sum((yhat == 1) & (ytrue == 0))
  TN = sum((yhat == 0) & (ytrue == 0))
  FN = sum((yhat == 0) & (ytrue == 1))
  0.5 * (FP/(TN+FP) + FN/(FN+TP))
}

# Data input ------------------------------------------------------------------
load("data/windstorm_weather_prev_month.RData")
data_merged = data_merged %>%
  mutate(outcome = ifelse(num_episodes > 0, 1, 0)) %>% 
  select(-ym, -city, -county, -num_episodes) %>%
  drop_na()

data_x = data_merged %>% 
  select(meantemp_avg, meantemp_sd, humidity_avg, humidity_sd,
         wind_speed_avg, wind_speed_sd) %>% 
  as.matrix() %>% scale()
K = ncol(data_x)
states = as.factor(data_merged$state) # levels(states)

# the hierarchical logistic model ---------------------------------------------

stan_data_hier = list(
  N = nrow(data_x),
  K = K,
  G = length(levels(states)),
  y = data_merged$outcome,
  X = data_x,
  group = as.numeric(states),
  alpha_mean = 0,
  alpha_s2 = 10,
  a_mean = 0,
  a_s2 = 5,
  beta_mean = rep(0, K),
  beta_s2 = rep(2.5, K)
)

# MCMC simulation ------------
if (dir("bayes/", pattern = EXPORT_FILE_HIER) %>% length() == 0) {
  # only re-fit the model when EXPORT_FILEs are not found
  fit_logit_hier = stan(
    "bayes/logit_hierarchical.stan", data = stan_data_hier, 
    pars = c("alpha", "a", "beta", "y_rep", "rho"), # not saving eta
    warmup = N_WARMUP, iter = N_ITER, refresh = 1000, cores = N_CORE,
    sample_file = sprintf("bayes/%s.csv", EXPORT_FILE_HIER)
  )
} # if

system.time({ # 30sec
  fit_logit_hier = read_stan_csv(
    dir("bayes/", pattern = EXPORT_FILE_HIER, full.names = TRUE)
  )
})

# Results --------------
traceplot(fit_logit_hier, c("alpha", "beta"), inc_warmup = TRUE)
traceplot(fit_logit_hier, c("a"), inc_warmup = TRUE)

fit_summary = summary(fit_logit_hier, pars = c("alpha", "beta"), probs = c(0.025, 0.975))$summary
fit_summary # %>% knitr::kable(digits = 3)

fit_summary = summary(fit_logit_hier, pars = c("a"), probs = c(0.025, 0.975))$summary
fit_summary # %>% knitr::kable(digits = 3)

plot(fit_logit, pars = c("alpha", "beta"))
library(bayesplot)
color_scheme_set("blue")
mcmc_intervals(as.matrix(fit_logit, pars = c("alpha", "beta")), prob = 0.95)
ppc_dens_overlay(stan_data$y, as.matrix(fit_logit, pars = "y_rep")[1:150,])


# the basic logistic model ----------------------------------------------------

stan_data = list(
  N = nrow(data_x),
  K = K,
  y = data_merged$outcome,
  X = data_x,
  alpha_mean = 0,
  alpha_s2 = 10,
  beta_mean = rep(0, K),
  beta_s2 = rep(2.5, K)
)

# MCMC simulation ---------
if (dir("bayes/", pattern = EXPORT_FILE) %>% length() == 0) {
  # only re-fit the model when EXPORT_FILEs are not found
  fit_logit = stan(
    "bayes/logit.stan", data = stan_data, 
    pars = c("alpha", "beta", "y_rep", "rho"), # not saving eta
    warmup = N_WARMUP, iter = N_ITER, refresh = 1000, cores = N_CORE,
    sample_file = sprintf("bayes/%s.csv", EXPORT_FILE)
  )
} # if

system.time({ # 30sec
  fit_logit = read_stan_csv(
    dir("bayes/", pattern = EXPORT_FILE, full.names = TRUE)
  )
})

# Results --------------
traceplot(fit_logit, c("alpha", "beta"), inc_warmup = TRUE)

get_posterior_mean(fit_logit, pars = c("alpha", "beta"))[,5]
fit_summary = summary(fit_logit, pars = c("alpha", "beta"), probs = c(0.025, 0.975))$summary
fit_summary # %>% knitr::kable(digits = 3)

plot(fit_logit, pars = c("alpha", "beta"))
library(bayesplot)
color_scheme_set("blue")
mcmc_intervals(as.matrix(fit_logit, pars = c("alpha", "beta")), prob = 0.95)
ppc_dens_overlay(stan_data$y, as.matrix(fit_logit, pars = "y_rep")[1:150,])

# evaluation ------------------------------------------------------------------
y_pred = apply(as.matrix(fit_logit, pars = "y_rep"), 2, median)
y_pred = ifelse(y_pred == 0.5, 0, y_pred)
y_pred_prob = apply(as.matrix(fit_logit, pars = "rho"), 2, mean)
table(y_pred, data_merged$outcome)
mean(y_pred != data_merged$outcome)

fit_roc = pROC::roc(data_merged$outcome, y_pred_prob) # 0.7177
tibble(sensitivity = fit_roc$sensitivities,
       specificity = fit_roc$specificities) %>%
  ggplot(aes(specificity, sensitivity)) +
  theme_bw() +
  geom_line() +
  scale_x_reverse()
# ggroc(pROC::roc(data_merged$outcome, y_pred_prob))


if (FALSE) {
  # compare with frequentist approach
  data_fit = bind_cols(data_merged[,"outcome"], as.data.frame(data_x)) 
  mlogit = glm(outcome ~ ., data = data_fit, family = binomial)
  y_pred_freq = predict(mlogit, newdata = data_fit, type = "response")
  # plot(pROC::roc(data_merged$outcome, y_pred_freq), add = TRUE)
  
  
  load("data/windstorm_weather_prev_month.RData")
  data_merged = data_merged %>% 
    mutate(outcome = ifelse(num_episodes > 0, 1, 0)) %>% 
    drop_na()
  get_pred = function(dat) {
    mlogit = glm(outcome ~ ., data = dat, family = binomial)
    y_pred = predict(mlogit, newdata = dat, type = "response")
    y_pred
  } # get_pred()
  vars = c("outcome", "meantemp_avg", "meantemp_sd", "humidity_avg", "humidity_sd",
           "wind_speed_avg", "wind_speed_sd")
  y_pred = get_pred(dat = data_merged %>% filter(state == "Texas") %>% select(all_of(vars)))
  y_pred = get_pred(dat = data_merged %>% filter(state == "Florida") %>% select(all_of(vars)))
  y_pred = get_pred(dat = data_merged %>% filter(state == "Pennsylvania") %>% select(all_of(vars)))
}

