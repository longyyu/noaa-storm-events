data {
  // hyperparmeters
  int N; // data size
  int K; // number of variables
  int G; // number of groups (states)
  int Yr; // number of years
  
  // Prior parameters for the coefficients
  real alpha_mean;
  real alpha_s2;
  real alpha_st_mean;
  real alpha_st_s2;
  real alpha_yr_mean;
  real alpha_yr_s2;
  vector[K] beta_mean; 
  vector[K] beta_s2;
  
  // data
  int<lower = 0, upper = 1> y[N];
  row_vector[K] X[N];
  int<lower=1> group[N]; // maps obs to groups (states)
  int<lower=1> year[N]; // maps obs to years
}

parameters {
  real alpha; // generic intercept
  real alpha_st[G]; // state-specific intercept
  real alpha_yr[Yr]; // year-specific intercept
  // covariate coefficients
  vector[K] beta;
}

transformed parameters {
  vector[N] eta; // linear predictor
  vector[N] rho; // bernoulli mean
  for (n in 1:N)
    eta[n] = alpha + alpha_st[group[n]] + alpha_yr[year[n]] + X[n] * beta;
  rho = inv_logit(eta); // link function: logit
}

model {
  // priors on coefficients
  alpha ~ normal(alpha_mean, alpha_s2);
  alpha_st ~ normal(alpha_st_mean, alpha_st_s2);
  alpha_yr ~ normal(alpha_yr_mean, alpha_yr_s2);
  beta ~ normal(beta_mean, beta_s2);
  // sampling model
  y ~ bernoulli(rho);
}

// generate synthetic data
generated quantities {
  int y_rep[N];
  y_rep = bernoulli_rng(rho);
}
