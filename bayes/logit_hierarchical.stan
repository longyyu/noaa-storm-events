data {
   // hyperparmeters
   int N; // data size
   int K; // number of variables
   int G; // number of groups (states)
   
   // Prior parameters for the coefficients
   real alpha_mean;
   real alpha_s2;
   real a_mean;
   real a_s2;
   vector[K] beta_mean; 
   vector[K] beta_s2;
   
   // data
   int<lower = 0, upper = 1> y[N];
   row_vector[K] X[N];
   int<lower=1> group[N]; // maps obs to groups (states)
}
 
parameters {
  real alpha; // generic intercept
  real a[G]; // state-specific intercept
  // covariate coefficients
  vector[K] beta;
}

transformed parameters {
   vector[N] eta; // linear predictor
   vector[N] rho; // bernoulli mean
   for (n in 1:N)
     eta[n] = alpha + a[group[n]] + X[n] * beta;
   rho = inv_logit(eta); // link function: logit
}
 
model {
   // priors on coefficients
   alpha ~ normal(alpha_mean, alpha_s2);
   a ~ normal(a_mean, a_s2);
   beta ~ normal(beta_mean, beta_s2);
   // sampling model
   y ~ bernoulli(rho);
}

// generate synthetic data
generated quantities {
   int y_rep[N];
   y_rep = bernoulli_rng(rho);
}

