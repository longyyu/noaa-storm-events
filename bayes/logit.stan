data {
   // hyperparmeters
   int N; // data size
   int K; // number of variables
   
   // Prior parameters for the coefficients
   real alpha_mean;
   real alpha_s2;
   vector[K] beta_mean; 
   vector[K] beta_s2;
   
   // data
   int<lower = 0, upper = 1> y[N];
   matrix[N, K] X;
}
 
parameters {
  // intercept alpha
  real alpha; 
  // covariate coefficients
  vector[K] beta;
}

transformed parameters {
   // linear predictor
   vector[N] eta;
   // bernoulli mean
   vector[N] rho;
   eta = alpha + X * beta;
   rho = inv_logit(eta); // link function: logit
}
 
model {
   // priors on coefficients
   alpha ~ normal(alpha_mean, alpha_s2);
   beta ~ normal(beta_mean, beta_s2);
   // sampling model
   y ~ bernoulli(rho);
}

// generate synthetic data
generated quantities {
   int y_rep[N];
   y_rep = bernoulli_rng(rho);
}
