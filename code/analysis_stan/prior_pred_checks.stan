///
// This Stan program defines a model for estimating the prior predictive distributions for
// the analysis of the proportion of people that seek care.

data {
  // positivity rates from meta-analysis by test
  int<lower=1> N_obs; // number of observations included in main analysis
  array[N_obs] int<lower=1> num_survey; // number of individuals in each study surveyed
  
  // random effect
  int<lower=1> N_re; // number of unique random effect categories
  array[N_obs] int<lower=1, upper=N_re> re; // random effect corresponding to observation

  // covariates
  int<lower=1> p_vars; // number of variables to adjust for
  matrix[N_obs, p_vars] X; // covariate model matrix
}

generated quantities {
  real alpha; // intercept
  vector[p_vars] beta; // fixed regression coefficients
  real<lower=0> sigma_re; // variability of random effect
  vector[N_re] eta_re; // standard normals for the random effect
  vector[N_obs] logit_p; 
  vector<lower=0, upper=1>[N_obs] p;
  array[N_obs] int num_sought_pred; // predicted number of individuals that sought care
  
  // Sample from priors
  alpha = normal_rng(-0.67, 1.5);
  for (i in 1:p_vars) beta[i] = normal_rng(0, 2);

  // Truncated normal for sigma_re
  int valid_sigma = 0; // Track if a valid value has been sampled
  while (valid_sigma == 0) {
    sigma_re = normal_rng(0, 1);
    if (sigma_re > 0) valid_sigma = 1; // Break out of the loop when a positive value is obtained
  }

  for (i in 1:N_re) eta_re[i] = normal_rng(0, 1);

  // Compute logit and probability for each observation
  logit_p = alpha + X * beta + sigma_re * eta_re[re];
  p = inv_logit(logit_p);
  
  // Generate simulated data
  for (i in 1:N_obs) {
    num_sought_pred[i] = binomial_rng(num_survey[i], p[i]);
  }
}
