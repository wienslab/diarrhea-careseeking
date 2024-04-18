///
// This Stan program defines a model for estimating the proportion of people that seek care,
// using a meta-analysis across >100 studies with covariates and a random effect by study.

data {
  
  // positivity rates from meta-analysis by test
  int<lower=1> N_obs; // number of observations included in main analysis
  array[N_obs] int<lower=1> num_survey; // number of individuals in each study surveyed
  array[N_obs] int<lower=0> num_sought; // number of individuals that sought care
  
  // random effect
  int<lower=1> N_re; // number of unique random effect categories
  array[N_obs] int<lower=1, upper=N_re> re; // random effect corresponding to observation

  // covariates
  int<lower=1> p_vars; // number of variables to adjust for
  matrix[N_obs, p_vars] X; // covariate model matrix
  
}

parameters {
  real alpha; // intercept
  vector[p_vars] beta; // fixed regression coefficients
  real<lower=0> sigma_re; // variability of random effect
  vector[N_re] eta_re; // standard normals for the random effect
}

transformed parameters {
  // probability of seeking care for each observation
  vector[N_obs] logit_p; 
  vector<lower=0, upper=1>[N_obs] p;
  logit_p = alpha + X * beta + sigma_re * eta_re[re];
  p = inv_logit(logit_p);
}

model {
  // proprtion that sought care
  target += binomial_lpmf(num_sought | num_survey, p);
  
  // priors on p, intercept, coefficients, random effect, and sigma_re
  target += normal_lpdf(beta | 0, 2);
  target += normal_lpdf(eta_re | 0, 1);
  target += normal_lpdf(sigma_re | 0, 1);
  target += normal_lpdf(alpha | -0.9, 2); // shift left
}
