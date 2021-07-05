data {
  int t;
  int t_seq;
  int t_nots;
  int X[t_nots];
  int Y[t_seq];
  int N[t_seq];
}

transformed data {
  // initialise the prior beta and delta cases using observed data
  vector[2] mean_init_cases;
  vector[2] sd_init_cases;
  mean_init_cases[2] = X[1] * Y[1] / N[1];
  mean_init_cases[1] = X[1] - mean_init_cases[2];
  mean_init_cases = log(mean_init_cases);
  sd_init_cases = 0.025 * mean_init_cases;
}

parameters {
  real beta;
  real beta_noise;
  real<lower = 0> delta_mod;
  real<lower = 0> delta_noise;
  vector[t-2] eta;
  vector[t-2] delta_eta;
  real <lower = 0> init_cases[2];
  vector<lower = 0>[2] sqrt_phi;
}

transformed parameters {
  vector[t - 1] beta_r;
  vector[t - 1] delta_r;
  vector<lower = 0>[t] mean_beta_cases;
  vector<lower = 0>[t] mean_delta_cases;
  vector<lower = 0>[t] mean_cases;
  vector<lower = 0, upper = 1>[t] frac_delta;
  vector[2] phi;

  // beta growth rate
  beta_r = rep_vector(beta, t - 1);
  beta_r[2:(t-1)] = beta_r[2:(t-1)] + cumulative_sum(beta_noise * eta);

  // delta growth rate based on beta with AR residuals
  delta_r = beta_r + delta_mod;
  delta_r[2:(t-1)] = delta_r[2:(t-1)] + cumulative_sum(delta_noise * delta_eta);

  // initialise log mean cases
  mean_beta_cases = rep_vector(init_cases[1], t);
  mean_delta_cases = rep_vector(init_cases[2], t);

  // log cases combined with growth
  mean_beta_cases[2:t] = mean_beta_cases[2:t] + cumulative_sum(beta_r);
  mean_delta_cases[2:t] = mean_delta_cases[2:t] + cumulative_sum(delta_r);

  // natural scale cases
  mean_beta_cases = exp(mean_beta_cases);
  mean_delta_cases = exp(mean_delta_cases);
  mean_cases = mean_beta_cases + mean_delta_cases;

  // rescale observation model
  phi = 1 ./ sqrt(sqrt_phi);
  
  // calculate fraction delta (to log for stability);
  frac_delta = exp(log(mean_delta_cases) - log(mean_cases));
}

model {
  // initial log cases
  init_cases ~ normal(mean_init_cases, sd_init_cases);

  // growth priors
  beta ~ normal(0, 0.25);
  delta_mod ~ normal(0.2, 0.2);
  beta_noise ~ normal(0, 0.1) T[0,];
  delta_noise ~ normal(0, 0.1) T[0,]; 

  // random walk priors
  eta ~ std_normal();
  delta_eta ~ std_normal();

  // observation model priors
  for (i in 1:2) {
    sqrt_phi[i] ~ normal(0, 1) T[0,];
  }
  // observation model 
  X ~ neg_binomial_2(mean_cases[1:t_nots], phi[1]);
  Y ~ beta_binomial(N, frac_delta[1:t_seq] * phi[2], 
                    (1 - frac_delta[1:t_seq]) * phi[2]);
}

generated quantities {
  int sim_delta_cases[t];
  int sim_beta_cases[t];
  int sim_cases[t];

  for (i in 1:t) {
    sim_beta_cases[i] = neg_binomial_2_rng(mean_beta_cases[i], phi[1]);
    sim_delta_cases[i] = neg_binomial_2_rng(mean_delta_cases[i], phi[1]);
    sim_cases[i] = sim_beta_cases[i] + sim_delta_cases[i];
  }
}