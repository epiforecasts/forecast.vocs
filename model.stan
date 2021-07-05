data {
  int t;
  int t_seq;
  int t_nots;
  int X[t_nots];
  int Y[t_seq];
  int N[t_seq];
}
  
parameters {
  real beta;
  real beta_noise;
  real delta_mod;
  real delta_noise;
  vector[t-2] eta;
  vector[t-2] delta_eta;
  real <lower = 0> init_cases[2];
  vector<lower = 0>[2] sqrt_phi;
}

transformed parameters {
  vector[t - 1] beta_growth;
  vector[t - 1] delta_growth;
  vector[t] mean_beta_cases;
  vector[t] mean_delta_cases;
  vector[t] mean_cases;
  vector[t] frac_delta;
  vector[2] phi;

  // beta growth rate
  beta_growth = rep_vector(beta, t);
  beta_growth[2:(t-1)] = 
    beta_growth[2:(t-1)] + cumulative_sum(beta_noise * eta);

  // delta growth rate based on beta with AR residuals
  delta_growth = beta_growth + delta_mod;
  delta_growth[2:(t-1)] = 
    delta_growth[2:(t-1)] + cumulative_sum(delta_noise * delta_eta);

  // initialise log mean cases
  mean_beta_cases = rep_vector(init_cases[1], t);
  mean_delta_cases = rep_vector(init_cases[2], t);

  // cases growth
  mean_beta_cases[2:t] = mean_beta_cases[2:t] + beta_growth;
  mean_delta_cases[2:t] = mean_delta_cases[2:t] + delta_growth;
  
  //scale cases
  mean_beta_cases = exp(mean_beta_cases);
  mean_delta_cases = exp(mean_delta_cases);
  mean_cases = mean_delta_cases + mean_delta_cases;

  // rescale observation model
  phi = 1 ./ sqrt(sqrt_phi);
  
  //calculate fraction delta;
  frac_delta = mean_delta_cases ./ mean_cases;
}

model {
  // initial log cases
  init_cases ~ normal(5, 5);

  // growth priors
  beta_growth ~ std_normal();
  delta_mod ~ std_normal();
  beta_noise ~ std_normal();
  delta_noise ~ std_normal(); 

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

  sim_beta_cases = neg_binomial_rng(mean_beta_cases, phi[1]);
  sim_delta_cases = neg_binomial_rng(mean_delta_cases, phi[1]);
  for (i in 1:t) {
    sim_cases[i] = sim_beta_cases[i] + sim_delta_cases[i];
  }
}