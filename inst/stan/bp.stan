data {
  int t;
  int t_nots;
  int X[t_nots];
  int likelihood;
  int output_loglik;
}

transformed data {
  vector[t] log_X;
  // initialise cases using observed data
  real mean_init_cases;
  real sd_init_cases;
  mean_init_cases = X[1];
  mean_init_cases = log(mean_init_cases);
  sd_init_cases = 0.01;
  for (i in 1:t) {
    log_X[i] = log(X[i]);
  }
}

parameters {
  real r_init;
  real<lower = 0> r_noise;
  vector[t - 2] eta;
  real init_cases;
  real<lower = 0> sqrt_phi;
}

transformed parameters {
  vector[t - 1] r;
  vector<lower = 0>[t] mean_cases;
  real phi;

  r = rep_vector(r_init, t - 1);
  r[2:(t-1)] = r[2:(t-1)] + cumulative_sum(r_noise * eta);

  // initialise log mean cases
  mean_cases[1] = init_cases;
  for (i in 1:(t-1)) {
    mean_cases[i + 1] = log_X[i] + r[i];
  }
  mean_cases = exp(mean_cases) + rep_vector(1e-4, t);

  // rescale observation model
  phi = 1 ./ sqrt(sqrt_phi);

  {
  int j = 0;
  for (i in 1:t) {
    j += is_inf(mean_cases[i]) ? 1 : 0;
  }
  if (j) {
    print(mean_cases);
    print(mean_init_cases);
    print(sd_init_cases);
    print(init_cases);
    print(r_init);
    print(r);
  }
  }
}

model {
  // initial cases
  init_cases ~ normal(mean_init_cases, sd_init_cases);

  // growth priors
  r_init ~ normal(0.2, 0.01);
  r_noise ~ normal(0, 1) T[0,];
  
  // random walk priors
  eta ~ std_normal();

  // observation model priors
  sqrt_phi ~ normal(0, 1) T[0,];

  // observation model 
  if (likelihood) {
    X ~ neg_binomial_2(mean_cases[1:t_nots], phi);
  }

}

generated quantities {
  int sim_cases[t];
  vector[output_loglik ? t_nots : 0] log_lik;

  for (i in 1:t) {
    sim_cases[i] = neg_binomial_2_rng(mean_cases[i], phi);
  }

  if (output_loglik) {
    for (i in 1:t_nots) {
      log_lik[i] = neg_binomial_2_lpmf(X[i] | mean_cases[i], phi);
    }
  }
}
