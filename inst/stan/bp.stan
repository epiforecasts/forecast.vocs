data {
  int t;
  int t_nots;
  int X[t_nots];
  int likelihood;
  int output_loglik;
}

transformed data {
  // initialise cases using observed data
  real mean_init_cases;
  real sd_init_cases;
  mean_init_cases = X[1];
  mean_init_cases = log(mean_init_cases);
  sd_init_cases = 0.025 * mean_init_cases;
}

parameters {
  real r_init;
  real<lower = 0> r_noise;
  real<lower = -1, upper = 1> beta;
  vector[t-2] eta;
  real init_cases;
  real<lower = 0> sqrt_phi;
}

transformed parameters {
  vector[t - 1] r;
  vector[t - 2] diff;
  vector<lower = 0>[t] mean_cases;
  real phi;

  // growth rate
  for (i in 1:(t-2)) {
    if (i > 1) {
      diff[i] = beta * diff[i -1];
    }else{
      diff[i] = 0;
    }
    diff[i] += r_noise * eta[i];
  }
  r = rep_vector(r_init, t - 1);
  r[2:(t-1)] = r[2:(t-1)] + diff;

  // initialise log mean cases
  mean_cases = rep_vector(init_cases, t);

  // log cases combined with growth
  mean_cases[2:t] = mean_cases[2:t] + cumulative_sum(r);
  mean_cases = exp(mean_cases);

  // rescale observation model
  phi = 1 ./ sqrt(sqrt_phi);
}

model {
  // initial cases
  init_cases ~ normal(mean_init_cases, sd_init_cases);

  // growth priors
  r_init ~ normal(0, 0.25);
  r_noise ~ normal(0, 0.1) T[0,];
  
  // random walk priors
  eta ~ std_normal();
  beta ~ std_normal();

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
