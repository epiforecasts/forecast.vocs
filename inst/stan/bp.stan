data {
  int t;
  int t_nots;
  int X[t_nots];
  int likelihood;
  int output_loglik;
  int overdisp;
}

transformed data {
  // initialise cases using observed data
  real mean_init_cases;
  real sd_init_cases;
  mean_init_cases = X[1];
  mean_init_cases = log(mean_init_cases);
  sd_init_cases = 0.01;
}

parameters {
  real r_init;
  real<lower = 0> r_noise;
  real<lower = -1, upper = 1> beta;
  vector[t - 2] eta;
  real init_cases;
  real<lower = 0> sqrt_phi[overdisp ? 1 : 0];
}

transformed parameters {
  vector[t - 2] diff;
  vector[t - 1] r;
  vector<lower = 0>[t] mean_cases;
  real phi[overdisp ? 1 : 0];

  diff = rep_vector(0, t - 2);
  for (i in 1:(t-2)) {
    if (i > 1) {
      diff[i] = beta * diff[i - 1];
    }
    diff[i] += r_noise * eta[i];
  }
  r = rep_vector(r_init, t - 1);
  r[2:(t-1)] = r[2:(t-1)] + cumulative_sum(diff);

  // initialise log mean cases
  mean_cases = rep_vector(init_cases, t);
  // Combine with growth on the log scale
  mean_cases[2:t] = mean_cases[2:t] + cumulative_sum(r);
  // convert to natural scale (with small numeric stabiliser)
  mean_cases = exp(mean_cases) + rep_vector(1e-4, t);

  // rescale observation model
  if (overdisp) {
    phi[1] = 1 ./ sqrt(sqrt_phi[1]);
  }

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
    print(diff);
    print(r);
  }
  }
}

model {
  // initial cases
  init_cases ~ normal(mean_init_cases, sd_init_cases);

  // growth priors
  r_init ~ normal(0, 0.25);
  r_noise ~ normal(0, 0.2) T[0,];
  
  // random walk priors
  beta ~ std_normal();
  eta ~ std_normal();

  // observation model priors
  if (overdisp) {
    sqrt_phi[1] ~ std_normal() T[0,];
  }

  // observation model 
  if (likelihood) {
    if (overdisp){
      X ~ neg_binomial_2(mean_cases[1:t_nots], phi[1]);
    }else{
      X ~ poisson(mean_cases[1:t_nots]);
    }
  }
}

generated quantities {
  int sim_cases[t];
  vector[output_loglik ? t_nots : 0] log_lik;

  for (i in 1:t) {
    if (overdisp) {
      sim_cases[i] = neg_binomial_2_rng(mean_cases[i], phi[1]);
    }else{
      sim_cases[i] = poisson_rng(mean_cases[i]);
    }
  }

  if (output_loglik) {
    for (i in 1:t_nots) {
      if (overdisp) {
        log_lik[i] = neg_binomial_2_lpmf(X[i] | mean_cases[i], phi[1]);
      }else{
        log_lik[i] = poisson_lpmf(X[i] | mean_cases[i]);
      }
    }
  }
}
