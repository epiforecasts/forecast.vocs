data {
  int t;
  int t_nseq;
  int t_seq;
  int t_seqf;
  int t_nots;
  int X[t_nots];
  int Y[t_seq];
  int N[t_seq];
  int likelihood;
  int output_loglik;
  real delta_mean;
  real delta_sd;
}

transformed data {
  // initialise cases using observed data
  vector[2] mean_init_cases;
  vector[2] sd_init_cases;
  mean_init_cases[2] = X[t_nseq + 1] * Y[1] / N[1];
  mean_init_cases[1] = X[1];
  mean_init_cases = log(mean_init_cases);
  sd_init_cases = 0.025 * mean_init_cases;
}

parameters {
  real r_init;
  real<lower = 0> r_noise;
  real<lower = -1, upper = 1> beta;
  real delta_mod;
  real<lower = 0> delta_noise;
  real<lower = 0> ndelta_noise;
  vector[t - 2] eta;
  vector[t_seqf - 2] delta_eta;
  vector[t_seqf - 2] ndelta_eta;
  vector[2] init_cases;
  vector<lower = 0>[2] sqrt_phi;
}

transformed parameters {
  vector[t - 1] r;
  vector[t - 2] diff;
  vector[t_seqf - 1] delta_r;
  vector[t] mean_ndelta_cases;
  vector[t_seqf] mean_delta_cases;
  vector<lower = 0>[t] mean_cases;
  vector<lower = 0, upper = 1>[t_seqf] frac_delta;
  vector[2] phi;

  // random walk growth rate
  for (i in 1:(t-2)) {
    if (i > 1) {
      diff[i] = beta * diff[i - 1];
    }else{
      diff[i] = 0;
    }
    diff[i] += r_noise * eta[i];
  }
  r = rep_vector(r_init, t - 1);
  r[2:(t-1)] = r[2:(t-1)] + cumulative_sum(diff);

  // delta growth rate scaled to overall with RW residuals
  delta_r = tail(r, t_seqf - 1) + delta_mod;
  delta_r[2:(t_seqf-1)] = tail(delta_r, t_seqf - 2) +
                             cumulative_sum(delta_noise * delta_eta);
  
  // non-delta growth rate based on overall with RW residuals
  r[(t_nseq+2):(t-1)] = tail(r, t_seqf - 2) +
                           cumulative_sum(ndelta_noise * ndelta_eta);
  
  // initialise log mean cases
  mean_ndelta_cases = t_nseq > 0 ? rep_vector(init_cases[1], t) : 
    rep_vector(log_sum_exp(init_cases[1], -init_cases[2]), t);
  mean_delta_cases = rep_vector(init_cases[2], t_seqf);

  // log cases combined with growth
  mean_ndelta_cases[2:t] = mean_ndelta_cases[2:t] + cumulative_sum(r);
  mean_delta_cases[2:t_seqf] = mean_delta_cases[2:t_seqf] +
                                 cumulative_sum(delta_r);

  // natural scale cases
  mean_ndelta_cases = exp(mean_ndelta_cases);
  mean_delta_cases = exp(mean_delta_cases);
  mean_cases = mean_ndelta_cases;
  mean_cases[(t_nseq + 1):t] = mean_cases[(t_nseq + 1):t] + mean_delta_cases;

  // rescale observation model
  phi = 1 ./ sqrt(sqrt_phi);
  
  // calculate fraction delta
  frac_delta = mean_delta_cases ./ mean_cases[(t_nseq + 1):t];
}

model {
  // initial cases
  init_cases ~ normal(mean_init_cases, sd_init_cases);

  // growth priors
  r_init ~ normal(0, 0.25);
  delta_mod ~ normal(delta_mean, delta_sd);
  r_noise ~ normal(0, 0.1) T[0,];
  delta_noise ~ normal(0, 0.1) T[0,]; 
  ndelta_noise ~ normal(0, 0.1) T[0,]; 

  // random walk priors
  beta ~ std_normal();
  eta ~ std_normal();
  delta_eta ~ std_normal();
  ndelta_eta ~ std_normal();

  // observation model priors
  for (i in 1:2) {
    sqrt_phi[i] ~ normal(0, 1) T[0,];
  }
  // observation model 
  if (likelihood) {
    X ~ neg_binomial_2(mean_cases[1:t_nots], phi[1]);
    Y ~ beta_binomial(N, frac_delta[1:t_seq] * phi[2], 
                      (1 - frac_delta[1:t_seq]) * phi[2]);
  }
}

generated quantities {
  real avg_delta_mod;
  vector[t - 1] com_r;
  int sim_delta_cases[t_seqf];
  int sim_ndelta_cases[t];
  int sim_cases[t];
  vector[output_loglik ? t_nots : 0] log_lik;

  // summary measures
  avg_delta_mod = mean(delta_r - r[(t_nseq+1):(t-1)]);
  com_r = r;
  com_r[(t_nseq+1):t] = (1 - frac_delta[2:t_seqf]) .* r[(t_nseq+1):t] +
     frac_delta[2:t_seqf] .* delta_r;

  // simulated cases
  for (i in 1:t) {
    sim_ndelta_cases[i] = neg_binomial_2_rng(mean_ndelta_cases[i], phi[1]);
  }
  sim_cases = sim_ndelta_cases;
  for (i in 1:t_seqf) {
    sim_delta_cases[i] = neg_binomial_2_rng(mean_delta_cases[i], phi[1]);
    sim_cases[t_nseq+i] = sim_cases[t_nseq+i] + sim_delta_cases[i];
  }
  // include log likelihood
  if (output_loglik) {
    for (i in 1:t_nots) {
      log_lik[i] = neg_binomial_2_lpmf(X[i] | mean_cases[i], phi[1]);
    }
    for (i in 1:t_seq) {
      log_lik[t_nseq + i] += beta_binomial_lpmf(Y[i] | N[i],
                                                frac_delta[i] * phi[2], 
                                                (1 - frac_delta[i]) * phi[2]);
    }
  }
}
