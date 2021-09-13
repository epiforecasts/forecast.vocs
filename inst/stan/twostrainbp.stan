data {
  int t;
  int t_nseq;
  int t_dep;
  int t_seq;
  int t_seqf;
  int t_nots;
  int X[t_nots];
  int Y[t_seq];
  int N[t_seq];
  int likelihood;
  int output_loglik;
  real r_init_mean;
  real r_init_sd;
  real voc_mean;
  real voc_sd;
  int relat;
  int overdisp;
  int debug;

}

transformed data {
  // initialise cases using observed data
  vector[2] mean_init_cases;
  vector[2] sd_init_cases;
  mean_init_cases[2] = max(2, X[t_nseq + 1] * Y[1] / N[1]);
  mean_init_cases[1] = X[1];
  mean_init_cases = log(mean_init_cases);
  sd_init_cases = rep_vector(0.1, 2);
}

parameters {
  real<upper = 4> r_init;
  real<lower = 0> r_noise;
  real<lower = -1, upper = 1> beta;
  real voc_mod;
  real<lower = 0> voc_noise[relat ? 1 : 0];
  real<lower = 0> nvoc_noise[relat ? 1 : 0];
  vector[t_dep] eta;
  vector[relat ? t_seqf - 2 : 0] voc_eta;
  vector[relat ? t_seqf - 2 : 0] nvoc_eta;
  vector[2] init_cases;
  vector<lower = 0>[overdisp ? 2 : 0] sqrt_phi;
}

transformed parameters {
  vector[t - 1] r;
  vector[t - 2] diff;
  vector[t_seqf - 1] voc_r;
  vector[t] mean_nvoc_cases;
  vector[t_seqf] mean_voc_cases;
  vector<lower = 0>[t] mean_cases;
  vector<lower = 0, upper = 1>[t_seqf] frac_voc;
  vector[overdisp ? 2 : 0] phi;

  // random walk growth rate
  diff = rep_vector(0, t - 2);
  for (i in 1:(t-2)) {
    if (i <= t_dep) {
      if (i > 1) {
        diff[i] = beta * diff[i - 1];
      }
      diff[i] += r_noise * eta[i];
    }
  }
  r = rep_vector(r_init, t - 1);
  r[2:(t-1)] = r[2:(t-1)] + cumulative_sum(diff);

  // voc growth rate scaled to overall
  voc_r = tail(r, t_seqf - 1) + voc_mod;
  // Independent RW residuals for variant and non-variant
  if (relat) {
    voc_r[2:(t_seqf-1)] = tail(voc_r, t_seqf - 2) +
                             cumulative_sum(voc_noise[1] * voc_eta);
    r[(t_nseq+2):(t-1)] = tail(r, t_seqf - 2) +
                            cumulative_sum(nvoc_noise[1] * nvoc_eta);
  }
  // initialise log mean cases
  mean_nvoc_cases = t_nseq > 0 ? rep_vector(init_cases[1], t) : 
    rep_vector(log_sum_exp(init_cases[1], -init_cases[2]), t);
  mean_voc_cases = rep_vector(init_cases[2], t_seqf);

  // log cases combined with growth
  mean_nvoc_cases[2:t] = mean_nvoc_cases[2:t] + cumulative_sum(r);
  mean_voc_cases[2:t_seqf] = mean_voc_cases[2:t_seqf] +
                                 cumulative_sum(voc_r);

  // natural scale cases (add a small numeric shift to avoid at 0 errors)
  mean_nvoc_cases = exp(mean_nvoc_cases) + rep_vector(1e-4, t);
  mean_voc_cases = exp(mean_voc_cases) + rep_vector(1e-4, t_seqf);

  // combine to overall cases
  mean_cases = mean_nvoc_cases;
  mean_cases[(t_nseq + 1):t] = mean_cases[(t_nseq + 1):t] + mean_voc_cases;

  // rescale observation model
  if (overdisp) {
    phi = 1 ./ sqrt(sqrt_phi);
  }

  // calculate fraction voc
  frac_voc = mean_voc_cases ./ mean_cases[(t_nseq + 1):t];
  
  //Debug information
  if (debug) {
    int j = 0;
    for (i in 1:t_seqf) {
      j += is_nan(frac_voc[i]) ? 1 : 0;
    }
    if (j) {
      print(frac_voc);
      print(mean_voc_cases);
      print(mean_nvoc_cases);
      print(mean_cases);
      print(mean_init_cases);
      print(sd_init_cases);
      print(init_cases);
      print(r_init);
      print(diff);
      print(r);
      print(voc_mod);
    }
  }
}

model {
  // initial cases
  init_cases ~ normal(mean_init_cases, sd_init_cases);

  // growth priors
  r_init ~ normal(r_init_mean, r_init_sd);
  voc_mod ~ normal(voc_mean, voc_sd);
  r_noise ~ normal(0, 0.2) T[0,];
  if (relat) {
    voc_noise[1] ~ normal(0, 0.1) T[0,]; 
    nvoc_noise[1] ~ normal(0, 0.1) T[0,]; 
  }

  // random walk priors
  beta ~ std_normal();
  eta ~ std_normal();
  if (relat) {
    voc_eta ~ std_normal();
    nvoc_eta ~ std_normal();
  }

  // observation model priors
  if (overdisp) {
    for (i in 1:2) {
      sqrt_phi[i] ~ std_normal() T[0,];
    }
  }

  // observation model 
  if (likelihood) {
    if (overdisp) {
      X ~ neg_binomial_2(mean_cases[1:t_nots], phi[1]);
      Y ~ beta_binomial(N, frac_voc[1:t_seq] * phi[2], 
                       (1 - frac_voc[1:t_seq]) * phi[2]);
    }else{
      X ~ poisson(mean_cases[1:t_nots]);
      Y ~ binomial(N, frac_voc[1:t_seq]);
    }
  }
}

generated quantities {
  real avg_voc_mod;
  vector[t - 1] com_r;
  int sim_voc_cases[t_seqf];
  int sim_nvoc_cases[t];
  int sim_cases[t];
  vector[output_loglik ? t_nots : 0] log_lik;

  // summary measures
  avg_voc_mod = mean(voc_r - r[(t_nseq+1):(t-1)]);
  com_r = r;
  com_r[(t_nseq+1):(t-1)] = (1 - frac_voc[2:t_seqf]) .* r[(t_nseq+1):(t-1)] +
     frac_voc[2:t_seqf] .* voc_r;

  // simulated cases
  for (i in 1:t) {
    if (overdisp) {
      sim_nvoc_cases[i] = neg_binomial_2_rng(mean_nvoc_cases[i], phi[1]);
    }else{
      sim_nvoc_cases[i] = poisson_rng(mean_nvoc_cases[i]);
    }
  }
  sim_cases = sim_nvoc_cases;
  for (i in 1:t_seqf) {
    if (overdisp) {
      sim_voc_cases[i] = neg_binomial_2_rng(mean_voc_cases[i], phi[1]);
    }else{
      sim_voc_cases[i] = poisson_rng(mean_voc_cases[i]);
    }
    sim_cases[t_nseq+i] += sim_voc_cases[i];
  }
  // include log likelihood
  if (output_loglik) {
    for (i in 1:t_nots) {
      if (overdisp) {
        log_lik[i] = neg_binomial_2_lpmf(X[i] | mean_cases[i], phi[1]);
      }else{
        log_lik[i] = poisson_lpmf(X[i] | mean_cases[i]);
      }
    }
    for (i in 1:t_seq) {
      if (overdisp) {
        log_lik[t_nseq + i] += beta_binomial_lpmf(Y[i] | N[i],
                                                  frac_voc[i] * phi[1], 
                                                  (1 - frac_voc[i]) * phi[1]);
      }else{
        log_lik[t_nseq + i] += binomial_lpmf(Y[i] | N[i], frac_voc[i]);
      }
    }
  }
}
