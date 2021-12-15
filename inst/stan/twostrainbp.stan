functions {
#include functions/convolve.stan
#include functions/diff_ar.stan
#include functions/cases_ar.stan
}

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
  real r_init_mean;
  real r_init_sd;
  real voc_mean;
  real voc_sd;
  int relat;
  int overdisp;
  int debug;
  int eta_n;
  int eta_loc[t - 2];  
  int voc_eta_n;
  int voc_eta_loc[relat ? t_seqf - 2 : 0];
}

transformed data {
  // initialise cases using observed data
  vector[2] mean_init_cases;
  vector[2] sd_init_cases;
  mean_init_cases = to_vector(
    {X[1], max({2.0, X[t_nseq + 1] * Y[1] * 1.0 / N[1]})}
  );
  mean_init_cases = log(mean_init_cases);
  sd_init_cases = rep_vector(0.1, 2);
}

parameters {
  real<upper = 4> r_init;
  real rn_mean[relat == 2 ? 1 : 0];
  real<lower = 0> rn_sd[relat == 2 ? 1 : 0];
  real<lower = 0> r_noise;
  real<lower = -1, upper = 1> beta_mean[relat == 2 ? 1 : 0];
  real<lower = 0> beta_sd[relat == 2 ? 1 : 0];
  real<lower = -1, upper = 1>  beta;
  vector[eta_n] eta;
  real voc_mod;
  real<lower = -1, upper = 1> voc_beta[relat ? 1 : 0];
  real<lower = 0> voc_noise[relat ? 1 : 0];
  real<lower = 0> eta_sd[relat == 2 ? 1 : 0];
  vector[voc_eta_n] voc_eta;
  vector[2] init_cases;
  vector<lower = 0>[overdisp ? 2 : 0] sqrt_phi;
}

transformed parameters {
  vector[t - 1] r;
  vector[t - 2] diff;
  vector[relat ? t_seqf - 2 : 0] voc_diff;
  vector[t_seqf - 1] voc_r;
  vector[t] mean_nvoc_cases;
  vector[t_seqf] mean_voc_cases;
  vector<lower = 0>[t] mean_cases;
  vector<lower = 0, upper = 1>[t_seqf] frac_voc;
  vector[overdisp ? 2 : 0] phi;

  // differenced AR(1) growth rate
  diff = diff_ar(beta, r_noise * eta, eta_loc, t - 2);
  diff = cumulative_sum(diff);

  // non-voc evolves according to first diff AR(1)
  r = rep_vector(r_init, t - 1);
  r[2:(t-1)] = r[2:(t-1)] + diff;
  
  if (relat) {
    // Initial VOC growth based on non-voc + mod
    voc_r = rep_vector(r[t_nseq + 1], t_seqf - 1) + voc_mod;
    // VOC AR(1) differened onwards variation
    voc_diff = diff_ar(voc_beta[1], voc_noise[1] * voc_eta, voc_eta_loc,
                       t_seqf - 2);
    voc_diff = cumulative_sum(voc_diff);
    voc_r[2:(t_seqf-1)] = voc_r[2:(t_seqf-1)] + voc_diff;
  }else{
    // voc growth rate scaled to non-voc
    voc_r = tail(r, t_seqf - 1) + voc_mod;
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
  if (relat < 2) {
    r_noise ~ normal(0, 0.2) T[0,];
    if (relat) {
      voc_noise[1] ~ normal(0, 0.2) T[0,];
    }
  }else{
    rn_mean[1] ~ normal(0, 0.2) T[0,];
    rn_sd[1] ~ normal(0, 0.1) T[0, ];
    r_noise ~ normal(rn_mean, rn_sd) T[0,];
    voc_noise[1] ~ normal(rn_mean, rn_sd) T[0,];
  }

  // AR(1) priors for non-voc and voc
  if (relat < 2) {
    beta ~ std_normal();
    eta ~ std_normal();
    if (relat) {
      voc_beta ~ std_normal();
      voc_eta ~ std_normal();
    }
  }else{
    beta_mean ~ std_normal();
    beta_sd[1] ~ std_normal() T[0,];
    beta ~ normal(beta_mean, beta_sd);
    voc_beta ~ normal(beta_mean, beta_sd);

    eta ~ std_normal(); 
    eta_sd[1] ~ std_normal() T[0,];
    voc_eta ~ normal(tail(eta, voc_eta_n), eta_sd[1]);
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
  vector[t_seqf - 1] voc_advantage;
  real avg_voc_advantage;
  vector[t - 1] com_r;
  int sim_voc_cases[t_seqf];
  int sim_nvoc_cases[t];
  int sim_cases[t];
  vector[output_loglik ? t_nots : 0] log_lik;

  // summary measures
  voc_advantage = voc_r - r[(t_nseq+1):(t-1)];
  avg_voc_advantage = mean(voc_advantage);
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
