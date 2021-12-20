functions {
#include functions/convolve.stan
#include functions/diff_ar.stan
#include functions/cases_ar.stan
#include functions/periodic_adjustment.stan
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
  real beta_mean;
  real beta_sd;
  real voc_mean;
  real voc_sd;
  real lkj_prior;
  int relat;
  int overdisp;
  int debug;
  int eta_n;
  int eta_loc[t - 2];  
  int voc_eta_n;
  int voc_eta_loc[relat ? t_seqf - 2 : 0];
  int period;
  int periodic[t];
}

transformed data {
  // initialise cases using observed data
  vector[1] mean_init_cases;
  vector[1] sd_init_cases;
  vector[1] mean_init_voc_cases;
  vector[1] sd_init_voc_cases;
  int nvoc_eta_n = eta_n - voc_eta_n;
  vector[1] gt = rep_vector(1, 1);
  vector[1] gt_voc = rep_vector(1, 1);
  vector[1] case_delay = rep_vector(1, 1);
  vector[1] seq_delay = rep_vector(1, 1);
  mean_init_cases = to_vector({X[1]});
  mean_init_cases = log(mean_init_cases);
  sd_init_cases = rep_vector(0.1, 1);
  mean_init_voc_cases = to_vector(
    {max({2.0, X[t_nseq + 1] * Y[1] * 1.0 / N[1]})}
  );
  mean_init_voc_cases = log(mean_init_voc_cases);
  sd_init_voc_cases = rep_vector(0.1, 1);
}

parameters {
  real<upper = 4> r_init;
  real<lower = 0> r_scale;
  real<lower = -1, upper = 1>  beta;
  vector[eta_n] eta;
  real voc_mod;
  real<lower = -1, upper = 1> voc_beta[relat == 1 ? 1 : 0];
  real<lower = 0> voc_scale[relat ? 1 : 0];
  cholesky_factor_corr[relat == 2 ? 2 : 0] L_Omega;  
  vector[voc_eta_n] voc_eta;
  vector[1] init_cases;
  vector[1] init_voc_cases;
  vector[period > 1 ? 1 : 0] period_sd;
  vector[period] period_eff;
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
  vector<lower = 0>[t] rep_by_case;
  vector<lower = 0, upper = 1>[t_seqf] frac_voc;
  vector[overdisp ? 2 : 0] phi;
  vector[eta_n] snvoc_eta;
  vector[relat ? voc_eta_n : 0] svoc_eta;
  // if variants are correlated set up correlation
  if (relat == 2) {
    matrix[2, 2] L;
    vector[2] t_eta;
    if (nvoc_eta_n) {
      snvoc_eta[1:nvoc_eta_n] = r_scale * head(eta, nvoc_eta_n);
    }
    L = diag_pre_multiply(to_vector({r_scale, voc_scale[1]}), L_Omega);
    for (i in 1:voc_eta_n) {
      t_eta = to_vector({eta[nvoc_eta_n + i], voc_eta[i]});
      t_eta = L * t_eta;
      snvoc_eta[nvoc_eta_n + i] = t_eta[1];
      svoc_eta[i] = t_eta[2];
    }
  }else{
    // If they aren't then residuals are independent or not present
    snvoc_eta = r_scale * eta;
    if (relat) {
      svoc_eta = voc_scale[1] * voc_eta;
    }
  }

  // differenced AR(1) growth rate
  diff = diff_ar(beta, snvoc_eta, eta_loc, t - 2);
  
  // non-voc evolves according to first diff AR(1)
  r = rep_vector(r_init, t - 1);
  r[2:(t-1)] = r[2:(t-1)] + diff;
  
  if (relat) {
    real vbeta;
    // Initial VOC growth based on non-voc + mod
    voc_r = rep_vector(r[t_nseq + 1], t_seqf - 1) + voc_mod;
    // VOC AR(1) differenced onwards variation
    if (relat == 1) {
      vbeta = voc_beta[1];
    }else{
      vbeta = beta;
    }
    voc_diff = diff_ar(vbeta, svoc_eta, voc_eta_loc, t_seqf - 2);
    voc_r[2:(t_seqf-1)] = voc_r[2:(t_seqf-1)] + voc_diff;
  }else{
    // voc growth rate scaled to non-voc
    voc_r = tail(r, t_seqf - 1) + voc_mod;
  }  

  // update case using initial cases, generation time and growth
  {
    vector[1] init_nvoc_cases;
    init_nvoc_cases[1] = t_nseq > 0 ? init_cases[1] : 
                                      init_cases[1]  - init_nvoc_cases[1];
    mean_nvoc_cases = cases_ar(init_nvoc_cases, gt, exp(r), t); 
  }
  mean_voc_cases = cases_ar(init_voc_cases, gt_voc, exp(voc_r), t_seqf); 

  // combine to overall cases
  mean_cases = mean_nvoc_cases;
  mean_cases[(t_nseq + 1):t] = mean_cases[(t_nseq + 1):t] + mean_voc_cases;
  rep_by_case = convolve(mean_cases, case_delay);
  rep_by_case = periodic_adjustment(rep_by_case, periodic, period_eff,
                                    period_sd);
  // rescale observation model
  if (overdisp) {
    phi = 1 ./ sqrt(sqrt_phi);
  }

  // calculate fraction voc by date of sequencing
  {
    vector[t] rep_by_seq = convolve(mean_cases, seq_delay);
    vector[t_seq] voc_by_seq = convolve(mean_voc_cases, seq_delay);
    frac_voc = voc_by_seq ./ rep_by_seq[(t_nseq + 1):t];
  }
  
  
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
  init_cases ~ lognormal(mean_init_cases, sd_init_cases);
  init_voc_cases ~ lognormal(mean_init_voc_cases, sd_init_voc_cases);

  // growth priors
  r_init ~ normal(r_init_mean, r_init_sd);
  voc_mod ~ normal(voc_mean, voc_sd);
  r_scale ~ normal(0, 0.2) T[0,];
  if (relat) {
    voc_scale[1] ~ normal(0, 0.2) T[0,];
  }

  // AR(1) priors for non-voc and voc
  beta ~ normal(beta_mean, beta_sd);
  eta ~ std_normal();
  if (relat) {
    voc_eta ~ std_normal();
  }
  if (relat == 1) {
    voc_beta ~ normal(beta_mean, beta_sd);
  }
  if (relat == 2) {
    L_Omega ~ lkj_corr_cholesky(lkj_prior);
  }

  // observation model priors
  if (overdisp) {
    for (i in 1:2) {
      sqrt_phi[i] ~ std_normal() T[0,];
    }
  }

  // periodic effect if any
  if (period > 1) {
    period_sd[1] ~ std_normal() T[0,];
    period_eff ~ std_normal();
  }

  // observation model 
  if (likelihood) {
    if (overdisp) {
      X ~ neg_binomial_2(rep_by_case[1:t_nots], phi[1]);
      Y ~ beta_binomial(N, frac_voc[1:t_seq] * phi[2], 
                       (1 - frac_voc[1:t_seq]) * phi[2]);
    }else{
      X ~ poisson(rep_by_case[1:t_nots]);
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
  vector[output_loglik ? max(t_nots, t_seq) : 0] log_lik;

  // summary measures
  voc_advantage = voc_r - r[(t_nseq+1):(t-1)];
  avg_voc_advantage = mean(voc_advantage);
  com_r = r;
  com_r[(t_nseq+1):(t-1)] = (1 - frac_voc[2:t_seqf]) .* r[(t_nseq+1):(t-1)] +
     frac_voc[2:t_seqf] .* voc_r;


  // simulate report cases by VOC status
  {
    vector[t_seqf] voc_by_case;
    vector[t] nvoc_by_case;

    nvoc_by_case = convolve(mean_nvoc_cases, case_delay);
    nvoc_by_case = periodic_adjustment(nvoc_by_case, periodic, period_eff,
                                       period_sd);
    voc_by_case = convolve(mean_voc_cases, case_delay);
    voc_by_case = periodic_adjustment(voc_by_case, periodic, period_eff,
                                      period_sd);
    
    // simulated cases
    for (i in 1:t) {
      if (overdisp) {
        sim_nvoc_cases[i] = neg_binomial_2_rng(nvoc_by_case[i], phi[1]);
      }else{
        sim_nvoc_cases[i] = poisson_rng(nvoc_by_case[i]);
      }
    }
    sim_cases = sim_nvoc_cases;
    for (i in 1:t_seqf) {
      if (overdisp) {
        sim_voc_cases[i] = neg_binomial_2_rng(voc_by_case[i], phi[1]);
      }else{
        sim_voc_cases[i] = poisson_rng(voc_by_case[i]);
      }
      sim_cases[t_nseq+i] += sim_voc_cases[i];
    }
  }
  // include log likelihood
  if (output_loglik) {
    log_lik = rep_vector(0, max(t_nots, t_seq));
    for (i in 1:t_nots) {
      if (overdisp) {
        log_lik[i] = neg_binomial_2_lpmf(X[i] | rep_by_case[i], phi[1]);
      }else{
        log_lik[i] = poisson_lpmf(X[i] | rep_by_case[i]);
      }
    }
    for (i in 1:t_seq) {
      if (overdisp) {
        log_lik[t_nseq + i] += beta_binomial_lpmf(Y[i] | N[i],
                                                  frac_voc[i] * phi[2], 
                                                  (1 - frac_voc[i]) * phi[2]);
      }else{
        log_lik[t_nseq + i] += binomial_lpmf(Y[i] | N[i], frac_voc[i]);
      }
    }
  }
}
