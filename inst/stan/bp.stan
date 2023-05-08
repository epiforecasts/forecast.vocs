functions {
#include functions/convolve.stan
#include functions/diff_ar.stan
#include functions/cases_ar.stan
#include functions/periodic_adjustment.stan
}

data {
  int t;
  int t_nots;
  array[t_nots] int X;
  real r_init_mean;
  real r_init_sd;
  int likelihood;
  int output_loglik;
  int overdisp;
  int debug;
  int eta_n;
  array[t - 2] int eta_loc;
  real beta_mean;
  real beta_sd;
  int period;
  array[t] int periodic;
}

transformed data {
  // initialise cases using observed data
  real mean_init_cases;
  real sd_init_cases;
  vector[1] gt = rep_vector(1, 1);
  vector[1] case_delay = rep_vector(1, 1);
  mean_init_cases = log(X[1]);
  sd_init_cases = 0.1;
}

parameters {
  real r_init;
  real<lower = 0> r_scale;
  real<lower = -1, upper = 1> beta;
  vector[eta_n] eta;
  vector[1] init_cases;
  vector[period > 1 ? 1 : 0] period_sd;
  vector[period > 1 ? period : 0] period_eff;
  array[overdisp ? 1 : 0] real<lower = 0> sqrt_phi;
}

transformed parameters {
  vector[t - 2] diff;
  vector[t - 1] r;
  vector<lower = 0>[t] mean_cases;
  vector<lower = 0>[t] rep_by_case;
  array[overdisp ? 1 : 0] real phi;

  diff = diff_ar(beta, r_scale * eta, eta_loc, t - 2);
  r = rep_vector(r_init, t - 1);
  r[2:(t-1)] = r[2:(t-1)] + diff;

  // update case using initial cases, generation time and growth
  mean_cases = cases_ar(init_cases, gt, exp(r), t); 
  rep_by_case = convolve(mean_cases, case_delay);
  rep_by_case = periodic_adjustment(rep_by_case, periodic, period_eff,
                                    period_sd);
  // rescale observation model
  if (overdisp) {
    phi[1] = 1 ./ sqrt(sqrt_phi[1]);
  }

  if (debug) {
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
  init_cases ~ lognormal(mean_init_cases, sd_init_cases);

  // growth priors
  r_init ~ normal(r_init_mean, r_init_sd);
  r_scale ~ normal(0, 0.2) T[0,];
  
  // random walk priors
  beta ~ normal(beta_mean, beta_sd);
  eta ~ std_normal();

  // observation model priors
  if (overdisp) {
    sqrt_phi[1] ~ std_normal() T[0,];
  }

  // periodic effect if any
  if (period > 1) {
    period_sd[1] ~ std_normal() T[0,];
    period_eff ~ std_normal();
  }

  // observation model 
  if (likelihood) {
    if (overdisp){
      X ~ neg_binomial_2(rep_by_case[1:t_nots], phi[1]);
    }else{
      X ~ poisson(rep_by_case[1:t_nots]);
    }
  }
}

generated quantities {
  array[t] int sim_cases;
  vector[output_loglik ? t_nots : 0] log_lik;

  for (i in 1:t) {
    if (overdisp) {
      sim_cases[i] = neg_binomial_2_rng(rep_by_case[i], phi[1]);
    }else{
      sim_cases[i] = poisson_rng(rep_by_case[i]);
    }
  }

  if (output_loglik) {
    for (i in 1:t_nots) {
      if (overdisp) {
        log_lik[i] = neg_binomial_2_lpmf(X[i] | rep_by_case[i], phi[1]);
      }else{
        log_lik[i] = poisson_lpmf(X[i] | rep_by_case[i]);
      }
    }
  }
}
