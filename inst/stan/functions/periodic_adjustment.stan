vector periodic_adjustment(vector cases, int[] period, vector effect,
                           vector eff_sd) {
  int t = num_elements(cases);
  int pl = num_elements(effect);
  vector[t] scaled_cases;
  if (pl > 1) {
    vector[pl] scaled_eff = exp(eff_sd[1] * effect);
    for (s in 1:t) {
      scaled_cases[s] = cases[s] * scaled_eff[period[s]];
    }
  }else{
    scaled_cases = cases;
  }
  return(scaled_cases);
}
