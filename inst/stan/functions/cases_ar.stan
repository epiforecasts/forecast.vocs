vector cases_ar(vector init_cases, vector gt, vector R, int t) {
  int p = num_elements(init_cases);
  vector[t] cases = rep_vector(0, t);
  for (i in 1:p) {
    cases[i] = init_cases[p];
  }
  for (i in (p + 1):t) {
    cases[i] = R[i - p] * convolve_step(cases, gt, i - 1);
  }
  return(cases);
}
