vector periodic_adjustment(vector cases, int[] period, vector effect) {
  int t = num_elements(cases);
  int pl = num_elements(effect);
  vector[t] scaled_cases;
  for (s in 1:t) {
    scaled_cases[s] = cases[s] * effect[period[s]];
   }
  return(scaled_cases);
}
