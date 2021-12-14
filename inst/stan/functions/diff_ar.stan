vector diff_ar(real beta, vector eta, int[] eta_loc, int t) {
  vector[t] diff = rep_vector(0, t);
  int loc = 0;
  for (i in 1:t){
    if (eta_loc[i] > 0) {
      loc += 1;
      if (i > 1) {
        diff[i] = beta * diff[i - 1];
      }
      diff[i] += r_noise * eta[loc];
    }
  }
  return(diff);
}