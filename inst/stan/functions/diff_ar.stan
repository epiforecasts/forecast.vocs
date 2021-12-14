vector diff_ar(real beta, real eta_scale, vector eta, int[] eta_loc, int t) {
  vector[t] diff = rep_vector(0, t);
  int loc = 0;
  for (i in 1:t){
    if (eta_loc[i] > 0) {
      loc += 1;
      if (i > 1) {
        diff[i] = beta * diff[i - 1];
      }
      diff[i] += eta_scale * eta[loc];
    }
  }
  return(diff);
}