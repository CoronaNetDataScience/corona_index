// generated with brms 2.18.0
functions {
  /* integer sequence of values
   * Args:
   *   start: starting integer
   *   end: ending integer
   * Returns:
   *   an integer sequence from start to end
   */
  int[] sequence(int start, int end) {
    int seq[end - start + 1];
    for (n in 1:num_elements(seq)) {
      seq[n] = n + start - 1;
    }
    return seq;
  }
  // compute partial sums of the log-likelihood
  real partial_log_lik_lpmf(int[] seq, int start, int end, 
                          data vector Y, data matrix Xc, vector b, real Intercept, 
                          vector bsp, real sigma, vector Xme_1, vector Xme_2, 
                          vector Xme_3, vector Xme_4, vector Xme_5, vector Xme_6,
                          vector Xn_1, vector Xn_2, 
                          vector Xn_3, vector Xn_4, vector Xn_5, vector Xn_6,
                          vector noise_1, vector noise_2, 
                          vector noise_3, vector noise_4, vector noise_5, vector noise_6,
                          vector zme_1, vector zme_2, 
                          vector zme_3, vector zme_4, vector zme_5, vector zme_6) {
    real ptarget = 0;
    int N = end - start + 1;
    // initialize linear predictor term
    vector[N] mu = rep_vector(0.0, N);
    mu += Intercept;
    for (n in 1:N) {
      // add more terms to the linear predictor
      int nn = n + start - 1;
      mu[n] += (bsp[1]) * Xme_1[nn] + (bsp[2]) * Xme_2[nn] + (bsp[3]) * Xme_3[nn] + (bsp[4]) * Xme_4[nn] + (bsp[5]) * Xme_5[nn] + (bsp[6]) * Xme_6[nn];
    }
    ptarget += normal_id_glm_lpdf(Y[start:end] | Xc[start:end], mu, b, sigma);
    
    ptarget += normal_lpdf(Xn_1[start:end] | Xme_1[start:end], noise_1[start:end]);
    ptarget += normal_lpdf(Xn_2[start:end] | Xme_2[start:end], noise_2[start:end]);
    ptarget += normal_lpdf(Xn_3[start:end] | Xme_3[start:end], noise_3[start:end]);
    ptarget += normal_lpdf(Xn_4[start:end] | Xme_4[start:end], noise_4[start:end]);
    ptarget += normal_lpdf(Xn_5[start:end] | Xme_5[start:end], noise_5[start:end]);
    ptarget += normal_lpdf(Xn_6[start:end] | Xme_6[start:end], noise_6[start:end]);
    ptarget += std_normal_lpdf(zme_1[start:end]);
    ptarget += std_normal_lpdf(zme_2[start:end]);
    ptarget += std_normal_lpdf(zme_3[start:end]);
    ptarget += std_normal_lpdf(zme_4[start:end]);
    ptarget += std_normal_lpdf(zme_5[start:end]);
    ptarget += std_normal_lpdf(zme_6[start:end]);
    
    return ptarget;
  }
}
data {
  int<lower=1> N;  // total number of observations
  vector[N] Y;  // response variable
  int<lower=1> K;  // number of population-level effects
  matrix[N, K] X;  // population-level design matrix
  int<lower=1> Ksp;  // number of special effects terms
  int grainsize;  // grainsize for threading
  // data for noise-free variables
  int<lower=1> Mme_1;  // number of groups
  vector[N] Xn_1;  // noisy values
  vector<lower=0>[N] noise_1;  // measurement noise
  vector[N] Xn_2;  // noisy values
  vector<lower=0>[N] noise_2;  // measurement noise
  vector[N] Xn_3;  // noisy values
  vector<lower=0>[N] noise_3;  // measurement noise
  vector[N] Xn_4;  // noisy values
  vector<lower=0>[N] noise_4;  // measurement noise
  vector[N] Xn_5;  // noisy values
  vector<lower=0>[N] noise_5;  // measurement noise
  vector[N] Xn_6;  // noisy values
  vector<lower=0>[N] noise_6;  // measurement noise
  int prior_only;  // should the likelihood be ignored?
}
transformed data {
  int Kc = K - 1;
  matrix[N, Kc] Xc;  // centered version of X without an intercept
  vector[Kc] means_X;  // column means of X before centering
  int seq[N] = sequence(1, N);
  for (i in 2:K) {
    means_X[i - 1] = mean(X[, i]);
    Xc[, i - 1] = X[, i] - means_X[i - 1];
  }
}
parameters {
  vector[Kc] b;  // population-level effects
  real Intercept;  // temporary intercept for centered predictors
  vector[Ksp] bsp;  // special effects coefficients
  real<lower=0> sigma;  // dispersion parameter
  // parameters for noise free variables
  vector[Mme_1] meanme_1;  // latent means
  vector<lower=0>[Mme_1] sdme_1;  // latent SDs
  vector[N] zme_1;  // standardized latent values
  vector[N] zme_2;  // standardized latent values
  vector[N] zme_3;  // standardized latent values
  vector[N] zme_4;  // standardized latent values
  vector[N] zme_5;  // standardized latent values
  vector[N] zme_6;  // standardized latent values
}
transformed parameters {
  vector[N] Xme_1;  // actual latent values
  vector[N] Xme_2;  // actual latent values
  vector[N] Xme_3;  // actual latent values
  vector[N] Xme_4;  // actual latent values
  vector[N] Xme_5;  // actual latent values
  vector[N] Xme_6;  // actual latent values
  real lprior = 0;  // prior contributions to the log posterior
  // compute actual latent values
  Xme_1 = meanme_1[1] + sdme_1[1] * zme_1;
  // compute actual latent values
  Xme_2 = meanme_1[2] + sdme_1[2] * zme_2;
  // compute actual latent values
  Xme_3 = meanme_1[3] + sdme_1[3] * zme_3;
  // compute actual latent values
  Xme_4 = meanme_1[4] + sdme_1[4] * zme_4;
  // compute actual latent values
  Xme_5 = meanme_1[5] + sdme_1[5] * zme_5;
  // compute actual latent values
  Xme_6 = meanme_1[6] + sdme_1[6] * zme_6;
  lprior += normal_lpdf(b | 0, 5);
  lprior += student_t_lpdf(Intercept | 3, 0.5, 2.5);
  lprior += normal_lpdf(bsp | 0, 5);
  lprior += student_t_lpdf(sigma | 3, 0, 2.5)
    - 1 * student_t_lccdf(0 | 3, 0, 2.5);
  lprior += normal_lpdf(meanme_1 | 0, 1);
  lprior += exponential_lpdf(sdme_1 | 1);
}
model {
  // likelihood including constants
  if (!prior_only) {
    target += reduce_sum(partial_log_lik_lpmf, seq, grainsize, Y, Xc, b, 
                        Intercept, bsp, sigma, 
                        Xme_1, Xme_2, Xme_3, Xme_4, Xme_5, Xme_6,
                        Xn_1, Xn_2, Xn_3, Xn_4, Xn_5, Xn_6,
                        noise_1, noise_2, noise_3, noise_4, noise_5, noise_6,
                        zme_1, zme_2, zme_3, zme_4, zme_5, zme_6);
  }
  // priors including constants
  target += lprior;
}
generated quantities {
  // actual population-level intercept
  real b_Intercept = Intercept - dot_product(means_X, b);
}