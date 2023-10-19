// generated with brms 2.18.0
functions {
  
    // prior from Michael Betancourt for ordered cutpoints
    real induced_dirichlet_lpdf(vector c, vector alpha, real phi) {
    int K = num_elements(c) + 1;
    vector[K - 1] sigma = inv_logit(phi - c);
    vector[K] p;
    matrix[K, K] J = rep_matrix(0, K, K);
    
    // Induced ordinal probabilities
    p[1] = 1 - sigma[1];
    for (k in 2:(K - 1))
      p[k] = sigma[k - 1] - sigma[k];
    p[K] = sigma[K - 1];
    
    // Baseline column of Jacobian
    for (k in 1:K) J[k, 1] = 1;
    
    // Diagonal entries of Jacobian
    for (k in 2:K) {
      real rho = sigma[k - 1] * (1 - sigma[k - 1]);
      J[k, k] = - rho;
      J[k - 1, k] = rho;
    }
    
    return   dirichlet_lpdf(p | alpha)
           + log_determinant(J);
  }
  
  real ord_beta_reg_lpdf(real y, real mu, real phi, real cutzero, real cutone) {
    vector[2] thresh;
    thresh[1] = cutzero;
    thresh[2] = cutzero + exp(cutone);
  if(y==0) {
      return log1m_inv_logit(mu - thresh[1]);
    } else if(y==1) {
      return log_inv_logit(mu  - thresh[2]);
    } else {
      return log_diff_exp(log_inv_logit(mu   - thresh[1]), log_inv_logit(mu - thresh[2])) +
                beta_lpdf(y|exp(log_inv_logit(mu) + log(phi)),exp(log1m_inv_logit(mu) + log(phi)));
    }
  }
  
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
                           vector meanme_1,
                           vector sdme_1,vector cutpoints, real kappa) {
    real ptarget = 0;
    int N = end - start + 1;
    // initialize linear predictor term
    vector[N] mu;
    mu = inv_logit(Intercept + bsp[1] * Xme_1[start:end] +
                      bsp[2] * Xme_2[start:end] +
                      bsp[3] * Xme_3[start:end] +
                      bsp[4] * Xme_4[start:end] +
                      bsp[5] * Xme_5[start:end] +
                      bsp[6] * Xme_6[start:end] +
                      Xc[start:end,1:cols(Xc)] * b);
    for (n in 1:N) {
      // add more terms to the linear predictor
      ptarget += beta_proportion_lpdf(Y[start + n - 1] | mu[n], kappa);
    }
    //ptarget += normal_id_glm_lpdf(Y[start:end] | Xc[start:end], mu, b, sigma);
    
    ptarget += normal_lpdf(Xn_1[start:end] | Xme_1[start:end], noise_1[start:end]);
    ptarget += normal_lpdf(Xn_2[start:end] | Xme_2[start:end], noise_2[start:end]);
    ptarget += normal_lpdf(Xn_3[start:end] | Xme_3[start:end], noise_3[start:end]);
    ptarget += normal_lpdf(Xn_4[start:end] | Xme_4[start:end], noise_4[start:end]);
    ptarget += normal_lpdf(Xn_5[start:end] | Xme_5[start:end], noise_5[start:end]);
    ptarget += normal_lpdf(Xn_6[start:end] | Xme_6[start:end], noise_6[start:end]);
    ptarget += normal_lpdf(Xme_1[start:end] | meanme_1[1], sdme_1[1]);
    ptarget += normal_lpdf(Xme_2[start:end] | meanme_1[2], sdme_1[2]);
    ptarget += normal_lpdf(Xme_3[start:end] | meanme_1[3], sdme_1[3]);
    ptarget += normal_lpdf(Xme_4[start:end] | meanme_1[4], sdme_1[4]);
    ptarget += normal_lpdf(Xme_5[start:end] | meanme_1[5], sdme_1[5]);
    ptarget += normal_lpdf(Xme_6[start:end] | meanme_1[6], sdme_1[6]);
    
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
  int<lower=1> NCme_1;  // number of latent correlations
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
    // using separate vectors increases efficiency
  vector[N] Xme_1;
  // using separate vectors increases efficiency
  vector[N] Xme_2;
  // using separate vectors increases efficiency
  vector[N] Xme_3;
  // using separate vectors increases efficiency
  vector[N] Xme_4;
  // using separate vectors increases efficiency
  vector[N] Xme_5;
  // using separate vectors increases efficiency
  vector[N] Xme_6;

  //matrix[Mme_1, N] zme_1;  // standardized latent values
  //cholesky_factor_corr[Mme_1] Lme_1;  // cholesky factor of the latent correlation matrix
  corr_matrix[Mme_1] Omega;        // prior correlation
  //vector[N] zme_1;  // standardized latent values
  //vector[N] zme_2;  // standardized latent values
  //vector[N] zme_3;  // standardized latent values
  //vector[N] zme_4;  // standardized latent values
  //vector[N] zme_5;  // standardized latent values
  //vector[N] zme_6;  // standardized latent values
  ordered[2] cutpoints; // cutpoints on ordered (latent) variable (also stand in as intercepts)
  real<lower=0> kappa; // scale parameter for beta regression
}
transformed parameters {
  // compute actual latent values
  //Xme_1 = meanme_1[1] + sdme_1[1] * zme_1;
  // compute actual latent values
  //Xme_2 = meanme_1[2] + sdme_1[2] * zme_2;
  // compute actual latent values
  //Xme_3 = meanme_1[3] + sdme_1[3] * zme_3;
  // compute actual latent values
  //Xme_4 = meanme_1[4] + sdme_1[4] * zme_4;
  // compute actual latent values
  //Xme_5 = meanme_1[5] + sdme_1[5] * zme_5;
  // compute actual latent values
  //Xme_6 = meanme_1[6] + sdme_1[6] * zme_6;
  
    //matrix[N, Mme_1] Xme1;  // actual latent values
  
    real lprior = 0;  // prior contributions to the log posterior

  // compute actual latent values
  //Xme1 = rep_matrix(transpose(meanme_1), N) + transpose(diag_pre_multiply(sdme_1, Lme_1) * zme_1);
  //Xme_1 = Xme1[, 1];
  //Xme_2 = Xme1[, 2];
  //Xme_3 = Xme1[, 3];
  //Xme_4 = Xme1[, 4];
  //Xme_5 = Xme1[, 5];
  //Xme_6 = Xme1[, 6];
  
  lprior += normal_lpdf(b | 0, 5);
  lprior += student_t_lpdf(Intercept | 3, 0.5, 2.5);
  lprior += normal_lpdf(bsp | 0, 5);
  lprior += student_t_lpdf(sigma | 3, 0, 2.5)
    - 1 * student_t_lccdf(0 | 3, 0, 2.5);
  lprior += exponential_lpdf(sdme_1 | 1);
  //lprior += lkj_corr_cholesky_lpdf(Lme_1 | 4);
}
model {
  // likelihood including constants
  if (!prior_only) {
    target += reduce_sum(partial_log_lik_lpmf, seq, grainsize, Y, Xc, b, 
                        Intercept, bsp, sigma, 
                        Xme_1, Xme_2, Xme_3, Xme_4, Xme_5, Xme_6,
                        Xn_1, Xn_2, Xn_3, Xn_4, Xn_5, Xn_6,
                        noise_1, noise_2, noise_3, noise_4, noise_5, noise_6,
                        meanme_1,sdme_1,cutpoints, kappa);
  }
  Omega ~ lkj_corr(2);
  meanme_1 ~ multi_normal(rep_vector(0,Mme_1), quad_form_diag(Omega, sdme_1));
  target += induced_dirichlet_lpdf(cutpoints | rep_vector(1, 3), 0);
  kappa ~ exponential(.1);
  // priors including constants
  target += lprior;
}
generated quantities {
  // actual population-level intercept
  real b_Intercept = Intercept - dot_product(means_X, b);
  // obtain latent correlation matrix
  //corr_matrix[Mme_1] Corme_1 = multiply_lower_tri_self_transpose(Lme_1);
  //vector<lower=-1,upper=1>[NCme_1] corme_1;
  // extract upper diagonal of correlation matrix
  //for (k in 1:Mme_1) {
  //  for (j in 1:(k - 1)) {
  //    corme_1[choose(k - 1, 2) + j] = Corme_1[j, k];
  //  }
  //}
}