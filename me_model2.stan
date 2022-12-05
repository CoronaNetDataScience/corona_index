// generated with brms 2.16.3
functions {
}
data {
  int<lower=1> N;  // total number of observations
  int Y[N];  // response variable
  int trials[N];  // number of trials
  int<lower=1> Ksp;  // number of special effects terms
  // data for noise-free variables
  int<lower=1> Mme_1;  // number of groups
  vector[N] Xn_1;  // noisy values
  vector<lower=0>[N] noise_1;  // measurement noise
  int prior_only;  // should the likelihood be ignored?
}
transformed data {
}
parameters {
  real Intercept;  // temporary intercept for centered predictors
  vector[Ksp] bsp;  // special effects coefficients
  // parameters for noise free variables
  vector[N] zme_1;  // standardized latent values
}
transformed parameters {
}
model {
  // likelihood including constants
  if (!prior_only) {
    // initialize linear predictor term
    vector[N] mu = Intercept + rep_vector(0.0, N);
    for (n in 1:N) {
      // add more terms to the linear predictor
      mu[n] += (bsp[1]) * zme_1[n];
    }
    target += binomial_logit_lpmf(Y | trials, mu);
  }
  // priors including constants
  target += normal_lpdf(Intercept | -10,5);
  target += normal_lpdf(zme_1|0,3);
  target += normal_lpdf(Xn_1  | zme_1, noise_1);
  target += normal_lpdf(bsp | 0, 10);
}
generated quantities {
  // actual population-level intercept
  real b_Intercept = Intercept;
}