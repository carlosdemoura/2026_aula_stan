functions {
  int star(real pi1, real pi2) {
    real c;
    c = 0.01;
    return (
      pi1 > 0 &&
      pi1 < 1 &&
      pi2 > 0 &&
      pi2 < 1 &&
      pi2 < c / pi1
    );
  }
}
data {
  int<lower=1> n_row;
  int<lower=1> n_col;
  int<lower=1> n_fac;
  matrix[n_row, n_col] X;
  int<lower=1> n_alpha_free;
  real<lower=0> sigma2_shape;
  real<lower=0> sigma2_rate;
  int<lower=0> alpha_status[n_row, n_fac];
}
parameters {
  vector[n_alpha_free] alpha_;
  matrix<lower=0,upper=1>[30,2] Pi_;
  matrix[2, n_col] lambda;
  matrix<lower=0>[n_row,1] sigma2;
}
transformed parameters {
  matrix[n_row, n_fac] alpha;
  matrix[n_row, n_fac] Pi;
  for (i in 1:n_row) { for (k in 1:n_fac) {
    if (alpha_status[i,k] == 0) {
      alpha[i,k] = 0;
    } else {
      alpha[i,k] = alpha_[alpha_status[i,k]];
    }
  }}
  for (i in 1:n_row) { for (k in 1:n_fac) {
    if (i <= 60) {
      Pi[i,k] = 1;
    } else {
      Pi[i,k] = Pi_[i-60,k];
    }
  }}
}
model {
  matrix[n_row, n_col] alpha_lambda_pi;
  alpha_lambda_pi = (Pi .* alpha) * lambda;
  // Likelihood
  for(i in 1:n_row){ for (j in 1:n_col) {
    X[i,j] ~ normal( alpha_lambda_pi[i,j], sqrt(sigma2[i,1]) );
  }}
  // Priors
  for(i in 1:n_alpha_free) {
    alpha_[i] ~ normal(0,10);
  }
  for(i in 1:30){
    if (!star(Pi_[i,1], Pi_[i,2])) target += negative_infinity();
  }
  for(i in 1:n_row){
    sigma2[i,1] ~ gamma(sigma2_shape, sigma2_rate);
  }
  for (k in 1:n_fac) { for(j in 1:n_col){ 
    lambda[k,j] ~ normal(0, 1);
  }}
}
