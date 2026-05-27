mod_FA_pred = stan_model(model_name = "FA_pred", model_code = "
data {
  int<lower=1> n_row;
  int<lower=1> n_col;
  int<lower=1> n_fac;
  matrix[n_row, n_col] X;
  int<lower=1> n_alpha_free;
  real<lower=0> sigma_shape;
  real<lower=0> sigma_rate;
  int<lower=0> alpha_status[n_row, n_fac];
  int<lower=0> is_missing[n_row, n_col];
}
parameters {
  vector[n_alpha_free] alpha_;
  matrix[n_fac, n_col] lambda;
  matrix<lower=0>[n_row,1] sigma;
}
transformed parameters {
  matrix[n_row, n_fac] alpha;
  for (i in 1:n_row) { for (k in 1:n_fac) {
    if (alpha_status[i,k] == 0) {
      alpha[i,k] = 0;
    } else {
      alpha[i,k] = alpha_[alpha_status[i,k]];
    }
  }}
  matrix[n_row, n_col] alpha_lambda;
  alpha_lambda = alpha * lambda;
}
model {
  for(i in 1:n_row){ for (j in 1:n_col) {
    if (is_missing[i,j] == 0) {
      X[i,j] ~ normal( alpha_lambda[i,j], sigma[i,1] );
    }
  }}
  for(i in 1:n_alpha_free) {
    alpha_[i] ~ normal(0,10);
  }
  for (i in 1:n_fac) { for(j in 1:n_col){ 
    lambda[i,j] ~ normal(0,1);
  }}
  for(i in 1:n_row){
    sigma[i,1] ~ gamma(sigma_shape, sigma_rate);
  }
}
generated quantities {
  matrix[n_row, n_col] pred;
  for (i in 1:n_row) { for (j in 1:n_col) {
    if (is_missing[i,j] == 0) {
      pred[i,j] = 0;
    } else {
      pred[i,j] = normal_rng(alpha_lambda[i,j], sigma[i,1]);
    }
  }}
}

")

# saveRDS(mod_FA_pred, "stuff/mod_FA_pred.rds", compress = "xz")