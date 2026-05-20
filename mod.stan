functions {
  // Removida a função star() — substituída por prior suave
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
  // Pi_ em escala logit — sem bounds, geometria livre para HMC
  matrix[30, 2] Pi_logit;
  matrix[2, n_col] lambda;
  // sigma direto (desvio padrão), mais estável que sigma2
  vector<lower=0>[n_row] sigma;
}
transformed parameters {
  matrix[n_row, n_fac] alpha;
  matrix[n_row, n_fac] Pi;
  matrix[30, 2] Pi_raw;

  // Reconstrução de alpha
  for (i in 1:n_row) {
    for (k in 1:n_fac) {
      alpha[i,k] = (alpha_status[i,k] == 0) ? 0.0 : alpha_[alpha_status[i,k]];
    }
  }

  // Pi_raw via inv_logit — mapeamento suave de R -> (0,1)
  Pi_raw = inv_logit(Pi_logit);

  // Monta Pi completo
  for (i in 1:n_row) {
    for (k in 1:n_fac) {
      Pi[i,k] = (i <= 60) ? 1.0 : Pi_raw[i-60, k];
    }
  }
}
model {
  matrix[n_row, n_col] mu;
  mu = (Pi .* alpha) * lambda;

  // Likelihood
  for (i in 1:n_row) {
    for (j in 1:n_col) {
      X[i,j] ~ normal(mu[i,j], sigma[i]);
    }
  }

  // Priors
  // alpha: mais regularizado para ajudar identificabilidade
  alpha_ ~ normal(0, 3);

  // sigma: Half-Normal — geometria melhor que Gamma na variância
  sigma ~ exponential(1.0 / sqrt(sigma2_shape / sigma2_rate));

  // lambda: mantido
  for (k in 1:n_fac) {
    lambda[k] ~ normal(0, 1);
  }

  // Pi_logit: prior Normal em escala logit
  // Equivale a concentrar Pi_raw em torno de 0.5,
  // com caudas penalizando valores próximos de 0 ou 1
  for (i in 1:30) {
    Pi_logit[i] ~ normal(0, 1.5);
  }

  // Restrição suave da condição star: penaliza pi1*pi2 > c
  // Substitui o target += negative_infinity() por penalidade suave
  {
    real c = 0.01;
    for (i in 1:30) {
      real p1 = Pi_raw[i, 1];
      real p2 = Pi_raw[i, 2];
      // Penaliza suavemente quando pi1*pi2 se aproxima de c
      target += -log1p(exp(20.0 * (p1 * p2 - c)));
    }
  }
}
