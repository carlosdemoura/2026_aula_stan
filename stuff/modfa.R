library(rstan)

mod_FA = stan_model(model_name = "FA", model_code = "
data {
  int<lower=1> n_row;
  int<lower=1> n_col;
  int<lower=1> n_fac;
  matrix[n_row, n_col] X;
  int<lower=1> n_alpha_free;
  real<lower=0> sigma_shape;
  real<lower=0> sigma_rate;
  int<lower=0> alpha_status[n_row, n_fac];
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
}
model {
  matrix[n_row, n_col] alpha_lambda;
  alpha_lambda = alpha * lambda;
  for(i in 1:n_row){ for (j in 1:n_col) {
    X[i,j] ~ normal( alpha_lambda[i,j], sigma[i,1] );
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

")

saveRDS(mod_FA, "stuff/mod_FA.rds", compress = "xz")

mod_FA = readRDS("stuff/mod_FA.rds")

temperatura = read.csv(url("https://raw.githubusercontent.com/carlosdemoura/2026_aula_stan_dani/refs/heads/main/stuff/temperatura.csv"))

group.sizes =
  temperatura |>
  pull(regiao) |>
  table() |>
  unname()

n = sum(group.sizes)
n.fac = 3

group.limits =
  group.sizes |>
  {\(x) list(
    c(1, (cumsum(x) + 1)[1:(length(x)-1)])[1:length(x)],
    cumsum(x)
  )}()

contr = matrix(0, nrow = n, ncol = n.fac)
for (i in 1:n.fac) {
  contr[group.limits[[1]][i] : group.limits[[2]][i], i] = rep(1, group.sizes[i])
}

contr[contr!=0] = 1:sum(contr!=0)

X =
  temperatura |>
  select(starts_with("X"))

data = list(
  X = X,
  n_row = nrow(X),
  n_col = ncol(X),
  n_fac = 3,
  n_alpha_free = max(contr),
  sigma_shape = .1,
  sigma_rate  = .1,
  alpha_status = contr
)

init = list()
n_chains = 2
for (i in 1:n_chains) {
  init[[i]] = list(
    alpha_ = rep(10, data$n_alpha_free),
    lambda = matrix(0, data$n_fac, data$n_col),
    sigma  = matrix(1, data$n_row, 1)
  )
}

fit_FA = sampling(mod_FA, data = data, chains = n_chains, init = init)



post = rstan::extract(fit_FA, pars = c("alpha", "lambda", "sigma"))
resumo = list()

for ( parameter in names(post) ) {
  hpd_temp = apply( post[parameter][[1]], 2:3,
                    function(x) {
                      coda::HPDinterval(coda::as.mcmc(x))[,c("lower", "upper")] |>
                        as.numeric()
                    })
  
  resumo[[parameter]] = list(
    "media"   = apply( post[parameter][[1]], 2:3, mean          )                   ,
    "mediana" = apply( post[parameter][[1]], 2:3, stats::median )                   ,
    "hpd_min" = apply( hpd_temp, 2:3, function(x) { unlist(x) |> purrr::pluck(1) }) ,
    "hpd_max" = apply( hpd_temp, 2:3, function(x) { unlist(x) |> purrr::pluck(2) })
  )
}


resumo = sapply(resumo, function(x) { abind::abind(x, along = 3) })

plot(resumo$sigma[,1,"media"], type="b", ylab = "sigma", lwd = 2)

plot(resumo$lambda[1,,"media"], type="l", ylim = c(1.2,2), ylab = "escore", lwd = 2)
lines(resumo$lambda[2,,"media"], col = "red", lwd = 2)
lines(resumo$lambda[3,,"media"], col = "blue", lwd = 2, lty = 2)

resumo$alpha[,,"media"] |>
  as.data.frame() |>
  {\(.) `colnames<-`(., 1:ncol(.))}() |>
  {\(.) dplyr::mutate(., row = as.numeric(rownames(.))) }() |>
  tidyr::pivot_longer(cols = -row, names_to = "factor", values_to = "value") |>
  ggplot(aes(.data$factor, .data$row, fill = .data$value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  scale_y_reverse() +
  theme_minimal()
