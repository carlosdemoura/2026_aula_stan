group_limits = function(x) {
  list(
    c(1, (cumsum(x) + 1)[1:(length(x)-1)])[1:length(x)],
    cumsum(x)
  )
}

group.sizes = rep(30,3)
n = sum(group.sizes)
n.fac = 2
columns = 30
semi.conf = T

group_limits = group_limits(group.sizes)
contr = matrix(0, nrow = n, ncol = n.fac)

set.seed(12345)
for (i in 1:n.fac) {
  contr[group_limits[[1]][i] : group_limits[[2]][i], i] = rep(1, group.sizes[i])
}
if (semi.conf) {
  i = i + 1
  contr[group_limits[[1]][i] : group_limits[[2]][i], ] =
    replicate(
      group.sizes[i],
      sample(c(1,0), 2)
    ) |>
    t()
}

alpha = runif(prod(dim(contr)), -20, 20) |> matrix(ncol = n.fac)
alpha = contr * alpha
#heatmap(alpha, Rowv = NA, Colv = NA, revC = TRUE)

lambda = matrix(1, nrow = 2, ncol = columns)
for (k in 1:2) { for (j in 2:columns) {
  lambda[k,j] = rnorm(1, lambda[k,j-1], .1)
}}

sigma2 =
  rgamma(n, 1, 1) |>
  matrix(ncol = n) |>
  t()

epsilon = matrix(0, nrow = n, ncol = columns)

for (i in 1:n) {
  epsilon[i,] = rnorm(columns, 0, sqrt(sigma2[i,]))
}

X = alpha %*% lambda + epsilon

Pi_status = c(rep(0,sum(group.sizes[1:2])), rep(1,group.sizes[3]))
alpha_status = contr
alpha_status[group_limits[[1]][3] : group_limits[[2]][3],] = 1
alpha_status[alpha_status!=0] = 1:sum(alpha_status!=0)

data = list(
  X = X,
  n_row = nrow(X),
  n_col = ncol(X),
  n_fac = 2,
  n_alpha_free = sum(group.sizes[1:2]) + 2*group.sizes[3],
  sigma2_shape = .1,
  sigma2_rate  = .1,
  alpha_status = alpha_status
)

chains = 2

init = list()

for (i in 1:chains) {
  init[[i]] = list(
    Pi_ = matrix(.01, nrow=30, ncol=2),
    alpha_ = rep(10, data$n_alpha_free),
    lambda = matrix(1,data$n_fac,data$n_col),
    sigma2 = matrix(1,data$n_row,1)
  )
}

out = rstan::stan(
  file   = "mod.stan",
  #file   = "mod_fa_semiconf.stan",
  data   = data,
  pars   = c("alpha", "lambda", "sigma", "Pi"),
  init   = init,
  chains = chains,
  iter = 10000,
  seed = 12345
  )

# rstan::traceplot(out, par = c("Pi[61,1]","Pi[61,2]"))