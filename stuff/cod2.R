# Dados simulados
mu_real = 2.5
sigma = sigma2 = 1
n = 20
dados = rnorm(n, mean = mu_real)

mu0 = 0
tau02 = 1

# Parametros post
tau_post = 1 / (n / sigma2 + 1 / tau02)
mu_post = tau_n2 * (sum(dados) / sigma2 + mu0 / tau02)


plot(
  dnorm(seq(-4,4,length.out=1e4),
        mean = mu_post,
        sd = sqrt(tau_post)
  ),
  col = "red",
  type = "l",
  lwd = 2
)
