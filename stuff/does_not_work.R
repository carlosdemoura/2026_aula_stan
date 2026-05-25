download_mod = function(url) {
  fl = tempfile()
  download.file(
    paste0("https://github.com/carlosdemoura/2026_aula_stan_dani/raw/refs/heads/main/stuff/", name, ".rds"),
    #"https://github.com/carlosdemoura/2026_aula_stan_dani/raw/refs/heads/main/stuff/mod_GLM.rds",
    destfile = fl,
    mode = "wb"
  )
  mod = readRDS(fl)
}
