library(cmdstanr)

if (is.null(cmdstan_version(error_on_NA = FALSE))) {
  install_cmdstan(cores = 2)
}

dat <- readRDS("dat.rds")

modelo_evol <- cmdstan_model(
  "evol.stan"
)

fit_modelo_evol <- modelo_evol$sample(
  data = dat,
  iter_warmup = 1000,
  iter_sampling = 1000,
  chains = 4,
  parallel_chains = 4
)

fit_modelo_evol$save_object("fit_modelo_evol.rds", compress = "xz")
