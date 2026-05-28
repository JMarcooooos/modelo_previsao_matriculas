library(cmdstanr)

if (is.null(cmdstan_version(error_on_NA = FALSE))) {
  install_cmdstan(cores = 2)
}

dat6 <- readRDS("dat6.rds")

modelo_dinamico <- cmdstan_model(
  "model_dinamico.stan"
)

fit_modelo_dinamico <- modelo_dinamico$sample(
  data = dat6,
  iter_warmup = 1000,
  iter_sampling = 1000,
  chains = 4,
  parallel_chains = 4
)

fit_modelo_dinamico$save_object("fit_modelo_dinamico.rds", compress = "xz")
