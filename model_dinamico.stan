data {
  int<lower=1> N;
  array[N] int<lower=0> Y;
  array[N] int<lower=0> Y_anterior;
  array[N] int<lower=1> C_serie;

  int<lower=1> K;
  array[N] int<lower=1, upper=K> serie_id;

  int<lower=1> E;
  array[N] int<lower=1, upper=E> escola_id;

  int<lower=1> F;
  array[N] int<lower=1, upper=F> tipo_id;

  int<lower=1> C_cidades;
  array[E] int<lower=1, upper=C_cidades> escola_cidade;
}

parameters {
  vector[C_cidades] mu_cidade;
  real<lower=0> sigma_escola;
  vector[E] alpha_escola;
  vector[K] beta_serie;
  vector[F] gamma_tipo;

  real omega;
  vector<lower=0>[F] phi_tipo;
}

model {
  phi_tipo ~ gamma(2, 0.5);

  mu_cidade ~ normal(1.38, 0.5);
  sigma_escola ~ normal(0, 1);

  alpha_escola ~ normal(mu_cidade[escola_cidade], sigma_escola);
  beta_serie ~ normal(0, 0.3);
  gamma_tipo ~ normal(0, 0.3);

  omega ~ normal(1, 0.2);

  for (i in 1:N) {
    real eta = alpha_escola[escola_id[i]]
               + beta_serie[serie_id[i]]
               + gamma_tipo[tipo_id[i]]
               + omega * log(Y_anterior[i] + 1);

    real p = inv_logit(eta);

    real A = p * phi_tipo[tipo_id[i]] + 0.001;
    real B = (1 - p) * phi_tipo[tipo_id[i]] + 0.001;

    Y[i] ~ beta_binomial(C_serie[i], A, B);
  }
}

generated quantities {
  array[N] int y_rep;
  vector[N] logLik;
  vector[N] taxa_lotacao_prevista;

  for (i in 1:N) {
    real eta = alpha_escola[escola_id[i]]
               + beta_serie[serie_id[i]]
               + gamma_tipo[tipo_id[i]]
               + omega * log(Y_anterior[i] + 1);

    real p = inv_logit(eta);
    real A = p * phi_tipo[tipo_id[i]] + 0.001;
    real B = (1 - p) * phi_tipo[tipo_id[i]] + 0.001;

    y_rep[i] = beta_binomial_rng(C_serie[i], A, B);
    logLik[i] = beta_binomial_lpmf(Y[i] | C_serie[i], A, B);

    taxa_lotacao_prevista[i] = y_rep[i] * 1.0 / C_serie[i];
  }
}
