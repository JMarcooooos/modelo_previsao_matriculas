data {
  int<lower=1> N;
  array[N] int<lower=0> Y;
  array[N] int<lower=0> Y_anterior;

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

  // A trava de segurança de 0.01 mantida para evitar o erro de 'inf' inicial
  vector<lower=0.01>[F] phi_tipo;
}

model {
  phi_tipo ~ gamma(2, 0.5);

  // O prior mudou: agora estamos na escala LOG (exp(3.5) = ~33 alunos base)
  mu_cidade ~ normal(3.5, 1);
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

    // Transformação Logarítmica para a média esperada de alunos
    real mu = exp(eta);

    // Likelihood de contagem infinita e sobredispersa
    Y[i] ~ neg_binomial_2(mu, phi_tipo[tipo_id[i]]);
  }
}

generated quantities {
  array[N] int y_rep;
  vector[N] logLik;

  for (i in 1:N) {
    real eta = alpha_escola[escola_id[i]]
               + beta_serie[serie_id[i]]
               + gamma_tipo[tipo_id[i]]
               + omega * log(Y_anterior[i] + 1);

    real mu = exp(eta);

    // Geração das simulações da demanda livre
    y_rep[i] = neg_binomial_2_rng(mu, phi_tipo[tipo_id[i]]);
    logLik[i] = neg_binomial_2_lpmf(Y[i] | mu, phi_tipo[tipo_id[i]]);
  }
}
