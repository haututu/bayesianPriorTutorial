library(tidyverse)
library(brms)

# Load the data
syntheticNz <- readRDS("data/syntheticNz.RDS")

# Specify priors - I name the coefficient then specify a normal distribution to encompass where I think the true coefficient lies.
# These are the coefficients reported in the US model and the errors have been doubled.
priors <- c(
  prior("normal(-1.97, 0.5)", coef="age"),
  prior("normal(-0.29, 0.1)", coef="sexM"),
  prior("normal(-0.91, 0.4)", coef="nzdep2013"),
  prior("normal(1.03, 0.18)", coef="sexidentGayDLesbian"),
  prior("normal(1.18, 0.18)", coef="sexidentBisexual"),
  prior("normal(-0.07, 0.2)", coef="sexM:sexidentGayDLesbian"),
  prior("normal(-0.01, 0.2)", coef="sexM:sexidentBisexual")
)

########################### Basic model with stress as outcome

# Model stress as binomial outcome using US priors
brms.test <- brm(k10high ~ age + sex * sexident + nzdep2013,
                       family = bernoulli(),
                       prior = priors,
                       sample_prior = "yes",
                       cores = 4,
                       chains = 4,
                       data=nzhs)

# Model stress as binomial outcome without any priors (the large normal distribution just speeds up sampling).
# This looks analogous to a frequentist logistic regression.
brms.test.naive <- brm(k10high ~ age + sex * sexident + nzdep2013,
                 family = bernoulli(),
                 prior = prior("normal(0, 5)", class = "b"),
                 #sample_prior = "yes",
                 cores = 4,
                 chains = 4,
                 data=nzhs)

# Save out models
saveRDS(brms.test, "nzAnalysis20190428.RDS")
saveRDS(brms.test.naive, "nzAnalysisNaive20190428.RDS")

########################### Mediation model with stress as outcome

# Mediation model with same US priors
brms.test.mediation <- brm(bf(hazardous ~ age + sex + sexident + nzdep2013 + k10high) +
                             bf(k10high ~ age + sex + sexident + nzdep2013),
                           family = bernoulli(),
                           prior = priors,
                           sample_prior = "yes",
                           cores = 4,
                           chains = 4,
                           data=nzhs)

# Mediation model with no priors
brms.test.mediation.naive <- brm(bf(hazardous ~ age + sex + sexident + nzdep2013 + k10high) +
                             bf(k10high ~ age + sex + sexident + nzdep2013),
                           family = bernoulli(),
                           cores = 4,
                           chains = 4,
                           data=nzhs)

# Save out models
saveRDS(brms.test.mediation, "nzMediation20190428.RDS")
saveRDS(brms.test.mediation.naive, "nzMediationNaive20190428.RDS")
