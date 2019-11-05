library(tidyverse)
library(brms)

syntheticUs <- readRDS("data/syntheticNz.RDS")

priors <- c(prior("normal(1, 1)", coef="IRSEX2"),
            prior("normal(-1, 1)", coef="moAGE2"),
            prior("normal(-1, 1)", coef="moIRFAMIN3"))

brms.test <- brm(SPDMON ~ mo(AGE2) + IRSEX * SEXIDENT + mo(IRFAMIN3),
                 family = bernoulli(),
                 prior = priors,
                 cores = 4,
                 chains = 4,
                 data=dat)

saveRDS(brms.test, "usAnalysis20190320.RDS")
