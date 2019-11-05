library(tidyverse)
library(brms)

# Load the data
syntheticUs <- readRDS("data/syntheticUs.RDS")

# Make some weakly informative priors purely to speed up sampling
priors <- c(prior("normal(1, 1)", coef="IRSEX2"),
            prior("normal(-1, 1)", coef="moAGE2"),
            prior("normal(-1, 1)", coef="moIRFAMIN3"))

# Run basic brms model with stress as a binomial outcome. mo() denotes a monotonic effect.
brms.test <- brm(SPDMON ~ mo(AGE2) + IRSEX * SEXIDENT + mo(IRFAMIN3),
                 family = bernoulli(),
                 prior = priors,
                 cores = 4,
                 chains = 4,
                 data=syntheticUs)

# Save out the model
saveRDS(brms.test, "usAnalysis20190320.RDS")
