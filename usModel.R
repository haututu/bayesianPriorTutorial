library(tidyverse)
library(haven)
library(brms)

dat2017 <- read_spss("NSDUH-2017-DS0001-bndl-data-spss/NSDUH_2017.SAV") %>%
  select(SEXIDENT, AGE2, IRFAMIN3, IRSEX, K6SCMON, SPDMON) %>%
  mutate(SEXIDENT = replace(SEXIDENT, SEXIDENT %in% c(85, 89, 94, 97, 98, 99), NA),
         SEXIDENT = droplevels(as.factor(SEXIDENT)),
         IRSEX = as.factor(IRSEX),
         subset_data = sample(1:5, n(), replace = TRUE))

dat2016 <- read_spss("NSDUH_2002_2016_SM_subset.SAV") %>%
  filter(YEAR == 2016) %>%
  select(SEXIDENT, AGE2, IRFAMIN3, IRSEX, K6SCMON, SPDMON) %>%
  mutate(SEXIDENT = replace(SEXIDENT, SEXIDENT %in% c(85, 89, 94, 97, 98, 99), NA),
         SEXIDENT = droplevels(as.factor(SEXIDENT)),
         IRSEX = as.factor(IRSEX),
         subset_data = sample(1:5, n(), replace = TRUE))

dat <- rbind(dat2016, dat2017)

dat_split <- split(dat, f = dat$subset_data)

priors <- c(prior("normal(1, 1)", coef="IRSEX2"),
            prior("normal(-1, 1)", coef="moAGE2"),
            prior("normal(-1, 1)", coef="moIRFAMIN3"))

brms.test <- brm(SPDMON ~ mo(AGE2) + IRSEX * SEXIDENT + mo(IRFAMIN3),
                 family = bernoulli(),
                 prior = priors,
                 cores = 4,
                 chains = 4,
                 data=dat)

plan(multiprocess)
brms.test <- brm_multiple(SPDMON ~ mo(AGE2) + IRSEX*SEXIDENT + mo(IRFAMIN3),
                          family = bernoulli(),
                          prior = priors,
                          cores = 4,
                          chains = 4,
                          data=dat_split)

saveRDS(brms.test, "usAnalysis20190320.RDS")
