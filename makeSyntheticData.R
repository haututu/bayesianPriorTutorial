
library(tidyverse)
library(brms)

us.size <- 10000
nz.size  <- 1000

nsduh <- read_spss("NSDUH-2017-DS0001-bndl-data-spss/NSDUH_2017.SAV") %>%
  select(SEXIDENT, AGE2, IRFAMIN3, IRSEX, SPDMON) %>%
  mutate(SEXIDENT = replace(SEXIDENT, SEXIDENT %in% c(85, 89, 94, 97, 98, 99), NA)) %>%
  na.omit()

us.sample <- with(nsduh,
                  data.frame(
                    "SEXIDENT" = sample(SEXIDENT, us.size), 
                    "AGE2" = sample(AGE2, us.size), 
                    "IRFAMIN3" = sample(IRFAMIN3, us.size), 
                    "IRSEX" = sample(IRSEX, us.size), 
                    "SPDMON" = sample(SPDMON, us.size)
                    )
                  ) %>%
  mutate(
    SEXIDENT = as.factor(SEXIDENT),
    IRSEX = as.factor(IRSEX)
  )

us.fit <- readRDS("usAnalysis20190320.RDS")

us.posteriors <- posterior_samples(us.fit) %>% 
  mutate(rows = n()) %>% 
  select(b_Intercept, rows) %>%
  filter(b_Intercept < -1.64 & b_Intercept > -1.72)

us.sample <- mutate(us.sample,
                    SPDMON = ifelse(runif(us.size) < fitted(us.fit, newdata=us.sample, subset = us.posteriors$rows)[,1], 1, 0)
                    )

########################################### NZ Data

nzhs <- readRDS("nzhs.RDS") %>%
  mutate(sexident = droplevels(replace(sexident, sexident %in% c("Don't know", "Refused answer", "Other"), NA))) %>%
  select(sexident, age, sex, nzdep2013, hazardous, k10high) %>%
  na.omit()

nz.sample <- with(nzhs,
                  data.frame(
                    "sexident" = sample(sexident, nz.size), 
                    "age" = sample(age, nz.size), 
                    "sex" = sample(sex, nz.size), 
                    "nzdep2013" = sample(nzdep2013, nz.size), 
                    "hazardous" = sample(hazardous, nz.size),
                    "k10high" = sample(k10high, nz.size)
                    )
                  ) %>%
  mutate(
    sex = as.factor(sex),
    sexident = as.factor(sexident)
    )

nz.fit <- readRDS("nzAnalysis20190428.RDS")
nz.mediation <- readRDS("nzMediation20190428.RDS")

nz.posteriors <- posterior_samples(nz.mediation) %>% 
  mutate(rows = n()) %>% 
  select(b_hazardous_Intercept, b_k10high_Intercept, rows) %>%
  filter(
    (b_hazardous_Intercept < -0.78 & b_hazardous_Intercept > -0.82) &
      (b_k10high_Intercept < -2.63 & b_k10high_Intercept > -2.69)
    )

nz.sample <- mutate(
  nz.sample,
  hazardous = ifelse(runif(nz.size) < fitted(nz.mediation, newdata=nz.sample, subset = nz.posteriors$rows)[,1,'hazardous'], 1, 0),
  k10high = ifelse(runif(nz.size) < fitted(nz.mediation, newdata=nz.sample, subset = nz.posteriors$rows)[,1,'k10high'], 1, 0)
  )

priors <- c(#prior("normal(-1.68, 0.5)", class="Intercept"),
  prior("normal(-1.97, 0.5)", coef="age"),
  prior("normal(-0.29, 0.1)", coef="sexM"),
  prior("normal(-0.91, 0.4)", coef="nzdep2013"),
  prior("normal(1.03, 0.18)", coef="sexidentGayDLesbian"),
  prior("normal(1.18, 0.18)", coef="sexidentBisexual"),
  prior("normal(-0.07, 0.2)", coef="sexM:sexidentGayDLesbian"),
  prior("normal(-0.01, 0.2)", coef="sexM:sexidentBisexual"))

brms.test <- brm(k10high ~ age + sex * sexident + nzdep2013,
                 family = bernoulli(),
                 prior = priors,
                 sample_prior = "yes",
                 cores = 4,
                 chains = 4,
                 data=nz.sample)
