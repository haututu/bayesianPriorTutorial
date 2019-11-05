
#
# This script was used to create some simulated data for users to play around with.
# Note that the simulated data has no relationship between variables and does not exactly line up with the manuscript.
#

library(tidyverse)
library(brms)

# Sample size to generate in each dataset
us.size <- 10000
nz.size  <- 1000


########################################### Simulate a US dataset

# Load the US data and do some light cleaning
nsduh <- read_spss("NSDUH-2017-DS0001-bndl-data-spss/NSDUH_2017.SAV") %>%
  select(SEXIDENT, AGE2, IRFAMIN3, IRSEX, SPDMON) %>%
  mutate(SEXIDENT = replace(SEXIDENT, SEXIDENT %in% c(85, 89, 94, 97, 98, 99), NA)) %>%
  na.omit()

# Randomly sample us.size number of rows from each variable
# May consider sampling with replacement
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

# Load the fitted model used in the publication
us.fit <- readRDS("usAnalysis20190320.RDS")

# Trim outlieing posterior samples to 'overfit' the small samples to the model.
# I used the intercept to select samples closer to the average posterior sample
us.posteriors <- posterior_samples(us.fit) %>% 
  mutate(rows = n()) %>% 
  select(b_Intercept, rows) %>%
  filter(b_Intercept < -1.64 & b_Intercept > -1.72)

# Add the stress outcome to the fabricated sample
us.sample <- mutate(us.sample,
                    SPDMON = ifelse(runif(us.size) < fitted(us.fit, newdata=us.sample, subset = us.posteriors$rows)[,1], 1, 0)
                    )

########################################### Simulate a NZ dataset

# Load and clean NZ data
nzhs <- readRDS("nzhs.RDS") %>%
  mutate(sexident = droplevels(replace(sexident, sexident %in% c("Don't know", "Refused answer", "Other"), NA))) %>%
  select(sexident, age, sex, nzdep2013, hazardous, k10high) %>%
  na.omit()

# Take a sample of nz.size rows for each variable
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

# Load the mediation model for fitting and by proxy the data will fit the simple model
nz.mediation <- readRDS("nzMediation20190428.RDS")

# Trimming posterior samples
nz.posteriors <- posterior_samples(nz.mediation) %>% 
  mutate(rows = n()) %>% 
  select(b_hazardous_Intercept, b_k10high_Intercept, rows) %>%
  filter(
    (b_hazardous_Intercept < -0.78 & b_hazardous_Intercept > -0.82) &
      (b_k10high_Intercept < -2.63 & b_k10high_Intercept > -2.69)
    )

# Add the two outcomes used in mediation
nz.sample <- mutate(
  nz.sample,
  hazardous = ifelse(runif(nz.size) < fitted(nz.mediation, newdata=nz.sample, subset = nz.posteriors$rows)[,1,'hazardous'], 1, 0),
  k10high = ifelse(runif(nz.size) < fitted(nz.mediation, newdata=nz.sample, subset = nz.posteriors$rows)[,1,'k10high'], 1, 0)
  )
