nzhs <- readRDS("nzhs.RDS") %>%
  mutate(sexident = droplevels(replace(sexident, sexident %in% c("Don't know", "Refused answer", "Other"), NA)))

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
                       data=nzhs)

brms.test.naive <- brm(k10high ~ age + sex * sexident + nzdep2013,
                 family = bernoulli(),
                 prior = prior("normal(0, 5)", class = "b"),
                 #sample_prior = "yes",
                 cores = 4,
                 chains = 4,
                 data=nzhs)

priors <- c(prior("normal(-1.68, 0.5)", class="Intercept", resp="k10high"),
            prior("normal(-1.97, 0.5)", coef="age", resp="k10high"),
            prior("normal(-0.29, 0.1)", coef="sexM", resp="k10high"),
            prior("normal(-0.91, 0.4)", coef="nzdep2013", resp="k10high"),
            prior("normal(1.03, 0.18)", coef="sexidentGayDLesbian", resp="k10high"),
            prior("normal(1.18, 0.18)", coef="sexidentBisexual", resp="k10high"),
            prior("normal(0.7, 0.35)", coef="sexidentGayDLesbian", resp="hazardous"),
            prior("normal(0.7, 0.35)", coef="sexidentBisexual", resp="hazardous"))

brms.test.mediation <- brm(bf(hazardous ~ age + sex + sexident + nzdep2013 + k10high) +
                             bf(k10high ~ age + sex + sexident + nzdep2013),
                           family = bernoulli(),
                           prior = priors,
                           sample_prior = "yes",
                           cores = 4,
                           chains = 4,
                           data=nzhs)

brms.test.mediation.naive <- brm(bf(hazardous ~ age + sex + sexident + nzdep2013 + k10high) +
                             bf(k10high ~ age + sex + sexident + nzdep2013),
                           family = bernoulli(),
                           cores = 4,
                           chains = 4,
                           data=nzhs)

saveRDS(brms.test, "nzAnalysis20190428.RDS")
saveRDS(brms.test.naive, "nzAnalysisNaive20190428.RDS")

saveRDS(brms.test.mediation, "nzMediation20190428.RDS")
saveRDS(brms.test.mediation.naive, "nzMediationNaive20190428.RDS")
