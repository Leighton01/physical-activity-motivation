# Libraries ---------------------------------------------------------------
set.seed(2025)
library(tidyverse)
library(lavaan)

# SEM ---------------------------------------------------------------------

# Free model
m0 <- '
  # Mediators: controlling for age, gender, and ethnicity (group-specific coefficients)
  enjoyb ~ c(a1_adult, a1_youth)*age + c(g1_adult, g1_youth)*gender + c(e1_adult, e1_youth)*eth
  guiltb ~ c(a2_adult, a2_youth)*age + c(g2_adult, g2_youth)*gender + c(e2_adult, e2_youth)*eth
  oppb   ~ c(a3_adult, a3_youth)*age + c(g3_adult, g3_youth)*gender + c(e3_adult, e3_youth)*eth
  fitb   ~ c(a4_adult, a4_youth)*age + c(g4_adult, g4_youth)*gender + c(e4_adult, e4_youth)*eth
  socialb~ c(a5_adult, a5_youth)*age + c(g5_adult, g5_youth)*gender + c(e5_adult, e5_youth)*eth
  relxb~ c(a6_adult, a6_youth)*age + c(g6_adult, g6_youth)*gender + c(e6_adult, e6_youth)*eth

  # Main outcome: motives predicting mins, controlling for demographics (group-specific coefficients)
  mins ~ c(b1_adult, b1_youth)*enjoyb + c(b2_adult, b2_youth)*guiltb + c(b3_adult, b3_youth)*oppb +
          c(b4_adult, b4_youth)*fitb + c(b5_adult, b5_youth)*socialb + c(b6_adult, b6_youth)*relxb
          + c(c_adult, c_youth)*age +
          c(g7_adult, g7_youth)*gender + c(e7_adult, e7_youth)*eth

# For Adults
indirect_age_enjoyb_adult  := a1_adult * b1_adult
indirect_age_guiltb_adult  := a2_adult * b2_adult
indirect_age_oppb_adult    := a3_adult * b3_adult
indirect_age_fitb_adult    := a4_adult * b4_adult
indirect_age_socialb_adult := a5_adult * b5_adult
indirect_age_relxb_adult := a6_adult * b6_adult
total_age_adult := c_adult + indirect_age_enjoyb_adult + indirect_age_guiltb_adult +
                    indirect_age_oppb_adult + indirect_age_fitb_adult +
                    indirect_age_socialb_adult + indirect_age_relxb_adult

# For Youth
indirect_age_enjoyb_youth  := a1_youth * b1_youth
indirect_age_guiltb_youth  := a2_youth * b2_youth
indirect_age_oppb_youth    := a3_youth * b3_youth
indirect_age_fitb_youth    := a4_youth * b4_youth
indirect_age_socialb_youth := a5_youth * b5_youth
indirect_age_relxb_youth := a6_youth * b6_youth
total_age_youth := c_youth + indirect_age_enjoyb_youth + indirect_age_guiltb_youth +
                   indirect_age_oppb_youth + indirect_age_fitb_youth +
                   indirect_age_socialb_youth + indirect_age_relxb_youth
'


f0 <- sem(m0, data = dallb, group = "group")
sem.free <- summary(f0, fit.measures = TRUE, standardized = TRUE)

# Constrain all to be equal
f.con <- sem(m0, dallb, group = "group",
             group.equal = c("intercepts", "regressions"))

# Check if significantly different
f0fcon <- anova(f0, f.con)
f0fcon

# Spec one constraint at a time
m1 <- '
  # Mediators
  enjoyb ~ age + gender + eth
  guiltb ~ age + gender + eth
  oppb ~ age + gender + eth
  fitb ~ age + gender + eth
  socialb ~ age + gender + eth
  relxb ~ age + gender + eth

  # Main outcome
  mins ~ c("a1","a1")*enjoyb + guiltb + oppb + fitb + socialb + age + gender + eth + relxb
'

m2 <- '
  # Mediators
  enjoyb ~ age + gender + eth
  guiltb ~ age + gender + eth
  oppb ~ age + gender + eth
  fitb ~ age + gender + eth
  socialb ~ age + gender + eth
  relxb ~ age + gender + eth

  # Main outcome
  mins ~ enjoyb + c(a,a)*guiltb + oppb + fitb + socialb + age + gender + eth + relxb
'

m3 <- '
  # Mediators
  enjoyb ~ age + gender + eth
  guiltb ~ age + gender + eth
  oppb ~ age + gender + eth
  fitb ~ age + gender + eth
  socialb ~ age + gender + eth
  relxb ~ age + gender + eth

  # Main outcome
  mins ~ enjoyb + guiltb + c(a,a)*oppb + fitb + socialb + age + gender + eth + relxb
'

m4 <- '
  # Mediators
  enjoyb ~ age + gender + eth
  guiltb ~ age + gender + eth
  oppb ~ age + gender + eth
  fitb ~ age + gender + eth
  socialb ~ age + gender + eth
  relxb ~ age + gender + eth

  # Main outcome
  mins ~ enjoyb + guiltb + oppb + c(a,a)*fitb + socialb + age + gender + eth + relxb
'

m5 <- '
  # Mediators
  enjoyb ~ age + gender + eth
  guiltb ~ age + gender + eth
  oppb ~ age + gender + eth
  fitb ~ age + gender + eth
  socialb ~ age + gender + eth
  relxb ~ age + gender + eth

  # Main outcome
  mins ~ enjoyb + guiltb + oppb + fitb + c(a,a)*socialb + age + gender + eth + relxb
'

m6 <- '
  # Mediators
  enjoyb ~ age + gender + eth
  guiltb ~ age + gender + eth
  oppb ~ age + gender + eth
  fitb ~ age + gender + eth
  socialb ~ age + gender + eth
  relxb ~ age + gender + eth

  # Main outcome
  mins ~ enjoyb + guiltb + oppb + fitb + c(a,a)*relxb + age + gender + eth + socialb
'

# Small eigenvalue close to 0, does not matter
f1 <- sem(m1, data = dallb, group = "group", meanstructure = TRUE)
f2 <- sem(m2, data = dallb, group = "group", meanstructure = TRUE)
f3 <- sem(m3, data = dallb, group = "group", meanstructure = TRUE)
f4 <- sem(m4, data = dallb, group = "group", meanstructure = TRUE)
f5 <- sem(m5, data = dallb, group = "group", meanstructure = TRUE)
f6 <- sem(m6, data = dallb, group = "group", meanstructure = TRUE)

# Check all models are significantly different from m0
anova(f0, f1)
anova(f0, f2)
anova(f0, f3)
anova(f0, f4)
anova(f0, f5)
anova(f0, f6)


# Put slope diff. in a table
params <- parameterEstimates(f0, standardized = T)
# filter
slopes <- params %>%
  filter(lhs == "mins", op == "~") %>%
  dplyr::select(var=rhs, group, est, se)
# filtre more
slopes.ad <- slopes %>% filter(group == 1) %>%
  dplyr::select(var, est.adult = est, se.adult = se)
slopes.ch <- slopes %>% filter(group == 2) %>%
  dplyr::select(var, est.youth = est, se.youth = se)
# join!
slopes.diff <- data.frame()
slopes.diff <- left_join(slopes.ch, slopes.ad, by = "var")
# calculate
slopes.diff <- slopes.diff %>%
  mutate(
    diff = est.youth - est.adult
    # se.diff = sqrt(se.adult^2 + se.youth^2),
    # z = diff / se.diff,
    # p = 2 * (1 - pnorm(abs(z)))
  ) %>%
  filter(!var %in% c("gender","eth")) %>%
  dplyr::select(-se.youth, -se.adult)


slopes.diff

