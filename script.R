# NEXT STEPS --------------------------------------------------------------


# name the extra variables, tidy up code
# figure out tidyLPA syntax







# 1. go throuhg checklist (ask for the checklist
# 2. do free sem, then contrain var one by one (5 times basically), report differences if sig.
# 3. Respec models with mediating effects of age and gender based on lit
  # age has effect on enjoy, guilt, opp, fitness
  # gender has effect on guilt, enjoy

# ???LAST After rnning model on full data, run on subsamples
# focus on measurement invariance, fit, and robustness checks.

# Libraries ---------------------------------------------------------------
library(tidyverse)
library(lavaan)
# library(piecewiseSEM)
library(tidyLPA)

# Read & Save ---------------------------------------------------------------

# data.child <- read.csv('data/child_main.tab', header=T, sep='\t')
# data.adult <- read.csv('data/adult.tab', header=T, sep='\t')
#
child.var <- data.child %>% select('PL_Enjoy_bc_ans', 'PL_Conf_bc_ans',
                                   'PL_Easy_bc_ans', 'PL_GdMe_bc_ans',
                                   'PL_Know_c_ans', 'MO_Opp_c',
                                   'MO_Fit_c', 'MO_Relax_c', 'MO_Fun_c',
                                   'MO_Guilt_c', 'MO_Haveto_b_36',
                                   'MO_Haveto_c_711', 'PR_Fam_c', 'PR_Oth_c',
                                   'Try_bc', 'outdoor_bcd_Overall',
                                   'Exeramt_bc', 'ExeramtMore_bc1_2',
                                   'ExeramtMore_bc2_2', 'ExeramtMore_bc3_2',
                                   'mins_modplus_outschool_Week_ALL',

                                   # profile
                                   'age_11', 'eth2', 'gend3', 'eth6',
                                   'Disab_All_POP',

                                   # binary
                                   'PL_Enjoy_bc_SA_gr2', 'MO_Fun_c_SA',
                                   'MO_Fit_c_SA', 'MO_Guilt_c_SA', 'MO_Opp_c_SA'
)

save(child.var, file = "child.var.RData")


adult.var <- data.adult %>% select('Motiva_POP',
                                   'motivb_POP',
                                   'motivc_POP',
                                   'motivd_POP',
                                   'motive_POP',
                                   'READYAB1_POP',
                                   'READYOP1_POP',
                                   'motivex2a',
                                   'motivex2b',
                                   'motivex2c',
                                   'motivex2d',
                                   'inclus_a',
                                   'inclus_b',
                                   'inclus_c',
                                   'indev',
                                   'indevtry',
                                   'workactlvl',
                                   'DUR_HVY_CAPPED_SPORTCOUNT_A01',
                                   'DUR_MOD_CAPPED_SPORTCOUNT_A01',

                                   # profile
                                   'Age17',
                                   'Disab2_POP',
                                   'Gend3',
                                   'Eth2',
                                   'Eth7',

                                   # binary
                                   'Motiva_POP_GR2', 'motivex2c_GR2',
                                   'motivex2a_GR2', 'motivc_POP_GR2',
                                   'READYOP1_POP_GR2'
)

save(adult.var, file = "adult.var.RData")


# Load & Check --------------------------------------------------------------------
load("child.var.RData")
load("adult.var.RData")

glimpse(child.var)
glimpse(adult.var)

# ethnicity
prop.table(table(adult.var$Eth7))
prop.table(table(child.var$eth6))

# 2 is no disa
table(child.var$Disab_All_POP)
table(adult.var$Disab2_POP)

# adult in bands of 5 years, child just in years
table(child.var$age_11)
table(adult.var$Age19plus)

# too few transgendered adults, filter out
table(adult.var$gend2_GR6)
table(adult.var$indevtry)
table(adult.var$motive_POP)


# Filter & Trim for Part 1  ------------------------------------------------------

child.bi <- child.var %>%
  filter(Disab_All_POP == 2, # remove disabled and no answer
         gend3 %in% c(1,2),
         eth2 %in% c(1,2),

         if_all(c(age_11, mins_modplus_outschool_Week_ALL), ~ .x > -1),

         if_all(c(PL_Enjoy_bc_SA_gr2, MO_Fun_c_SA, MO_Fit_c_SA,
                  MO_Guilt_c_SA, MO_Opp_c_SA), ~ .x > -1 & .x < 3)) %>%

  select(enjoyb=PL_Enjoy_bc_SA_gr2,
         socialb=MO_Fun_c_SA,
         fitb=MO_Fit_c_SA,
         guiltb=MO_Guilt_c_SA,
         oppb=MO_Opp_c_SA,

         gender=gend3,
         age=age_11,
         eth=eth2,
         # eth6=eth6,
         mins=mins_modplus_outschool_Week_ALL

  ) %>%
  # change 2 (not strongly agree) to 0, consistent with adult
  mutate(across(c(enjoyb,socialb,fitb,guiltb,oppb), ~ ifelse(.x==2, 0, .x)),
         gender = case_when(gender == 2 ~ 0, TRUE ~ gender),# 2 was originally female, recode to 0
         eth = case_when(eth == 1 ~ 0, eth == 2 ~ 1, TRUE ~ eth))# 0 = white british, 1 = other



adult.bi <- adult.var %>% filter(Disab2_POP==2,
                                 Gend3 %in% c(1,2),
                                 Eth2 %in% c(1,2),
                                 if_all(c(Age17,
                                          DUR_MOD_CAPPED_SPORTCOUNT_A01,
                                          DUR_HVY_CAPPED_SPORTCOUNT_A01),
                                        ~ .x > -1),
                                 if_all(c(Motiva_POP_GR2, motivex2c_GR2,
                                          motivex2a_GR2, motivc_POP_GR2,
                                          READYOP1_POP_GR2),
                                        ~ .x %in% c(0,1))) %>%


  mutate(mins=DUR_MOD_CAPPED_SPORTCOUNT_A01 +
           DUR_HVY_CAPPED_SPORTCOUNT_A01,
         Gend3 = case_when(Gend3 == 2 ~ 0,TRUE ~ Gend3), # 2 was originally girl, recode to 0
         Eth2 = case_when(Eth2 == 1 ~ 0, Eth2 == 2 ~ 1, TRUE ~ Eth2),
         Age_mid = case_when(
           Age17 == 1 ~ 17.5,  # 16–19
           Age17 == 2 ~ 22,    # 20–24
           Age17 == 3 ~ 27,    # 25–29
           Age17 == 4 ~ 32,    # 30–34
           Age17 == 5 ~ 37,    # 35–39
           Age17 == 6 ~ 42,    # 40–44
           Age17 == 7 ~ 47,    # 45–49
           Age17 == 8 ~ 52,    # 50–54
           Age17 == 9 ~ 57,    # 55–59
           Age17 == 10 ~ 62,   # 60–64
           Age17 == 11 ~ 67,   # 65–69
           Age17 == 12 ~ 72,   # 70–74
           Age17 == 13 ~ 77,   # 75–79
           Age17 == 14 ~ 82,   # 80–84
           Age17 == 15 ~ 87,   # 85–89
           Age17 == 16 ~ 92,   # 90–94
           Age17 == 17 ~ 97,    # 95+
            TRUE ~ Age17),
         age = Age_mid - mean(Age_mid)  # mean-centered, use this in sem
         ) %>% # 0 = white british, 1 = other



  select(enjoyb=Motiva_POP_GR2,
         socialb=motivex2c_GR2,
         fitb=motivex2a_GR2,
         guiltb=motivc_POP_GR2,
         oppb=READYOP1_POP_GR2,

         gender=Gend3,
         age,
         eth=Eth2,
         # eth7=Eth7,
         mins
  )

dallb <- bind_rows(
  adult.bi %>% mutate(group = "adult"),
  child.bi %>% mutate(group = "youth")
) %>%
  mutate(mins = ifelse(mins > 1680, 1680, mins))

# Filter & Trim for Part 2 ------------------------------------------------

child.trim <- child.var %>%
  filter(Disab_All_POP == 2,
         gend3 %in% c(1,2),
         eth2 %in% c(1,2),
         if_all(c(PL_Enjoy_bc_ans, MO_Fun_c, MO_Fit_c,
                  MO_Guilt_c, MO_Opp_c), ~ . < 6,),

         if_all(c(PL_Enjoy_bc_ans, MO_Fun_c, MO_Fit_c,
              MO_Guilt_c, MO_Opp_c,
              Try_bc, PL_GdMe_bc_ans,
              gend3, age_11,
              mins_modplus_outschool_Week_ALL), ~ .x > -1)) %>%

    select(enjoy=PL_Enjoy_bc_ans,
           social=MO_Fun_c,
           fit=MO_Fit_c,
           try=Try_bc,
           guilt=MO_Guilt_c,
           know=PL_GdMe_bc_ans,
           opp=MO_Opp_c,

           PL_Conf_bc_ans,
           PL_Easy_bc_ans,
           Exeramt_bc,

           # relevant but lots na due to survey routing
           # PL_Know_c_ans,
           # MO_Relax_c,
           # MO_Haveto_b_36,
           # MO_Haveto_c_711,
           # PR_Fam_c,
           # PR_Oth_c,
           # outdoor_bcd_Overall,
           # ExeramtMore_bc1_2,
           # ExeramtMore_bc2_2,
           # ExeramtMore_bc3_2,

           gender=gend3,
           age=age_11,
           eth=eth2,
           mins=mins_modplus_outschool_Week_ALL

    )



adult.trim <- adult.var %>% filter(Disab2_POP==2,
                                   Gend3 %in% c(1,2),
                                   Eth2 %in% c(1,2),
                                   if_all(c(Motiva_POP,motivex2c,motivex2a,
                                            motivc_POP,READYOP1_POP,
                                            Age17,
                                            DUR_MOD_SPORTCOUNT_A01,
                                            DUR_HVY_SPORTCOUNT_A01
                                            ), ~ .x > -1),
                                   ) %>%


  mutate(mins=DUR_HVY_CAPPED_SPORTCOUNT_A01+DUR_MOD_CAPPED_SPORTCOUNT_A01) %>%


  select(enjoy=Motiva_POP,
         social=motivex2c,
         fit=motivex2a,
         guilt=motivc_POP,
         opp=READYOP1_POP,

         motivb_POP,
         motivd_POP,
         READYAB1_POP,
         motivex2b,
         motivex2d,

         # relevant but lots na due to survey routing
         # know=motive_POP,
         # try=indevtry,
         # inclus_a,
         # inclus_b,
         # inclus_c,
         # indev,
         # workactlvl,

         gender=Gend3,
         gender2=gend2_GR6,
         age=Age17,
         eth=Eth2,
         mins
         )

dall <- bind_rows(
  adult.trim %>% mutate(group = "adult"),
  child.trim %>% mutate(group = "youth")
) %>%
  mutate(across(c(enjoy, social, fit, opp, guilt), ~ 6 - .),
         mins = ifelse(mins > 1680, 1680, mins),
         across(c(eth, gender), as.factor)

         )
  # group_by(group) %>%
  # mutate(
  #   enjoy_r  = (enjoy - min(enjoy, na.rm=TRUE)) / (max(enjoy, na.rm=TRUE) - min(enjoy, na.rm=TRUE)),
  #   guilt_r  = (guilt - min(guilt, na.rm=TRUE)) / (max(guilt, na.rm=TRUE) - min(guilt, na.rm=TRUE)),
  #   opp_r    = (opp - min(opp, na.rm=TRUE)) / (max(opp, na.rm=TRUE) - min(opp, na.rm=TRUE)),
  #   fit_r    = (fit - min(fit, na.rm=TRUE)) / (max(fit, na.rm=TRUE) - min(fit, na.rm=TRUE)),
  #   social_r = (social - min(social, na.rm=TRUE)) / (max(social, na.rm=TRUE) - min(social, na.rm=TRUE)),
  #   # mins_r = (mins - min(mins, na.rm=TRUE)) / (max(mins, na.rm=TRUE) - min(mins, na.rm=TRUE)),
  #   age_r = (age - min(age, na.rm=TRUE)) / (max(age, na.rm=TRUE) - min(age, na.rm=TRUE))
  #   ) %>%
  # ungroup()

# dall$mins_r <- (dall$mins - min(dall$mins, na.rm=TRUE)) / (max(dall$mins, na.rm=TRUE) - min(dall$mins, na.rm=TRUE))
# # Standardize age globally
# dall$age_r <- scale(dall$age)[,1]




# Checks -------------------------------------------------------------------
# Collinearity
dallb1 <- dall %>% dplyr::select(enjoyb, socialb, fitb, guiltb, oppb)
cor(dallb1, method = "pearson") # opp, fit and enjoy have mod corr with each other, others ok

vif_model <- lm(mins ~ enjoyb + socialb + fitb + guiltb + oppb, data = dallb1)
vif(vif_model) # vif good, all below 2

# basic stats
dallb2 <- dall %>% dplyr::select(enjoyb, socialb, fitb, guiltb, oppb, group)
prop.table(table(dallb2$enjoyb,dallb2$group),2)
prop.table(table(dallb2$socialb,dallb2$group),2)
prop.table(table(dallb2$fitb,dallb2$group),2)
prop.table(table(dallb2$guiltb,dallb2$group),2)
prop.table(table(dallb2$oppb,dallb2$group),2)

dallb2 %>% group_by(group) %>%
  summarize(across(
    .cols = all_of(setdiff(names(dallb2), "group")),
    .fns = list(mean = mean, sd = sd)
))

summary(dallb2$enjoyb)

# Binary SEM --------------------------------------------------------------
dallb <- dall %>% dplyr::select(enjoyb, socialb, fitb, guiltb, oppb,mins,
                                age,gender, eth, group)

m0 <- '
  # Mediators: age and gender predicting motives, controlling for ethnicity
  enjoyb ~ age + gender + eth
  guiltb ~ age + gender + eth
  oppb ~ age + eth
  fitb ~ age + eth
  socialb ~ eth

  # Main outcome: motives predicting mins, controlling for ethnicity
  mins ~ enjoyb +guiltb +oppb +fitb +socialb +eth
'

f0 <- sem(m0, data = dallb, group = "group")
summary(f0, fit.measures = TRUE, standardized = TRUE)

# Constrain all to be equal
f.con <- sem(m0, dallb, group = "group",
             group.equal = c("intercepts", "regressions"))
summary(f.con)

#fit.con significantly worse
anova(f0, fit.con)

# Try some constraints..
m1 <- '
  # Mediators
  enjoyb ~ age + gender + eth
  guiltb ~ age + gender + eth
  oppb ~ age + eth
  fitb ~ age + eth
  socialb ~ eth

  # Main outcome
  mins ~ c("a1","a1")*enjoyb + guiltb + oppb + fitb + socialb + eth
'

m2 <- '
  # Mediators
  enjoyb ~ age + gender + eth
  guiltb ~ age + gender + eth
  oppb ~ age + eth
  fitb ~ age + eth
  socialb ~ eth

  # Main outcome
  mins ~ enjoyb + c(a,a)*guiltb + oppb + fitb + socialb + eth
'

m3 <- '
  # Mediators
  enjoyb ~ age + gender + eth
  guiltb ~ age + gender + eth
  oppb ~ age + eth
  fitb ~ age + eth
  socialb ~ eth

  # Main outcome
  mins ~ enjoyb + guiltb + c(a,a)*oppb + fitb + socialb + eth
'

m4 <- '
  # Mediators
  enjoyb ~ age + gender + eth
  guiltb ~ age + gender + eth
  oppb ~ age + eth
  fitb ~ age + eth
  socialb ~ eth

  # Main outcome
  mins ~ enjoyb + guiltb + oppb + c(a,a)*fitb + socialb + eth
'

m5 <- '
  # Mediators
  enjoyb ~ age + gender + eth
  guiltb ~ age + gender + eth
  oppb ~ age + eth
  fitb ~ age + eth
  socialb ~ eth

  # Main outcome
  mins ~ enjoyb + guiltb + oppb + fitb + c(a,a)*socialb + eth
'

# contrained all sig diff. FREE=BEST
# samll eigenvalue close to 0, does not matter
f1 <- sem(m1, data = dallb, group = "group", meanstructure = TRUE)
f2 <- sem(m2, data = dallb, group = "group", meanstructure = TRUE)
f3 <- sem(m3, data = dallb, group = "group", meanstructure = TRUE)
f4 <- sem(m4, data = dallb, group = "group", meanstructure = TRUE)
f5 <- sem(m5, data = dallb, group = "group", meanstructure = TRUE)

# ALL sig dfiferent, none should be constrained/equal :) cool
anova(f0, f1)
anova(f0, f2)
anova(f0, f3)
anova(f0, f4)
anova(f0, f5)

# library(metaSEM)
# calEffSizes(f1, dallb)


cohen_d_lavaan <- function(model, param_name) {
  slopes <- parameterEstimates(model, standardized = FALSE)
  # filter for the path from enjoyb to mins
  slopes_path <- slopes[slopes$lhs == "mins" & slopes$rhs == param_name, ]

  # slopes_path$est will contain the slope for each group in the same order as your data
  # group 1 = adult, 2 youth
  slope_youth <- slopes_path$est[slopes_path$group == 2]
  slope_adult <- slopes_path$est[slopes_path$group == 1]

  # 2. Get pooled SD of your outcome variable (mins)
  mins_youth <- dallb$mins[dallb$group=="youth"]
  mins_adult <- dallb$mins[dallb$group=="adult"]
  pooled_sd <- sqrt(((length(mins_youth)-1)*var(mins_youth) +
                       (length(mins_adult)-1)*var(mins_adult)) /
                      (length(mins_youth) + length(mins_adult) - 2))

  # 3. Cohen's d-like effect of slope difference
  cohen_d <- (slope_youth - slope_adult) / pooled_sd

  return(c(cohen_d, cohen_d*pooled_sd))
}

# all have small effect size but ~20min diff. high variance in mins
# just nuanced.. i guess.. interpret later..
cohen_d_lavaan(f0, "enjoyb")
cohen_d_lavaan(f0, "socialb")
cohen_d_lavaan(f0, "fitb")
cohen_d_lavaan(f0, "guiltb")
cohen_d_lavaan(f0, "oppb")




# PiecewiseSEM ------------------------------------------------------------


