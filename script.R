# NEXT STEPS --------------------------------------------------------------


# name the extra variables, tidy up code
# figure out tidyLPA syntax





# note: child.bi+adult.bi=dallb, child.lik, adult.lik



# 1. go throuhg checklist (ask for the checklist
# 2. do free sem, then contrain var one by one (5 times basically), report differences if sig.
# 3. Respec models with mediating effects of age and gender based on lit
  # age has effect on enjoy, guilt, opp, fitness
  # gender has effect on guilt, enjoy

# ???LAST After rnning model on full data, run on subsamples
# focus on measurement invariance, fit, and robustness checks.

# Libraries ---------------------------------------------------------------
set.seed(2025)
library(tidyverse)
library(lavaan)
library(ggplot2)
library(nnet)
library(tidyLPA)
library(poLCA)

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


adult.var <- data.adult %>% dplyr::select('Motiva_POP',
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
                                   'Age17','Age3','AgeTGC',
                                   'Age4','Age5','Age5_2',
                                   'Age9','Disab2_POP',
                                   'Gend3','Eth2','Eth7',

                                   # binary
                                   'Motiva_POP_GR2', 'motivex2c_GR2',
                                   'motivex2a_GR2', 'motivc_POP_GR2',
                                   'READYOP1_POP_GR2')

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

# # Check if collapsing is necessary
# child.lik %>% dplyr::select(-max_post,-mins,-age,-eth) %>%
#   pivot_longer(
#     cols = everything(),   # or specify your Likert vars if df has other columns
#     names_to = "Variable",
#     values_to = "Response"
#   ) %>%
#   group_by(Variable, Response) %>%
#   summarise(n = n(), .groups = "drop_last") %>%
#   #"drop_last" drops the response variable,
#   #so that the next part (proportion) does not calculate within each response
#
#   mutate(prop = n / sum(n)) %>%
#   arrange(Variable, Response) %>% filter(prop < 0.05)


child.lik <- child.var %>%

  # 1-4, 1=strong agree, 4=strong disagree, 5=can't say
  dplyr::select(enjoy=PL_Enjoy_bc_ans,
           social=MO_Fun_c,
           fit=MO_Fit_c,
           try=Try_bc,
           guilt=MO_Guilt_c, #99 instead of 5 for "can't say"
           know=PL_GdMe_bc_ans,
           opp=MO_Opp_c, #99 instead of 5 for "can't say"
           conf=PL_Conf_bc_ans,
           easy=PL_Easy_bc_ans,
           more=Exeramt_bc, #only 3, 1=more,2=same,3=less

           dsbl=Disab_All_POP,
           gender=gend3,
           age=age_11,
           eth=eth2,
           mins=mins_modplus_outschool_Week_ALL
    ) %>%

  filter(dsbl == 2,
       gender %in% c(1,2),
       eth %in% c(1,2),
       mins > -1,
       if_all(c(enjoy,social,fit,guilt,opp,know,try,conf,easy,more),
              ~ .x > -1 & .x < 5)) %>%
  mutate(
         mins = ifelse(mins > 1680, 1680, mins),
         across(c(conf,easy,enjoy,fit,know,more,opp,try),
                ~ case_when(.x==4~3L, TRUE ~ as.integer(.x)))


         ) %>%
  dplyr::select(-dsbl)

child.lik.back <- child.lik

adult.lik <- adult.var %>%


  mutate(mins=DUR_HVY_CAPPED_SPORTCOUNT_A01+DUR_MOD_CAPPED_SPORTCOUNT_A01) %>%

# 1=strong agree, 5=strong disagree
  dplyr::select(enjoy=Motiva_POP,
         social=motivex2c,
         fit=motivex2a,
         guilt=motivc_POP,
         opp=READYOP1_POP,

         imp=motivb_POP,
         dis=motivd_POP, #disappoint
         abil=READYAB1_POP, #ability
         relx=motivex2b,
         chal=motivex2d,

         dsbl=Disab2_POP,
         gender=Gend3,
         age=AgeTGC,
         eth=Eth2,
         mins
         ) %>%

  filter(dsbl==2,
         if_all(c(gender,eth), ~ .x %in% c(1,2)),
         if_all(everything(), ~ .x > -1),
  ) %>%

  mutate(dis = 6 - dis,
         across(c(abil,chal,enjoy,fit,guilt,imp,opp,relx,dis),
                ~ case_when(.x==5~4L, TRUE ~ as.integer(.x)))
         ) %>%


  dplyr::select(-dsbl)


adult.lik.back <- adult.lik
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




# Part II LCA youths ------------------------------------------------------------
child.lik <- child.lik.back
child.lik <- child.lik %>% dplyr::select(-mins,-age,-gender,-eth)

lca.f.child <- as.matrix(child.lik) ~ 1

# run 1-7 classes
poLCA.child <- list()
for(k in 1:7){
  poLCA.child[[k]] <- poLCA(lca.f.child, data = child.lik,
                              nclass = k,
                              maxiter = 5000,
                              nrep = 10,          # multiple random starts
                              na.rm = TRUE,       # not needed if NAs already removed
                              verbose = TRUE)
}


save(poLCA.child, file="poLCA.child.RData")

lca.stats.child <- data.frame(
  classes = 1:7,
  BIC = sapply(poLCA.child[1:7], function(x) x$bic),
  AIC = sapply(poLCA.child[1:7], function(x) x$aic)
)

# elbow plot
ggplot(lca.stats.child, aes(x = classes)) +
  geom_line(aes(y = BIC), color = "blue") +
  geom_point(aes(y = BIC), color = "blue") +
  geom_line(aes(y = AIC), color = "red") +
  geom_point(aes(y = AIC), color = "red") +
  labs(y = "Information Criterion", x = "Number of Classes",
       title = "Elbow Plot for poLCA Model Selection",
       caption = "Blue = BIC, Red = AIC") +
  theme_minimal()

# pick best model
lca.best.ch <- poLCA.child[[3]]

# check class size (need >0.5 per)
child.lik$class <- lca.best.ch$predclass
table(child.lik$class)  # modal assignment
prop.table(table(child.lik$class))

# entropy, <0.8 good, higher != better
post.ch <- lca.best.ch$posterior
N <- nrow(post.ch)
C <- ncol(post.ch)
entro.ch <- 1 + (1/(N*log(C))) * sum(post.ch * log(post.ch))
entro.ch  # ranges 0–1, higher = better separation

# check each class prob
child.lik$post <- apply(lca.best.ch$posterior, 1, max)
ggplot(child.lik, aes(x = post, fill = factor(class))) +
  geom_histogram(binwidth = 0.05, alpha = 0.7, position = "identity") +
  labs(x = "Max Posterior Probability", y = "Count", fill = "Class") +
  theme_minimal()

# boxplot per class
ggplot(child.lik, aes(x = factor(class), y = post)) +
  geom_boxplot(fill = "skyblue") +
  labs(x = "Class", y = "Max Posterior Probability") +
  theme_minimal()

# Weighted means
child.lik$mins <- child.lik.back$mins
child.lik$age <- child.lik.back$age
child.lik$gender <- child.lik.back$gender
child.lik$eth <- child.lik.back$eth
child.lik$mins.log <- log(child.lik$mins+1)

# probability weighted means
wmeans.child <- colSums(post.ch * child.lik$mins) / colSums(post.ch)
wmeans.log.child <- colSums(post.ch * child.lik$mins.log) / colSums(post.ch)


wsd.child <- sapply(1:ncol(post.ch), function(k) {
  sqrt( sum(post.ch[,k] * (child.lik$mins - wmeans.child[k])^2)
        / sum(post.ch[,k]) )
})

wsd.log.child <- sapply(1:ncol(post.ch), function(k) {
  sqrt( sum(post.ch[,k] * (child.lik$mins.log - wmeans.log.child[k])^2)
        / sum(post.ch[,k]) )
})


mins.child <- data.frame(
  Class = factor(1:ncol(post.ch)),
  Mean = wmeans.child,
  Mean.log= exp(wmeans.log.child),
  SD =wsd.child,
  SD.log = wsd.log.child,
  SD.low = exp(wmeans.log.child - wsd.log.child) - 1,
  SD.up = exp(wmeans.log.child + wsd.log.child) - 1
)

mins.child

ggplot(mins.child, aes(x = Class, y = Mean.log)) +
  geom_col() +
  labs(x = "Latent Class", y = "Probability-weighted mean minutes")


# Part II Child Regression ------------------------------------------------
# AGE
fit.ch.age <- multinom(class ~ age,
                       data = child.lik %>% dplyr::select(-post,-mins,-eth,-gender))
summary(fit.ch.age)
# Odds ratios for easier interpretation
exp(coef(fit.ch.age))

# ETHNICITY
fit.ch.eth <- multinom(class ~ eth,
                       data = child.lik %>% dplyr::select(-post,-mins,-age,-gender))
summary(fit.ch.eth)
# Odds ratios for easier interpretation
exp(coef(fit.ch.eth))

# GENDER
fit.ch.gender <- multinom(class ~ gender,
                          data = child.lik %>% dplyr::select(-post,-mins,-eth,-age))
summary(fit.ch.gender)
# Odds ratios for easier interpretation
exp(coef(fit.ch.gender))

or.ch <- cbind(exp(coef(fit.ch.age)),exp(coef(fit.ch.eth)),exp(coef(fit.ch.gender)))


# Part II LCA adults  -----------------------------------------------------------

adult.lik <- adult.lik.back
adult.lik <- adult.lik %>% dplyr::select(-mins,-age,-gender,-eth)

lca.f.adult <- as.matrix(adult.lik) ~ 1
# run 2-7 classes
poLCA.adult <- list()
for(k in 1:7){
  poLCA.adult[[k]] <- poLCA(lca.f.adult, data = adult.lik,
                            nclass = k,
                            maxiter = 5000,
                            nrep = 10,          # multiple random starts
                            na.rm = TRUE,       # not needed if NAs already removed
                            verbose = TRUE)
}

save(poLCA.adult, file="poLCA.adult.RData")

lca.stats.adult <- data.frame(
  Classes = 1:7,
  BIC = sapply(poLCA.adult[1:7], function(x) x$bic),
  AIC = sapply(poLCA.adult[1:7], function(x) x$aic)
)

# elbow plot
ggplot(lca.stats.adult, aes(x = Classes)) +
  geom_line(aes(y = BIC), color = "blue") +
  geom_point(aes(y = BIC), color = "blue") +
  geom_line(aes(y = AIC), color = "red") +
  geom_point(aes(y = AIC), color = "red") +
  labs(y = "Information Criterion", x = "Number of Classes",
       title = "Elbow Plot for poLCA Model Selection",
       caption = "Blue = BIC, Red = AIC") +
  theme_minimal()


# INDIVIDUAL CLASSES
# 3, 5, or 6
lca.best.ad <- poLCA.adult[[3]]

# check class size (>0.5 per)
adult.lik$class <- lca.best.ad$predclass
# table(adult.lik$class)  # modal assignment
prop.table(table(adult.lik$class))

# # posterior prob
# max.prob.ad <- apply(lca.best.ad$posterior, 1, max)

# entropy, <0.8 good, higher != better
post.ad <- lca.best.ad$posterior
N <- nrow(post.ad)
C <- ncol(post.ad)
entro.ad <- 1 + (1/(N*log(C))) * sum(post.ad * log(post.ad))
entro.ad  # ranges 0–1, higher = better separation



# check each class prob
adult.lik$post <- apply(lca.best.ad$posterior, 1, max)
ggplot(adult.lik, aes(x = post, fill = factor(class))) +
  geom_histogram(binwidth = 0.05, alpha = 0.7, position = "identity") +
  labs(x = "Max Posterior Probability", y = "Count", fill = "Class") +
  theme_minimal()

# boxplot per class
ggplot(adult.lik, aes(x = factor(class), y = post)) +
  geom_boxplot(fill = "skyblue") +
  labs(x = "Class", y = "Max Posterior Probability") +
  theme_minimal()

# interpret
poLCA.adult[[3]]$probs
# poLCA.adult[[4]]$probs
poLCA.adult[[5]]$probs
#


# Weighted means
adult.lik$mins <- adult.lik.back$mins
adult.lik$age <- adult.lik.back$age
adult.lik$gender <- adult.lik.back$gender
adult.lik$eth <- adult.lik.back$eth

adult.lik$mins.log <- log(adult.lik$mins+1)

# probability weighted means
wmeans.adult <- colSums(post.ad * adult.lik$mins) / colSums(post.ad)
wmeans.log.adult <- colSums(post.ad * adult.lik$mins.log) / colSums(post.ad)


wsd.adult <- sapply(1:ncol(post.ad), function(k) {
  sqrt( sum(post.ad[,k] * (adult.lik$mins - wmeans.adult[k])^2)
        / sum(post.ad[,k]) )
})

wsd.log.adult <- sapply(1:ncol(post.ad), function(k) {
  sqrt( sum(post.ad[,k] * (adult.lik$mins.log - wmeans.log.adult[k])^2)
        / sum(post.ad[,k]) )
})


mins.adult <- data.frame(
  Class = factor(1:ncol(post.ad)),
  Mean = wmeans.adult,
  Mean.log= exp(wmeans.log.adult)-1,
  SD =wsd.adult,
  SD.log = wsd.log.adult,
  SD.low = exp(wmeans.log.adult - wsd.log.adult) - 1,
  SD.up = exp(wmeans.log.adult + wsd.log.adult) - 1
)

mins.adult

ggplot(mins.adult, aes(x = Class, y = Mean.log)) +
  geom_col() +
  labs(x = "Latent Class", y = "Probability-weighted mean minutes")

#
#
# # Lieklihood
#
# mod_null <- poLCA.adult[[6]]
# mod_alt <- poLCA.adult[[7]]
#
# # store values baseline model
# n <- mod_null$Nobs #number of observations (should be equal in both models)
# null_ll <- mod_null$llik #log-likelihood
# null_param <- mod_null$npar # number of parameters
# null_classes <- length(mod_null$P) # number of classes
#
# # Store values alternative model
# alt_ll <- mod_alt$llik #log-likelihood
# alt_param <- mod_alt$npar # number of parameters
# alt_classes <- length(mod_alt$P) # number of classes
#
# # use calc_lrt from tidyLPA package
# calc_lrt(n, null_ll, null_param, null_classes, alt_ll, alt_param, alt_classes)
#


# Part II Adults Regression --------------------------------------------------------------
# AGE
fit.ad.age <- multinom(class ~ age,
                data = adult.lik %>% dplyr::select(-post,-mins,-eth,-gender))
summary(fit.ad.age)
# Odds ratios for easier interpretation
exp(coef(fit.ad.age))

# ETHNICITY
fit.ad.eth <- multinom(class ~ eth,
                       data = adult.lik %>% dplyr::select(-post,-mins,-age,-gender))
summary(fit.ad.eth)
# Odds ratios for easier interpretation
exp(coef(fit.ad.eth))

# GENDER
fit.ad.gender <- multinom(class ~ gender,
                       data = adult.lik %>% dplyr::select(-post,-mins,-eth,-age))
summary(fit.ad.gender)
# Odds ratios for easier interpretation
exp(coef(fit.ad.gender))

or.ad <- cbind(exp(coef(fit.ad.age)),exp(coef(fit.ad.eth)),exp(coef(fit.ad.gender)))

# Interpretation Child ----------------------------------------------------------

probs.child <- poLCA.child[[3]]$probs
# Convert list to a long data frame: one row per variable-response-class
probs.child.l <- lapply(names(probs.child), function(var) {
  mat <- probs.child[[var]]
  df <- as.data.frame(mat)
  df$Response <- rownames(mat)
  df$Variable <- var
  df
}) %>%
  bind_rows() %>%
  pivot_longer(
    cols = starts_with("Response"),
    names_to = "Type",
    values_to = "Class"
  )


class1.ch <- probs.child.l %>% filter(Class == "class 1: ") %>%
  dplyr::select(-Type,-Class) %>%
  arrange(Variable) %>%
  relocate(Variable, 'Pr(1)','Pr(2)','Pr(3)','Pr(4)')

class2.ch <- probs.child.l %>% filter(Class == "class 2: ") %>%
  dplyr::select(-Type,-Class) %>%
  arrange(Variable) %>%
  relocate(Variable, 'Pr(1)','Pr(2)','Pr(3)','Pr(4)')

class3.ch <- probs.child.l %>% filter(Class == "class 3: ") %>%
  dplyr::select(-Type,-Class) %>%
  arrange(Variable) %>%
  relocate(Variable, 'Pr(1)','Pr(2)','Pr(3)','Pr(4)')

class1.ch
class2.ch
class3.ch

or.ch
# Interpretation Adults ---------------------------------------------------


probs.adult <- poLCA.adult[[3]]$probs
# Convert list to a long data frame: one row per variable-response-class
probs.adult.l <- lapply(names(probs.adult), function(var) {
  mat <- probs.adult[[var]]
  df <- as.data.frame(mat)
  df$Response <- rownames(mat)
  df$Variable <- var
  df
}) %>%
  bind_rows() %>%
  pivot_longer(
    cols = starts_with("Response"),
    names_to = "Type",
    values_to = "Class"
  )


class1.ad <- probs.adult.l %>% filter(Class == "class 1: ") %>%
  dplyr::select(-Type,-Class) %>%
  arrange(Variable) %>%
  relocate(Variable, 'Pr(1)','Pr(2)','Pr(3)','Pr(4)')

class2.ad <- probs.adult.l %>% filter(Class == "class 2: ") %>%
  dplyr::select(-Type,-Class) %>%
  arrange(Variable) %>%
  relocate(Variable, 'Pr(1)','Pr(2)','Pr(3)','Pr(4)')

class3.ad <- probs.adult.l %>% filter(Class == "class 3: ") %>%
  dplyr::select(-Type,-Class) %>%
  arrange(Variable) %>%
  relocate(Variable, 'Pr(1)','Pr(2)','Pr(3)','Pr(4)')

class1.ad
class2.ad
class3.ad

or.ad
