################### Data Cleaning ###################

# Library -----------------------------------------------------------------
set.seed(2025)
library(tidyverse)
library(car)

# Read Data ---------------------------------------------------------------


# data.child <- read.csv('data/child_main.tab', header=T, sep='\t')
# data.adult <- read.csv('data/adult.tab', header=T, sep='\t')

# Read relevant fields
# child.var <- data.child %>% select(# likert predictors
                                   # 'PL_Enjoy_bc_ans', 'PL_Conf_bc_ans',
                                   # 'PL_Easy_bc_ans', 'PL_GdMe_bc_ans',
                                   # 'PL_Know_c_ans', 'MO_Opp_c',
                                   # 'MO_Fit_c', 'MO_Relax_c', 'MO_Fun_c',
                                   # 'MO_Guilt_c', 'MO_Haveto_b_36',
                                   # 'MO_Haveto_c_711', 'PR_Fam_c', 'PR_Oth_c',
                                   # 'Try_bc', 'outdoor_bcd_Overall',
                                   # 'Exeramt_bc', 'ExeramtMore_bc1_2',
                                   # 'ExeramtMore_bc2_2', 'ExeramtMore_bc3_2',
                                   # 'mins_modplus_outschool_Week_ALL',
                                   #
                                   # # demographic
                                   # 'age_11', 'eth2', 'gend3', 'eth6',
                                   # 'Disab_All_POP',
                                   #
                                   # # binary predictors
                                   # 'PL_Enjoy_bc_SA_gr2', 'MO_Fun_c_SA',
                                   # 'MO_Fit_c_SA',
                                   # 'MO_Guilt_c_SA', 'MO_Opp_c_SA',
                                   # 'MO_Relax_c_SA'
# )

# Save to save computation time
# save(child.var, file = "child.var.RData")
#

# Same process for adults, different variables
# adult.var <- data.adult %>% dplyr::select('Motiva_POP','motivb_POP',
#                                           'motivc_POP','motivd_POP',
#                                           'motive_POP','READYAB1_POP',
#                                           'READYOP1_POP','motivex2a',
#                                           'motivex2b','motivex2c',
#                                           'motivex2d','inclus_a',
#                                           'inclus_b','inclus_c',
#                                           'indev','indevtry',
#                                           'workactlvl',
#                                           'DUR_HVY_CAPPED_SPORTCOUNT_A01',
#                                           'DUR_MOD_CAPPED_SPORTCOUNT_A01',
#
#                                           # demographic
#                                           'Age17','Age3','AgeTGC',
#                                           'Age4','Age5','Age5_2',
#                                           'Age9','Disab2_POP',
#                                           'Gend3','Eth2','Eth7',
#                                           'Educ6',
#
#                                           # binary predictors
#                                           'Motiva_POP_GR2', 'motivex2c_GR2',
#                                           'motivex2a_GR2', 'motivc_POP_GR2',
#                                           'READYOP1_POP_GR2','motivex2b_GR2')
#
# save(adult.var, file = "adult.var.RData")

# Basic Distributions and Stats -------------------------------------------

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



# Clean Data for SEM ------------------------------------------------------


child.bi <- child.var %>%
  filter(Disab_All_POP == 2, # remove disabled and no answer
         gend3 %in% c(1,2),
         eth2 %in% c(1,2),

         if_all(c(age_11, mins_modplus_outschool_Week_ALL), ~ .x > -1),

         if_all(c(PL_Enjoy_bc_SA_gr2, MO_Fun_c_SA, MO_Fit_c_SA,
                  MO_Guilt_c_SA, MO_Opp_c_SA, MO_Relax_c_SA), ~ .x > -1 & .x < 3)) %>%

  dplyr::select(enjoyb=PL_Enjoy_bc_SA_gr2,
                socialb=MO_Fun_c_SA,
                fitb=MO_Fit_c_SA,
                guiltb=MO_Guilt_c_SA,
                oppb=MO_Opp_c_SA,
                relxb=MO_Relax_c_SA,

                gender=gend3,
                age=age_11,
                eth=eth2,
                mins=mins_modplus_outschool_Week_ALL

  ) %>%


  # change 2 (not strongly agree) to 0, consistent with adult
  mutate(across(c(enjoyb,socialb,fitb,guiltb,oppb,relxb), ~ ifelse(.x==2, 0, .x)),
         gender = gender-1,
         eth = eth-1,
         age = age-11)



adult.bi <- adult.var %>% filter(Disab2_POP==2,
                                 Gend3 %in% c(1,2),
                                 Eth2 %in% c(1,2),
                                 if_all(c(AgeTGC,
                                          DUR_MOD_CAPPED_SPORTCOUNT_A01,
                                          DUR_HVY_CAPPED_SPORTCOUNT_A01),
                                        ~ .x > -1),
                                 if_all(c(Motiva_POP_GR2, motivex2c_GR2,
                                          motivex2a_GR2, motivc_POP_GR2,
                                          READYOP1_POP_GR2, motivex2b_GR2),
                                        ~ .x %in% c(0,1))) %>%


  mutate(mins=DUR_MOD_CAPPED_SPORTCOUNT_A01 +
           DUR_HVY_CAPPED_SPORTCOUNT_A01,
         Gend3 = Gend3-1,
         Eth2 = Eth2-1,
         age = case_when(Age9==2~3L,
                         Age9==9~8L,
                         TRUE~as.integer(Age9)),
         age=as.integer(age-3)
  ) %>%


  dplyr::select(enjoyb=Motiva_POP_GR2,
                socialb=motivex2c_GR2,
                fitb=motivex2a_GR2,
                guiltb=motivc_POP_GR2,
                oppb=READYOP1_POP_GR2,
                relxb=motivex2b_GR2,
                gender=Gend3,
                age,
                eth=Eth2,
                mins
  )

dallb <- bind_rows(
  adult.bi %>% mutate(group = "adult"),
  child.bi %>% mutate(group = "youth")
) %>%
  mutate(mins = ifelse(mins > 1680, 1680, mins))

dallb$gender <- relevel(factor(dallb$gender), ref = "0")
dallb$eth <- relevel(factor(dallb$eth), ref = "0")

# Clean Data for LCA ------------------------------------------------------
# Check which motive responses need to be collapsed
prop.table(table(child.var$PL_Enjoy_bc_ans))
prop.table(table(child.var$MO_Fun_c))
prop.table(table(child.var$MO_Fit_c))
prop.table(table(child.var$MO_Opp_c))
prop.table(table(child.var$MO_Guilt_c))
prop.table(table(child.var$MO_Relax_c))
prop.table(table(child.var$PL_Conf_bc_ans))
prop.table(table(child.var$PL_GdMe_bc_ans))
prop.table(table(child.var$Try_bc))

prop.table(table(adult.var$Motiva_POP))
prop.table(table(adult.var$motivex2c))
prop.table(table(adult.var$motivex2a))
prop.table(table(adult.var$motivc_POP))
prop.table(table(adult.var$READYOP1_POP))
prop.table(table(adult.var$READYAB1_POP))
prop.table(table(adult.var$motivb_POP))
prop.table(table(adult.var$motivex2d))
prop.table(table(adult.var$motivex2b))

child.lik <- child.var %>%

  # 1-4, 1=strong agree, 4=strong disagree, 5=can't say
  dplyr::select(enjoy=PL_Enjoy_bc_ans,
                social=MO_Fun_c,
                fit=MO_Fit_c,
                opp=MO_Opp_c,
                guilt=MO_Guilt_c, #99 instead of 5 for "can't say"
                relx=MO_Relax_c,

                abl=PL_Conf_bc_ans,
                imp=PL_GdMe_bc_ans,
                chal=Try_bc,

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
         if_all(c(enjoy,social,fit,guilt,opp,imp,chal,relx,abl),
                ~ .x > -1 & .x < 5)) %>%
  mutate(
    mins = ifelse(mins > 1680, 1680, mins),
    across(c(enjoy,social,fit,guilt,imp,chal,opp,relx,abl),
           ~ case_when(.x==4~3L, TRUE ~ as.integer(.x))),
    age=age-10


  ) %>%
  dplyr::select(-dsbl)

child.lik.back0 <- child.lik



adult.lik <- adult.var %>%
  mutate(mins=DUR_HVY_CAPPED_SPORTCOUNT_A01+DUR_MOD_CAPPED_SPORTCOUNT_A01) %>%

  # 1=strong agree, 5=strong disagree
  dplyr::select(enjoy=Motiva_POP,
                social=motivex2c,
                fit=motivex2a,
                guilt=motivc_POP,
                opp=READYOP1_POP,

                abl=READYAB1_POP,
                imp=motivb_POP,
                chal=motivex2d,
                relx=motivex2b,

                dsbl=Disab2_POP,
                gender=Gend3,
                age=Age9,
                eth=Eth2,
                # edu=Educ6,
                mins
  ) %>%

  filter(dsbl==2,
         if_all(c(gender,eth), ~ .x %in% c(1,2)),
         if_all(everything(), ~ .x > -1)
         # edu != 5
  ) %>%

  mutate(across(c(enjoy,social,fit,guilt,opp,imp,chal,relx,abl),
                ~ case_when(.x==5~4L, TRUE ~ as.integer(.x))),
         # edu = case_when(edu==6~5L, TRUE~edu),
         age = as.integer(case_when(age==2~3L,
                         age==9~8L,
                         TRUE~as.integer(age)))-2

  ) %>%


  dplyr::select(-dsbl)

adult.lik.back0 <- adult.lik




# Checks -------------------------------------------------------------------
# Collinearity
dallb1 <- dallb %>% dplyr::select(-gender,-eth,-group)
cor(dallb1, method = "pearson")
# opp, fit and enjoy have mod corr with each other, others ok

# Check adult lik corr
cor(child.lik.back0 %>% dplyr::select(-gender,-eth, -age), method = "pearson")
cor(adult.lik.back0 %>% dplyr::select(-gender,-eth,-age), method = "pearson")

# Check sparsity of highly correlated (>.05) items
prop.table(table(child.lik$abl, child.lik$enjoy))

prop.table(table(adult.lik$fit, adult.lik$enjoy))
prop.table(table(adult.lik$imp, adult.lik$enjoy))
prop.table(table(adult.lik$fit, adult.lik$imp))
prop.table(table(adult.lik$abl, adult.lik$opp))


child.lik.back <- child.lik
adult.lik.back <- adult.lik


# VIF
vif_model <- lm(mins ~ enjoyb + socialb + fitb + guiltb + oppb + relxb, data = dallb1)
vif(vif_model)
