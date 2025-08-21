# NEXT STEPS --------------------------------------------------------------

# DONE 1A. FURTHER TRIM DATA SO IT ONLY CONTAINS VARS I WANT
# 2. WRITE IN WORDS WHAT I WANT TO TEST, FROM SIMPLEST UP
# DONE run free model
# DONE all constrained > check anova
# DONE constrainted one by one > check anova

# DONE 2a. check literature on effects



# 3. Respec models with mediating effects of age and gender based on lit
  # age has effect on enjoy, guilt, opp, fitness
  # gender has effect on guilt, enjoy
# 4.




# ???LAST After rnning model on full data, run on subsamples
# focus on measurement invariance, fit, and robustness checks.

# Libraries ---------------------------------------------------------------
library(tidyverse)
library(lavaan)

# Duckdb (not workin) -----------------------------------------------------

#
# # Create a database driver
# # drv <- duckdb(tempfile(fileext = ".duckdb"))
# drv <- duckdb("my_database.duckdb", read_only = TRUE)
# # Create a database connection object
# con <- dbConnect(drv)
#
# dbExecute(con, "SET memory_limit = '14GB';")
# dbExecute(con, "SET threads=4;")
# dbExecute(con, "SET temp_directory = '/temp_dir.tmp/';")
# dbExecute(con, "SET preserve_insertion_order=false;")
#
# dbExecute(con, "
#   CREATE TABLE 'tbl.child' AS
#   SELECT * FROM read_csv_auto('data/child_main.tab', delim='\t')
# ")
#
# # dbExecute(con, "
# #   CREATE TABLE 'tbl.adult' AS
# #   SELECT * FROM read_csv_auto('data/adult.tab', delim='\t')
# # ")
#
# # duckdb_read_csv(con, "tbl.child", "data/child_main.tab",
# #                 delim = "\t")
# #
# # duckdb_read_csv(con, "tbl.adult", "data/adult.tab",
# #                 delim = "\t")
#
# # dbGetQuery(con, "SHOW TABLES")
# #
# # dbGetQuery(con, "SELECT * FROM tbl.child LIMIT 10")
# #
# #
# #
# # dbDisconnect(con)

# Read & Save ---------------------------------------------------------------

# data.child <- read.csv('data/child_main.tab', header=T, sep='\t')
# data.adult <- read.csv('data/adult.tab', header=T, sep='\t')
#
child.var <- data.child %>% select("xsurvey_year","age_11",
  "SCHYR_11", "SCHYR_2", "SCHYR_2_2", "SCHYR_3", "SCHYR_5_2",
  "gend3","eth2","eth6", "Disab_All_POP", "Disab_Limit_POP",
  "PL_Enjoy_bc_ans","PL_Conf_bc_ans",
"PL_Easy_bc_ans","PL_GdMe_bc_ans","PL_Know_c_ans","MO_Opp_c","MO_Fit_c",
"MO_Relax_c","MO_Fun_c","MO_Guilt_c","MO_Haveto_b_36","MO_Haveto_c_711",
"Try_bc","outdoor_bcd_Overall","Exeramt_bc","ExeramtMore_bc1_2",
"ExeramtMore_bc2_2","ExeramtMore_bc3_2","DAYSNUMMPWK_inschool",
"DAYSNUMMPWK_outschool","DAYSNUMMPWK_everywhere",
"mins_modplus_everywhere_Week_ALL","mins_modplus_everywhere_week_GR_ALL",
"mins_modplus_inschool_Week_ALL","mins_modplus_inschool_week_GR_ALL",
"mins_modplus_outschool_Week_ALL","mins_modplus_outschool_week_GR_ALL",
"mins_modplus_inandoutschool_week_GR2_all",
"mins_allintense_everywhere_Week_ALL",
"mins_allintense_inschool_Week_ALL",
"mins_allintense_outschool_Week_ALL")
# %>%
  # filter(if_any(everything(),~ .x >= 0))

save(child.var, file = "child.var.RData")


adult.var <- data.adult %>% select('Age16plus','Age19plus','Age17',
                                   'Age3',
                                   'Age4',
                                   'Age5',
                                   'Age5_2',
                                   'Age9',
                                   'AgeTGC',
                                   "Eth2",
                                   "Eth7",
                                   'GendMaternity',
                                   'GendMaternityAge2',
                                   'gend2_GR2',
                                   'gend2_GR6',
                                   'Disab2_POP',
                                   'Disab3',
                                   'Gend3',
                                   'Motiva_POP',
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
                                   'Motiva_POP_GR2',
                                   'motivb_POP_GR2',
                                   'motivc_POP_GR2',
                                   'motivd_POP_GR2',
                                   'motive_POP_GR2',
                                   'READYAB1_POP_GR2',
                                   'READYOP1_POP_GR2',
                                   'motivex2a_GR2',
                                   'motivex2b_GR2',
                                   'motivex2c_GR2',
                                   'motivex2d_GR2',
                                   'inclus_a',
                                   'inclus_b',
                                   'inclus_c',
                                   'indev',
                                   'indevtry',
                                   'workactlvl',
                                   'MEMS7_SPORTCOUNT_A01',
                                   'MEMS7GR30MIN_SPORTCOUNT_A01',
                                   'MEMS7GR7_SPORTCOUNT_A01',
                                   'ACT7GR_SPORTCOUNT_A01',
                                   'DAYS10P_SPORTCOUNT_A01',
                                   'DAYS10P60_SPORTCOUNT_A01',
                                   'DAYS10P60GR_SPORTCOUNT_A01',
                                   'DUR_HVY_SPORTCOUNT_A01',
                                   'DUR_MOD_SPORTCOUNT_A01',
                                   'DUR_LHT_SPORTCOUNT_A01',
                                   'DUR_HVY_CAPPED_SPORTCOUNT_A01',
                                   'DUR_MOD_CAPPED_SPORTCOUNT_A01',
                                   'DUR_LHT_CAPPED_SPORTCOUNT_A01',
                                   'DURATION_SPORTCOUNT_A01',
                                   'DURATION1PL_SPORTCOUNT_A01',
                                   'DURATIONGR_SPORTCOUNT_A01',
                                   'FREQUENCY_SPORTCOUNT_A01',
                                   'FREQUENCY_CAPPED_SPORTCOUNT_A01',
                                   'FREQUENCYGR_SPORTCOUNT_A01',
                                   'FREQUENCYGR2_SPORTCOUNT_A01',
                                   'FREQUENCY_GR4_SPORTCOUNT_A01',
                                   'FREQUENCY_GR6_SPORTCOUNT_A01',
                                   'MINS_SESS_SPORTCOUNT_A01',
                                   'MINS_SESS_CAPPED_SPORTCOUNT_A01',
                                   'Mins_Sess_GR4_SPORTCOUNT_A01',
                                   'Mins_Sess_GR5min_SPORTCOUNT_A01',
                                   'Number_Activities',
                                   'Number_Activities_CAPPED',
                                   'Number_Activities_Gr2',
                                   'Number_Activities_Gr3',
                                   'Number_Activities_Gr5',
                                   'Number_Activities_150',
                                   'Number_Activities_150_Gr2',
                                   'Number_Activities_150_Gr5',
                                   'DAYS10P_SPORTFUND_A02',
                                   'DUR_HVY_SPORTFUND_A02',
                                   'DUR_LHT_SPORTFUND_A02',
                                   'DUR_MOD_SPORTFUND_A02',
                                   'DURATION_SPORTFUND_A02',
                                   'DURATION1PL_SPORTFUND_A02',
                                   'DURATIONGR_SPORTFUND_A02',
                                   'FREQUENCY_SPORTFUND_A02',
                                   'FREQUENCYGR_SPORTFUND_A02',
                                   'FREQUENCYGR2_SPORTFUND_A02',
)

save(adult.var, file = "adult.var.RData")


# Load & Check --------------------------------------------------------------------
load("child.var.RData")
load("adult.var.RData")

glimpse(child.var)
glimpse(adult.var)

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


# Filter & Trim & Combine  ------------------------------------------------------

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
              mins_modplus_outschool_Week_ALL
              ), ~ .x > -1)) %>%

    select(
           enjoy=PL_Enjoy_bc_ans,
           social=MO_Fun_c, #> 11yo only
           fit=MO_Fit_c,
           # try=Try_bc, #adult no data
           guilt=MO_Guilt_c,
           # know=PL_GdMe_bc_ans, #adult no data
           opp=MO_Opp_c, # > 11yo only
           gender=gend3,
           age=age_11,
           eth=eth2,
           mins=mins_modplus_outschool_Week_ALL #this or all intensity?

    )



adult.trim <- adult.var %>% filter(Disab2_POP==2,
                                   Gend3 %in% c(1,2),
                                   Eth2 %in% c(1,2),
                                   if_all(c(Motiva_POP,motivex2c,motivex2a,
                                            motivc_POP,READYOP1_POP,
                                            Gend3,Age17,
                                            DUR_MOD_SPORTCOUNT_A01,
                                            DUR_HVY_SPORTCOUNT_A01
                                            ), ~ .x > -1)) %>%
  mutate(mins=DUR_HVY_CAPPED_SPORTCOUNT_A01+DUR_MOD_CAPPED_SPORTCOUNT_A01) %>%
         # ,eth=ifelse(Eth7==5, 3,
         #            ifelse(Eth7==6, 5,
         #                   ifelse(Eth7==7, 6, Eth7))))


  select(
         enjoy=Motiva_POP,
         social=motivex2c,
         fit=motivex2a,
         # try=indevtry, #unsure why so many NAs, not included
         guilt=motivc_POP,
         # know=motive_POP, #unsure why so many NAs, not included
         opp=READYOP1_POP,
         gender=Gend3,
         # gender2=gend2_GR6,
         age=Age17,
         eth=Eth2,
         mins
         )

dall <- bind_rows(
  adult.trim %>% mutate(group = "adult"),
  child.trim %>% mutate(group = "youth")
) %>%
  mutate(across(c(enjoy, social, fit, opp, guilt), ~ 6 - .),
         eth = as.factor(eth),
         gender = as.factor(gender),
         mins = ifelse(mins > 1680, 1680, mins))%>%
  group_by(group) %>%
  mutate(
    enjoy_r  = (enjoy - min(enjoy, na.rm=TRUE)) / (max(enjoy, na.rm=TRUE) - min(enjoy, na.rm=TRUE)),
    guilt_r  = (guilt - min(guilt, na.rm=TRUE)) / (max(guilt, na.rm=TRUE) - min(guilt, na.rm=TRUE)),
    opp_r    = (opp - min(opp, na.rm=TRUE)) / (max(opp, na.rm=TRUE) - min(opp, na.rm=TRUE)),
    fit_r    = (fit - min(fit, na.rm=TRUE)) / (max(fit, na.rm=TRUE) - min(fit, na.rm=TRUE)),
    social_r = (social - min(social, na.rm=TRUE)) / (max(social, na.rm=TRUE) - min(social, na.rm=TRUE)),
    # mins_r = (mins - min(mins, na.rm=TRUE)) / (max(mins, na.rm=TRUE) - min(mins, na.rm=TRUE)),
    age_r = (age - min(age, na.rm=TRUE)) / (max(age, na.rm=TRUE) - min(age, na.rm=TRUE))
    ) %>%
  ungroup()

dall$mins_r <- (dall$mins - min(dall$mins, na.rm=TRUE)) / (max(dall$mins, na.rm=TRUE) - min(dall$mins, na.rm=TRUE))
# # Standardize age globally
# dall$age_r <- scale(dall$age)[,1]





# Check basic SEM ---------------------------------------------------------------------

# test whether predictors influence mins separately for each group

m0 <- '
  # Mediators: age and gender predicting motives, controlling for ethnicity
  enjoy_r ~ age_r + gender + eth
  guilt ~ age_r + gender + eth
  opp_r ~ age_r + eth
  fit_r ~ age_r + eth
  social_r ~ eth

  # Main outcome: motives predicting mins, controlling for ethnicity
  mins_r ~ enjoy_r +guilt_r +opp_r +fit_r +social_r +eth
'

f0 <- sem(m0, data = dall, group = "group")
summary(f0, fit.measures = TRUE, standardized = TRUE)

# Constrain all to be equal
f.con <- sem(m0, dall, group = "group",
               group.equal = c("intercepts", "regressions"))
summary(f.con)

anova(fit, fit.con) #significantly different, great sign

# Try some constraints..

f1 <- sem(m1, data = dall, group = "group", meanstructure = TRUE)
f2 <- sem(m2, data = dall, group = "group", meanstructure = TRUE)
f3 <- sem(m3, data = dall, group = "group", meanstructure = TRUE)
f4 <- sem(m4, data = dall, group = "group", meanstructure = TRUE)
f5 <- sem(m5, data = dall, group = "group", meanstructure = TRUE)

# ALL sig dfiferent, none should be constrained/equal :) cool
anova(f0, f1)
anova(f0, f2)
anova(f0, f3)


# Complete model -------------------------------------------------------------
model0 <- '

  # Mediators: age and gender predicting motives, controlling for ethnicity
  enjoy ~ c(a1_adult, a1_child)*age + c(a2_adult, a2_child)*gender + eth
  guilt ~ c(b1_adult, b1_child)*age + c(b2_adult, b2_child)*gender + eth
  opp ~ c(c1_adult, c1_child)*age + eth
  fit ~ c(d1_adult, d1_child)*age + eth
  social ~ eth

  # Main outcome: motives predicting mins, controlling for ethnicity
  mins ~ c(e1_adult, e1_child)*enjoy +
         c(e2_adult, e2_child)*guilt +
         c(e3_adult, e3_child)*opp +
         c(e4_adult, e4_child)*fit +
         c(e5_adult, e5_child)*social +
         eth

  # Indirect effects via age
  age_indirect_adult  := a1_adult*e1_adult + b1_adult*e2_adult
                          + c1_adult*e3_adult + d1_adult*e4_adult
  age_indirect_child  := a1_child*e1_child + b1_child*e2_child
                          + c1_child*e3_child + d1_child*e4_child

  # Indirect effects via gender
  gender_indirect_adult := a2_adult*e1_adult + b2_adult*e2_adult
  gender_indirect_child := a2_child*e1_child + b2_child*e2_child

  # Total effects
  age_total_adult   := age_indirect_adult
  age_total_child   := age_indirect_child
  gender_total_adult := gender_indirect_adult
  gender_total_child := gender_indirect_child
'





model <- '

  # Mediators: age and gender predicting motives, controlling for ethnicity
  enjoy_r ~ c(a1_adult, a1_child)*age_r + c(a2_adult, a2_child)*gender + eth
  guilt_r ~ c(b1_adult, b1_child)*age_r + c(b2_adult, b2_child)*gender + eth
  opp_r ~ c(c1_adult, c1_child)*age_r + eth
  fit_r ~ c(d1_adult, d1_child)*age_r + eth
  social_r ~ eth

  # Main outcome: motives predicting mins, controlling for ethnicity
  mins_r ~ c(e1_adult, e1_child)*enjoy_r +
         c(e2_adult, e2_child)*guilt_r +
         c(e3_adult, e3_child)*opp_r +
         c(e4_adult, e4_child)*fit_r +
         c(e5_adult, e5_child)*social_r +
         eth

  # Indirect effects via age
  age_indirect_adult  := a1_adult*e1_adult + b1_adult*e2_adult
                          + c1_adult*e3_adult + d1_adult*e4_adult
  age_indirect_child  := a1_child*e1_child + b1_child*e2_child
                          + c1_child*e3_child + d1_child*e4_child

  # Indirect effects via gender
  gender_indirect_adult := a2_adult*e1_adult + b2_adult*e2_adult
  gender_indirect_child := a2_child*e1_child + b2_child*e2_child

  # Total effects
  age_total_adult   := age_indirect_adult
  age_total_child   := age_indirect_child
  gender_total_adult := gender_indirect_adult
  gender_total_child := gender_indirect_child
'
fit0 <- sem(model0, data = dall, group = "group")
fit <- sem(model, data = dall, group = "group")  # group = adult vs child
summary(fit0, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)
summary(fit, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)
