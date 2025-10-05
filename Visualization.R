set.seed(2025)
library(tidyverse)
library(ggplot2)
library(poLCA)
library(poLCAExtra)
library(scales)
library(ggthemes)


# Descriptive -------------------------------------------------------------
child.summary.bi <- data.frame(colMeans(
  child.bi[, setdiff(names(child.bi),
                     c("gender", "eth", "age"))], na.rm = TRUE))
colnames(child.summary.bi) <- ("Mean")

adult.summary.bi <- data.frame(colMeans(
  adult.bi[, setdiff(names(adult.bi),
                     c("gender", "eth", "age"))], na.rm = TRUE))
colnames(adult.summary.bi) <- ("Mean")

cor.ie <- cor(adult.lik.back0 %>% dplyr::select(-gender,-eth), method = "pearson")[6,1]
cor.if <- cor(adult.lik.back0 %>% dplyr::select(-gender,-eth), method = "pearson")[6,3]
cor.imp <- data.frame("Imp,Enjoy"=cor.ie, "Imp,Fit"=cor.if)

# get summary of all motives
adult.summary <- adult.var %>%
  mutate(mins = DUR_HVY_CAPPED_SPORTCOUNT_A01+
                    DUR_MOD_CAPPED_SPORTCOUNT_A01) %>%
  dplyr::select(
    Enjoyment = Motiva_POP,
    Social = motivex2c,
    Fitness = motivex2a,
    Guilt = motivc_POP,
    Opportunity = READYOP1_POP,
    Importance = motivb_POP,
    Challenge = motivex2d,
    Relaxation = motivex2b,
    Minutes.Exercised = mins
  ) %>%
  summarise(
    across(everything(),
           list(
             Mean = ~mean(.x[.x > 0], na.rm = TRUE),
             Median = ~median(.x[.x > 0], na.rm = TRUE),
             SD = ~sd(.x[.x > 0], na.rm = TRUE),
             PercentNA = ~mean(.x < 0, na.rm = TRUE) * 100
           ),
           .names = "{.col}_{.fn}"
    )
  ) %>%
  pivot_longer(everything(), names_to = c("Variable", "Stat"), names_sep = "_") %>%
  pivot_wider(names_from = Stat, values_from = value)


child.summary <- child.var %>%
  dplyr::select(
    Enjoyment = PL_Enjoy_bc_ans,
    Social = MO_Fun_c,
    Fitness = MO_Fit_c,
    Opportunity = MO_Opp_c,
    Guilt = MO_Guilt_c,
    Importance = PL_GdMe_bc_ans,
    Challenge = Try_bc,
    Relaxation = MO_Relax_c
  ) %>%
  summarise(
    across(everything(),
           list(
             Mean = ~mean(.x[.x > 0 & .x <= 4], na.rm = TRUE),
             Median = ~median(.x[.x > 0 & .x <= 4], na.rm = TRUE),
             SD = ~sd(.x[.x > 0 & .x <= 4], na.rm = TRUE),
             PercentNA = ~mean(.x < 0 | .x > 4, na.rm = TRUE) * 100
           ),
           .names = "{.col}_{.fn}"
    )
  ) %>%
  pivot_longer(everything(), names_to = c("Variable", "Stat"), names_sep = "_") %>%
  pivot_wider(names_from = Stat, values_from = value)

c.mins <- child.var %>%
  summarise(Variable = "Minutes.Exercised",
            Mean = mean(mins_modplus_outschool_Week_ALL[mins_modplus_outschool_Week_ALL > 0 ], na.rm = TRUE),
            Median = median(mins_modplus_outschool_Week_ALL[mins_modplus_outschool_Week_ALL > 0], na.rm = TRUE),
            SD = sd(mins_modplus_outschool_Week_ALL[mins_modplus_outschool_Week_ALL > 0], na.rm = TRUE),
            PercentNA = mean(mins_modplus_outschool_Week_ALL < 0, na.rm = TRUE) * 100)

child.summary <- rbind(child.summary, c.mins)


# get demographic overview (gender, edu, eth, mins)
# adult
#
# # Disability
# gg.ad.dsbl <- ggplot(adult.var, aes(x = as.factor(Disab2_POP))) +
#   geom_bar() +
#   labs(x = "Disability") +
#   theme_clean()

# Gender
adult.lik$gender <- factor(adult.lik$gender, levels = c(1, 2),
                           labels = c("Male", "Female"))
gg.ad.gend <- ggplot(adult.lik, aes(x = as.factor(gender))) +
  geom_bar() +
  labs(x = "Gender") +
  theme_clean()

# Age
adult.lik$age <- factor(adult.lik$age, levels = c(1,2,3,4,5,6),
                        labels = c("16-34", "35-44", "45-54",
                                   "55-64", "65-74", "75+"))
gg.ad.age <-ggplot(adult.lik, aes(x = as.factor(age))) +
  geom_bar() +
  labs(x = "Age Group") +
  theme_clean()

# Ethnicity
adult.lik$eth <- factor(adult.lik$eth, levels = c(1, 2),
                        labels = c("White British", "Other"))
gg.ad.eth <- ggplot(adult.lik, aes(x = as.factor(eth))) +
  geom_bar() +
  labs(x = "Ethnicity") +
  theme_clean()

# Education
# gg.ad.edu <- ggplot(adult.lik, aes(x = as.factor(edu))) +
#   geom_bar() +
#   labs(x = "Education") +
#   theme_clean()

# YOuths
#
# Disability
# gg.ch.dsbl <- ggplot(child.var, aes(x = as.factor(Disab_All_POP))) +
#   geom_bar() +
#   labs(x = "Disability") +
#   theme_clean()

# Gender
child.lik$gender <- factor(child.lik$gender, levels = c(1, 2),
                           labels = c("Male", "Female"))
gg.ch.gend <- ggplot(child.lik, aes(x = as.factor(gender))) +
  geom_bar() +
  labs(x = "Gender") +
  theme_clean()

# Age
child.lik$age <- factor(child.lik$age, levels = c(1,2,3,4,5,6),
                        labels = c(11,12,13,14,15,16))
gg.ch.age <- ggplot(child.lik, aes(x = as.factor(age))) +
  geom_bar() +
  labs(x = "Age") +
  theme_clean()

# Ethnicity
child.lik$eth <- factor(child.lik$eth, levels = c(1, 2),
                        labels = c("White British", "Other"))
gg.ch.eth <- ggplot(child.lik, aes(x = as.factor(eth))) +
  geom_bar() +
  labs(x = "Ethnicity") +
  theme_clean()


# SEM ---------------------------------------------------------------------
# # slope_youth - slope_adult, pooled sd
# cohen <- rbind(cohen.enj, cohen.soc, cohen.fit,cohen.glt,cohen.opp)
# rownames(cohen) <- c("Enjoy", "Social", "Fit","Guilt","Opp")
# colnames(cohen) <- c("Std Eff", "Min")
# cohen
# LCA Youths---------------------------------------------------------------

# elbow plot
gg.elbow.ch <- ggplot(ch.lca.output, aes(x = nclass)) +
  geom_line(aes(y = BIC), color = "blue") +
  geom_point(aes(y = BIC), color = "blue") +
  geom_line(aes(y = AIC), color = "red") +
  geom_point(aes(y = AIC), color = "red") +
  labs(y = "Information Criterion", x = "Number of Classes",
       title = "Elbow Plot, Youths",
       caption = "Blue = BIC, Red = AIC") +
  theme_clean()
gg.elbow.ch

gg.llik.ch <- ggplot(ch.lca.output, aes(x = nclass)) +
  geom_line(aes(y = llike), color = "blue") +
  geom_point(aes(y = llike), color = "blue") +
  labs(y = "Log-Likelihood", x = "Number of Classes",
       title = "Log-Likelihood, Youths") +
  theme_clean()

gg.llik.ch
#
# # Max posterior
# gg.post.his.ch <- ggplot(child.lik, aes(x = post, fill = factor(class))) +
#   geom_histogram(binwidth = 0.05, alpha = 0.7, position = "identity") +
#   labs(x = "Max Posterior Probability", y = "Count", fill = "Class",
#        title = paste0(k," Classes, Youths")) +
#   theme_clean()
# gg.post.his.ch
#
# # Boxplot
# gg.post.box.ch <- ggplot(child.lik, aes(x = factor(class), y = post)) +
#   geom_boxplot(fill = "skyblue") +
#   labs(x = "Class", y = "Max Posterior Probability",
#        title = paste0(k," Classes, Youths")) +
#   theme_clean()


# class,size/proportion, average pp,entropy

tb.class3.ch <- data.frame(
  Class = 1:ncol(post3.ch),
  Proportion = as.numeric(class.size3.ch),
  Avg_Posterior = round(ave.pp3.ch, 3)
)
tb.class3.ch

tb.class4.ch <- data.frame(
  Class = 1:ncol(post4.ch),
  Proportion = as.numeric(class.size4.ch),
  Avg_Posterior = round(ave.pp4.ch, 3)
)

# Weighted minutes, youths
mins.child <- data.frame(
  Class = 1:n.classes,
  Weighted.Median = wmed.ch,
  Weighted.Q25 = wq25.ch,
  Weighted.Q75 = wq75.ch
)
mins.child

gg.mins.ch <- ggplot(mins.child, aes(x = factor(Class), y = Weighted.Median)) +
  geom_point(size = 3, color = "blue") +                 # median as a point
  geom_errorbar(aes(ymin = Weighted.Q25, ymax = Weighted.Q75),
                width = 0.2, color = "darkblue") +      # IQR as error bars
  labs(x = "Class", y = "Minutes (Weighted Median ± IQR)",
       title = "Weighted Median and IQR per Class") +
  theme_clean()

gg.mins.ch

gg.med.ch <- ggplot(mins.child, aes(x = Class, y = Weighted.Median)) +
  geom_col() +
  labs(x = "Latent Class", y = "Probability-Weighted Median Minutes")

gg.med.ch

# Predictor plot
plot(LCAE.ch, nclass = 2)

# Bootstrap Vuong-Lo-Mendell-Rubin Likelihood Ratio Test
or.ch

# Appendix
or.ci.ch

# Include actual coeffs in appendix
lca.best.ch$probs

tb.byage.ch

gg.byage.ch <- child.lik %>%
  dplyr::count(age, class) %>%
  group_by(age) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(aes(x = factor(age), y = prop, fill = factor(class))) +
  geom_col() +
  labs(x = "Age group", y = "Proportion", fill = "Class") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_clean()

gg.byage.ch

vars.ch <- setdiff(names(child.lik), c("age","mins","post","class",
                                       "gender","eth","edu"))

child.lik_long <- child.lik %>%
  pivot_longer(cols = all_of(vars.ch), names_to = "variable", values_to = "score") %>%
  count(age, variable, score) %>%
  group_by(age, variable) %>%
  mutate(prop = n / sum(n))

gg.vars.ch <- ggplot(child.lik_long, aes(x = factor(age), y = prop, fill = factor(score))) +
  geom_col() +
  facet_wrap(~variable, nrow = 3, ncol = 3) +
  labs(x = "Age group", y = "Proportion", fill = "Score") +
  scale_y_continuous(labels = percent_format()) +
  theme_clean() +
  theme(legend.position = "bottom", axis.text.y = element_text(size = 6))

# LCA Adults --------------------------------------------------------------


# elbow plot
gg.elbow.ad <- ggplot(ad.lca.output, aes(x = nclass)) +
  geom_line(aes(y = BIC), color = "blue") +
  geom_point(aes(y = BIC), color = "blue") +
  geom_line(aes(y = AIC), color = "red") +
  geom_point(aes(y = AIC), color = "red") +
  labs(y = "Information Criterion", x = "Number of Classes",
       title = "Elbow Plot, Adults",
       caption = "Blue = BIC, Red = AIC") +
  theme_clean()

gg.elbow.ad

gg.llik.ad <- ggplot(ad.lca.output, aes(x = nclass)) +
  geom_line(aes(y = llike), color = "blue") +
  geom_point(aes(y = llike), color = "blue") +
  labs(y = "Log-Likelihood", x = "Number of Classes",
       title = "Log-Likelihood, Adults") +
  theme_clean()

gg.llik.ad

# # Max posterior
# gg.post.his.ad <- ggplot(adult.lik, aes(x = post, fill = factor(class))) +
#   geom_histogram(binwidth = 0.05, alpha = 0.7, position = "identity") +
#   labs(x = "Max Posterior Probability", y = "Count", fill = "Class",
#        title = paste0(k," Classes, Adults")) +
#   theme_clean()
# gg.post.his.ad
#
# # Boxplot
# gg.post.box.ad <- ggplot(adult.lik, aes(x = factor(class), y = post)) +
#   geom_boxplot(fill = "skyblue") +
#   labs(x = "Class", y = "Max Posterior Probability",
#        title = paste0(k," Classes, Adults")) +
#   theme_clean()


# class,size/proportion, average pp,entropy

tb.class3.ad <- data.frame(
  Class = 1:ncol(post3.ad),
  Proportion = as.numeric(class.size3.ad),
  Avg_Posterior = round(ave.pp3.ad, 3)
)

tb.class3.ad

tb.class4.ad <- data.frame(
  Class = 1:ncol(post4.ad),
  Proportion = as.numeric(class.size4.ad),
  Avg_Posterior = round(ave.pp4.ad, 4)
)

tb.class4.ad

tb.class5.ad <- data.frame(
  Class = 1:ncol(post5.ad),
  Proportion = as.numeric(class.size5.ad),
  Avg_Posterior = round(ave.pp5.ad, 5)
)

tb.class5.ad

tb.class6.ad <- data.frame(
  Class = 1:ncol(post6.ad),
  Proportion = as.numeric(class.size6.ad),
  Avg_Posterior = round(ave.pp6.ad, 6)
)

tb.class6.ad

mins.adult <- data.frame(
  Class = 1:n.classes,
  Weighted.Median = wmed.ad,
  Weighted.Q25 = wq25.ad,
  Weighted.Q75 = wq75.ad
)

mins.adult

gg.mins.ad <- ggplot(mins.adult, aes(x = factor(Class), y = Weighted.Median)) +
  geom_point(size = 3, color = "blue") +                 # median as a point
  geom_errorbar(aes(ymin = Weighted.Q25, ymax = Weighted.Q75),
                width = 0.2, color = "darkblue") +      # IQR as error bars
  labs(x = "Class", y = "Minutes (Weighted Median ± IQR)",
       title = "Weighted Median and IQR per Class") +
  theme_clean()
gg.mins.ad
#
# # Weighted minutes, youths
# gg.med.ad <- ggplot(mins.adult, aes(x = Class, y = Weighted.Median)) +
#   geom_col() +
#   labs(x = "Latent Class", y = "Probability-Weighted Median Minutes")


# Predictor plot
plot(LCAE.ad, nclass = 2)
# plot(LCAE.ad, nclass = 3)

# Bootstrap Vuong-Lo-Mendell-Rubin Likelihood Ratio Test
# 100 reps
# blrt.ad
or.ad

or.ci.ad


# Include actual coeffs in appendix
lca.best.ad$probs

tb.byage.ad

gg.byage.ad <- adult.lik %>%
  dplyr::count(age, class) %>%
  group_by(age) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(aes(x = factor(age), y = prop, fill = factor(class))) +
  geom_col() +
  labs(x = "Age group", y = "Proportion", fill = "Class") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_clean()

gg.byage.ad

vars.ad <- setdiff(names(adult.lik), c("age","mins","post","class",
                                       "gender","eth","edu"))

adult.lik_long <- adult.lik %>%
  pivot_longer(cols = all_of(vars.ad), names_to = "variable", values_to = "score") %>%
  count(age, variable, score) %>%
  group_by(age, variable) %>%
  mutate(prop = n / sum(n))

gg.vars.ad <- ggplot(adult.lik_long, aes(x = factor(age), y = prop, fill = factor(score))) +
  geom_col() +
  facet_wrap(~variable, nrow = 3, ncol = 3) +
  labs(x = "Age group", y = "Proportion", fill = "Score") +
  scale_y_continuous(labels = percent_format()) +
  theme_clean() +
  theme(legend.position = "bottom", axis.text.y = element_text(size = 6),
        axis.text.x = element_text(size = 6))
gg.vars.ad
