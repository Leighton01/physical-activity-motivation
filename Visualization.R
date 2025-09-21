set.seed(2025)
library(tidyverse)
library(ggplot2)
library(poLCA)
library(poLCAExtra)

# SEM ---------------------------------------------------------------------
# slope_youth - slope_adult, pooled sd
cohen <- rbind(cohen.enj, cohen.soc, cohen.fit,cohen.glt,cohen.opp)
rownames(cohen) <- c("Enjoy", "Social", "Fit","Guilt","Opp")
colnames(cohen) <- c("Std Eff", "Min")
cohen
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
  theme_minimal()
gg.elbow.ch

gg.llik.ch <- ggplot(ch.lca.output, aes(x = nclass)) +
  geom_line(aes(y = llike), color = "blue") +
  geom_point(aes(y = llike), color = "blue") +
  labs(y = "Log-Likelihood", x = "Number of Classes",
       title = "Log-Likelihood, Youths") +
  theme_minimal()

gg.llik.ch
#
# # Max posterior
# gg.post.his.ch <- ggplot(child.lik, aes(x = post, fill = factor(class))) +
#   geom_histogram(binwidth = 0.05, alpha = 0.7, position = "identity") +
#   labs(x = "Max Posterior Probability", y = "Count", fill = "Class",
#        title = paste0(k," Classes, Youths")) +
#   theme_minimal()
# gg.post.his.ch
#
# # Boxplot
# gg.post.box.ch <- ggplot(child.lik, aes(x = factor(class), y = post)) +
#   geom_boxplot(fill = "skyblue") +
#   labs(x = "Class", y = "Max Posterior Probability",
#        title = paste0(k," Classes, Youths")) +
#   theme_minimal()


# class,size/proportion, average pp,entropy

tb.class3.ch <- data.frame(
  Class = 1:ncol(post3.ch),
  Proportion = as.numeric(class.size3.ch),
  Avg_Posterior = round(ave.pp3.ch, 3)
)

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

gg.mins.ch <- ggplot(mins.child, aes(x = factor(Class), y = Weighted.Median)) +
  geom_point(size = 3, color = "blue") +                 # median as a point
  geom_errorbar(aes(ymin = Weighted.Q25, ymax = Weighted.Q75),
                width = 0.2, color = "darkblue") +      # IQR as error bars
  labs(x = "Class", y = "Minutes (Weighted Median ± IQR)",
       title = "Weighted Median and IQR per Class") +
  theme_minimal()



gg.med.ch <- ggplot(mins.child, aes(x = Class, y = Weighted.Median)) +
  geom_col() +
  labs(x = "Latent Class", y = "Probability-Weighted Median Minutes")

gg.med.ch

# Predictor plot
plot(LCAE.ch, nclass = 2)

# Bootstrap Vuong-Lo-Mendell-Rubin Likelihood Ratio Test
blrt.ch
or.ch

# Appendix
or.ci.ch

# Include actual coeffs in appendix
lca.best.ch$probs

tb.byage.ch

gg.byage.ch <- child.lik %>%
  count(age, class) %>%
  group_by(age) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(aes(x = factor(age), y = prop, fill = factor(class))) +
  geom_col() +
  labs(x = "Age group", y = "Proportion", fill = "Class") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal()

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
  theme_minimal() +
  theme(legend.position = "bottom")

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
  theme_minimal()

gg.elbow.ad

gg.llik.ad <- ggplot(ad.lca.output, aes(x = nclass)) +
  geom_line(aes(y = llike), color = "blue") +
  geom_point(aes(y = llike), color = "blue") +
  labs(y = "Log-Likelihood", x = "Number of Classes",
       title = "Log-Likelihood, Adults") +
  theme_minimal()

gg.llik.ad

# # Max posterior
# gg.post.his.ad <- ggplot(adult.lik, aes(x = post, fill = factor(class))) +
#   geom_histogram(binwidth = 0.05, alpha = 0.7, position = "identity") +
#   labs(x = "Max Posterior Probability", y = "Count", fill = "Class",
#        title = paste0(k," Classes, Adults")) +
#   theme_minimal()
# gg.post.his.ad
#
# # Boxplot
# gg.post.box.ad <- ggplot(adult.lik, aes(x = factor(class), y = post)) +
#   geom_boxplot(fill = "skyblue") +
#   labs(x = "Class", y = "Max Posterior Probability",
#        title = paste0(k," Classes, Adults")) +
#   theme_minimal()


# class,size/proportion, average pp,entropy

tb.class3.ad <- data.frame(
  Class = 1:ncol(post3.ad),
  Proportion = as.numeric(class.size3.ad),
  Avg_Posterior = round(ave.pp3.ad, 3)
)

tb.class3.ad

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
  theme_minimal()
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
blrt.ad
or.ad

or.ci.ad


# Include actual coeffs in appendix
lca.best.ad$probs

tb.byage.ad

gg.byage.ad <- adult.lik %>%
  count(age, class) %>%
  group_by(age) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(aes(x = factor(age), y = prop, fill = factor(class))) +
  geom_col() +
  labs(x = "Age group", y = "Proportion", fill = "Class") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal()

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
  theme_minimal() +
  theme(legend.position = "bottom")
