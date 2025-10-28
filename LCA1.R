# Libraries ---------------------------------------------------------------
set.seed(2025)
library(tidyverse)
library(Hmisc)
library(ggplot2)
library(nnet)
library(tidyLPA)
library(poLCA)
library(poLCAExtra)

# LCA, Youths -------------------------------------------------------------

child.lik <- child.lik.back

# Predictors (motives)
child.lik.y <- (child.lik %>%
                           dplyr::select(-mins,-age,-gender,-eth))

child.lik.y <- as.matrix(child.lik.y %>% mutate(across(everything(), as.integer)))

# Spec formula for LCA
lca.f.child <- child.lik.y ~ gender + eth

# Run LCA with 2-7 classes
# LCAE.ch <- poLCA(lca.f.child, data = child.lik, nclass = 2:7)

#### Saved LCA models to save computation time
load("LCAE.ch.RData")

# blrt.ch <- poLCA.blrt(LCAE.ch,quick = T, nrep=5)
# save(blrt.ch,file="blrt.ch.RData")

# #### Saved blrt results to save computation time
load("blrt.ch.RData")

# Output
ch.lca.output <- LCAE.ch$output %>% dplyr::select(nclass,llike,AIC,BIC,
                                                  Rel.Entropy,LMR,p)

# Compare 3 and 4 class average posterior and class prop
post4.ch <- LCAE.ch$LCA[[3]]$posterior
class4.ch <- apply(post4.ch, 1, which.max)
class.size4.ch <- prop.table(table(class4.ch))

ave.pp4.ch <- sapply(1:ncol(post4.ch), function(k) {
  inds <- which(class4.ch == k)
  mean(post4.ch[inds, k])
})

post3.ch <- LCAE.ch$LCA[[2]]$posterior
class3.ch <- apply(post3.ch, 1, which.max)
class.size3.ch <- prop.table(table(class3.ch))

ave.pp3.ch <- sapply(1:ncol(post3.ch), function(k) {
  inds <- which(class3.ch == k)
  mean(post3.ch[inds, k])
})

# BEST CLASS
lca.best.ch <- LCAE.ch$LCA[[2]]
child.lik$class <- lca.best.ch$predclass

# Calculate median minutes
n.classes <- 3

wmed.ch <- numeric(n.classes)
wq25.ch <- numeric(n.classes)
wq75.ch <- numeric(n.classes)
for (k in 1:n.classes) {

  q <- wtd.quantile(child.lik$mins,
                    weights = lca.best.ch$posterior[,k],
                    probs = c(0.25, 0.5, 0.75))
  wq25.ch[k] <- q[1]
  wmed.ch[k] <- q[2]
  wq75.ch[k] <- q[3]
}

# Regressions
child.lik$age <- child.lik.back$age
child.lik$class <- relevel(factor(child.lik$class), ref = "1")
child.lik$age <- relevel(factor(child.lik$age), ref = "1")

fit.ch <- multinom(class ~ age,
                   data = child.lik)

# odds ratio
or.ch <- exp(coef(fit.ch))
colnames(or.ch) <- c("11 (ref)", "12", "13", "14", "15", "16")
rownames(or.ch) <- c("Low Motivation","Moderate Motivation")

sum.fit.ch <- summary(fit.ch)
se <- sum.fit.ch$standard.errors
# Coefficients
coefs.ch <- coef(fit.ch)

# 95% CI for odds ratios
ci.l.ch <- exp(coefs.ch - 1.96 * se)
ci.u.ch <- exp(coefs.ch + 1.96 * se)

# Odds ratios themselves
or <- exp(coefs.ch)

# Combine into a table
or.ci.ch <- data.frame(
  CI.lower = round(ci.l.ch, 3),
  CI.upper = round(ci.u.ch, 3)
)
colnames(or.ci.ch) <- c("Intercept.L", "Age2.L", "Age3.L", "Age4.L",
                        "Age5.L","Age6.L","Intercept.U","Age2.U", "Age3.U", "Age4.U",
                        "Age5.U","Age6.U")

# Check class distribution per age

tb.byage.ch <- child.lik %>%
count(age, class) %>%
  pivot_wider(names_from = class, values_from = n, values_fill = 0)

# LCA, Adults -------------------------------------------------------------

adult.lik <- adult.lik.back
# Predictors (motives)
adult.lik.y <- as.matrix(adult.lik %>%
                           dplyr::select(-mins,-age,-gender,-eth))

# Spec formula for LCA
lca.f.adult <- adult.lik.y ~ gender + eth

# LCAE.ad <- poLCA(lca.f.adult, data = adult.lik, nclass = 2:7)
# save(LCAE.ad, file="LCAE.ad.RData")

#### Saved LCA models to save computation time
load(file="LCAE.ad.RData")

## BLRT
# blrt.ad <- poLCA.blrt(LCAE.ad, quick = T,nreps = 5)

#### Saved blrt results to save computation time
load("blrt.ad.RData")

# Take relevant stats
ad.lca.output <- LCAE.ad$output %>% dplyr::select(nclass,llike,AIC,BIC,
                                                  Rel.Entropy,LMR,p)

# Compare class average posteriors and class prop

post6.ad <- LCAE.ad$LCA[[5]]$posterior
class6.ad <- apply(post6.ad, 1, which.max)
class.size6.ad <- prop.table(table(class6.ad))

ave.pp6.ad <- sapply(1:ncol(post6.ad), function(k) {
  inds <- which(class6.ad == k)
  mean(post6.ad[inds, k])
})


post5.ad <- LCAE.ad$LCA[[4]]$posterior
class5.ad <- apply(post5.ad, 1, which.max)
class.size5.ad <- prop.table(table(class5.ad))

ave.pp5.ad <- sapply(1:ncol(post5.ad), function(k) {
  inds <- which(class5.ad == k)
  mean(post5.ad[inds, k])
})


post4.ad <- LCAE.ad$LCA[[3]]$posterior
class4.ad <- apply(post4.ad, 1, which.max)
class.size4.ad <- prop.table(table(class4.ad))

ave.pp4.ad <- sapply(1:ncol(post4.ad), function(k) {
  inds <- which(class4.ad == k)
  mean(post4.ad[inds, k])
})


post3.ad <- LCAE.ad$LCA[[2]]$posterior
class3.ad <- apply(post3.ad, 1, which.max)
class.size3.ad <- prop.table(table(class3.ad))

ave.pp3.ad <- sapply(1:ncol(post3.ad), function(k) {
  inds <- which(class3.ad == k)
  mean(post3.ad[inds, k])
})

# BEST CLASS decided
# 3 classes is best
lca.best.ad <- LCAE.ad$LCA[[2]]
adult.lik$class <- lca.best.ad$predclass
adult.lik$post <- apply(lca.best.ad$posterior, 1, max)

# Calculate median minutes
n.classes <- 3

wmed.ad <- numeric(n.classes)
wq25.ad <- numeric(n.classes)
wq75.ad <- numeric(n.classes)
for (k in 1:n.classes) {

  q <- wtd.quantile(adult.lik$mins,
                    weights = lca.best.ad$posterior[,k],
                    probs = c(0.25, 0.5, 0.75))
  wq25.ad[k] <- q[1]
  wmed.ad[k] <- q[2]
  wq75.ad[k] <- q[3]
}


# Regressions
adult.lik$age <- adult.lik.back$age
adult.lik$class <- relevel(factor(adult.lik$class), ref = "1")
adult.lik$age <- relevel(factor(adult.lik$age), ref = "1")

fit.ad <- multinom(class ~ age,
                   data = adult.lik)
# odds ratio
or.ad <- exp(coef(fit.ad))
colnames(or.ad) <- c("16-34 (ref)", "35–44", "45–54", "55–64", "65–74", "75+")
rownames(or.ad) <- c("Low Motivation","Moderate Motivation")

sum.fit.ad <- summary(fit.ad)
se.ad <- sum.fit.ad$standard.errors
# Coefficients
coefs.ad <- coef(fit.ad)

# 95% CI for odds ratios
ci.l.ad <- exp(coefs.ad - 1.96 * se.ad)
ci.u.ad <- exp(coefs.ad + 1.96 * se.ad)

# Combine into a table
or.ci.ad <- data.frame(
  CI.lower = round(ci.l.ad, 3),
  CI.upper = round(ci.u.ad, 3)
)
colnames(or.ci.ad) <- c("Intercept.L", "Age2.L", "Age3.L", "Age4.L",
                        "Age5.L","Age6.L","Intercept.U","Age2.U", "Age3.U", "Age4.U",
                        "Age5.U","Age6.U")

# class distribution per age

tb.byage.ad <- adult.lik %>%
  count(age, class) %>%
  pivot_wider(names_from = class, values_from = n, values_fill = 0)
