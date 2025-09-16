# Libraries ---------------------------------------------------------------
set.seed(2025)
library(tidyverse)
library(ggplot2)
library(nnet)
library(tidyLPA)
library(poLCA)
library(poLCAExtra)


# LCA, Youths -------------------------------------------------------------

# Predictors (motives)
child.lik.y <- as.matrix(child.lik %>% dplyr::select(-mins,-age,-gender,-eth))

# Spec formula for LCA
lca.f.child <- child.lik.y ~ gender + eth

LCAE.ch <- poLCAExtra::poLCA(lca.f.child, data = child.lik, nclass = 2:7)
save(LCAE.ch, file="LCAE.ch.RData")
# load(file="LCAE.ch.RData")

# bootstrapped Vuong-Lo-Mendell-Rubin likelihood ratio test
blrt.ch <- poLCA.blrt(LCAE.ch)
save(blrt.ch,file="blrt.ch.RData")
# blrt.ch <- poLCA.blrt(LCAE.ch[3:5], nreps = 50)


# Take relevant stats
ch.lca.output <- LCAE.ch$output %>% dplyr::select(nclass,llike,AIC,BIC,
                                                  Rel.Entropy,LMR,p)

ch.lca.output

# elbow plot
elbow.ch <- ggplot(ch.lca.output, aes(x = nclass)) +
  geom_line(aes(y = BIC), color = "blue") +
  geom_point(aes(y = BIC), color = "blue") +
  geom_line(aes(y = AIC), color = "red") +
  geom_point(aes(y = AIC), color = "red") +
  labs(y = "Information Criterion", x = "Number of Classes",
       title = "Elbow Plot for poLCA Model Selection",
       caption = "Blue = BIC, Red = AIC") +
  theme_minimal()

# elbow.ch
poLCA.residual.pattern(LCAE.ch, nclass = 2)
poLCA.cov(LCAE.ch, nclass = 2)


# check posterior and boxplots
for(k in 3:4){

  child.lik$post <- apply(LCAE.ch$LCA[[k]]$posterior, 1, max)
  child.lik$class <- LCAE.ch$LCA[[k]]$predclass

  print(
    ggplot(child.lik, aes(x = post, fill = factor(class))) +
    geom_histogram(binwidth = 0.05, alpha = 0.7, position = "identity") +
    labs(x = "Max Posterior Probability", y = "Count", fill = "Class",
         title = paste0(k," Classes, Youths")) +
    theme_minimal()
  )

  print(ggplot(child.lik, aes(x = factor(class), y = post)) +
    geom_boxplot(fill = "skyblue") +
    labs(x = "Class", y = "Max Posterior Probability",
         title = paste0(k," Classes, Youths")) +
    theme_minimal()
  )
}

# check class size of candidates (need >0.5 per)
child.lik$class <- LCAE.ch$LCA[[3]]$predclass
prop.table(table(child.lik$class))


# 4 classes is best
lca.best.ch <- LCAE.ch$LCA[[3]]
child.lik$class <- lca.best.ch$predclass
child.lik$post <- lca.best.ch$posterior
post.ch <- child.lik$post


# Weighted means
child.lik$mins.log <- log(child.lik$mins+1)


# probability weighted means
wmeans.child <- colSums(post.ch * child.lik$mins) / colSums(post.ch)
wmeans.log.child <- colSums(post.ch * child.lik$mins.log) / colSums(post.ch)

# weighted SD
wsd.child <- sapply(1:ncol(post.ch), function(k) {
  sqrt( sum(post.ch[,k] * (child.lik$mins - wmeans.child[k])^2)
        / sum(post.ch[,k]) )
})

# weighted log sd
wsd.log.child <- sapply(1:ncol(post.ch), function(k) {
  sqrt( sum(post.ch[,k] * (child.lik$mins.log - wmeans.log.child[k])^2)
        / sum(post.ch[,k]) )
})

# Put everythign together
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







# LCA, Adults -------------------------------------------------------------
# remove
adult.lik.y <- adult.lik %>% dplyr::select(-mins,-age,-gender,-eth,-edu)

lca.f.adult <- as.matrix(adult.lik.y) ~ gender + eth + edu

# 7 classes
LCAE.ad <- poLCA(lca.f.adult, data = adult.lik, nclass = 1:7)

save(LCAE.ad, file="LCAE.ad.RData")


# bootstrap lrt, reduce computation time
LCAE.ad1 <- poLCA(lca.f.adult, data = adult.lik, nclass = 3:6)
blrt.ad <- poLCA.blrt(LCAE.ad1,nreps = 10)

save(blrt.ad,file="blrt.ad.RData")


LCAE.ad$output


# Take relevant stats
ad.lca.output <- LCAE.ad$output %>% dplyr::select(nclass,llike,AIC,BIC,
                                                  Rel.Entropy,LMR,p)

ad.lca.output

# elbow plot
elbow.ad <- ggplot(ad.lca.output, aes(x = nclass)) +
  geom_line(aes(y = BIC), color = "blue") +
  geom_point(aes(y = BIC), color = "blue") +
  geom_line(aes(y = AIC), color = "red") +
  geom_point(aes(y = AIC), color = "red") +
  labs(y = "Information Criterion", x = "Number of Classes",
       title = "Elbow Plot for poLCA Model Selection",
       caption = "Blue = BIC, Red = AIC") +
  theme_minimal()

elbow.ad

# elbow.ad
poLCA.residual.pattern(LCAE.ad, nclass = 4)
poLCA.cov(LCAE.ad, nclass = 4)


# check posterior and boxplots
for(k in 3:4){

  adult.lik$post <- apply(LCAE.ad$LCA[[k]]$posterior, 1, max)
  adult.lik$class <- LCAE.ad$LCA[[k]]$predclass

  print(
    ggplot(adult.lik, aes(x = post, fill = factor(class))) +
      geom_histogram(binwidth = 0.05, alpha = 0.7, position = "identity") +
      labs(x = "Max Posterior Probability", y = "Count", fill = "Class",
           title = paste0(k," Classes, adults")) +
      theme_minimal()
  )

  print(ggplot(adult.lik, aes(x = factor(class), y = post)) +
          geom_boxplot(fill = "skyblue") +
          labs(x = "Class", y = "Max Posterior Probability",
               title = paste0(k," Classes, adults")) +
          theme_minimal()
  )
}

# check class size of candidates (need >0.5 per)
adult.lik$class <- LCAE.ad$LCA[[4]]$predclass
prop.table(table(adult.lik$class))


# 4 classes is best
lca.best.ad <- LCAE.ad$LCA[[4]]
adult.lik$class <- lca.best.ad$predclass
adult.lik$post <- lca.best.ad$posterior
post.ad <- adult.lik$post


# Weighted means
adult.lik$mins.log <- log(adult.lik$mins+1)


# probability weighted means
wmeans.adult <- colSums(post.ad * adult.lik$mins) / colSums(post.ad)
wmeans.log.adult <- colSums(post.ad * adult.lik$mins.log) / colSums(post.ad)

# weighted SD
wsd.adult <- sapply(1:ncol(post.ad), function(k) {
  sqrt( sum(post.ad[,k] * (adult.lik$mins - wmeans.adult[k])^2)
        / sum(post.ad[,k]) )
})

# weighted log sd
wsd.log.adult <- sapply(1:ncol(post.ad), function(k) {
  sqrt( sum(post.ad[,k] * (adult.lik$mins.log - wmeans.log.adult[k])^2)
        / sum(post.ad[,k]) )
})

# Put everythign together
mins.adult <- data.frame(
  Class = factor(1:ncol(post.ad)),
  Mean = wmeans.adult,
  Mean.log= exp(wmeans.log.adult),
  SD =wsd.adult,
  SD.log = wsd.log.adult,
  SD.low = exp(wmeans.log.adult - wsd.log.adult) - 1,
  SD.up = exp(wmeans.log.adult + wsd.log.adult) - 1
)

mins.adult
