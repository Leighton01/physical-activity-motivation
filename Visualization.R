
# LCA Youths---------------------------------------------------------------

elbow.ch

# Posterior
ggplot(child.lik, aes(x = post, fill = factor(class))) +
  geom_histogram(binwidth = 0.05, alpha = 0.7, position = "identity") +
  labs(x = "Max Posterior Probability", y = "Count", fill = "Class",
       title = paste0(k," Classes, Youths")) +
  theme_minimal()

# Boxplot
ggplot(child.lik, aes(x = factor(class), y = post)) +
  geom_boxplot(fill = "skyblue") +
  labs(x = "Class", y = "Max Posterior Probability",
       title = paste0(k," Classes, Youths")) +
  theme_minimal()


# Weighted minutes, youths
ggplot(mins.child, aes(x = Class, y = Mean.log)) +
  geom_col() +
  labs(x = "Latent Class", y = "Probability-weighted mean minutes")

anova(poLCA.child[[3]], poLCA.child[[4]],
      poLCA.child[[5]], poLCA.child[[6]])

poLCA.plot(poLCA.child[[3]])

# Takes too long..???
# poLCA.plot(LCAE.ch)
# plot(LCAE.ch, nclass = 3)
# poLCA.plot(LCAE.ch, nclass = 3:4)
# # poLCA.plot(poLCA.child[[3]], poLCA.child[[4]])
