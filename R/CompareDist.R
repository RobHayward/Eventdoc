# This comes from http://memosisland.blogspot.co.uk/2015/08/practical-kullback-leibler-kl.html
# K-L divergence (Kullback-leiber)
# Measures the association between two random variables
set.seed(3951824)
# Table of counts (contingency)
X <- sample(letters[1:3], 100, 1)
Y <- sample(letters[4:5], 100, 1)
t  <- table(X,Y)
t
tp  <- t/100 # proportions
tn  <- tp/sum(tp)     # normalized, joints
p_x <- rowSums(tn)    # marginals
p_y <- colSums(tn)

P <- tn 
Q <- p_x %o% p_y 

# P(X, Y)   : bin frequency: P_i
# P(X) P(Y) : bin frequency, Q_i 
mi <- sum(P*log(P/Q))
library(entropy)
mi.empirical(t) == mi
