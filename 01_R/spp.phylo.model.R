
## Ch.3 script for species-level stuff.

library(rstan)
library(brms)
library(ape)
library(MCMCglmm)

inv.tree <- MCMCglmm::inverseA(tree, nodes = "TIPS", scale = TRUE)
A <- solve(inv.tree$Ainv)
rownames(A) <- rownames(inv.tree$Ainv)

model_kbk <- brm(iucn.no ~ gdp + rule_of_law + hdi + deathrate + num.countries + (1|binomial), data = d, 
                 family = cumulative(), cov_ranef = list(binomial = A),
                 prior = c(prior(normal(0, 10), "b"),
                           prior(normal(0, 100), "Intercept"),
                           prior(student_t(3, 0, 20), "sd")),
                           #prior(student_t(3, 0, 20), "sigma")),
                 chains= 3,iter =10000, warmup = 2000,
                 cores = 3, thin = 2, cluster_type = "FORK", seed = 12)
WAIC(model_kbk)
loo(model_kbk)






model_kbk
# 915.37 for 2000 iter
#915.45  for 10000 iter
m2 <- update(model_kbk, formula. = ~ . - gdp)
plot(model_kbk)
pareto_k_table(loo(model_kbk))

plot(loo(model_kbk))
plot(loo(m2))
WAIC(model_kbk,m2)
pareto_k_table(model_kbk,m2)


WAIC(model_kbk)
stancode(model_kbk)

summary(model_kbk, waic = T)
plot(model_kbk)
plot(marginal_effects(model_kbk), points = TRUE) 

hyp <- paste("sd_genus__Intercept^2 /", 
             "(sd_genus__Intercept^2 + sd_genus__Intercept^2) = 0")
(hyp <- hypothesis(model_kbk, hyp, class = NULL))
hyp <- hypothesis(model_kbk, "num.countries <  3", class = 'b')

993.22