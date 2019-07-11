## Ch.3 head to head spp vs. genus phylogeny

library(rstan)
library(brms)
library(ape)
library(MCMCglmm)
library(loo)

##Spp Model
d$s.gdp <- scale(d$gdp)
d$s.rule_of_law <- scale(d$rule_of_law)
d$s.hdi <- scale(d$hdi)
d$s.deathrate <- scale(d$deathrate)
d$s.num.countries <- scale(d$num.countries)

#Below is suspect. I stuck in cor_mat from previous work with Max. Not sure if it will work.
spp.model <- brm(iucn.no ~ s.gdp + s.rule_of_law + s.hdi + s.deathrate + s.num.countries +(1|binomial), data = d, 
                 family = cumulative(), cov_ranef = list(binomial = cor_mat.s),
                 prior = c(prior(normal(0, 10), "b"),
                           prior(normal(0, 100), "Intercept"),
                           prior(student_t(3, 0, 20), "sd")),
                 chains= 4,iter =10000, warmup = 2000,
                 cores = 6, thin = 2, seed = 12,
                 control = list(adapt_delta = 0.95))

summary(spp.model, waic = TRUE)
plot(spp.model) 
waic(spp.model)
loo(spp.model)
pareto_k_table(loo(spp.model))
pp_check(spp.model, x = 's.gdp', nsamples = 100)
pp_check(spp.model, type = "error_hist", nsamples = 11)
pp_check(spp.model, type = "stat_2d")
pp_check(spp.model, type = "dens_overlay")
pp_check(spp.model, type = "scatter_avg")

spp.predict <- posterior_predict(spp.model, draws = 500)
y <- d$iucn.no
ppc_dens_overlay(y, spp.predict[1:50, ])
ppc_stat(y, spp.predict, stat = "mean")

launch_shiny(spp.model) 
plot(marginal_effects(spp.model, probs = c(0.05, 0.95)))

library("ggplot2")
library("bayesplot")

color_scheme_set("red") # see help("bayesplot-colors")
color_scheme_set("brightblue") # see help("bayesplot-colors")
color_scheme_set("mix-brightblue-red") # see help("bayesplot-colors")


yrep_spp <- posterior_predict(spp.model, draws = 500)
yrep_nb <- posterior_predict(spp.model, draws = 500)
dim(yrep_spp)
y <- d$iucn.no
hist(y)
ppc_dens_overlay(y, yrep_spp[1:231, ])
ppc_stat(y, yrep_nb, stat = "prop_zero")
ppc_stat(y, yrep_spp, stat = "min")
ppc_stat(y, yrep_nb, stat = "max", binwidth = 100) + 
  coord_cartesian(xlim = c(-1, 5000))
available_ppc()








spp.model <- brm(iucn.no ~ s.gdp + s.rule_of_law + s.hdi + s.deathrate + s.num.countries + (1|binomial), data = d, 
                 family = cumulative(), cov_ranef = list(binomial = A),
                 prior = c(prior(normal(0, 10), "b"),
                           prior(normal(0, 100), "Intercept"),
                           prior(student_t(3, 0, 20), "sd")),
                 chains= 4,iter =10000, warmup = 2000,
                 cores = 6, thin = 2, seed = 12,
                 control = list(adapt_delta = 0.95))
WAIC(spp.model)
loo(spp.model)

waic_fit <- waic(ll)
plot(marginal_effects(spp.model), points = TRUE) 
post <- posterior_samples(spp.model)
View(post)
post2 <- as.data.frame(spp.model)
View(post2)
post3 <- posterior_samples(spp.model)
View(post3)
names(post3)
hist(post3$b_s.num.countries)
samples1 <- posterior_samples(spp.model, "^b")
View(samples1)
hist(samples1$b_s.num.countries)
# investigate the variance explained by the phylo effect
h <- "sd_phylo_Intercept^2 / (sd_phylo_Intercept^2 + sigma_phen^2) = 0" 
hypothesis(spp.model, h, class = NULL)

loo(genus.model)

psislw(spp.model, wcp = 0.2, wtrunc = 3/4, cores = getOption("loo.cores",
                                                      parallel::detectCores()))
## extract population and group-level coefficients separately
fixef(spp.model)
ranef(spp.model)
## extract combined coefficients 
coef(spp.model)

ps <- posterior_samples(spp.model, pars = NA, parameters = NA,
                  exact_match = FALSE, add_chain = FALSE, add_chains = FALSE,
                  subset = NULL, as.matrix = FALSE)

plot(spp.model)



#Genus Model

d$s.gdp <- scale(d$gdp)
d$s.rule_of_law <- scale(d$rule_of_law)
d$s.hdi <- scale(d$hdi)
d$s.deathrate <- scale(d$deathrate)
d$s.num.countries <- scale(d$num.countries)


genus.model <- brm(iucn.no ~ s.gdp + s.rule_of_law + s.hdi + s.deathrate + s.num.countries + (1|genus), data = d, 
                 family = cumulative(), cov_ranef = list(binomial = cor_mat.s),
                 prior = c(prior(normal(0, 10), "b"),
                           prior(normal(0, 100), "Intercept"),
                           prior(student_t(3, 0, 20), "sd")),
                 chains= 4,iter =20000, warmup = 5000,
                 cores = 6, thin = 2, seed = 12,
                 control = list(adapt_delta = 0.99))

pareto_k_table(loo(genus.model))
loo(genus.model, spp.model)
loo(genus.model)
waic(genus.model)
plot(genus.model)
genus.model

genus.model <- brm(iucn.no ~ (1|genus), data = d, 
                  family = cumulative(), cov_ranef = list(binomial = cor_mat.s),
                  prior = c(prior(normal(0, 10), "b"),
                            prior(normal(0, 100), "Intercept"),
                            prior(student_t(3, 0, 20), "sd")),
                  chains= 3,iter =1000, warmup = 100,
                  cores = 6, thin = 2, seed = 12,
                  control = list(adapt_delta = 0.95))
int.model <- update(genus.model,formula. = ~ . - (1|genus))
summary(int.model, waic = T)

summary(genus.model, waic = TRUE)
plot(genus.model) 
waic(genus.model)
loo(genus.model)
pareto_k_table(loo(genus.model))
pp_check(genus.model, x = 'b_s.num.countries', nsamples = 100)
pp_check(genus.model, type = "error_hist", nsamples = 11)
pp_check(genus.model, type = "stat_2d")
pp_check(genus.model, type = "dens_overlay")
pp_check(genus.model, type = "scatter_avg")
pp_check(genus.model)  # shows dens_overlay plot by default
pp_check(genus.model, type = "error_hist", nsamples = 11)
pp_check(genus.model, type = "scatter_avg", nsamples = 100)
pp_check(genus.model, type = "stat_2d")

pp_check(genus.model, x = 'b_s.num.countries', nsamples = 100)
pp_check(spp.model, x = 'b_s.num.countries', nsamples = 100, add = T)

par(mfrow = c(1,2))
pp_check(genus.model, type = "stat_2d")
pp_check(spp.model, type = "stat_2d")
pp <- predict(spp.model)
head(pp)

pp2 <- predict(spp.model, re_formula = ~ (1|binomial))
head(pp2)

pp2 - pp

inv.tree <- MCMCglmm::inverseA(tree, nodes = "TIPS", scale = TRUE)
A <- solve(inv.tree$Ainv)
rownames(A) <- rownames(inv.tree$Ainv)

genus.model <- brm(iucn.no ~ num.countries + (1|genus), data = d, 
                 family = cumulative(), cov_ranef = list(genus = A),
                 prior = c(prior(normal(0, 10), "b"),
                           prior(normal(0, 100), "Intercept"),
                           prior(student_t(3, 0, 20), "sd")),
                 chains= 3,iter =10000, warmup = 2000,
                 cores = 6, thin = 2, seed = 123,
                 control = list(adapt_delta = 0.99))

genus.model <- brm(iucn.no ~ gdp + rule_of_law + hdi + deathrate + num.countries + (1|genus), data = d, 
                  family = cumulative(), cov_ranef = list(genus = cor_mat.g),
                  prior = c(prior(normal(0, 10), "b"),
                            prior(normal(0, 100), "Intercept"),
                            prior(student_t(3, 0, 20), "sd")),
                  chains= 4,iter =10000, warmup = 2000,
                  cores = 6, thin = 2, seed = 12,
                  control = list(adapt_delta = 0.95))

genus.model <- brm(iucn.no ~ gdp + rule_of_law + hdi + deathrate + num.countries + (1|genus), data = d, 
                   family = cumulative(), cov_ranef = list(genus = cor_mat.g),
                   prior = c(prior(normal(0, 10), "b"),
                             prior(normal(0, 100), "Intercept"),
                             prior(student_t(3, 0, 20), "sd")),
                   chains= 4,iter =10000, warmup = 2000,
                   cores = 6, thin = 2, seed = 12,
                   control = list(adapt_delta = 0.95))




waic.g <- WAIC(genus.model)
loo.g <- loo(genus.model)
pareto_k_table(loo(genus.model))

WAIC(spp.model,genus.model)
loo(spp.model,genus.model)

loo.s <- loo(spp.model)
waic.s <- waic(spp.model)
compare(loo.s,loo.g)

plot(loo.g$pointwise[,2] ~ (d$s.deathrate), col = "red")
plot(loo.s$pointwise[,2]- loo.g$pointwise[,2] ~ (d$s.gdp), col = d$iucn.no)
plot(loo.s$pareto_k - loo.g$pareto_k ~ (d$s.gdp), col = d$iucn.no)
loo.s$se_looic
head(loo.g$pointwise)
library(ggplot2)
d$loo.g <- loo.g$pareto_k
d$loo.s <- loo.s$pareto_k
head(loo.g$pointwise)
d$loo.g <- loo.g$pointwise[,1]
d$loo.s <- loo.s$pointwise[,1]

p <- ggplot(d)
p + geom_point(aes(gdp, (loo.s-loo.g), colour = (iucn.no))) + 
  scale_colour_gradient(limits=c(1,5), low="red")
p + geom_point(aes((loo.s), (loo.g), colour = factor(iucn.no)))

p + geom_point(aes(d$gdp, (loo.s-loo.g), colour = factor(n.in.gen))) 
p + geom_point(aes((loo.s), (loo.g), colour = factor(n.in.gen))) 
  
p + geom_point(aes((loo.s), (loo.g), colour = (n.in.gen), size = var.in.gen)) 
p + geom_point(aes((loo.s), (loo.g), colour = (n.in.gen), size = var.in.gen)) +
  xlim(0,2.5)

p <- ggplot(d)
p + geom_point(aes(deathrate, (loo.s-loo.g), colour = (n.in.gen), size = var.in.gen)) 


n.in.gen <- aggregate(d$binomial, by = list(d$genus), FUN = length )
names(n.in.gen) <- c("genus","n.in.gen")
library(plyr)
d <- join(d, n.in.gen, by = "genus", match = 'all')
rm(n.in.gen)

var.in.gen <- aggregate(d$iucn, by = list(d$genus), FUN = var )
names(var.in.gen) <- c("genus","var.in.gen")
var.in.gen$var.in.gen[is.na(var.in.gen$var.in.gen)] <- 0
d <- join(d, var.in.gen, by = "genus", match = 'all')
rm(var.in.gen)

