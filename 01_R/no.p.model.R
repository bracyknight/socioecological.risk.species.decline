print(paste(i, "model", sep = " "))


p.vars <- t1[i][!is.na(t1[i])]
p.vars <- paste(p.vars, collapse = " + ")
#p.vars <- paste(p.vars, "(1|genus)", sep = " + ") #this is the bit that adds in the phylogeny
m.vars <- paste('iucn.no', p.vars, sep =" ~ ")
# m.vars <- paste('iucn.no', 1, sep =" ~ ") # for intercept only model
fit <- brm(m.vars, data = db, 
           family = cumulative(), #cov_ranef = list(genus = cor_mat.g),
           prior = c(prior(normal(0, 10), "b"),
                     prior(normal(0, 100), "Intercept"),
                     prior(student_t(3, 0, 20), "sd")),
           chains= 4,iter =10000, warmup = 2000, #reduced these for testing
           cores = 6, thin = 2, seed = 12,
           control = list(adapt_delta = 0.95))

w <- waic(fit)
l <- loo(fit)
s.fit <- summary(fit, waic = TRUE)

#Put the results to bed
m.results[i,6] <- phylo.model[i]
m.results[i,7] <- w$waic
m.results[i,8] <- w$se_waic
m.results[i,9] <- l$looic
m.results[i,10] <- l$se_looic

for(k in 1:sum(!is.na(t1[i]))){
  m.results[i,k] <- as.character(t1[k,i])
}
#View(m.results)

#set aside the results from each model as a separate thingy
Object = fit
Object[1] = 0
assign(paste0("m", i), Object)

#set aside the results from each model as a separate thingy
Object = s.fit
Object[1] = 0
assign(paste0("m", i, "summary"), Object)


