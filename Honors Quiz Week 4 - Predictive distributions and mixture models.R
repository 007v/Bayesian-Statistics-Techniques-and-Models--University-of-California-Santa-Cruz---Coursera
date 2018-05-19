# Q1
library(rjags)
dat = read.csv(file="callers.csv", header=TRUE)
str(dat)

mod_string = " model {
  for (i in 1:length(calls)) {
    calls[i] ~ dpois(lam[i]*days_active[i])
    log(lam[i]) = b0 + b[1]*age[i] + b[2]*isgroup2[i]
  }

  b0 ~ dnorm(0.0, 1.0/1e2)  
  for (j in 1:2) {
    b[j] ~ dnorm(0.0, 1.0/1e2)
  }
} "

data_jags = as.list(dat)

params = c("b0", "b")

mod = jags.model(textConnection(mod_string), data=data_jags, n.chains=3)
update(mod, 1e3)

mod_sim = coda.samples(model=mod,
                        variable.names=params,
                        n.iter=5e3)
mod_csim = as.mcmc(do.call(rbind, mod_sim))

## convergence diagnostics
plot(mod_sim)

gelman.diag(mod_sim)
autocorr.diag(mod_sim)
autocorr.plot(mod_sim)
effectiveSize(mod_sim)

## compute DIC
dic = dic.samples(mod, n.iter=1e3)

pmod_coef = apply(mod_csim, 2, mean)

# predictions
xpred = c(1, 29, 1)
pmod_coef = pmod_coef[c(3,1,2)]

llam_hat = mod_csim[,c(3,1,2)] %*% xpred
lam_hat = llam_hat  # 30 days active

lam = exp(lam_hat)*30

n_sim = length(lam)

y1 = rpois(n=n_sim, lambda = lam)
mean(y1 >= 3)

