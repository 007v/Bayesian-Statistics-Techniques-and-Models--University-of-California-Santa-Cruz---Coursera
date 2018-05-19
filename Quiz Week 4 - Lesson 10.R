# Q1

beta = c(1.5, -0.3, 1.0)
x = c(1, 0.8, 1.2)

loglam = t(beta) %*% x
lam = exp(loglam)

# Q2
library(COUNT)
data("badhealth")
library(rjags)

mod1_string = " model {
  for (i in 1:length(numvisit)) {
    numvisit[i] ~ dpois(lam[i])
    log(lam[i]) = int + b_badh*badh[i] + b_age*age[i] + b_intx*age[i]*badh[i]
  }

  int ~ dnorm(0.0, 1.0/1e6)
  b_badh ~ dnorm(0.0, 1.0/1e4)
  b_age ~ dnorm(0.0, 1.0/1e4)
  b_intx ~ dnorm(0.0, 1.0/1e4)
} "

mod2_string = " model {
  for (i in 1:length(numvisit)) {
    numvisit[i] ~ dpois(lam[i])
    log(lam[i]) = int + b_badh*badh[i] + b_age*age[i]
  }

  int ~ dnorm(0.0, 1.0/1e6)
  b_badh ~ dnorm(0.0, 1.0/1e4)
  b_age ~ dnorm(0.0, 1.0/1e4)
} "

set.seed(102)

data_jags = as.list(badhealth)

params1 = c("int", "b_badh", "b_age", "b_intx")
params2 = c("int", "b_badh", "b_age")

mod1 = jags.model(textConnection(mod1_string), data=data_jags, n.chains=3)
update(mod1, 1e3)
mod2 = jags.model(textConnection(mod2_string), data=data_jags, n.chains=3)
update(mod2, 1e3)

mod1_sim = coda.samples(model=mod1,
                       variable.names=params1,
                       n.iter=5e3)
mod1_csim = as.mcmc(do.call(rbind, mod1_sim))

mod2_sim = coda.samples(model=mod2,
                        variable.names=params2,
                        n.iter=5e3)
mod2_csim = as.mcmc(do.call(rbind, mod2_sim))

## compute DIC
dic1 = dic.samples(mod1, n.iter=1e3)
dic2 = dic.samples(mod2, n.iter=1e3)

# Q4
lambda = 15
t = 2
q4_ans = ppois(21, lambda = lambda*t)

# Q5
dat = read.csv(file="callers.csv", header=TRUE)
str(dat)
pairs(dat)
boxplot(calls/days_active ~ isgroup2, data=dat)
#plot(calls/days_active ~ age, data=dat)
#boxplot(age ~ isgroup2, data=dat)
#boxplot(calls ~ isgroup2, data=dat)

# Q7
mod3_string = " model {
  for (i in 1:length(calls)) {
    calls[i] ~ dpois(lam[i]*days_active[i])
    log(lam[i]) = b0 + b[1]*age[i] + b[2]*isgroup2[i]
  }
  
  b0 ~ dnorm(0.0, 1.0/1e2)  
  for (j in 1:2) {
    b[j] ~ dnorm(0.0, 1.0/1e2)
  }
} "

data3_jags = as.list(dat)

params3 = c("b0", "b")


mod3 = jags.model(textConnection(mod3_string), data=data3_jags, n.chains=3)
update(mod3, 1e3)

mod3_sim = coda.samples(model=mod3,
                        variable.names=params3,
                        n.iter=15e3)
mod3_csim = as.mcmc(do.call(rbind, mod3_sim))

## convergence diagnostics
plot(mod3_sim)

gelman.diag(mod3_sim)
autocorr.diag(mod3_sim)
autocorr.plot(mod3_sim)
effectiveSize(mod3_sim)

## compute DIC
dic3 = dic.samples(mod3, n.iter=1e3)

pmod3_coef = apply(mod3_csim, 2, mean)

X = as.matrix(dat[,-c(1,2)])
X = X[,c(2,1)]

llam_hat3 = pmod3_coef['b0'] + X %*% pmod3_coef[-3]
lam_hat3 = llam_hat3*dat$days_active

mean(mod3_csim[,2] > 0)
