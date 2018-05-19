source("Quiz Week 3 - Lesson 7A.R")

# Q3
print(dic.samples(mod, n.iter = 10000))

# Q4
mod1_string = " model {
  for (i in 1:length(education)) {
    education[i] ~ dnorm(mu[i], prec)
    mu[i] = b0 + b[1]*income[i] + b[2]*young[i]
  }

  b0 ~ dnorm(0.0, 1.0/1.0e6)
  for (i in 1:2) {
    b[i] ~ dnorm(0.0, 1.0/1.0e6)
  }

  prec ~ dgamma(1.0/2.0, 1.0*1500.0/2.0)
  ## Initial guess of variance based on overall
  ## variance of education variable. Uses low prior
  ## effective sample size. Technically, this is not
  ## a true 'prior', but it is not very informative.
  sig2 = 1.0 / prec
  sig = sqrt(sig2)
} "

mod2_string = " model {
  for (i in 1:length(education)) {
    education[i] ~ dnorm(mu[i], prec)
    mu[i] = b0 + b[1]*income[i] + b[2]*young[i] + b[3]*income[i]*young[i]
  }

  b0 ~ dnorm(0.0, 1.0/1.0e6)
  for (i in 1:3) {
    b[i] ~ dnorm(0.0, 1.0/1.0e6)
  }

  prec ~ dgamma(1.0/2.0, 1.0*1500.0/2.0)
  ## Initial guess of variance based on overall
  ## variance of education variable. Uses low prior
  ## effective sample size. Technically, this is not
  ## a true 'prior', but it is not very informative.
  sig2 = 1.0 / prec
  sig = sqrt(sig2)
} "

data_jags = as.list(Anscombe[,-4])

params = c("b", "sig")

inits1 = function() {
  inits = list("b"=rnorm(2,0.0,100.0), "prec"=rgamma(1,1.0,1.0))
}

inits2 = function() {
  inits = list("b"=rnorm(3,0.0,100.0), "prec"=rgamma(1,1.0,1.0))
}

mod1 = jags.model(textConnection(mod1_string), data=data_jags, inits=inits1, n.chains=3)
update(mod1, 1000) # burn-in

mod1_sim = coda.samples(model=mod1,
                       variable.names=params,
                       n.iter=5000)

mod1_csim = do.call(rbind, mod1_sim) # combine multiple chains

mod2 = jags.model(textConnection(mod2_string), data=data_jags, inits=inits2, n.chains=3)
update(mod2, 1000) # burn-in

mod2_sim = coda.samples(model=mod2,
                        variable.names=params,
                        n.iter=5000)

mod2_csim = do.call(rbind, mod2_sim) # combine multiple chains

print(dic.samples(mod1, n.iter = 10000))
print(dic.samples(mod2, n.iter = 10000))

# Q5
print(summary(mod_sim))
print(mean(mod_csim[,1] > 0.0))
