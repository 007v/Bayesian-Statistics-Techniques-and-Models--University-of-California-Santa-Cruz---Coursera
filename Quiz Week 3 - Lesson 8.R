# Q3
data("PlantGrowth")
library("rjags")

mod1_string = " model {
    for (i in 1:length(y)) {
        y[i] ~ dnorm(mu[grp[i]], prec)
    }
    
    for (j in 1:3) {
        mu[j] ~ dnorm(0.0, 1.0/1.0e6)
    }
    
    prec ~ dgamma(5/2.0, 5*1.0/2.0)
    sig = sqrt( 1.0 / prec )
} "

mod2_string = " model {
    for (i in 1:length(y)) {
      y[i] ~ dnorm(mu[grp[i]], prec[grp[i]])
    }

    for (j in 1:3) {
      mu[j] ~ dnorm(0.0, 1.0/1.0e6)
      prec[j] ~ dgamma(5/2.0, 5*1.0/2.0)
      sig[j] = sqrt( 1.0 / prec[j] )
    }
} "

set.seed(82)
str(PlantGrowth)
data_jags = list(y=PlantGrowth$weight, 
              grp=as.numeric(PlantGrowth$group))

params = c("mu", "sig")

inits1 = function() {
  inits = list("mu"=rnorm(3,0.0,100.0), "prec"=rgamma(1,1.0,1.0))
}

inits2 = function() {
    inits = list("mu"=rnorm(3,0.0,100.0), "prec"=rgamma(3,1.0,1.0))
}

mod1 = jags.model(textConnection(mod1_string), data=data_jags, inits=inits1, n.chains=3)
update(mod1, 1e3)

mod2 = jags.model(textConnection(mod2_string), data=data_jags, inits=inits2, n.chains=3)
update(mod2, 1e3)

mod1_sim = coda.samples(model=mod1,
                        variable.names=params,
                        n.iter=5e3)
mod2_sim = coda.samples(model=mod2,
                        variable.names=params,
                        n.iter=5e3)

mod1_csim = as.mcmc(do.call(rbind, mod1_sim)) # combined chains
mod2_csim = as.mcmc(do.call(rbind, mod2_sim)) # combined chains

print(summary(mod1_sim))
print(summary(mod2_sim))

# Q4
dic1 = dic.samples(mod1, n.iter = 10000)
dic2 = dic.samples(mod2, n.iter = 10000)

print(dic1-dic2)

# Q6
print(HPDinterval(mod1_csim[,3] - mod1_csim[,1]))
