# examples from Chapter 10 AHME
set.seed(24)
M <- 100
J <- 2
y <- matrix(NA, nrow=M, ncol=J)

#  parameter values
psi <- 0.8
p <- 0.5

# generate presence/absence data (truth)
z <- rbinom(n = M, size=1, prob=psi)

# generate detection/nondetection data
for(j in 1:J){
  y[,j] <- rbinom(n= M, size=1, prob=z*p)
}

# look at data
sum(z)

sum(apply(y,1,max))

head(cbind(z=z, y1=y[,1], y2=y[,2]))

library(unmarked)
umf <- unmarkedFrameOccu(y=y)
summary(umf)
(fm1 <- occu(~1 ~1, data=umf))

backTransform(fm1, "state")
backTransform(fm1, "det")

# Bayesian model
# bundle data
str(win.data<- list(y=y, M=nrow(y), J=ncol(y)))

# specify model
sink("model.txt")
cat("
    model {
    # priors
      psi ~ dunif(0, 1)
      p ~ dunif(0, 1)

    # likelihood
    for (i in 1:M){
      z[i] ~ dbern(psi)
      for(j in 1:J){
        y[i,j] ~ dbern(z[i]*p)
      }
    }


    }
    
    ", fill=TRUE)
sink()

# initial values
zst <- apply(y, 1, max)
inits <- function(){list(z=zst)}

# params monitored
params <- c("psi", "p")

# mcmc settings
ni <- 5000 ; nt <- 1 ; nb <- 1000 ; nc <- 3

# call jags, summarize posteriors
library(jagsUI)
fm2 <- jags(win.data, inits, params, "model.txt", n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, parallel = TRUE)
print(fm2, digits = 3)


