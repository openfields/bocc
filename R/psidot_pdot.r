# model script

sink("model.txt")
cat("
    model{
      # priors
      psi ~ dunif(0, 1)
      p ~ dunif(0, 1)

      # likelihood
      # ecological model
      for(i in 1:R){
        z[i] ~ dbern(psi)
        p.eff[i] <- z[i] * p

        # observation model
        for(j in 1:T){
          y[i,j] ~ dbern(p.eff[i])
        } #j
      } # i
      
      # derived quantities
      occ.fs <- sum(z[])
    } # model
    ", fill = TRUE)
sink()