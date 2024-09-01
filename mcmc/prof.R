source("read-admb.R")
source("write-dat.R")
ls()
m_orig<-read_dat("mod.ctl")
system("cp mod.ctl mod_orig.ctl")
names(m_orig)

q_r <- c()
for (i in 1:10)
  for (j in 1:10)
write_dat("mod.ctl",m_orig)



library(adnuts)                         # needs to be 1.0.9000
library(snowfall)
library(rstan)
library(shinystan)
reps <- parallel::detectCores()-4 # chains to run in parallel
reps
## Reproducible seeds are passed to ADMB
set.seed(352)
seeds <- sample(1:1e4, size=reps)
seeds
## Function to return starting values from a fit given the number of
## chains. It randomly samples from the posterior.
get.inits <- function(fit, chains){
  post <- extract_samples(fit)
  ind <- sample(1:nrow(post), size=chains)
  lapply(ind, function(i) as.numeric(post[i,]))
}

## Here we assume the pm.exe model is in a folder called 'pm'
## as well. This folder gets copied during parallel runs.
d<- 'mcmc'
here::here()
d<- here::here()
m <- 'fm' # the model name, folder is also assumed to be called this runs/base/mcmc
## First optimize the model to make sure the Hessian is good.
setwd(d);
system('fm -nox -mcmc 15 -hbf 1 -binp fm.bar -phase 50');


## Run--
iter <- 1000 # maybe too many...depends are number cores...I used 8...
chains=6
#iter <- 4000*thin; warmup <- iter/#8
inits <- NULL ## start chains from MLE
fit.mle <- sample_nuts(model=m, path=d, iter=iter, warmup=iter/4,
                   chains=chains, cores=chains, control=list(max_treedepth=14,
                    metric='mle'))

summary(fit.mle)
plot_uncertainties(fit.mle)
pairs_admb(fit.mle, pars=1:6, order='slow')
pairs_admb(fit.mle, pars=1:6, order='fast')
pairs_admb(fit.mle, pars=1:4)

pdf("pairs_rdev.pdf")
pairs_admb(fit.mle, pars=68:78)
pairs_admb(fit.mle2, pars=68:78)
dev.off()
pdf("marginals.pdf")
plot_marginals(fit.mle)
dev.off()
print(fit.mle)
plot_sampler_params(fit.mle)
launch_shinyadmb(fit.mle)

## It doesn't really need any fixes so rerun with NUTS. Reoptimize to get
## the correct mass matrix for NUTS. Note the -hbf 1 argument. This is a
## technical requirement b/c NUTS uses a different set of bounding
## functions and thus the mass matrix will be different.
## Use default MLE covariance (mass matrix) and short parallel NUTS chains
## started from the MLE.


## If good, run again for inference using updated mass matrix. Increase
## adapt_delta toward 1 if you have divergences (runs will take longer).
mass <- fit.mle$covar.est # note this is in unbounded parameter space
inits <- get.inits(fit.mle, reps) ## use inits from pilot run
reps
chains=6
fit.mle2 <- sample_nuts(model=m, path=d, iter=1000, warmup=iter/4,
                   chains=chains, cores=chains, control=list(max_treedepth=14,
                    metric=mass,adapt_delta=0.95))
plot_sampler_params(fit.mle2)
launch_shinyadmb(fit.mle)
launch_shinyadmb(fit.mle2)
pairs_admb(fit.mle2, pars=1:6, order='slow')
