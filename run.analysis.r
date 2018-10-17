#====================================================================
#loading packages
library(rstan)
library(devtools)
find_rtools()
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
#--------------------------------------------------------------------
#compiling the Stan model
rt = stanc("modelo.stan", model_name = 'model')
sm = stan_model(stanc_ret = rt, verbose = TRUE)

#loading and adjusting the data
source("data.adjustment.r")

#--------------------------------------------------------------------
#function to generate the results table
axtract = function(x, parnames=NA, quantiles = c(c(0.025,0.5,0.975)))
{
  
  if(!is.na(parnames)[1])
    dat = data.frame(rstan::extract(x, pars = parnames))
  else
    dat = data.frame(rstan::extract(x))
  
  summ = t(apply(dat, 2, quantile, quantiles))
  
  res = list(summary = summ, MCMC = dat)
  
  class(res) = "axmcmc"
  return(res)
}
#====================================================================

#################################
#Analysis of strength (apodeme)
#################################

list.ap = list(N = nrow(dat),
               M = 2,
               y = dat$sqapodeme,
               x = dat$scsize,
               sp = dat$sp.number,
               sex = dat$sex.number)


#running the MCMC
fit.ap = sampling(sm, data=list.ap, chains=3, iter = 5*1000, 
                  warmup=1000)

mcmc.ap = As.mcmc.list(fit.ap)
plot(mcmc.ap)
summary(mcmc.ap)

#########################
#Mechanical advantage
##########################
list.ma = list(N = nrow(dat),
               M = 2,
               y = dat$ma1,
               x = dat$scsize,
               sp = dat$sp.number,
               sex = dat$sex.number)

#ajustando o modelo
fit.ma = rstan::sampling(sm, data=list.ma, chains=3, iter = 5*1000, 
               warmup=1000)

mcmc.ma = axtract(fit.ma)
mcmc.ma$summary

##########################
#claw size - csize ~ cc
##########################

list.cs = list(N = nrow(dat),
               M = 2,
               y = dat$scsize,
               x = dat$scc,
               sp = dat$sp.number,
               sex = dat$sex.number)


fit.cs = rstan::sampling(sm, data=lista.ta, chains=3, iter = 5*1000, 
               warmup=1000)

mcmc.cs = axtract(fit.cs)
mcmc.sc$summary

##################################################
#claw shape (procrustes distance) - shape ~ csize
##################################################

#this is a simpler model, its just a one-way ANCOVA
#with varying sigmas

males = dat[dat$sex == "male",]


list.sh = list(N = nrow(machos),
               M = 2,
               y = machos$shape,
               x = machos$scsize,
               cat = machos$sp.number)


#compiling the simpler model
rh = stanc("modelo.simples.stan", model_name = 'modelH')
sh = stan_model(stanc_ret = rh, verbose = TRUE)

fit.sh = rstan::sampling(sh, data=list.sh, chains=3, iter = 10*1000, 
               warmup=5000)

mcmc.sh = axtract(fit.sh)
mcms.sh$summary
#====================================================================
