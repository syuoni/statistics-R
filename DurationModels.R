# Duration Models Estimation with MLE
source('MLE.R')

exp.lnlike <- function(beta, args){
  # d is a vector (dim=n), each element of which indicates the sample is censored 
  # d=0 means censored
  t <- args$t
  d <- args$d
  X <- args$X
  
  Xb <- X %*% beta
  lnlike <- d*Xb - exp(Xb)*t
  return(-sum(lnlike))
}

exp.gr <- function(beta, args){
  t <- args$t
  d <- args$d
  X <- args$X
  
  Xb <- X %*% beta
  gr <- apply(as.vector(d-exp(Xb)*t) * X, 2, sum)
  return(-gr)
}

mle.exp.estimate <- function(t, X, d){
  args <- list(t=t, X=as.matrix(X), d=d)
  
  # with specifying gradient function, the result would be accurate (consistent to Stata)
  # without gradient function, a finite-difference approximation will be used
  # the initial params0 should not make likelihood function return a infinite value
  params0 <- rep(1e-5, length(X))
  names(params0) <- colnames(X)
  model.res <- mle.model(exp.lnlike, args, params0=params0, gr=exp.gr)
  return(model.res)
}

weibull.lnlike <- function(theta, args){
  # the last one element in theta is lnp (scalar), 
  # and the others is beta (vector)
  n.theta <- length(theta)
  beta <- theta[1:n.theta-1]
  lnp <- theta[n.theta]
  
  # d is a vector (dim=n), each element of which indicates the sample is censored 
  # d=0 means censored
  t <- args$t
  d <- args$d
  X <- args$X
  
  Xb <- X %*% beta
  lnlike <- d*(Xb+lnp+(exp(lnp)-1)*log(t)) - exp(Xb)*(t**exp(lnp))
  return(-sum(lnlike))
}

weibull.gr <- function(theta, args){
  n.theta <- length(theta)
  beta <- theta[1:n.theta-1]
  lnp <- theta[n.theta]
  
  t <- args$t
  d <- args$d
  X <- args$X
  
  p <- exp(lnp)
  Xb <- X %*% beta
  beta.gr <- apply(as.vector(d-exp(Xb)*(t**p)) * X, 2, sum)
  lnp.gr <- sum(d*(1+log(t)*p) - exp(Xb)*(t**p)*log(t)*p)
  return(-append(beta.gr, lnp.gr))
}

mle.weibull.estimate <- function(t, X, d){
  args <- list(t=t, X=as.matrix(X), d=d)
  params0 <- rep(1e-5, length(X)+1)
  names(params0) <- c(colnames(X), 'lnp')
  model.res <- mle.model(weibull.lnlike, args, params0=params0, gr=weibull.gr)
  return(model.res)
}

duration.demo <- function(){
  setwd('D:/Documents/R/Statistics and Econometrics')
  
  library('foreign')
  df <- read.dta('recid.dta')
  all.params <- c('workprg', 'priors', 'tserved', 'felon', 'alcohol',
                  'drugs', 'black', 'married', 'educ', 'age', 'lnp')
  
  X <- df[c('workprg', 'priors', 'tserved', 'felon', 'alcohol',
            'drugs', 'black', 'married', 'educ', 'age')]
  X$const <- 1
  
  exp.res <- mle.exp.estimate(df$durat, X, 1-df$cens)
  # exponential model result from Stata 
  # stata_params <- c(.09558006, .09133702, .01440089, -.31222409, .46767064, .29416476, 
  #                   .47567491, -.1519512, -.02421234, -.00391121, -4.169213)
  
  weibull.res <- mle.weibull.estimate(df$durat, X, 1-df$cens)
  res.table <- mle.res.table.export(list(exp.res, weibull.res), all.params)
  print(res.table)
}

duration.demo()