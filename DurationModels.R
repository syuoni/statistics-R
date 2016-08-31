# Duration Models Estimation with MLE
setwd('D:/Documents/R/Statistics and Econometrics')
source('MLE.R')

exp.lnlike <- function(beta, args){
  # d is a vector (dim=n), each element of which indicates the sample is censored 
  # d=0 means censored
  t <- args$t
  d <- args$d
  X <- args$X
  
  # A %*% B always return a matrix, transfrom it to a vector
  Xb <- as.vector(X %*% beta)
  lnlike <- d*Xb - exp(Xb)*t
  return(-sum(lnlike))
}

exp.gr <- function(beta, args){
  t <- args$t
  d <- args$d
  X <- args$X
  
  Xb <- as.vector(X %*% beta)
  gr <- apply((d-exp(Xb)*t) * X, 2, sum)
  return(-gr)
}

# t: vector of duration time
# X: matrix of explanatory variables
# d: vector of whether the sample is failed (1=failure)
mle.exp.estimate <- function(t, X, d){
  args <- list(t=t, X=as.matrix(X), d=d)
  args <- na.drop(args)
  
  # with specifying gradient function, the result would be accurate (consistent to Stata)
  # without gradient function, a finite-difference approximation will be used
  # the initial params0 should not make likelihood function return a infinite value
  params0 <- rep(1e-5, dim(args$X)[2])
  names(params0) <- colnames(args$X)
  model.res <- mle.model(exp.lnlike, args, params0=params0, gr=exp.gr)
  return(model.res)
}

weibull.lnlike <- function(theta, args){
  # the last element in theta is lnp (scalar), 
  # and the others is beta (vector)
  n.theta <- length(theta)
  beta <- theta[1:n.theta-1]
  lnp <- theta[n.theta]
  
  # d is a vector (dim=n), each element of which indicates the sample is censored 
  # d=0 means censored
  t <- args$t
  d <- args$d
  X <- args$X
  
  Xb <- as.vector(X %*% beta)
  lnlike <- d*(Xb+lnp+(exp(lnp)-1)*log(t)) - exp(Xb)*(t**exp(lnp))
  return(-sum(lnlike))
}

weibull.gr <- function(theta, args){
  n.theta <- length(theta)
  beta <- theta[1:(n.theta-1)]
  lnp <- theta[n.theta]
  
  t <- args$t
  d <- args$d
  X <- args$X
  
  p <- exp(lnp)
  Xb <- as.vector(X %*% beta)
  beta.gr <- apply((d-exp(Xb)*(t**p)) * X, 2, sum)
  lnp.gr <- sum(d*(1+log(t)*p) - exp(Xb)*(t**p)*log(t)*p)
  return(-c(beta.gr, lnp.gr))
}

mle.weibull.estimate <- function(t, X, d){
  args <- list(t=t, X=as.matrix(X), d=d)
  args <- na.drop(args)
  
  params0 <- rep(1e-5, dim(args$X)[2]+1)
  names(params0) <- c(colnames(args$X), 'lnp')
  model.res <- mle.model(weibull.lnlike, args, params0=params0, gr=weibull.gr)
  return(model.res)
}

gompertz.lnlike <- function(theta, args){
  # the last element in theta is gamma (scalar), 
  # and the others is beta (vector)
  n.theta <- length(theta)
  beta <- theta[1:n.theta-1]
  gamma <- theta[n.theta]
  
  t <- args$t
  d <- args$d
  X <- args$X
  
  Xb <- as.vector(X %*% beta)
  lnlike <- d*(Xb+gamma*t) - exp(Xb)*(exp(gamma*t)-1)/gamma
  return(-sum(lnlike))
}

gompertz.gr <- function(theta, args){
  n.theta <- length(theta)
  beta <- theta[1:n.theta-1]
  gamma <- theta[n.theta]
  
  t <- args$t
  d <- args$d
  X <- args$X
  
  Xb <- as.vector(X %*% beta)
  beta.gr <- apply((d-exp(Xb)*(exp(gamma*t)-1)/gamma) * X, 2, sum)
  gamma.gr <- sum(d*t - exp(Xb)*(exp(gamma*t)*(gamma*t-1)+1)/gamma**2)
  return(-c(beta.gr, gamma.gr))
}

mle.gompertz.estimate <- function(t, X, d){
  args <- list(t=t, X=as.matrix(X), d=d)
  args <- na.drop(args)
  
  params0 <- rep(1e-5, dim(args$X)[2]+1)
  names(params0) <- c(colnames(args$X), 'gamma')
  model.res <- mle.model(gompertz.lnlike, args, params0=params0, gr=gompertz.gr)
  return(model.res)
}

duration.demo <- function(){
  library('foreign')
  df <- read.dta('recid.dta')
  all.params <- c('workprg', 'priors', 'tserved', 'felon', 'alcohol',
                  'drugs', 'black', 'married', 'educ', 'age', '_const', 'lnp', 'gamma')
  
  X <- df[c('workprg', 'priors', 'tserved', 'felon', 'alcohol',
            'drugs', 'black', 'married', 'educ', 'age')]

  exp.res <- mle.exp.estimate(df$durat, X, 1-df$cens)
  # exponential model result from Stata
  # args <- list(t=df$durat, X=as.matrix(X), d=1-df$cens)
  # stata_params <- c(.09558006, .09133702, .01440089, -.31222409, .46767064, .29416476,
  #                   .47567491, -.1519512, -.02421234, -.00391121, -4.169213)
  # print(exp.lnlike(stata_params, args), digits=10)
  
  weibull.res <- mle.weibull.estimate(df$durat, X, 1-df$cens)
  gompertz.res <- mle.gompertz.estimate(df$durat, X, 1-df$cens)
  
  # gompertz model result from stata
  # stata_params <- c(.08532224, .08691561, .01288472, -.28549804, .42928766, .27253692,
  #                   .43281454, -.15295268, -.02185928, -.00356317, -3.6110097, -.02170298)
  # print(gompertz.lnlike(stata_params, args), digits=10)
  res.table <- mle.res.table.export(list(exp.res, weibull.res, gompertz.res), all.params)
  print(res.table)
}

duration.demo()