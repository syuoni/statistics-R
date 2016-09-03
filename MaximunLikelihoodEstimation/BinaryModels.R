# Duration Models Estimation with MLE
setwd('D:/Documents/R/Statistics and Econometrics/MaximunLikelihoodEstimation')
source('MLE.R')

probit.lnlike <- function(beta, args){
  y <- args$y
  X <- args$X
  
  Xb <- as.vector(X %*% beta)
  lnlike <- y*pnorm(Xb, log.p=TRUE) + (1-y)*pnorm(-Xb, log.p=TRUE)
  return(-sum(lnlike))
}

probit.gr <- function(beta, args){
  y <- args$y
  X <- args$X
  
  Xb <- as.vector(X %*% beta)
  gr <- apply(y * dnorm(Xb)/pnorm(Xb)*X + (1-y) * dnorm(-Xb)/pnorm(-Xb)*(-X), 2, sum)
  return(-gr)
}

mle.probit.estimate <- function(y, X){
  args <- list(y=y, X=as.matrix(X))
  args <- na.drop(args)
  
  params0 <- rep(1e-5, dim(args$X)[2])
  names(params0) <- colnames(args$X)
  model.res <- mle.model(probit.lnlike, args, params0=params0, gr=probit.gr)
  return(model.res)
}

logit.lnlike <- function(beta, args){
  y <- args$y
  X <- args$X
  
  Xb <- as.vector(X %*% beta)
  expXb = exp(Xb)
  lnlike <- y*log(expXb/(1+expXb)) + (1-y)*log(1/(1+expXb))
  return(-sum(lnlike))
}

logit.gr <- function(beta, args){
  y <- args$y
  X <- args$X
  
  Xb <- as.vector(X %*% beta)
  expXb = exp(Xb)
  gr <- apply(y * 1/(expXb+1)*X + (1-y) * (-expXb)/(expXb+1)*X, 2, sum)
  return(-gr)
}

mle.logit.estimate <- function(y, X){
  args <- list(y=y, X=as.matrix(X))
  args <- na.drop(args)
  
  params0 <- rep(1e-5, dim(args$X)[2])
  names(params0) <- colnames(args$X)
  model.res <- mle.model(logit.lnlike, args, params0=params0, gr=logit.gr)
  return(model.res)
}

binary.demo <- function(){
  df <- read.csv('reg.csv')
  all.params <- c('x1', 'x2', 'x3', 'x4')
  y <- ifelse(df$y > 18, 1, 0)
  
  probit.res <- mle.probit.estimate(y, df[c('x1', 'x3', 'x4')])
  logit.res <- mle.logit.estimate(y, df[c('x1', 'x3', 'x4')])
  
  res.table <- mle.res.table.export(list(probit.res, logit.res), all.params)
  print(res.table)
}

binary.demo()

