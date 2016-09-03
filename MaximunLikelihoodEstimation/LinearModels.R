# Linear Models Estimation with MLE
setwd('D:/Documents/R/Statistics and Econometrics/MaximunLikelihoodEstimation')
source('MLE.R')

# y~const
normal.lnlike <- function(theta, args){
  mu <- theta[1]
  sigma <- theta[2]
  sigma <- abs(sigma)
  y <- args$y
  ln.prob.density <- dnorm(y, mean=mu, sd=sigma, log=TRUE)
  return(-sum(ln.prob.density))
}

# y~X
linear.lnlike <- function(theta, args){
  n.theta <- length(theta)
  # 1:n-1 is (1:n)-1, namely c(0, 1, 2, ..., n-1)
  # 1:(n-1) is c(1, 2, 3, ..., n-1)
  beta <- theta[1:(n.theta-1)]
  sigma <- theta[n.theta]
  
  y <- args$y
  X <- args$X
  e <- y - X %*% beta
  
  # With specifying sigma <- abs(sigma), there would be no warnings
  # sigma may be estimated to be negative
  # general solution: sigma should be transformed to ln(sigma)
  sigma <- abs(sigma)
  ln.prob.density <- dnorm(e, mean=0., sd=sigma, log=TRUE)
  return(-sum(ln.prob.density))
}

mle.linear.estimate <- function(y, X){
  args <- list(y=y, X=as.matrix(X))
  args <- na.drop(args)
  
  # converge only when the initial params are specified accurately
  # BFGS method can converge, with warnings? but BFGS fails in Python
  # With specifying sigma <- abs(sigma), there would be no warnings
  # params0 <- c(1, 1, 1, -0.05, 0.5, 0.27)
  params0 <- rep(1, dim(args$X)[2]+1)
  names(params0) <- c(colnames(args$X), 'sigma')
  model.res <- mle.model(linear.lnlike, args, params0=params0)
  return(model.res)
}

mle.linear.demo <- function(){
  df <- read.csv('reg.csv')
  all.params <- c('x1', 'x2', 'x3', 'x4', 'x5', '_const', 'sigma')
  
  model.res1 <- mle.linear.estimate(df$y, df[c('x1', 'x3', 'x4')])
  model.res2 <- mle.linear.estimate(df$y, df[c('x1', 'x2', 'x4')])
  model.res3 <- mle.linear.estimate(df$y, df[c('x1', 'x2', 'x3', 'x4')])
  
  res.table <- mle.res.table.export(list(model.res1, model.res2, model.res3), all.params)
  print(res.table)
  
  # library(rJava)
  # library(xlsxjars)
  # library(xlsx)
  # write.xlsx(res.table, 'RegResult.xlsx', 'RegResult')
}

mle.linear.demo()