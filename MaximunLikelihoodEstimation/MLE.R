# Maximun Likelihood Estimation Model
source('ModelExport.R')
source('Util.R')

# Maximun Likelihood Estimation Model
# lnlike:  log-likelihood function
# args:    fixed arguments for lnlike and gr, usually are data to fit
# args$y:  vector of explained variables
# args$X:  matrix of explanatory variables
# params0: initial parameters to optimize
# gr:      gradient function
# method:  optimization method
mle.model <- function(lnlike, args, params0, gr=NULL, method='BFGS'){
  n <- nrow(args$X)
  K <- ncol(args$X)
  
  # Rescale optimization
  rescale.res <- optim(1, fn=function(c, params0, args){
    return(lnlike(c*params0, args))
  }, params0=params0, args=args, lower=0, upper=1000, method='Brent')
  params0 <- rescale.res$par * params0
  
  # Global Optimization 
  res <- optim(params0, fn=lnlike, gr=gr, args=args, method=method, hessian=TRUE)

  # coefficient
  coef <- res$par
  lnlike <- res$value
  convergence <- ifelse(res$convergence==0, TRUE, FALSE)
  
  # sqrt(n)*(b-beta) ~ N(0, Avarb)
  # No freedom adjustment
  Avarb <- n * solve(res$hessian)
  est.cov <- Avarb / n
  est.std.err <- sqrt(diag(est.cov))
  t.statistic <- coef / est.std.err
  p.value <- 2 * (1-pt(abs(t.statistic), n-K))
  
  # confident interval
  # Use the normal distribution to be consistent with stata
  # Option: use the t(n-K) distribution
  # t95 <- qt(0.975, df=n-K)
  n95 <- qnorm(0.975)
  conf.int.lower <- coef - n95 * est.std.err
  conf.int.upper <- coef + n95 * est.std.err

  coef.table <- data.frame(coef, est.std.err, t.statistic, p.value, 
                           conf.int.lower, conf.int.upper,
                           row.names=names(params0))
  model.res <- list(convergence =convergence,
                    observations=n,
                    lnlikelihood=lnlike,
                    table       =coef.table)
  return(model.res)
}

# export one mle model, each is one column
mle.res.export <- function(model.res, ...){
  export.col <- model.res.export(model.res, ...)
  index <- names(export.col)
  export.col <- c(export.col, model.res$observations, get.formatted(model.res$lnlikelihood))
  names(export.col) <- c(index, 'observations', 'lnlike')
  return(export.col)
}

# export several models
mle.res.table.export <- function(model.res.list, ...){
  res.table <- sapply(model.res.list, mle.res.export, ...)
  res.table <- data.frame(res.table)
  colnames(res.table) <- paste0('(', 1:length(res.table), ')')
  return(res.table)
}
