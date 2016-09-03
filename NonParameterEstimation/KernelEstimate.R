uniform.kernel <- function(z){
  return(ifelse((z>-1) & (z<=1), 0.5, 0))
}

normal.kernel <- function(z){
  return(dnorm(z))
}

kernel.density <- function(x, kernel.func, h=NULL, k=100){
  minx <- min(x)
  maxx <- max(x)
  n <- length(x)
  
  # get the optimal bandwidth
  if(is.null(h)){
    a <- integrate(function(t){return(kernel.func(t)**2)}, -5, 5)
    b <- integrate(function(t){return(t**2*kernel.func(t))}, -5, 5)
    delta <- (a$value/(b$value)**2)**0.2
    h <- 1.3643 * delta * n**(-0.2) * sd(x)
  }
  
  dx <- (maxx-minx)/k
  x.vec <- NULL
  fx.vec <- NULL
  for(i in 1:k){
    xi <- minx+(i-0.5)*dx
    z <- (x-xi)/h
    fxi <- sum(kernel.func(z))/(n*h)
    
    x.vec <- c(x.vec, xi)
    fx.vec <- c(fx.vec, fxi)
  }
  return(data.frame(x=x.vec, fx=fx.vec))
}

get.non.parameter.func <- function(x.vec, fx.vec){
  k <- length(x.vec)
  
  non.parameter.func <- function(x){
    if(x<=x.vec[1]){
      return(fx.vec[1])
    }else if(x>=x.vec[k]){
      return(fx.vec[k])
    }else{
      for(i in 2:k){
        if(x<=x.vec[i]){
          ratio <- (x-x.vec[i-1])/(x.vec[i]-x.vec[i-1])
          return(fx.vec[i-1]+ratio*(fx.vec[i]-fx.vec[i-1]))
        }
      }
    }
  }
  
  return(Vectorize(non.parameter.func))
}

kernel.regression <- function(y, x, kernel.func, h=NULL, k=100){
  minx <- min(x)
  maxx <- max(x)
  n <- length(x)
  
  # use the optimal bandwidth for kernel density temporarily
  if(is.null(h)){
    a <- integrate(function(t){return(kernel.func(t)**2)}, -5, 5)
    b <- integrate(function(t){return(t**2*kernel.func(t))}, -5, 5)
    delta <- (a$value/(b$value)**2)**0.2
    h <- 1.3643 * delta * n**(-0.2) * sd(x)
  }
  
  dx <- (maxx-minx)/k
  x.vec <- NULL
  y.vec <- NULL
  
  for(i in 1:k){
    xi <- minx+(i-0.5)*dx
    z <- (x-xi)/h
    kfz <- kernel.func(z)
    yi <- sum(kfz*y) / sum(kfz)
    
    x.vec <- c(x.vec, xi)
    y.vec <- c(y.vec, yi)
  }
  
  return(data.frame(x=x.vec, y=y.vec))
}

non.parameter.demo <- function(){
  set.seed(1226)
  n <- 5000
  x <- rnorm(n, 0, 1)
  kd.df <- kernel.density(x, normal.kernel)
  kd.func <- get.non.parameter.func(kd.df$x, kd.df$fx)
  
  print(dnorm(-2:2))
  print(kd.func(-2:2))
  
  y <- x**2-x + rnorm(n, 0, 0.5)
  kr.df <- kernel.regression(y, x, normal.kernel)
  kr.func <- get.non.parameter.func(kr.df$x, kr.df$y)
  
  print(sapply(-2:2, function(x){return(x**2-x)}))
  print(kr.func(-2:2))
  
  # library(ggplot2)
  # p <- ggplot(data=kr.df, aes(x=x, y=y))
  # p <- p + geom_line()
  # print(p)
}

non.parameter.demo()

