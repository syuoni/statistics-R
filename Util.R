# numeric gradient function
numeric.gr <- function(func){
  dx <- 1e-3
  func.gr <- function(theta, ...){
    n.theta <- length(theta)
    gr <- NULL
    for (i in 1:n.theta){
      theta0 <- theta
      theta1 <- theta
      theta0[i] <- theta[i] - dx/2
      theta1[i] <- theta[i] + dx/2
      gr <- c(gr, (func(theta1, ...)-func(theta0, ...)) / dx)
    }
    return(gr)
  }
  return(func.gr)
}

numeric.gr.demo <- function(){
  foo <- function(theta, c){
    return(theta[1]*c+theta[2]**2)
  }
  
  foo.gr <- numeric.gr(foo)
  print(foo.gr(c(2,3), 2))
}

# numeric.gr.demo()