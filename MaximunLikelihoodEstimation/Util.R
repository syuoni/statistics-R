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

# drop NA samples for args
# assign.lbs: drop NA samples in args with specific labels
# exempt.cond: vector, sample would not be dropped if it is TRUE
# add.const: whether add const variable for matrix
na.drop <- function(args, assign.lbs=NULL, exempt.cond=FALSE, add.const=TRUE){
  if(is.null(assign.lbs)){
    assign.lbs <- names(args)
  }
  na.indicator <- NULL
  for(lb in assign.lbs){
    na.indicator <- cbind(na.indicator, is.na(args[[lb]]))
  }
  drop.indicator <- apply(na.indicator, 1, any) & (!exempt.cond)
  
  model.args <- list()
  for(lb in names(args)){
    if(is.vector(args[[lb]])){
      model.args[[lb]] <- args[[lb]][!drop.indicator]
    }else{
      model.args[[lb]] <- args[[lb]][!drop.indicator, ]
      
      # for matrix: drop variables with unique value
      # so if const variable is in matrix, it would be dropped, but may be added latter if add.const=TRUE
      model.args[[lb]] <- model.args[[lb]][, apply(model.args[[lb]], 2, function(vec){
        if(length(unique(vec)) > 1){
          return(TRUE)
        }else{
          return(FALSE)
        }
      })]

      if(add.const){
        model.args[[lb]] <- cbind(model.args[[lb]], `_const`=1)
      }
    }
  }
  return(model.args)
}

get.dummies <- function(vec, levels=NULL, head=NULL){
  if(is.null(levels)){
    levels <- unique(vec)
  }
  dummies <- NULL
  for(lv in levels){
    dummies <- cbind(dummies, as.integer(vec==lv))
  }
  colnames(dummies) <- head
  return(dummies)
}

na.drop.demo <- function(){
  df <- data.frame(x=1:6,
                   y=c(2, 2, 2, 1, 3, 2),
                   z=c(1, 6, 7, NA, NA, 3))
  args <- list(X=as.matrix(df))
  print(na.drop(args))
}

# na.drop.demo()


