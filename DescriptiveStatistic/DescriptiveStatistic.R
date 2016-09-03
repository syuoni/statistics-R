setwd('D:/Documents/R/Statistics and Econometrics/DescriptiveStatistic')

library(rJava)
library(xlsxjars)
library(xlsx)

descriptive.stats.variable <- function(x, na.rm=FALSE){
  if(na.rm){
    x <- x[!is.na(x)]
  }
  n <- length(x)
  if(is.numeric(x)){
    mean <- mean(x)
    sd <- sd(x)
    min <- min(x)
    max <- max(x)
  }else{
    mean <- NA
    sd <- NA
    min <- NA
    max <- NA
  }
  return(c(observations=n, mean=mean, std.dev=sd, min=min, max=max))
}

descriptive.stats <- function(df, na.rm=FALSE){
  return(t(sapply(df, descriptive.stats.variable, na.rm=na.rm)))
}

demo <- function(){
  df <- read.xlsx('Sample-Student.xlsx', 1, stringsAsFactors=FALSE)
  des.res <- descriptive.stats(df)
  print(des.res)
  # write.xlsx(des.res, 'Descriptive-Stats.xlsx', 'Descriptive-Stats')
}

# demo()
