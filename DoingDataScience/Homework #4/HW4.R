#Homework #2

#Normal Distribution - One Sample:
  
bootstrap<- function(d,r,n){
  norm <- rnorm(d)
  bootmean <- numeric(r)
  for(i in 1:r){
    boot.sample <- sample(norm,n,replace=TRUE)
    bootmean[i] <- mean(boot.sample)
  }
  hist(bootmean, density=70, main=paste("Histogram of Bootsample Means"),
       border="red",col="blue", axes=TRUE)
}

bootstrap(100,1000,50)
bootstrap(100,1000,10)

# Exponential Distribution - One Sample:

bootstrap<- function(d,r,end,n){
  expo <- rexp(d)
  bootmean <- numeric(r)
  for(i in 1:r){
    boot.sample <- sample(expo,n,replace=TRUE)
    bootmean[i] <- mean(boot.sample)
  }
  hist(bootmean, density=70, main=paste("Histogram of Bootsample Means"),
       border="red",col="purple", axes=TRUE)
}

bootstrap(100,100,50)
bootstrap(100,50,10)

# Normal Distribution - Two Samples:

bootstrap<- function(d,r,n){
  group1 <- rnorm(d)
  group2 <- rnorm(d)
  bootmean <- numeric(r)
  for(i in 1:r){
    boot.sample <- abs(sample(group1,n,replace=TRUE)-sample(group2,n,replace=TRUE))
    bootmean[i] <- mean(boot.sample)
  }
  hist(bootmean, density=70, main=paste("Histogram of Bootsample Means"),
       border="red",col="blue", axes=TRUE)
}

bootstrap(100,1000,100)
bootstrap(10,1000,50)


# Exponential Distribution - Two Samples:
bootstrap<- function(d,r,end,n){
  group1 <- rexp(d)
  group2 <- rexp(d)
  bootmean <- numeric(r)
  for(i in 1:r){
    boot.sample <- abs(sample(group1,n,replace=TRUE)-sample(group2,n,replace=TRUE))
    bootmean[i] <- mean(boot.sample)
  }
  hist(bootmean, density=70, main=paste("Histogram of Bootsample Means"),
       border="red",col="purple", axes=TRUE)
}

bootstrap(100,100,50)
bootstrap(100,50,10)