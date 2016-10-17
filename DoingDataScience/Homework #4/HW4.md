# Doing Data Science



#### Trace Smith
#### Homework #4

Write bootstrap code to illustrate the central limit theorem in R markdown and push the result to GitHub. Use a normal distribution with two different sample sizes and an exponential distribution with two different sample sizes. Correct code alone is insufficient. Please also comment on the code and explain the results. For help, see the lotsa.medians function in Unit 4. The deliverable is a link to a GitHub repo containing the code.


**Central Limit Theorem:** The sampling distribution of the mean of any independent, random variable will be normal or nearly normal, if the sample size is large enough. The distribution of means will increasingly approximate a normal distribution as the size N of samples increases (i.e. >30).

### Normal Distribution - One Sample:

```r
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
```

![](Trace_Smith_HW4_files/figure-html/unnamed-chunk-1-1.png)<!-- -->

```r
bootstrap(100,1000,10)
```

![](Trace_Smith_HW4_files/figure-html/unnamed-chunk-1-2.png)<!-- -->

### Exponential Distribution - One Sample:


```r
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
```

![](Trace_Smith_HW4_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
bootstrap(100,50,10)
```

![](Trace_Smith_HW4_files/figure-html/unnamed-chunk-2-2.png)<!-- -->

### Normal Distribution - Two Samples:


```r
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
```

![](Trace_Smith_HW4_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
bootstrap(10,100,50)
```

![](Trace_Smith_HW4_files/figure-html/unnamed-chunk-3-2.png)<!-- -->

### Exponential Distribution - Two Samples:


```r
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
```

![](Trace_Smith_HW4_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
bootstrap(100,50,10)
```

![](Trace_Smith_HW4_files/figure-html/unnamed-chunk-4-2.png)<!-- -->
