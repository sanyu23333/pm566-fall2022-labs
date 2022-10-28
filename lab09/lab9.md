Lab 09
================
Xuan Huang
2022-10-28

## Problem 2.

Create a n x k matrix of Poisson variables with mean lambda

``` r
set.seed(1235)
fun1 <- function(n = 100, k = 4, lambda = 4) {
  x <- NULL
  
  for (i in 1:n)
    x <- rbind(x, rpois(k, lambda))
  
  return(x)
}
f1 <- fun1(100, 4)
f1
```

    ##        [,1] [,2] [,3] [,4]
    ##   [1,]    3    4    2    7
    ##   [2,]    6    3    4    5
    ##   [3,]    4    2    8    4
    ##   [4,]    4    5    5    0
    ##   [5,]    7    4    5    6
    ##   [6,]    2    8    6    3
    ##   [7,]    2    6    3    4
    ##   [8,]    2    4    5    4
    ##   [9,]    5    4    3    4
    ##  [10,]    3    3    4    2
    ##  [11,]    4    7    1    4
    ##  [12,]    6    7    3    4
    ##  [13,]    5    2    3    3
    ##  [14,]    3    3    4    5
    ##  [15,]    2    7    4    1
    ##  [16,]    2    4    1    5
    ##  [17,]    9    4    3    7
    ##  [18,]    8    2    6    4
    ##  [19,]    5    2    4    6
    ##  [20,]    0    3    8    1
    ##  [21,]    4    5    2    3
    ##  [22,]    4    6    4    6
    ##  [23,]    3    3    4    1
    ##  [24,]    2    3    5    3
    ##  [25,]    5    5   12    4
    ##  [26,]    4    6    1    5
    ##  [27,]    4    3    4    7
    ##  [28,]    7    1    8    2
    ##  [29,]    5    2    3    3
    ##  [30,]    3    8    3    8
    ##  [31,]    5    5    4    7
    ##  [32,]    4    4    1    2
    ##  [33,]    5    4    5    6
    ##  [34,]    1    6    5    5
    ##  [35,]    3    3    3    2
    ##  [36,]    4    5    5    5
    ##  [37,]    4    8    6    2
    ##  [38,]    3    0    5    3
    ##  [39,]    2    6    3    3
    ##  [40,]    3    4    2    3
    ##  [41,]    4    8    7    4
    ##  [42,]    5    4    4    5
    ##  [43,]    1    5    7    3
    ##  [44,]    5    6    6    3
    ##  [45,]    6    2    1    5
    ##  [46,]    1    3    4    2
    ##  [47,]    5    2    4    7
    ##  [48,]    5    4    7    6
    ##  [49,]    1    5    9    2
    ##  [50,]    9    3    2    7
    ##  [51,]   10    5    4    3
    ##  [52,]    5    4    2    5
    ##  [53,]    6    2    5    7
    ##  [54,]    1    6    5    3
    ##  [55,]    5    4    3    4
    ##  [56,]    3    3    4    7
    ##  [57,]    2    2    3    2
    ##  [58,]    1    7    5    5
    ##  [59,]    6    3    2    3
    ##  [60,]    6    0    7    8
    ##  [61,]    3    2    5    2
    ##  [62,]    1    4    3    4
    ##  [63,]    7    6    2    4
    ##  [64,]    6    2    4    5
    ##  [65,]    5    7    1    5
    ##  [66,]   10    5    5    1
    ##  [67,]    2    5    3    3
    ##  [68,]    7    1    2    6
    ##  [69,]    3    4    1    4
    ##  [70,]    5    6    7    3
    ##  [71,]    4    2    3    3
    ##  [72,]    9    6    4    4
    ##  [73,]    0    4    4    3
    ##  [74,]    5    5    5    6
    ##  [75,]    7    1    3    2
    ##  [76,]    5    4    2    2
    ##  [77,]    4    4    4    4
    ##  [78,]    2    2    6    4
    ##  [79,]    6    6    4    4
    ##  [80,]    3    6    5    3
    ##  [81,]    3    6    6    3
    ##  [82,]    2    3    2    5
    ##  [83,]    2    6    3    4
    ##  [84,]    3    8    0    6
    ##  [85,]    1    5    4    4
    ##  [86,]    5    6    7    6
    ##  [87,]    1    3    5    3
    ##  [88,]    1    4    6    5
    ##  [89,]    6    7    2    5
    ##  [90,]    3    8    7    5
    ##  [91,]    2    5    5    4
    ##  [92,]    5    4    5    2
    ##  [93,]    5    4    8    7
    ##  [94,]    4    5    4    5
    ##  [95,]    4    2    2    5
    ##  [96,]    3    4    2    4
    ##  [97,]    3    4    5    4
    ##  [98,]    4    5    5    4
    ##  [99,]    8    5    2    6
    ## [100,]    1    6    5    3

``` r
fun1alt <- function(n = 100, k = 4, lambda = 4) {
  # YOUR CODE HERE
  
  x <- matrix( rpois(n*k, lambda) , ncol = 4)
  
  return(x)
}
f1 <- fun1alt(50000,4)


# Benchmarking
microbenchmark::microbenchmark(
  fun1(),
  fun1alt()
)
```

    ## Warning in microbenchmark::microbenchmark(fun1(), fun1alt()): less accurate
    ## nanosecond times to avoid potential integer overflows

    ## Unit: microseconds
    ##       expr     min      lq      mean   median       uq      max neval
    ##     fun1() 161.868 178.391 206.62196 191.4905 199.9570 1823.967   100
    ##  fun1alt()  12.382  13.489  23.56598  14.2065  14.9035  939.105   100

``` r
d <- matrix(1:16,ncol=4)
d
```

    ##      [,1] [,2] [,3] [,4]
    ## [1,]    1    5    9   13
    ## [2,]    2    6   10   14
    ## [3,]    3    7   11   15
    ## [4,]    4    8   12   16

``` r
diag(d)
```

    ## [1]  1  6 11 16

``` r
d[2]
```

    ## [1] 2

``` r
d[2,1]
```

    ## [1] 2

``` r
d[c(1,6,11,16)]
```

    ## [1]  1  6 11 16

``` r
cbind(1:4,1:4)
```

    ##      [,1] [,2]
    ## [1,]    1    1
    ## [2,]    2    2
    ## [3,]    3    3
    ## [4,]    4    4

``` r
d[cbind(1:4,1:4)]
```

    ## [1]  1  6 11 16

## Problem 3.

Find the column max (hint: Checkout the function max.col()).

``` r
# Data Generating Process (10 x 10,000 matrix)
set.seed(1234)
M <- matrix(runif(12), ncol=4)
M
```

    ##           [,1]      [,2]        [,3]      [,4]
    ## [1,] 0.1137034 0.6233794 0.009495756 0.5142511
    ## [2,] 0.6222994 0.8609154 0.232550506 0.6935913
    ## [3,] 0.6092747 0.6403106 0.666083758 0.5449748

``` r
# Find each column's max value
fun2 <- function(x) {
  apply(x, 2, max)
}
fun2(x=M)
```

    ## [1] 0.6222994 0.8609154 0.6660838 0.6935913

``` r
fun2alt <- function(x) {
  # YOUR CODE HERE
   idx <- max.col( t(x))
   x[cbind(idx,1:4)]
}
fun2alt(x=M)
```

    ## [1] 0.6222994 0.8609154 0.6660838 0.6935913

``` r
x <- matrix(rnorm(1e4), nrow=10)
# Benchmarking
microbenchmark::microbenchmark(
  fun2(x),
  fun2alt(x)
)
```

    ## Unit: microseconds
    ##        expr     min       lq      mean   median       uq      max neval
    ##     fun2(x) 515.616 539.4165 623.86994 562.9095 610.8590 2932.566   100
    ##  fun2alt(x)  68.716  75.4400  94.93755  79.8885  85.0545 1395.804   100

## Problem 4. Show PSOCK cluster example

``` r
library(parallel)
my_boot <- function(dat, stat, R, ncpus = 1L) {
  
  # Getting the random indices
  n <- nrow(dat)
  idx <- matrix(sample.int(n, n*R, TRUE), nrow=n, ncol=R)
 
  # Making the cluster using `ncpus`
  # STEP 1: GOES HERE
  
  cl <- makePSOCKcluster(4)  
  clusterSetRNGStream(cl, 123) # Equivalent to `set.seed(123)`
  # STEP 2: GOES HERE
  
  clusterExport(cl,c("stat","dat","idx"),envir=environment())
  
  # STEP 3: THIS FUNCTION NEEDS TO BE REPLACES WITH parLapply
  ans <- parLapply( cl,seq_len(R), function(i) {
    stat(dat[idx[,i], , drop=FALSE])
  })
  
  # Coercing the list into a matrix
  ans <- do.call(rbind, ans)
  
  # STEP 4: GOES HERE
  
  ans
  
}
```

``` r
# Bootstrap of an OLS
my_stat <- function(d) coef(lm(y ~ x, data=d))
# DATA SIM
set.seed(1)
n <- 500; R <- 1e4
x <- cbind(rnorm(n)); y <- x*5 + rnorm(n)
# Checking if we get something similar as lm
ans0 <- confint(lm(y~x))
ans1 <- my_boot(dat = data.frame(x, y), my_stat, R = R, ncpus = 2L)
#stopCluster(cl)
# You should get something like this
t(apply(ans1, 2, quantile, c(.025,.975)))
```

    ##                   2.5%      97.5%
    ## (Intercept) -0.1386903 0.04856752
    ## x            4.8685162 5.04351239

``` r
ans0
```

    ##                  2.5 %     97.5 %
    ## (Intercept) -0.1379033 0.04797344
    ## x            4.8650100 5.04883353
