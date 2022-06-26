## ---- echo=FALSE--------------------------------------------------------------
area <- function(x) UseMethod("area")
area.circle <- function(x) pi * x$r**2
area.rectangle <- function(x) x$height * x$width

circumference <- function(x) UseMethod("circumference")
circumference.circle <- function(x) 2 * pi * x$r
circumference.rectangle <- function(x) 2 * x$height + 2 * x$width

rectangle <- function(width, height) {
    structure(list(width = width, height = height),
              class = c("rectangle", "shape"))
}

circle <- function(radius) {
    structure(list(r = radius),
              class = c("circle", "shape"))
}


## -----------------------------------------------------------------------------
area <- function(x) UseMethod("area")
circumference <- function(x) UseMethod("circumference")

rectangle <- function(width, height) {
    structure(list(width = width, height = height),
              class = c("rectangle", "shape"))
}
area.rectangle <- function(x) x$height * x$width
circumference.rectangle <- function(x) 2 * x$height + 2 * x$width

r <- rectangle(width = 2, height = 4)
area(r)
circumference(r)


## -----------------------------------------------------------------------------
r <- rectangle(width = 2, height = 4)
if (area(r) != 2*4) {
    stop("Area not computed correctly!")
}
if (circumference(r) != 2*2 + 2*4) {
    stop("Circumference not computed correctly!")
}


## ---- eval=FALSE--------------------------------------------------------------
## context("Testing area and circumference")
## 
## test_that("we compute the correct area and circumference", {
##   r <- rectangle(width = 2, height = 4)
## 
##   expect_equal(area(r), 2*4)
##   expect_equal(circumference(r), 2*2 + 2*4)
## })


## ---- eval=FALSE--------------------------------------------------------------
## circle <- function(radius) {
##     structure(list(r = radius),
##               class = c("circle", "shape"))
## }
## area.circle <- function(x) pi * x$r**2
## circumference.circle <- function(x) 2 * pi * x$r
## 
## test_that("we compute the correct area and circumference", {
##   radius <- 2
##   circ <- circle(radius = radius)
## 
##   expect_equal(area(circ), pi * radius^2)
##   expect_equal(circumference(circ), 2 * radius * pi)
## })


## ---- eval=FALSE--------------------------------------------------------------
## test_that("Dimensions are positive", {
##   expect_error(rectangle(width = -1, height =  4))
##   expect_error(rectangle(width =  2, height = -1))
##   expect_error(rectangle(width = -1, height = -1))
## 
##   expect_error(rectangle(width =  0, height =  4))
##   expect_error(rectangle(width =  2, height =  0))
##   expect_error(rectangle(width =  0, height =  0))
## })


## ---- eval=FALSE--------------------------------------------------------------
## seed <- as.integer(1000 * rnorm(1))
## test_that(paste("The test works with seed", seed), {
##   set.seed(seed)
##   # test code that uses random numbers
## })


## ---- eval=FALSE--------------------------------------------------------------
## seed <- as.integer(1000 * rnorm(1))
## test_that(paste("Sample mean is close to true, seed", seed), {
##   set.seed(seed)
## 
##   data <- rnorm(10000)
##   sample_size <- 100
##   samples <- sample(data, size = sample_size, replace = TRUE)
## 
##   true_mean <- mean(data)
##   sample_mean <- mean(samples)
## 
##   standard_error <- sd(samples) / sqrt(sample_size)
##   Z <- (true_mean - sample_mean) / standard_error
##   threshold <- qnorm(1 - 1/2000)
## 
##   expect_less_than(abs(Z), threshold)
## })

