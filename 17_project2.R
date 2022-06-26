## ---- echo=FALSE, warning=FALSE-----------------------------------------------
suppressPackageStartupMessages(library(MASS, quietly = TRUE))


## -----------------------------------------------------------------------------
mvrnorm(n = 5, mu = c(0,0), Sigma = diag(1, nrow = 2))


## ---- echo=FALSE--------------------------------------------------------------
make_prior <- function(alpha) {
  mean <- c(0, 0)
  S <- diag(1/alpha, nrow = 2)
  list(mean = mean, S = S)
}

sample_from_prior <- function(n, alpha) {
  prior <- make_prior(alpha)
  samples <- mvrnorm(n = n, mu = prior$mean, Sigma = prior$S)
  data.frame(w1 = samples[,1], w0 = samples[,2])  
}


## ----points-from-prior, fig.cap="Weight vectors sampled from the prior distribution.", echo=FALSE----
samples <- sample_from_prior(10, 1)
plot(samples$w0, samples$w1, pch=20,
     xlab=expression(w[0]), ylab=expression(w[1]))


## ----lines-from-prior, fig.cap="Weight vectors sampled from the prior distribution represented as lines.", echo=FALSE----
plot(c(-1,1), c(-1,1), type="n", xlab="x", ylab="y")
for (i in seq(samples$w0)) {
    abline(a = samples$w0[i], b = samples$w1[i])
}


## -----------------------------------------------------------------------------
w0 <- 0.3 ; w1 <- 1.1 ; beta <- 1.3
x <- rnorm(50)
y <- rnorm(50, w1 * x + w0, 1/beta)


## ----sampled-points, fig.cap="Randomly sampled $(x,y)$ values. The true linear model is shown in red.", echo=FALSE----
plot(x, y, pch=20)
abline(a = w0, b = w1, col="red")


## ---- echo=FALSE--------------------------------------------------------------
make_model_matrix <- function(x) cbind(1, x)

make_posterior <- function(x, y, alpha, beta) {
    phi <- make_model_matrix(x)
    S <- solve(diag(alpha, 2) + beta * t(phi) %*% phi)
    mean <- beta * S %*% t(phi) %*% y
    list(mean = mean, S = S)
}

sample_from_posterior <- function(n, x, y, alpha, beta) {
  posterior <- make_posterior(x, y, alpha, beta)
  samples <- mvrnorm(n = n, mu = posterior$mean, Sigma = posterior$S)
  data.frame(w0 = samples[,1], w1 = samples[,2])  
}


## ----samples-from-posterior, fig.cap="Lines drawn from the posterior. The true line is shown in red.", echo=FALSE----
plot_samples <- function(n) {
    x <- rnorm(n)
    y <- rnorm(n, w1 * x + w0, 1/beta)
    samples <- sample_from_posterior(10, x, y, beta, 1)

    plot(x, y, pch=20, main=paste(n, "points"), xlim=c(-3,3), ylim=c(-3,3))
    for (i in seq(samples$w0)) {
        abline(a = samples$w0[i], b = samples$w1[i], col="lightgray")
    }
    points(x, y, pch=20)
    abline(a = w0, b = w1, col="red")
}

op <- par(mar = c(2,2,3,1))
layout(matrix(1:4, nrow=2, byrow=TRUE))
plot_samples(5)  ; plot_samples(10)
plot_samples(20) ; plot_samples(50)
par(op)


## ----predicted-line, echo = FALSE, fig.cap="True linear model in red and predicted values in blue."----
predict_new_dist <- function(x, posterior, beta) {
  phi <- function(x) matrix(c(1, x), ncol=1)
  
  mean = t(posterior$mean) %*% phi(x)
  var = 1/beta + t(phi(x)) %*% posterior$S %*% phi(x)
  list(mean = mean, var = var)
}

predict_new_map <- function(predict_dist) predict_dist$mean

alpha <- 1; beta <- 0.2
w0 <- 0.2; w1 <- 1.2
x <- rnorm(10, sd=50)
y <- rnorm(10, w1 * x + w0, 1/beta)

posterior <- make_posterior(x, y, alpha, beta)
predict_x <- Vectorize(function(x) predict_new_map(predict_new_dist(x, posterior, beta)))

plot(x, y, pch=20)
abline(a = w0, b = w1, col="red")

new_x <- seq(min(x)-10, max(x)+10, length.out = 100)
new_y <- predict_x(new_x)
lines(new_x, new_y, col="blue")



## ----prediction-with-support, fig.cap="Prediction with 95% support interval.", echo=FALSE----
predict_new_quantile <- function(x, q, posterior, beta) {
  predict_dist <- predict_new_dist(x, posterior, beta)
  qnorm(q, mean = predict_dist$mean, sd = sqrt(predict_dist$var))
}

new_y_q <- rbind(vapply(new_x, function(x) predict_new_quantile(x, 0.975, posterior, beta), 1),
                 vapply(new_x, function(x) predict_new_quantile(x, 0.025, posterior, beta), 1))

xx <- vapply(new_x, function(x) predict_new_quantile(x, 0.975, posterior, beta), 1)


plot(x, y, pch=20, xlim=c(min(x)-10,max(x)+10), ylim=c(-100,200))
abline(a = w0, b = w1, col="red")
lines(new_x, new_y, col="blue")
lines(new_x, new_y_q[1,], col="blue", lty="dashed")
lines(new_x, new_y_q[2,], col="blue", lty="dashed")


## -----------------------------------------------------------------------------
predictors <- data.frame(x = rnorm(5), z = rnorm(5))
y <- with(predictors, rnorm(5, mean = 3*x + 5*z + 2))

model <- y ~ x + z

model.frame(model, data = predictors)


## -----------------------------------------------------------------------------
x <- runif(10)
model.frame(~ x + I(x^2))


## -----------------------------------------------------------------------------
x <- runif(10)
y <- rnorm(10, mean=x)

model.no.intercept <- y ~ x + 0
(frame.no.intercept <- model.frame(model.no.intercept))
model.matrix(model.no.intercept, frame.no.intercept)

model.with.intercept <- y ~ x
(frame.with.intercept <- model.frame(model.with.intercept))
model.matrix(model.with.intercept, frame.with.intercept)


## -----------------------------------------------------------------------------
model.response(frame.with.intercept)


## -----------------------------------------------------------------------------
training.data <- data.frame(x = runif(5), y = runif(5))
frame <- model.frame(y ~ x, training.data)
model.matrix(y ~ x, frame)

predict.data <- data.frame(x = runif(5))


## -----------------------------------------------------------------------------
frame <- model.frame(~ x, predict.data)
model.matrix(~ x, frame)


## -----------------------------------------------------------------------------
# assume this is a parameter you don't know
unknown <- y ~ x 

# get the formula without the response
responseless_formula <- delete.response(terms(unknown))
# and then you can use it with model.frame
frame <- model.frame(responseless_formula, predict.data)
model.matrix(responseless_formula, frame)

