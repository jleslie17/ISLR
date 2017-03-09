# 5.3 Lab: Cross-validation and the Bootstrap

# 5.3.1 The validation set approach
library(ISLR)
set.seed(1)
train <- sample(392, 196)
?sample
lm.fit <- lm(mpg ~ horsepower, data = Auto, subset = train)

attach(Auto)
mean((mpg - predict(lm.fit, Auto))[-train] ^ 2)
# Therefore the estimated test MSE for the linear regressino fit is 26.14. We
# can use the poly() function to estimate the test error for the polynomial and
# cubic regressions
lm.fit2 <- lm(mpg ~ poly(horsepower, 2), data = Auto, subset = train)
mean((mpg - predict(lm.fit2, Auto))[-train]^2)
lm.fit3 <- lm(mpg ~ poly(horsepower, 3), data = Auto, subset = train)
mean((mpg - predict(lm.fit3, Auto))[-train]^2)

set.seed(2)
train <- sample(392, 196)
lm.fit <- lm(mpg ~ horsepower, subset = train)
mean((mpg - predict(lm.fit, Auto))[-train]^2)
lm.fit2 <- lm(mpg ~ poly(horsepower, 2), subset = train)
mean((mpg - predict(lm.fit2, Auto))[-train] ^ 2)
lm.fit3 <- lm(mpg ~ poly(horsepower, 3), subset = train)
mean((mpg - predict(lm.fit3, Auto))[-train] ^ 2)

# 5.3.2 Leave-one-out cross-validation
library(boot)
glm.fit <- glm(mpg ~ horsepower, data = Auto)
cv.err <- cv.glm(Auto, glm.fit)
cv.err$delta
?cv.glm

cv.error <- rep(0, 5)
for (i in 1:5) {
  glm.fit <- glm(mpg ~ poly(horsepower, i), data = Auto)
  cv.error[i] <- cv.glm(Auto, glm.fit)$delta[1]
}
cv.error

# 5.3.3 k-fold cros-validation
set.seed(17)
cv.error.10 <- rep(0, 10)
for (i in 1:10) {
  glm.fit <- glm(mpg ~ poly(horsepower, i), data = Auto)
  cv.error.10[i] <- cv.glm(Auto, glm.fit, K = 10)$delta[1]
}
cv.error.10

# 5.3.4 The Bootstrap
alpha.fn <- function(data, index) {
  X <- data$X[index]
  Y <- data$Y[index]
  return((var(Y) - cov(X, Y))/(var(X) + var(Y) - 2*cov(X,Y)))
}
alpha.fn(Portfolio, 1:100)

set.seed(1)
alpha.fn(Portfolio, sample(100, 100, replace = T))

boot(Portfolio, alpha.fn, R = 1000)

boot.fn <- function(data, index) 
  return(coef(lm(mpg ~ horsepower, data = data, subset = index)))
boot.fn(Auto, 1:392)
set.seed(1)
boot.fn(Auto, sample(392, 392, replace = T))
boot(Auto, boot.fn, 1000)
summary(lm(mpg ~ horsepower, data = Auto))$coef
# Bootstrap gives more accurate estimates of standard errors of B0 and B1 than
# the summary() function

# Try quadratic for the same...should be a better fit
boot.fn <- function(data, index)
  coefficients(lm(mpg ~ horsepower + I(horsepower^2), 
                  data = data, subset = index))
set.seed(1)
boot(Auto, boot.fn, 1000)
summary(lm(mpg ~ horsepower + I(horsepower ^ 2), 
           data = Auto))$coef
detach(Auto)

# 5.4 Exercises
# 5
attach(Default)
set.seed(1)
# a
glm.fit <- glm(
  default ~ income + balance,
  data = Default,
  family = binomial
)
summary(glm.fit)

# b
train <- sample(dim(Default)[1], dim(Default)[1]/2)
glm.fit <- glm(
  default ~ income + balance,
  data = Default,
  family = binomial,
  subset = train
)
# summary(glm.fit)
glm.probs <- predict(
  glm.fit,
  newdata = Default[-train,],
  type = "response"
)
glm.pred <- rep("No", length(train))
glm.pred[glm.probs > 0.5] <- "Yes"
mean(glm.pred != Default$default[-train])

# d
head(Default)
set.seed(1)
train <- sample(dim(Default)[1], dim(Default)[1]/2)
glm.pred <- glm(
  default ~ income + balance + student,
  data = Default,
  family = binomial,
  subset = train
)
glm.probs <- predict(glm.fit, Default[-train, ], type = "response")
glm.pred <- rep("No", length(train))
glm.pred[glm.probs > 0.5] <- "Yes"
mean(glm.pred != Default$default[-train])
# Adding dummy variables doesn't appear to have an effect

# 6
# a
set.seed(1)
glm.fit <- glm(
  default ~ income + balance,
  data = Default,
  family = binomial
)
summary(glm.fit)
# glm() esimates for SE of B0, B1 and B2 are 0.43, 4.99^-6 and 2.27^-4
# b
boot.fn <- function(data, index) {
  fit <- glm(default ~ income + balance, 
             data = data,
             family = binomial,
             subset = index)
  return(coef(fit))
}
boot.fn(Default, 1:1000)
# c
library(boot)
boot(Default, boot.fn, 1000)
# d
# Not much difference in the estimated std errors
detach(Auto)

# 7
# a
attach(Weekly)
head(Weekly)
set.seed(1)
glm.fit <- glm(
  Direction ~ Lag1 + Lag2,
  data = Weekly,
  family = binomial
)
# b
glm.fit.not1 <- glm(
  Direction ~ Lag1 + Lag2,
  data = Weekly,
  family = binomial,
  subset = -1
)
summary(glm.fit)
summary(glm.fit.not1)
# c
first_obs <- predict(
  glm.fit.not1,
  newdata = Weekly[1, ],
  type = "response"
)
first_obs > 0.5
Direction[1]
# Incorrectly classified
# d
error <- rep(0, nrow(Weekly))
for (i in 1:nrow(Weekly)) {
  glm.fit <- glm(
    Direction ~ Lag1 + Lag2,
    data = Weekly,
    family = binomial,
    subset = -i
  )
  glm.probs <- predict(
    glm.fit,
    Weekly[i,],
    type = "response"
  )
  if (glm.probs > 0.5) {
    glm.pred <- "Up"
  } else {
    glm.pred <- "Down"
  }
  error[i] <- glm.pred != Weekly$Direction[i]
}
# e
mean(error)
# The LOOCV estimate for test error rate is 44.995%

# 8
# a
set.seed(1)
y <- rnorm(100)
x <- rnorm(100)
y <- x - 2*x^2 + rnorm(100)
# n = 100, p = 2; y = x - 2x^2 + e
# b
plot(x, y)
# There appears to be a curved relationship
# c
library(boot)
set.seed(1)
glm.fit <- glm(
  y ~ x
)
cv.error <- cv.glm(
  data.frame(y = y, x = x),
  glmfit = glm.fit
)$delta[1]
cv.error
glm.fit2 <- glm(y ~ poly(x, 2))
cv.glm(data.frame(x = x, y = y), glm.fit2)$delta[1]

glm.fit3 <- glm(y ~ poly(x, 3))
cv.glm(data.frame(x = x, y = y), glm.fit3)$delta[1]

glm.fit4 <- glm(y ~ poly(x, 4))
cv.glm(data.frame(x = x, y = y), glm.fit4)$delta[1]
# e
set.seed(10)
glm.fit <- glm(
  y ~ x
)
cv.error <- cv.glm(
  data.frame(y = y, x = x),
  glmfit = glm.fit
)$delta[1]
cv.error
glm.fit2 <- glm(y ~ poly(x, 2))
cv.glm(data.frame(x = x, y = y), glm.fit2)$delta[1]

glm.fit3 <- glm(y ~ poly(x, 3))
cv.glm(data.frame(x = x, y = y), glm.fit3)$delta[1]

glm.fit4 <- glm(y ~ poly(x, 4))
cv.glm(data.frame(x = x, y = y), glm.fit4)$delta[1]
# errors are the same since LOOCV evaluates n folds of a single observation
# f
summary(glm.fit4)
# The p-values show that the linear and quadratic terms are statistically
# significants and that the cubic and 4th degree terms are not statistically
# significants. This agree strongly with our cross-validation results which were
# minimum for the quadratic model.
detach(Weekly)

# 9
library(MASS)
attach(Boston)
# a
mu_hat <- mean(medv)
mu_hat
# b
se_hat <- sd(medv)/sqrt(nrow(Boston))
se_hat
# c
set.seed(1)
boot.fn <- function(data, index) {
  mu <- mean(data[index])
  return(mu)
}
boot.fn(medv, nrow(Boston))
boot(medv, boot.fn, 1000)
# d
t.test(medv)
CI.mu.hat <- 22.53 + c(-1, 1) * 2 *0.4119
CI.mu.hat
# e
med.hat <- median(medv)
med.hat
# f
boot.fn <- function(data, index) {
  med.hat <- median(data[index])
  return(med.hat)
}
boot(medv, boot.fn, 1000)
# g
quantile(medv, probs = 0.1)
# h
boot.fn <- function(data, index) {
  mu.10 <- quantile(medv[index], probs = 0.1)
  return(mu.10)
}
boot(medv, boot.fn, 1000)
# We get an estimated tenth percentile value of 12.75 which is again equal to
# the value obtained in (g), with a standard error of 0.5113 which is relatively
# small compared to percentile value.