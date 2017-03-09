# 6.5 Lab 1: Subset Selection Methods ====
# 6.5.1 Best Subset Selection ====
library(ISLR)
fix(Hitters)
?fix
names(Hitters)
dim(Hitters)
sum(is.na(Hitters$Salary))
Hitters <- na.omit(Hitters)
dim(Hitters)
sum(is.na(Hitters))

library(leaps)
regfit.full <- regsubsets(Salary ~ ., Hitters)
summary(regfit.full)

regfit.full <- regsubsets(Salary ~ ., data = Hitters, nvmax = 19)
reg.summary <- summary(regfit.full)
names(reg.summary)
reg.summary$rsq
par(mfrow = c(2,2))
plot(reg.summary$rss, xlab = "Number of Variables", 
     ylab = "RSS",
     type = 'l')
plot(reg.summary$adjr2,
     xlab = "Number of Variables",
     ylab = "Adjusted RSq", 
     type = 'l')
which.max(reg.summary$adjr2)
points(11, reg.summary$adjr2[11], col = "red", cex = 2, pch = 20)
plot(reg.summary$cp,
     xlab = "Number of Variables",
     ylab = "Cp",
     type = 'l')
which.min(reg.summary$cp)
points(10, reg.summary$cp[10], col = "red", cex = 2, pch = 20)
which.min(reg.summary$bic)
plot(reg.summary$bic, 
     xlab = "Number of Variables", 
     ylab = "BIC",
     type = "l")
points(6, reg.summary$bic[6], col = "red", cex = 2, pch = 20)

plot(regfit.full, scale = "r2")
plot(regfit.full, scale = "adjr2")
plot(regfit.full, scale = "Cp")
plot(regfit.full, scale = "bic")
coef(regfit.full, 6)

# 6.5.2 Forward and Backward Stepwise Selection ====
regfit.fwd <- regsubsets(Salary ~ ., 
                         data = Hitters,
                         nvmax = 19, 
                         method = "forward")
summary(regfit.fwd)
regfit.bwd <- regsubsets(Salary ~ .,
                         data = Hitters,
                         nvmax = 19,
                         method = "backward")
summary(regfit.bwd)
coef(regfit.full, 7)
coef(regfit.fwd, 7)
coef(regfit.bwd, 7)

# 6.5.3 Choosing Among Models: Vald set and Cross-valid ====
set.seed(1)
train <- sample(c(TRUE, FALSE), nrow(Hitters), rep = TRUE)
test <- (!train)
regfit.best <- regsubsets(Salary ~ ., 
                          data = Hitters[train, ],
                          nvmax = 19)
test.mat <- model.matrix(Salary ~ ., 
                         data = Hitters[test, ])
test.mat
val.errors <- rep(NA, 19)
for (i in 1:19) {
  coefi = coef(regfit.best, id = i)
  pred = test.mat[,names(coefi)] %*% coefi
  val.errors[i] = mean((Hitters$Salary[test] - pred) ^ 2)
}
val.errors
which.min(val.errors)
coef(regfit.best,10)
predict.regsubsets <- function(object, newdata, id, ...) {
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form, newdata)
  coefi <- coef(object, id = id)
  xvars <- names(coefi)
  mat[, xvars] %*% coefi
}

regfit.best <- regsubsets(Salary ~ ., 
                          data = Hitters,
                          nvmax = 19)
coef(regfit.best, 10)
