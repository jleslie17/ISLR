library(ISLR)
data(Default)
head(Default)
boxplot(Default$default, Default$balance)
prop.table(table(Default$default))

fit <- glm(default ~ balance, 
           family = "binomial",
           data = Default)
summary(fit)
fit2 <- glm(default ~ student,
           family = "binomial",
           data = Default)
summary(fit2)
predict(fit2, newdata = data.frame(student <- "Yes"))
exp(predict(fit2, newdata = data.frame(student <- "Yes")))
exp(predict(fit2, newdata = data.frame(student <- "No")))

exp(predict(fit, newdata = data.frame(balance = 1000)))
?glm

# 4.6 Lab ========
names(Smarket)
data("Smarket")
head(Smarket)
dim(Smarket)
summary(Smarket)
pairs(Smarket)
#cor(Smarket)
cor(Smarket[-9])
attach(Smarket)
plot(Volume)
?plot

# 4.6.2 Logistic Regression ====
glm.fit = glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
              data = Smarket,
              family = binomial)
summary(glm.fit)
coef(glm.fit)
summary(glm.fit)$coef
summary(glm.fit)$coef[,4]
glm.probs <- predict(glm.fit, type = "response")
glm.probs[1:10]
contrasts(Direction)
glm.pred <- rep("Down", 1250)
glm.pred[glm.probs > 0.5] <- "Up"
table(glm.pred, Direction)
(507 + 145)/1250
mean(glm.pred == Direction)

train <- (Year < 2005)
Smarket.2005 <- Smarket[!train, ]
dim(Smarket.2005)
Direction.2005 <- Direction[!train]
glm.fit <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
               data = Smarket, family = binomial, subset = train)
glm.probs <- predict(glm.fit, Smarket.2005, type = "response")
glm.pred <- rep("Down", 252)
glm.pred[glm.probs > 0.5] <- "Up"
table(glm.pred, Direction.2005)
mean(glm.pred == Direction.2005)
mean(glm.pred != Direction.2005)

glm.fit <- glm(Direction ~ Lag1 + Lag2,
               data = Smarket,
               family = binomial,
               subset = train)
summary(glm.fit)
glm.probs <- predict(glm.fit, Smarket.2005, type = "response")
glm.probs[1:10]
glm.pred <- rep("Down", 252)
glm.pred[glm.probs > 0.5] <- "Up"
table(glm.pred, Direction.2005)
mean(glm.pred == Direction.2005)
mean(glm.pred != Direction.2005)
106/(35 + 106)
141/(141 + 111)
106/(76 + 106)
table(Direction.2005)

predict(glm.fit, 
        newdata = data.frame(Lag1 = c(1.2, 1.5),
                             Lag2 = c(1.1, -0.8)),
        type = "response")

# 4.6.3 Linear Discriminant Analysis ====
library(MASS)
lda.fit <- lda(Direction ~ Lag1 + Lag2,
               data = Smarket,
               subset = train)
lda.fit
plot(lda.fit)
lda.pred <- predict(lda.fit, Smarket.2005)
names(lda.pred)
lda.pred[1]
lda.pred[2]
lda.pred[3]

lda.class <- lda.pred$class
table(lda.class, Direction.2005)
mean(lda.class == Direction.2005)
head(lda.pred[2])
sum(lda.pred$posterior[,1] >= 0.5)
sum(lda.pred$posterior[,1] <= 0.5)
lda.pred$posterior[1:10]
head(lda.pred$posterior)
lda.pred$posterior[1:20, 1]
lda.class[1:20]
sum(lda.pred$posterior[,1] > 0.9)

# 4.6.4 Quadratic Discriminant Analysis ====
qda.fit <- qda(Direction ~ Lag1 + Lag2,
               data = Smarket,
               subset = train)
qda.fit
qda.class = predict(qda.fit, Smarket.2005)$class
table(qda.class, Direction.2005)
mean(qda.class == Direction.2005)

# 4.6.5 K-Nearest Neighbors
library(class)
train.X <- cbind(Lag1, Lag2)[train, ]
test.X <- cbind(Lag1, Lag2)[!train, ]
train.Direction <- Direction[train]
set.seed(1)
knn.pred <- knn(train.X, test.X, train.Direction, k = 1)
table(knn.pred, Direction.2005)
(83 + 43)/252
knn.pred <- knn(train.X, test.X, train.Direction, k = 3)
table(knn.pred, Direction.2005)
mean(knn.pred == Direction.2005)

# 4.6.6 An Application to Caravan Insurance Data ====
dim(Caravan)
detach(Smarket)
attach(Caravan)
summary(Purchase)
348/5822
standarized.X <- scale(Caravan[, -86])
var(Caravan[,1])
var(Caravan[,2])
var(standarized.X[,1])
var(standarized.X[,2])

test <- 1:1000
train.X <- standarized.X[-test,]
test.X <- standarized.X[test,]
train.Y <- Purchase[-test]
test.Y <- Purchase[test]
set.seed(1)
knn.pred <- knn(train.X, test.X, train.Y, k = 1)
mean(test.Y != knn.pred)
mean(test.Y != "No")
table(knn.pred, test.Y)
9/(68+9)

knn.pred <- knn(train.X, test.X, train.Y, k = 3)
table(knn.pred, test.Y)
5/26
knn.pred <- knn(train.X, test.X, train.Y, k = 5)
table(knn.pred, test.Y)
4/15

glm.fit <- glm(Purchase ~ .,
               data = Caravan,
               family = binomial,
               subset = -test)
glm.probs <- predict(glm.fit, Caravan[test, ], type = "response")
glm.pred <- rep("No", 1000)
glm.pred[glm.probs > 0.5] <- "Yes"
table(glm.pred, test.Y)
glm.pred <- rep("No", 1000)
glm.pred[glm.probs > 0.25] <- "Yes"
table(glm.pred, test.Y)
11/(11+22)

# 4.7 Exercises =====
# 6
B0 <- -6
B1 <- 0.05
B2 <- 1
x1 <- 40
x2 <- 3.5




p <- exp(B0 + B1*x1 + B2*x2)/(1 + exp(B0 + B1*x1 + B2*x2))
p
p <- 0.5
x <- (log(p/(1 - p)) - B0 - B2*x2)/B1
x

# 9
37/137

0.16/(1 - 0.16)

# 10 ====
detach(Caravan)
rm(list = ls())

# a
data("Weekly")
head(Weekly)
nrow(Weekly[Weekly$Year == 1991,])
attach(Weekly)
plot(Today)
plot(Volume)
summary(Weekly)
cor(Weekly[,-9])

# b
?glm
fit.glm <- glm(
  Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
  data = Weekly,
  family = binomial
)
summary(fit.glm)
# Lag2 appears to be statistically significant

# c
glm.probs <- predict(fit.glm, type = "response")
glm.pred <- rep("Down", 1089)
glm.pred[glm.probs > 0.5] <- "Up"
table(glm.pred, Direction)
mean(glm.pred == Direction) #Accuracy
557/(557 + 48) #Sensitivity
54/(54 + 430) #Specificiy
430/(430 + 54) #False positive rate

# d
train <- Year < 2009
glm.fit <- glm(
  Direction ~ Lag2,
  data = Weekly,
  family = binomial,
  subset = train
)
Weekly_test <- Weekly[!train,]
Direction_test <- Direction[!train]
glm.probs <- predict(
  glm.fit, 
  newdata = Weekly_test,
  type = "response")
glm.pred <- rep("Down", 104)
glm.pred[glm.probs > 0.5] <- "Up"
table(glm.pred, Direction_test)
mean(glm.pred == Direction_test)
56/(56+5) #True positive rate/Sensitivity
9/(9+34) #Specificity
34/(9+34) #False positive rate

# e
lda.fit <- lda(
  Direction ~ Lag2,
  data = Weekly,
  subset = train
)
lda.pred <- predict(
  lda.fit,
  newdata = Weekly_test
)
names(lda.pred)
lda.class <- lda.pred$class
table(lda.class, Direction_test)
mean(lda.class == Direction_test)
56/(56+5) #True positive rate/Sensitivity
9/(9+34) #Specificity
34/(9+34) #False positive rate

# f
qda.fit <- qda(
  Direction ~ Lag2,
  data = Weekly,
  subset = train
)
qda.pred <- predict(
  qda.fit,
  Weekly_test
)
names(qda.pred)
qda.class <- qda.pred$class
table(qda.class, Direction_test)
mean(qda.class == Direction_test)

# g
train.X <- as.matrix(Lag2[train])
test.X <- as.matrix(Lag2[!train])
train.Direction <- Direction[train]
set.seed(1)
knn.pred <- knn(train.X, test.X, train.Direction, k = 1)
table(knn.pred, Direction_test)
mean(knn.pred == Direction_test)
31/(31+30) #True positive rate/sensitivity
21/(21 + 22) #True negative rate/specificity
22/(21 + 22) #False positive rate

# h
mean(glm.pred == Direction_test)
mean(lda.class == Direction_test)
mean(qda.class == Direction_test)
mean(knn.pred == Direction_test)


#i
plot(Volume)
plot(exp(Volume))
plot(log(Volume))
plot(sqrt(Volume))
glm.fit2 <- glm(
  Direction ~ log(Volume),
  data = Weekly,
  family = binomial,
  subset = train
)
summary(glm.fit2)
plot(Volume, Direction)
detach(Weekly)

# 11 ====
rm(list = ls())
attach(Auto)
head(Auto)
summary(Auto)
# a
mpg01 <- as.integer(mpg > median(mpg))
mpg01
Auto$mpg01 <- mpg01
summary(Auto)
mean(mpg01)

# b
cor(Auto[-9])
plot(Auto)
boxplot(cylinders ~ mpg01, main = "Cylinders vs. mpg01")
boxplot(displacement ~ mpg01, main = "Displacement vs. mpg01")
boxplot(horsepower ~ mpg01, main = "Horespower vs. mpg01")
boxplot(weight ~ mpg01, main = "Weight vs. mpg01")
plot(displacement, horsepower, pch = mpg01)
?plot

# c
train <- year %% 2 == 0
table(train)
Auto.train <- Auto[train,]
Auto.test <- Auto[!train,]
mpg01.test <- mpg01[!train]


# d
lda.fit <- lda(
  mpg01 ~ cylinders + displacement + horsepower + weight,
  data = Auto,
  subset = train
)
lda.fit
lda.pred <- predict(
  lda.fit,
  Auto.test
)
names(lda.pred)
lda.class <- lda.pred$class
table(lda.class, mpg01.test)
mean(lda.class != mpg01.test) #Test error rate

# e
qda.fit <- qda(
  mpg01 ~ cylinders + displacement + horsepower + weight,
  data = Auto,
  subset = train
)
qda.fit
qda.pred <- predict(
  qda.fit,
  Auto.test
)
qda.class <- qda.pred$class
table(qda.class, mpg01.test)
mean(qda.class != mpg01.test) #Test error rate

# f
glm.fit <- glm(
  mpg01 ~ cylinders + displacement + horsepower + weight,
  data = Auto,
  family = binomial,
  subset = train
)
glm.probs <- predict(
  glm.fit,
  newdata = Auto.test,
  type = "response"
)
glm.pred <- rep(0, length(mpg01.test))
glm.pred[glm.probs > 0.5] <- 1
table(glm.pred, mpg01.test)
mean(glm.pred != mpg01.test) #Test error rate

# g
train.X <- cbind(cylinders, displacement, horsepower, weight)[train,]
test.X <- cbind(cylinders, displacement, horsepower, weight)[!train,]
train.Direction <- mpg01[train]
set.seed(1)
knn.pred <- knn(train.X, test.X, train.Direction, k = 1)
table(knn.pred, mpg01.test)
mean(knn.pred != mpg01.test) #Test error rate
for(k in c(1, 3, 5, 10, 50, 100)){
  set.seed(1)
  knn.pred <- knn(train.X, test.X, train.Direction, k = k)
  print(k)
  print(mean(knn.pred != mpg01.test))
}

# 12====
# a
Power <- function(){
  print(2^3)  
}
Power()

# b
Power2 <- function(x, a){
  x^a
}
Power2(3,8)

# c
Power2(10, 3)
Power2(8, 17)
Power2(131, 3)

# d
Power3 <- function(x, a){
  result <- x ^ a
  return(result)
}
# e
x <- 1:10
y <- Power3(x, 2)
y
plot(x, y)
plot(x, log(y))
plot(x, y, log = "xy",
     main = "Y vs. X",
     xlab = "log(x)",
     ylab = "log(x ^ 2)")

# f
PlotPower <- function(x, a) {
  y <- x ^ a
  plot(x, y, xlab = "x", ylab = "x ^ a")
}
PlotPower(1:100, 2)
detach(Auto)
rm(list = ls())

# 13====
library(MASS)
data("Boston")
attach(Boston)
head(Boston)
crim01 <- as.integer(crim > median(crim))
Boston$crim01 <- crim01

train <- 1:(length(crim01)/2)
Boston.train <- Boston[train, ]
Boston.test <- Boston[-train, ]
crim01.test <- crim01[-train]
pairs(Boston)
cor(Boston)
# Logistic regression
glm.fit <- glm(
  crim01 ~ . -crim -crim01 -chas -nox -tax -lstat,
  data = Boston,
  family = binomial,
  subset = train
)
summary(glm.fit)

glm.probs <- predict(
  glm.fit,
  newdata = Boston.test,
  type = "response"
)
glm.pred <- rep(0, length(crim01.test))
glm.pred[glm.probs > 0.5] <- 1
table(glm.pred, crim01.test)
mean(glm.pred != crim01.test) #Test error rate

# LDA
lda.fit <- lda(
  crim01 ~ . -crim -crim01 -chas -nox -tax -lstat,
  data = Boston,
  subset = train
)
plot(lda.fit)
lda.pred <- predict(
  lda.fit,
  Boston.test
)
lda.class <- lda.pred$class
table(lda.class, crim01.test)
mean(lda.class != crim01.test) #Test error rate

# QDA
qda.fit <- qda(
  crim01 ~ . -crim -crim01 -chas -nox -tax -lstat,
  data = Boston,
  subset = train
)
qda.pred <- predict(
  qda.fit,
  Boston.test
)
qda.class <- qda.pred$class
table(qda.class, crim01.test)
mean(qda.class != crim01.test)

# KNN
library(class)
names(Boston.train)
train.X <- Boston.train[,-c(1, 15)]
test.X <- Boston.test[, -c(1, 15)]
train.crim01 <- crim01[train]

knn.pred <- knn(
  train.X,
  test.X,
  train.crim01,
  k = 1
)
table(knn.pred, crim01.test)
mean(knn.pred != crim01.test) #Test error rate

knn.pred <- knn(
  train.X,
  test.X,
  train.crim01,
  k = 5
)
table(knn.pred, crim01.test)
mean(knn.pred != crim01.test) #Test error rate

for(k in c(1, 5, 10, 50, 100)) {
  print(k)
  knn.pred <- knn(
    train.X,
    test.X,
    train.crim01,
    k = k
  )
  
  print(mean(knn.pred != crim01.test))
}
