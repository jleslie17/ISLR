?contour
x <- rnorm(100)
y <- rnorm(100)
plot(x,y)

x <- seq(-pi, pi, length = 50)
y <- x
f <- outer(x, y, function(x, y) cos(y)/(1 + x^2))
f
head(f)
contour(x, y, f)
contour(x, y, f, nlevels = 45, add = T)
fa <- (f - t(f))/2
contour(x, y, fa, nlevels = 15)

image(x, y, fa)
persp(x, y, fa)
persp(x, y, fa, theta = 30)
persp(x, y, fa, theta = 30, phi = 20)
persp(x, y, fa, theta = 30, phi = 70)
persp(x, y, fa, theta = 30, phi = 40)

2.3.3 Indexing data
A = matrix(1:16, 4, 4)
A
A[2,3]
A[c(1,3), c(2,4)]
A[1:3, 2:4]

Auto <- read.table("Auto.data")
fix(Auto)
Auto <- read.table("Auto.data", header = T, na.strings = "?")
fix(Auto)
Auto <- read.csv("Auto.csv", header = T, na.strings = "?")
dim(Auto)
Auto <- na.omit(Auto)
names(Auto)
plot(Auto$cylinders, Auto$mpg)
attach(Auto)
plot(cylinders, mpg)
typeof(cylinders)
cylinders <- as.factor(cylinders)
plot(cylinders, mpg)
plot(cylinders, mpg, col = 'red')
plot(cylinders, mpg, col = 'red', varwidth = T)
plot(cylinders, mpg, col = 'red', varwidth = T, horizontal = T)
plot(cylinders, mpg, col = 'red', varwidth = T, xlab = "cylinders", 
     ylab = "mpg")
typeof(Auto$cylinders)
hist(mpg)
hist(mpg, col = 2)
hist(mpg, col = 2, breaks = 15)

pairs(Auto)
pairs(~ mpg + displacement + horsepower + weight + acceleration, Auto)
search()
detach(Auto)

attach(Auto)
plot(horsepower, mpg)
identify(horsepower, mpg, name) # !!!!!!!!!!!!!!!!!1
summary(Auto)
summary(cylinders)
summary(Auto$cylinders)
search()
detach(Auto)

# Exercises
Q7 <- data.frame(X1 = c(0, 2, 0, 0, -1, 1),
                 X2 = c(3, 0, 1, 1, 0, 1),
                 X3 = c(0, 0, 3, 2, 1, 1),
                 Y = as.factor(c("R", "R", "R", "G", "G", "R")))
Q7
test <- rep(0, 3)
test
dist(test, as.matrix(Q7[,1:3]))
?dist
dist(test, Q7[1,1:3])
Q7[1,1:3]
dist(rbind(test, Q7[1,1:3]))
Distance <- numeric()
for (i in 1:nrow(Q7)) {
  Distance[i] <- dist(rbind(test, Q7[i, 1:3]))
}
Q7$Distance <- Distance  
Q7

# 8
library(ISLR)
data("College")
rm(college)
names(College)
college <- College
fix(college)
View(college)
head(college)
rownames(college)
summary(college)
pairs(college[1:10])
?par
par()
par(mfrow = c(2,2))
plot(college$Private, college$Outstate)
par(mfrow  = c(1,1))

Elite = rep("No", nrow(college))
Elite[college$Top10perc > 50] = "Yes"
Elite <- as.factor(Elite)
college <- data.frame(college, Elite)
summary(college)
plot(college$Elite, college$Outstate)
par(mfrow = c(2,2))
attach(college)
hist(Apps)
hist(Personal)
hist(Books)
hist(Accept)
par(mfrow = c(1,1))
detach(college)

# 9
table(is.na(Auto))
summary(Auto)
range(Auto$mpg)
mean(Auto$mpg)
AutoSub <- Auto[-(10:85),]
AutoSub[1:20,]
mean(AutoSub$mpg)
str(Auto)
Auto$cylinders <- as.factor(Auto$cylinders)
Auto$year <- as.factor(Auto$year)
Auto$origin <- as.factor(Auto$origin)
pairs(Auto)
plot(Auto$cylinders, Auto$mpg)
Auto$horsepower <- as.numeric(Auto$horsepower)
cor(Auto$weight, Auto$horsepower)
cor(Auto$weight, Auto$displacement)
cor(Auto$displacement, Auto$horsepower)

# 10
library(MASS)
Boston
?Boston
dim(Boston)
pairs(Boston)
hist(Boston$crim, breaks = 50)
hist(Boston$tax, breaks = 50)
hist(Boston$ptratio, breaks = 50)
table(Boston$chas)
median(Boston$ptratio)

row.names(Boston[min(Boston$medv), ])
Boston[5,]
nrow(Boston[Boston$rm > 7,])
nrow(Boston[Boston$rm > 8,])
