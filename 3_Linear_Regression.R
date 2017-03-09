library(ISLR)
advertising <- read.csv("Advertising.csv", header = T, row.names = 1)
head(advertising)
fit_tv <- lm(Sales ~ TV, data = advertising)
summary(fit_tv)
fit_radio <- lm(Sales ~ Radio, data = advertising)
fit_newpaper <- lm(Sales ~ Newspaper, data = advertising)
summary(fit_radio)
summary(fit_newpaper)
fit <- lm(Sales ~ TV + Radio + Newspaper, data = advertising)
summary(fit)
cor(advertising)

fit_tv_radio <- lm(Sales ~ TV + Radio, data = advertising)
summary(fit_tv_radio)

credit <- read.csv("credit.csv", header = T, row.names = 1)
head(credit)
pairs(credit)
gender <- lm(Balance ~ Gender, data = credit)
summary(gender)
summary(credit$Ethnicity)

ethnicity <- lm(Balance ~ Ethnicity, data = credit)
summary(ethnicity)

advert_inter <- lm(Sales ~ TV + Radio + TV*Radio, 
                   data = advertising)
summary(advert_inter)

credit_income_student <- lm(Balance ~ Income + Student,
                            data = credit)
summary(credit_income_student)
credit_income_student_inter <- lm(Balance ~ Income +
                                    Student + 
                                    Income * Student, 
                                  data = credit)
summary(credit_income_student_inter)

data("Auto")
plot(Auto$horsepower, Auto$mpg)
abline(lm(Auto$mpg ~ Auto$horsepower), col = 'red')
horsepower <- lm(mpg ~ horsepower, data = Auto)
horsepower_quad <- lm(mpg ~ horsepower + I(horsepower ^ 2),
                      data = Auto)
summary(horsepower)
summary(horsepower_quad)

auto_horsepower <- lm(mpg ~ horsepower, data = Auto)
plot(auto_horsepower)
plot(horsepower_quad)
plot(horsepower_quad, which = 1)

credit_age_limit <- lm(Balance ~ Age + Limit, data = credit)
credit_rating_limit <- lm(
  Balance ~ Rating + Limit, 
  data = credit
)
summary(credit_age_limit)
summary(credit_rating_limit)
library(car) # http://www.statmethods.net/stats/rdiagnostics.html
vif(credit_rating_limit)
vif(credit_age_limit)
outlierTest(credit_rating_limit)
qqplot(credit_rating_limit)
leveragePlots(credit_rating_limit)
avPlots(credit_rating_limit)

# 3.4 The marketing plan
head(advertising)
summary(fit)
mean(advertising$Sales)

# 3.6 Lab: Linear Regression
ls()
rm(list = ls())
library(MASS)
library(ISLR)

# 3.6.2 Simple linear regression
fix(Boston)
names(Boston)
lm.fit <- lm(medv ~ lstat, data = Boston)
attach(Boston)
lm.fit <- lm(medv ~ lstat)
lm.fit
summary(lm.fit)
names(lm.fit)
coef(lm.fit)
# to obtain a confidence interval for the coefficient estimates:
confint(lm.fit)
# The predict() function can be used to produce confidence intervals and
# prediction intervals for the prediction of medv for a given value of lstat:
predict(lm.fit, data.frame(lstat = c(5, 10, 15)),
        interval = "confidence")
predict(lm.fit, data.frame(lstat = c(5, 10, 15)),
        interval = "prediction")
# We will now plot medv and lstat along with the least squares regression
# line using the plot() and abline() functions:
plot(lstat, medv)
abline(lm.fit)
abline(lm.fit, lwd = 3)
abline(lm.fit, lwd = 3, col = 'red')
plot(lstat, medv, col = 'red')
plot(lstat, medv, pch = 20)
plot(lstat, medv, pch = "+")
plot(1:20, 1:20, pch = 1:20)

par(mfrow = c(2,2))
plot(lm.fit)
# Alternatively, we can compute the residuals from a linear regression fit
# using the residuals() function. The function rstudent() will return the
# studentized residuals, and we can use this function to plot the residuals
# against the fitted values.
plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))
# On the basis of the residual plots, there is some evidence of non-linearity.
# Leverage statistics can be computed for any number of predictors using the
# hatvalues() function.
plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit))

# 3.6.3 Multiple linear regression
lm.fit <- lm(medv ~ lstat + age)
summary(lm.fit)
lm.fit <- lm(medv ~ ., data = Boston)
summary(lm.fit)
summary(lm.fit)$sigma
vif(lm.fit)
# What if we would like to perform a regression using all of the variables but
# one? For example, in the above regression output, age has a high p-value.
# So we may wish to run a regression excluding this predictor. The following
# syntax results in a regression using all predictors except age:
lm.fit1 <- lm(medv ~ . -age, data = Boston)
summary(lm.fit1)
# Alternatively, the update() function can be used.
lm.fit1 <- update(lm.fit, ~ . -age)

# 3.6.4 Interaction terms
# It is easy to include interaction terms in a linear model using the lm() function.
# The syntax lstat:black tells R to include an interaction term between
# lstat and black. The syntax lstat*age simultaneously includes lstat, age,
# and the interaction term lstat×age as predictors; it is a shorthand for
# lstat+age+lstat:age.
summary(lm(medv ~ lstat * age))

# 3.6.5 Non-linear tranformations of the predictors
# The lm() function can also accommodate non-linear transformations of the
# predictors. For instance, given a predictor X, we can create a predictor X2
# using I(X^2). The function I() is needed since the ^ has a special meaning
# I()
# in a formula; wrapping as we do allows the standard usage in R, which is
# to raise X to the power 2. We now perform a regression of medv onto lstat
# and lstat2
lm.fit2 <- lm(medv ~ lstat + I(lstat ^ 2))
summary(lm.fit2)
lm.fit <- lm(medv ~ lstat)
anova(lm.fit, lm.fit2)
plot()
plot(1:20, 1:20, pch = 1:20)
plot(lm.fit2)

# The lm() function can also accommodate non-linear transformations of the
# predictors. For instance, given a predictor X, we can create a predictor X2
# using I(X^2). The function I() is needed since the ^ has a special meaning
# I()
# in a formula; wrapping as we do allows the standard usage in R, which is
# to raise X to the power 2. We now perform a regression of medv onto lstat
# and lstat2
lm.fit5 <- lm(medv ~ poly(lstat, 5))
summary(lm.fit5)
summary(lm(medv ~ log(rm)))
detach(Boston)

# 3.6.6 Qualitative predictors
fix(Carseats)
names(Carseats)
attach(Carseats)
lm.fit <- lm(Sales ~ . +Income:Advertising + Price:Age,
             data = Carseats)
summary(lm.fit)
# The contrasts() function returns the coding that R uses for the dummy
# variables.
contrasts(ShelveLoc)
?contrasts
detach(Carseats)

# 3.7 Exercises
GPA <- 4
IQ <- 110
85 + 10 * GPA + 0.07 * IQ + 0.01 * (GPA * IQ)

# 8
fit_8 <- lm(mpg ~ horsepower, data = Auto)
summary(fit_8)
mean(Auto$mpg)
names(fit_8)
summary(fit_8)$sigma
summary(fit_8)$sigma/mean(Auto$mpg)
predict(fit_8, data.frame(horsepower = 98))
predict(fit_8, data.frame(horsepower = 98),
        interval = "confidence")
predict(fit_8, data.frame(horsepower = 98),
        interval = "prediction")
plot(Auto$horsepower, Auto$mpg, 
     xlab = "horsepower",
     ylab = "mpg",
     main = "Auto data") 
abline(fit_8, col = 'red')
plot(fit_8)
par(mfrow = c(1,1))

# 9
pairs(Auto)
names(Auto)
cor(Auto[1:8])
lm_9 <- lm(mpg ~ . -name, data = Auto)
summary(lm_9)
# p-value on F-statistic indicates relationship
# all but cylinders, horsepower and acceleration are sig.
# coef of year indicates positive relationship...increasse in 1 year
# on averague has the effect of an increase of 0.75 mpg
par(mfrow = c(2,2))
plot(lm_9)
# non-linearity, some outlier, some high-leverarge points
lm_9.2 <- lm(mpg ~ cylinders * displacement +
               displacement * weight, data = Auto)
summary(lm_9.2)
# Interaction between displacement and weight is sig., between cyl and
# displacement is not
plot(Auto$displacement, Auto$mpg)
plot(log(Auto$displacement), Auto$mpg)

# 10
# a
lm_10 <- lm(Sales ~ Price + Urban + US,
            data = Carseats)
summary(lm_10)
# e
lm_10a <- lm(Sales ~ Price + US,
             data = Carseats)
summary(lm_10a)
# g
confint(lm_10a)
par()
plot(lm_10a)
plot()
plot(predict(lm_10a), rstudent(lm_10a))
plot(hatvalues(lm_10a))
plot(rstudent(lm_10a), hatvalues(lm_10a))
plot(hatvalues(lm_10a), rstudent(lm_10a))

# 11
set.seed(1)
x <- rnorm(100)
y <- 2 * x + rnorm(100)
# a
lm_11 <- lm(y ~ x + 0)
summary(lm_11)
# coef = 1.99, se = 0.1065, t statistic = 18.73, p-value = 0
# b
lm_11b <- lm(x ~ y + 0)
summary(lm_11b)
# coef B = 0.3911, se = .0209, t statistic = 18.73, p=value = 0 
# c 
# We obtain the
# same value for the t-statistic and consequently the same value for the
# corresponding p-value. Both results in (a) and (b) reflect the same line
# created in (a). In other words, y=2x+εy=2x+ε could also be written
# x=0.5(y−ε)x=0.5(y−ε).

# d
# e
# f
lm_11f1 <- lm(y ~ x)
lm_11f2 <- lm(x ~ y)
summary(lm_11f1)
summary(lm_11f2)

# 12
# 13
set.seed(1)
x <- rnorm(100)
eps <- rnorm(100, 0, sd = sqrt(0.25))
y <- -1 + 0.5 * x + eps
length(y)
# B0 = -1, B1 = 0.5
plot(x, y)
lm_13 <- lm(y ~ x)
summary(lm_13)
plot(x, y)
abline(lm_13, col = 'red')
abline(-1, 0.5, col = 'blue')
legend("topleft", c("Least square", "Regression"), col = c("red", "blue"), lty = c(1,1))
??legend.lty
?plot
?legend
lm_13q <- lm(y ~ x + I(x^2))
summary(lm_13q)
# h
set.seed(1)
eps <- rnorm(100, sd = 0.125)
x <- rnorm(100)
y <- -1 + 0.5 * x + eps
plot(x, y)
lm_13a <- lm(y ~ x)
summary(lm_13a)
# i
set.seed(1)
eps <- rnorm(100, sd = 1)
x <- rnorm(100)
y <-  -1 + 0.5 * x + eps
lm_13more <- lm(y ~ x)
summary(lm_13more)
plot(x, y)
abline(lm_13more, col = 'red')
abline(-1, 0.5, col = "blue")

# j
confint(lm_13)
confint(lm_13a)
confint(lm_13more)

# 14
set.seed(1)
x1 = runif(100)
x2 = 0.5 * x1 + rnorm(100)/10
y = 2 + 2 * x1 + 0.3 * x2 + rnorm(100)
# a B0 = 2, B1 = 2, B3 = 0.3
# b
cor(x1, x2)
plot(x1, x2)
fit_14 <- lm(y ~ x1 + x2)
summary(fit_14)
# d
fit_14d <- lm(y ~ x1)
summary(fit_14d)
# e
fit_14e <- lm(y ~ x2)
summary(fit_14e)
# g
x1 <- c(x1, 0.1)
x2 <- c(x2, 0.8)
y <- c(y, 6)
fit_14g1 <- lm(y ~ x1 + x2)
fit_14g2 <- lm(y ~ x1)
fit_14g3 <- lm(y ~ x2)
summary(fit_14g1)
summary(fit_14g2)
summary(fit_14g3)
plot(1:100, 100:1)
plot(fit_14g1)
plot(fit_14g2)
plot(fit_14g3)

# 15
# a
names(Boston)
?Boston
fit_zn <- lm(crim ~ zn, data = Boston)
summary(fit_zn)
fit_indus <- lm(crim ~ indus, data = Boston)
summary(fit_indus)
fit_chas <- lm(crim ~ chas, data = Boston)
summary(fit_chas)
fit_nox <- lm(crim ~ nox, data = Boston)
summary(fit_nox)
fit_rm <- lm(crim ~ rm, data = Boston)
summary(fit_rm)
fit_age <- lm(crim ~ age, data = Boston)
summary(fit_age)
fit_dis <- lm(crim ~ age, data = Boston)
summary(fit_dis)
fit_rad <- lm(crim ~ rad, data = Boston)
summary(fit_rad)
fit_tax <- lm(crim ~ tax, data = Boston)
summary(fit_tax)
fit_ptratio <- lm(crim ~ ptratio, data = Boston)
summary(fit_ptratio)
fit_black <- lm(crim ~ black, data = Boston)
summary(fit_black)
fit_lstat <- lm(crim ~ lstat, data = Boston)
summary(fit_lstat)
fit_medv <- lm(crim ~ medv, data = Boston)
summary(fit_medv)
# To find which predictors are significant, we have to test H0:β1=0H0:β1=0. All
# predictors have a p-value less than 0.05 except “chas”, so we may conclude
# that there is a statistically significant association between each predictor
# and the response except for the “chas” predictor.

# b
fit_all <- lm(crim ~ ., data = Boston)
summary(fit_all)
# reject null for zn, dis, rad, black, medv

# c
mult_reg <- numeric()
fit_all$coefficients
mult_reg <- c(mult_reg, fit_all$coefficients[-1])
mult_reg
attributes(mult_reg)

simple_reg <- numeric()
simple_reg <- c(simple_reg, fit_zn$coefficients[2])
simple_reg <- c(simple_reg, fit_indus$coefficients[2])
simple_reg <- c(simple_reg, fit_chas$coefficients[2])
simple_reg <- c(simple_reg, fit_nox$coefficients[2])
simple_reg <- c(simple_reg, fit_rm$coefficients[2])
simple_reg <- c(simple_reg, fit_age$coefficients[2])
simple_reg <- c(simple_reg, fit_dis$coefficients[2])
simple_reg <- c(simple_reg, fit_rad$coefficients[2])
simple_reg <- c(simple_reg, fit_tax$coefficients[2])
simple_reg <- c(simple_reg, fit_ptratio$coefficients[2])
simple_reg <- c(simple_reg, fit_black$coefficients[2])
simple_reg <- c(simple_reg, fit_lstat$coefficients[2])
simple_reg <- c(simple_reg, fit_medv$coefficients[2])
length(simple_reg)
length(mult_reg)
par(mfrow = c(1, 1))
plot(simple_reg, mult_reg)
plot(simple_reg[-4], mult_reg[-4])
simple_reg
mult_reg

# There is a difference between the simple and multiple regression coefficients.
# This difference is due to the fact that in the simple regression case, the
# slope term represents the average effect of an increase in the predictor,
# ignoring other predictors. In contrast, in the multiple regression case, the
# slope term represents the average effect of an increase in the predictor,
# while holding other predictors fixed. It does make sense for the multiple
# regression to suggest no relationship between the response and some of the
# predictors while the simple linear regression implies the opposite because the
# correlation between the predictors show some strong relationships between some
# of the predictors.
cor(Boston[-c(1, 4)])
# So for example, when “age” is high there is a tendency in “dis” to be low,
# hence in simple linear regression which only examines “crim” versus “age”, we
# observe that higher values of “age” are associated with higher values of
# “crim”, even though “age” does not actually affect “crim”. So “age” is a
# surrogate for “dis”; “age” gets credit for the effect of “dis” on “crim”.

# d
fit_zn2 <- lm(crim ~ poly(zn, 3), data = Boston)
summary(fit_zn2)
summary(fit_zn)
fit_indus2 <- lm(crim ~ poly(indus, 3), data = Boston)
summary(fit_indus2)
fit_nox2 <- lm(crim ~ poly(nox, 3), data = Boston)
summary(fit_nox2)
fit_rm2 <- lm(crim ~ poly(rm, 3), data = Boston)
summary(fit_rm2)
fit_age2 <- lm(crim ~ poly(age, 3), data = Boston)
summary(fit_age2)
fit_dis2 <- lm(crim ~ poly(dis, 3), data = Boston)
summary(fit_dis2)
fit_rad2 <- lm(crim ~ poly(rad, 3), data = Boston)
summary(fit_rad2)
fit_tax2 <- lm(crim ~ poly(tax, 3), data = Boston)
summary(fit_tax2)
fit_ptratio2 <- lm(crim ~ poly(ptratio, 3), data = Boston)
summary(fit_ptratio2)
fit_black2 <- lm(crim ~ poly(black, 3), data = Boston)
summary(fit_black2)
fit_lstat2 <- lm(crim ~ poly(lstat, 3), data = Boston)
summary(fit_lstat2)
fit_medv2 <- lm(crim ~ poly(medv, 3), data = Boston)
summary(fit_medv2)

# For “zn”, “rm”, “rad”, “tax” and “lstat” as predictor, the p-values suggest
# that the cubic coefficient is not statistically significant; for “indus”,
# “nox”, “age”, “dis”, “ptratio” and “medv” as predictor, the p-values suggest
# the adequacy of the cubic fit; for “black” as predictor, the p-values suggest
# that the quandratic and cubic coefficients are not statistically significant,
# so in this latter case no non-linear effect is visible.

medv2_line <- predict(fit_medv2, data.frame(medv = 5:50))
plot(Boston$medv, Boston$crim)
abline(fit_medv, col = 'blue')
lines(medv2_line, col = 'red')
plot(Boston$indus, Boston$crim)

nox2_line <- predict(fit_nox2, data.frame(nox = 0.4:0.9))
plot(Boston$nox, Boston$crim)
lines(nox2_line, col = 'red')
