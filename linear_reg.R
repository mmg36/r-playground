library(MASS)
library(ISLR)
names(Boston)
attach(Boston)
lm.linfit = lm(medv~lstat, data = Boston)

plot(lstat, medv, pch=1:20)
abline(lm.linfit, lwd=3)


plot(1:20, 1:20, pch=1:20)

par(mfrow=c(4,1))
plot(lm.linfit)

plot(predict(lm.linfit), residuals(lm.linfit))
plot(predict(lm.linfit), rstudent(lm.linfit))

plot(hatvalues(lm.linfit))
which.max(hatvalues(lm.linfit))

lm.mlinfit = lm (medv~lstat + age, data = Boston)
summary(lm.mlinfit)

lm.mlinfit = lm(medv~., data = Boston)
summary(lm.mlinfit)

library(car)
vif(lm.mlinfit)
