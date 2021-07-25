library(ISLR)
set.seed(1)
train = sample(392, 196)
#first set of fitting
lm.fit = lm(mpg~horsepower, data=Auto, subset=train)
attach(Auto)
mean((mpg-predict(lm.fit, Auto))[-train]^2)
#second fitting 
lm.fit2=lm(mpg~poly(horsepower,2), data=Auto, subset=train)
mean((mpg-predict(lm.fit2, Auto))[-train]^2)
#third set of fitting
lm.fit3=lm(mpg~poly(horsepower,3), data=Auto, subset= train)
mean((mpg-predict(lm.fit3, Auto))[-train]^2)

#using a different training set 
set.seed(2)
train=sample(392, 196)
lm.fit=lm(mpg~horsepower, subset=train)
mean((mpg-predict(lm.fit, Auto))[-train]^2)
#second fitting 
lm.fit2=lm(mpg~poly(horsepower,2), data=Auto, subset=train)
mean((mpg-predict(lm.fit2, Auto))[-train]^2)
#third set of fitting
lm.fit3=lm(mpg~poly(horsepower,3), data=Auto, subset= train)
mean((mpg-predict(lm.fit3, Auto))[-train]^2)


glm.fit = glm(mpg~horsepower, data= Auto)
coef(glm.fit)

l,