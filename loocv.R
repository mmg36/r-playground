library(boot)
glm.fit=glm(mpg~horsepower, data = Auto)
cv.err = cv.glm(Auto, glm.fit)
cv.err$delta

cv.error = rep(0,5)
for (i in 1:5){
  glm.fit = glm(mpg~poly(horsepower, i), data = Auto)
  cv.error[i]=cv.glm(Auto, glm.fit)$delta[1]
}
cv.error


#K-fold cross validation 
set.seed(17)
cv.error.10=rep(0,10)
for (i in 1:10){
  glm.fit=glm(mpg~poly(horsepower, i), data=Auto)
  cv.error.10[i]=cv.glm(Auto, glm.fit, K=10)$delta[1]
}
cv.error.10

library(tidyverse)
m1 = matrix(cv.error, ncol=1, byrow=TRUE)
d1 = as.data.frame(m1, stringAsFactors=FALSE)
d1$xval = 1:5
colnames(d1) = c(xval, yval)
ggplot(data=d1) + geom_point(mapping = aes(x=xval,y=V1))


m1 = matrix(cv.error.10, ncol=1, byrow=TRUE)
m2 = matrix(cv.error, ncol=1, byrow=TRUE)
m3 = 1: 10
for (i in 1:10){
  if (i<6) {m3[i]=m2[i]}
  else {m3[i]=0}
}
  
d1 = as.data.frame(m1, stringAsFactors=FALSE)
d1$xval = 1:10
d1$cverror = as.data.frame(m3, stringAsFactors=FALSE)

ggplot(data=d1) +
  geom_point(mapping = aes(x=xval,y=V1)) +
  geom_point(mapping = aes(x=xvall,y=cverror.m3))
