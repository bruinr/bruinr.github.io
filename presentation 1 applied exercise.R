rm(list=ls())

#load libraries 
library(ISLR) #data
library(e1071)

#load and visualize data
data(Auto)
str(Auto)

#part a
medianMPG <- median(Auto$mpg)
Auto$binaryMPG <- as.factor(Auto$mpg > medianMPG)
Auto <- subset(Auto, select=-c(mpg, name))

#part b
#use the tune() function to perform cross validation with various costs
set.seed(12)
tune.out <- tune(svm, binaryMPG~., data=Auto, kernel='linear', 
                 ranges=list(cost=c(0.001, 0.01, 0.1, 1, 5, 10, 100)), scale=TRUE)
summary(tune.out)

bestmod <- tune.out$best.model
summary(bestmod)
coef(bestmod)

plot(bestmod, Auto, weight~acceleration, slice=list(displacement=400, 
                                               horsepower=100, 
                                               year=76,
                                               cylinders=6,
                                               origin=1))

yhat <- predict(bestmod, newdata = subset(Auto, select=-c(binaryMPG)))
table(yhat, Auto$binaryMPG)

#part c
#polynomial kernel

##tune the model
polysvm <- tune(svm, binaryMPG~.-mpg, data=Auto, kernel='polynomial', 
                ranges=list(cost=c(0.001, 0.01, 0.1, 1, 5, 10, 100), 
                            degree=c(2,3,4,5)))
summary(polysvm) 

## one standard deviation rule 
err <- polysvm$performances$error
degrees <- polysvm$performances$degree
polysd <- sd(err)

plot(degrees, err)
abline(h=min(err) +polysd, col = "red", lty="dashed")

bestpolyd <- 2
bestpolyerr <- 0.2531166

#radial kernel 
radsvm <- tune(svm, binaryMPG~., data=Auto, kernel='polynomial', 
                ranges=list(cost=c(0.001, 0.01, 0.1, 1, 5, 10, 100), 
                            gamma=c(0.5,1,2,3,4)))
summary(radsvm) 


## one standard deviation rule 
err <- radsvm$performances$error
gammas <- radsvm$performances$gamma
radcosts <- radsvm$performances$cost

plot(gammas, err)

bestrad <- radsvm$best.model
summary(bestrad)

#crosses: support vectors
#colors represent the class
plot(bestrad, Auto, weight~year)



