############################################################################
#######################----- IE 500 PROJECT -----###########################
############################################################################
rm(list = ls())
install.packages("tidyverse")
library(tidyverse)
Impact_and_Communication_about_OFF_Periods <- read_csv('Desktop/Impact_and_Communication_about_OFF_Periods.csv')
############################################################################
###### DATA TRANSFORMATION #################################################
############################################################################
Data <- Impact_and_Communication_about_OFF_Periods
### Except fox_insight_id everything become integer ###
Data <- as.integer(Data)
str(Data)
data <- Data[,-c(1,3,4,9,11,12,13,14:37,62,78:127)]
d <- data[, -which(colMeans(is.na(data)) > 0.9)]
dd <- d[-which(rowMeans(is.na(d)) > 0.8),]
summary(dd)
### Changing NA values with 1(No Impact) for rating of Symptoms, since the first answers of Do you have this symptom? questions are 2 (No)###
dd$OFF1WeekDur[(dd$OFF1WeekDur == 6)] <- 0
dd$OFF1UnpredProp[(dd$OFF1UnpredProp == 6)] <- 0
dd$OFF1SympFatg1[is.na(dd$OFF1SympFatg1)] <- 1
dd$OFF1SympSleep1[is.na(dd$OFF1SympSleep1)] <- 1
dd$OFF1SympTrem1[is.na(dd$OFF1SympTrem1)] <- 1
dd$OFF1SympStiff1[is.na(dd$OFF1SympStiff1)] <- 1
dd$OFF1SympSlow1[is.na(dd$OFF1SympSlow1)] <- 1
dd$OFF1SympGait1[is.na(dd$OFF1SympGait1)] <- 1
dd$OFF1SympHand1[is.na(dd$OFF1SympHand1)] <- 1
dd$OFF1SympSpeak1[is.na(dd$OFF1SympSpeak1)] <- 1
dd$OFF1SympPain1[is.na(dd$OFF1SympPain1)] <- 1
dd$OFF1SympAnx1[is.na(dd$OFF1SympAnx1)] <- 1
dd$OFF1SympIrrit1[is.na(dd$OFF1SympIrrit1)] <- 1
dd$OFF1SympAgit1[is.na(dd$OFF1SympAgit1)] <- 1
dd$OFF1SympMotiv1[is.na(dd$OFF1SympMotiv1)] <- 1
dd$OFF1SympSad1[is.na(dd$OFF1SympSad1)] <- 1
dd$OFF1SympSocial1[is.na(dd$OFF1SympSocial1)] <- 1
dd$OFF1SympBladd1[is.na(dd$OFF1SympBladd1)] <- 1
dd$OFF1SympThink1[is.na(dd$OFF1SympThink1)] <- 1
dd$OFF1ImpactDay[is.na(dd$OFF1ImpactDay)] <- 1 ## unsure
dd$OFF1ImpactPhys[is.na(dd$OFF1ImpactPhys)] <- 1
dd$OFF1ImpactLeis[is.na(dd$OFF1ImpactLeis)] <- 1
dd$OFF1ImpactEmpl[is.na(dd$OFF1ImpactEmpl)] <- 1
dd$OFF1ImpactRelat[is.na(dd$OFF1ImpactRelat)] <- 1
dd$OFF1ImpactFriend[is.na(dd$OFF1ImpactFriend)] <- 1
dd$OFF1ImpactHouse[is.na(dd$OFF1ImpactHouse)] <- 1
dd$OFF1ImpactDrive[is.na(dd$OFF1ImpactDrive)] <- 1
dd$OFF1ImpactGroom[is.na(dd$OFF1ImpactGroom)] <- 1
dd$OFF1ImpactIndep[is.na(dd$OFF1ImpactIndep)] <- 1
dd$OFF1ImpactComm[is.na(dd$OFF1ImpactComm)] <- 1
dd$OFF1ImpactLeave[is.na(dd$OFF1ImpactLeave)] <- 1
dd$OFF1ImpactActiv[is.na(dd$OFF1ImpactActiv)] <- 1
dd$OFF1ImpactFrust[is.na(dd$OFF1ImpactFrust)] <- 1

View(dd)
colSums(is.na(dd))  
summary(dd)
############################################################################
###### DATA VISUALIZATION ##################################################
############################################################################
library(corrplot)
par(mfrow = c(1,1))
D <- cor(dd[, 2:ncol(dd)])
corrplot(D, method = "number") 

library(ggplot2)
ggplot(dd, aes(x =dd$OFF1SympFatg1, y = dd$age)) + geom_boxplot(fill = 'lightgoldenrod1') + coord_flip() +
  xlab("Symptom of Fatigue") +ylab("Age") + ggtitle("Box Plot of Fatigue and Age")

ggplot(dd, aes(dd$OFF1SympSleep1, dd$age))+ geom_boxplot(fill='darkgoldenrod')+
  xlab("Sleepiness") +ylab("Age") + ggtitle("Boxplot of Sleepiness and Age ")

mosaicplot(~ dd$OFF1SympPain1 + dd$OFF1SympTrem1, data = dd, color = TRUE, ggtitle("Pain & Tremor"), xlab("Pain"), ylab("Tremor")) 

ggplot(data = dd, mapping = aes(x = dd$age)) + 
  geom_histogram(binwidth = 5,
                 color = "red",
                 fill = "chocolate1") +
  ylab("Number of Patient") + ggtitle("Distribution of Number of Patients' Age") +
  scale_x_continuous(name = "Age Distribution",
                     breaks = seq(0, 90, 5),
                     limits=c(30, 90)) 

ggplot(dd, aes(x = dd$age, fill = dd$OFF1SympHand1)) + geom_bar(position = "dodge") +
  xlab("Age") +ylab("Symptom of Hand Coordination") + ggtitle("Hand Coordination of Ages ")

ggplot(dd, aes(x=dd$age, y=dd$OFF1WeekNum)) + 
  geom_bar(stat="identity", width=1, fill="tomato3")  + 
  theme(axis.text.x = element_text(angle=0, vjust=1)) +
  xlab("Age Distribution") +ylab("Off periods in a week") + ggtitle("CDF of Off Period in a Week - Age ")


############################################################################
###### LINEAR MODEL ########################################################
############################################################################
attach(dd)
set.seed(925)
F_Split <- sample(x = 1:nrow(dd), size = floor(0.80*nrow(dd)))
dd_train <- dd[F_Split,]
dd_test <-  dd[-F_Split,]

#install.packages("DAAG")#
library(DAAG)
library(caret)
fit=lm(OFF1WeekNum~., data = dd_train)
summary(fit)

fit1=lm(OFF1WeekNum~ age + OFF1ExpStart + OFF1UnpredProp +OFF1Track + OFF1SympGait1  
        + OFF1SympSocial1 + OFF1ImpactDay+ OFF1ImpactPhys 
        + OFF1ImpactDrive + OFF1ImpactLeave+ OFF1ImpactActiv, data = dd_train)
summary(fit1)

fit2=lm(OFF1WeekNum~ age + OFF1ExpStart + OFF1UnpredProp + OFF1SympGait1  
        + OFF1SympSocial1 + OFF1ImpactDay+ OFF1ImpactPhys 
        + OFF1ImpactDrive + OFF1ImpactLeave+ OFF1ImpactActiv, data = dd_train)
summary(fit2)

fit3=lm(OFF1WeekNum~ age + OFF1ExpStart + OFF1UnpredProp + OFF1SympGait1  
        + OFF1SympSocial1 + OFF1ImpactDay+ OFF1ImpactPhys 
        + OFF1ImpactDrive + OFF1ImpactLeave+ OFF1ImpactActiv, data = dd_test)
summary(fit3)

plot(fit2)
prediction1=predict(fit3, newdata = dd_test)
predict(fit3, newdata = dd_test, interval = "confidence", level=0.95)
# Compute confidence interval
confint(fit2,level=0.90)
# Calculate the training error and the prediction error
linear.train.mse = mean(fit2$residuals^2)
linear.mse = mean((prediction1-dd_test$OFF1WeekNum)^2)
linear.mse

model.cv <- train(OFF1WeekNum~ age + OFF1ExpStart + OFF1UnpredProp + OFF1SympGait1  
               + OFF1SympSocial1 + OFF1ImpactDay+ OFF1ImpactPhys 
               + OFF1ImpactDrive + OFF1ImpactLeave+ OFF1ImpactActiv, dd, method = "lm", trControl = trainControl(method = 'cv',number = 5,verboseIter = TRUE))
print(model.cv)

############################################################################
###### GAM MODEL ###########################################################
############################################################################
#look at the distribution of response #
hist(dd$OFF1WeekNum, prob=T, xlab='',
     main='Histogram of OFF Period in Week ',col='pink')  

lines(density(dd$OFF1WeekNum,na.rm=T)) 
rug(dd$OFF1WeekNum) 

boxplot(dd$OFF1WeekNum~I(dd$age), main = "Off Period in a Week vs Age",col="grey", xlab = "Age", 
        ylab = "OFF Period in Week", varwidth = TRUE) 
rug(jitter(dd$OFF1WeekNum),side=2)
abline(h=mean(dd$OFF1WeekNum,na.rm=T),lty=2) 
#install.packages("nlme")#
library(nlme)
library(mgcv)
attach(dd)


b1 <- mgcv::gam( OFF1WeekNum~ s(age, bs='ps', sp=0.6)
                 + s(OFF1ExpStart, bs='ps', sp=0.6)
                 + s(OFF1WeekDur, bs='ps', sp=0.6) 
                 + s(OFF1UnpredProp, bs='ps', sp=0.6) +s(OFF1SympFatg1, bs='ps', sp=0.6)
                 + s(OFF1SympSleep1, bs='ps', sp=0.6) + s(OFF1SympTrem1, bs='ps', sp=0.6)
                 + s(OFF1SympStiff1, bs='ps', sp=0.6) + s(OFF1SympSlow1, bs='ps', sp=0.6)
                 + s(OFF1SympGait1, bs='ps', sp=0.6) + s(OFF1SympHand1, bs='ps', sp=0.6) 
                 + s(OFF1SympSpeak1, bs='ps', sp=0.6)+  s(OFF1SympPain1, bs='ps', sp=0.6)
                 + s(OFF1SympAnx1, bs='ps', sp=0.6)+ s(OFF1SympIrrit1, bs='ps', sp=0.6)
                 + s(OFF1SympAgit1, bs='ps', sp=0.6) + s(OFF1SympMotiv1, bs='ps', sp=0.6) 
                 + s(OFF1SympSad1, bs='ps', sp=0.6) + s(OFF1SympSocial1, bs='ps', sp=0.6)
                 + s(OFF1SympBladd1, bs='ps', sp=0.6) +s(OFF1SympThink1, bs='ps', sp=0.6) 
                 + s(OFF1ImpactDay, bs='ps', sp=0.6) + s(OFF1ImpactPhys, bs='ps', sp=0.6)
                 + s(OFF1ImpactLeis, bs='ps', sp=0.6) + s(OFF1ImpactEmpl, bs='ps', sp=0.6)
                 + s(OFF1ImpactRelat, bs='ps', sp=0.6) + s(OFF1ImpactFriend, bs='ps', sp=0.6)
                 + s(OFF1ImpactHouse, bs='ps', sp=0.6) + s(OFF1ImpactDrive, bs='ps', sp=0.6) 
                 + s(OFF1ImpactGroom, bs='ps', sp=0.6) + s(OFF1ImpactIndep, bs='ps', sp=0.6)
                 + s(OFF1ImpactLeave, bs='ps', sp=0.6) + s(OFF1ImpactActiv, bs='ps', sp=0.6) 
                 + s(OFF1ImpactFrust, bs='ps', sp=0.6),  data = dd_train)
summary(b1)
plot(b1)

b2 <- update( b1, .~. - s(OFF1WeekDur, bs='ps', sp=0.6) 
              - s(OFF1UnpredProp, bs='ps', sp=0.6) - s(OFF1SympFatg1, bs='ps', sp=0.6)
              - s(OFF1SympSleep1, bs='ps', sp=0.6) - s(OFF1SympTrem1, bs='ps', sp=0.6)
              - s(OFF1SympStiff1, bs='ps', sp=0.6) - s(OFF1SympSlow1, bs='ps', sp=0.6)
              - s(OFF1SympHand1, bs='ps', sp=0.6) 
              - s(OFF1SympSpeak1, bs='ps', sp=0.6) -  s(OFF1SympPain1, bs='ps', sp=0.6)
              - s(OFF1SympAnx1, bs='ps', sp=0.6) - s(OFF1SympIrrit1, bs='ps', sp=0.6)
              - s(OFF1SympAgit1, bs='ps', sp=0.6) - s(OFF1SympMotiv1, bs='ps', sp=0.6) 
              - s(OFF1SympSad1, bs='ps', sp=0.6) - s(OFF1SympSocial1, bs='ps', sp=0.6)
              - s(OFF1SympBladd1, bs='ps', sp=0.6) - s(OFF1SympThink1, bs='ps', sp=0.6) 
              - s(OFF1ImpactDay, bs='ps', sp=0.6) - s(OFF1ImpactPhys, bs='ps', sp=0.6)
              - s(OFF1ImpactLeis, bs='ps', sp=0.6) 
              - s(OFF1ImpactRelat, bs='ps', sp=0.6) - s(OFF1ImpactFriend, bs='ps', sp=0.6)
              - s(OFF1ImpactHouse, bs='ps', sp=0.6) - s(OFF1ImpactDrive, bs='ps', sp=0.6) 
              - s(OFF1ImpactGroom, bs='ps', sp=0.6) - s(OFF1ImpactIndep, bs='ps', sp=0.6)
              - s(OFF1ImpactLeave, bs='ps', sp=0.6) - s(OFF1ImpactActiv, bs='ps', sp=0.6) 
              - s(OFF1ImpactFrust, bs='ps', sp=0.6) 
              + OFF1WeekDur + OFF1UnpredProp+ OFF1SympFatg1 + OFF1SympSlow1
              + OFF1SympSleep1 + OFF1SympTrem1 + OFF1SympStiff1
              + OFF1SympHand1 + OFF1SympSpeak1 + OFF1SympPain1
              + OFF1SympAnx1 + OFF1SympIrrit1 + OFF1SympAgit1
              + OFF1SympBladd1 + OFF1SympMotiv1
              + OFF1SympSad1 + OFF1SympSocial1 + OFF1SympThink1
              + OFF1ImpactDay + OFF1ImpactPhys + OFF1ImpactLeis  + OFF1ImpactRelat 
              + OFF1ImpactFriend + OFF1ImpactHouse + OFF1ImpactDrive + OFF1ImpactGroom 
              + OFF1ImpactIndep + OFF1ImpactLeave + OFF1ImpactActiv + OFF1ImpactFrust,
              data = dd_train)
summary(b2)
par(mfrow=c(1,1))
plot(b2)

layout(matrix(c(1:1),1,1,byrow=TRUE))

residuals.gam <- c()
gam.predict <- predict(b2, newdata = dd_test, type="response")

gam.predict
gam.predict.train <- predict(b2, newdata = dd_train, type="response")
gam.mse <- mean((gam.predict-dd_test$OFF1WeekNum)^2)
gam.train.mse <- mean((gam.predict.train-dd_train$OFF1WeekNum)^2)
print(gam.mse)
sqrt(gam.mse) 


library(car)

plot(dd_test$OFF1WeekNum, gam.predict, pch="o", col='black',lty=10,  main="GAM: Actual vs. Predicted",
     xlab = "Actual ", ylab="Predicted")
abline(0,1)

gam.insample.RMSE = sqrt(mean((dd_test$OFF1WeekNum-gam.predict)^2))
gam.insample.RMSE 
gam.insample.MAE = mean(abs(dd_test$OFF1WeekNum-gam.predict))
gam.insample.MAE 
############################################################################
###### MARS ################################################################
############################################################################
library(earth)

mars.2D <- earth(OFF1WeekNum ~ . , data=dd_train, degree=1, penalty=1, pmethod="forward", nfold=5, ncross=5)
summary(mars.2D)
MARS.varimp <- evimp(mars.2D, trim=FALSE)
MARS.pred <- predict(mars.2D, newdata = dd_test)
MARS.pred
MARS.mse <- mean((MARS.pred - dd_test$OFF1WeekNum)^2)
mars.model <- earth(OFF1WeekNum ~ age + OFF1ExpStart + OFF1WeekDur + OFF1UnpredProp + OFF1SympFatg1 + OFF1SympGait1 + OFF1SympMotiv1 
                    + OFF1ImpactDay + OFF1ImpactDrive, data=dd_train, degree=1, penalty=1, pmethod="backward", nfold=5, ncross=5)
summary(mars.model)
MARS.pred <- predict(mars.model, newdata = dd_test)
MARS.pred
MARS.mse <- mean((MARS.pred - dd_test$OFF1WeekNum)^2)
MARS.mse
MARS.pred.train <- predict(mars.model, newdata = dd_train)
MARS.train.mse <- mean((MARS.pred.train - dd_train$OFF1WeekNum)^2)

print(MARS.mse)
sqrt(MARS.mse)
print(MARS.varimp)
plot(MARS.varimp)
plot(mars.2D)
############################################################################
###### RIDGE ###############################################################
############################################################################
library(ISLR)
library(glmnet)
library(dplyr)
library(tidyr)


train.X <- as.matrix(dd_train[,-3])
train.Y <- as.matrix(dd_train[,3])
test.X <- as.matrix(dd_test[,-3])
test.Y <- as.matrix(dd_test[,3])
library(glmnet)
ridge.fit <- cv.glmnet(x=train.X, y=train.Y, family='gaussian', alpha = 0)
ridge.pred <- predict(ridge.fit, newx=test.X)
ridge.rmse <- sqrt(mean((ridge.pred - test.Y)^2))
ridge.mse <- mean((ridge.pred - test.Y)^2)
ridge.predt <- predict(ridge.fit, newx=train.X)
ridge.train.mse <- mean((ridge.predt - train.Y)^2)

plot(test.Y, ridge.pred, pch="o", col='black',lty=5,  main="LM: Ridge Actual vs Predicted",
     xlab = "Actual Sales",
     ylab="Predicted Sales")
coef(ridge.fit)


ridge.predin <- predict(ridge.fit, newx=train.X)
ridge.msein <- mean((ridge.predin - train.Y)^2)
ridge.rmsein <- sqrt(mean((ridge.predin - train.Y)^2))
plot(train.Y, ridge.predin, pch="o", col='black',lty=5,  main="LM: In sample Ridge Actual vs Predicted",
     xlab = "Actual Sales",
     ylab="Predicted Sales")
############################################################################
###### LASSO ###############################################################
############################################################################
lasso.fit <- cv.glmnet(x=train.X, y=train.Y, family='gaussian', alpha = 1)
lasso.pred <- predict(lasso.fit, newx=test.X)
lasso.mse <- mean((lasso.pred - test.Y)^2)
lasso.rmse <- sqrt(mean((lasso.pred - test.Y)^2))
lasso.predt <- predict(lasso.fit, newx=train.X)
lasso.train.mse <- mean((lasso.predt - train.Y)^2)

plot(test.Y, lasso.pred, pch="o", col='black',lty=5,  main="LM: Lasso Actual vs Predicted",
     xlab = "Actual Sales",
     ylab="Predicted Sales")
coef(lasso.fit)

lasso.predin <- predict(lasso.fit, newx=train.X)
lasso.msein <- mean((lasso.predin - train.Y)^2)
lasso.rmsein <- sqrt(mean((lasso.predin - train.Y)^2))
plot(train.Y, lasso.predin, pch="o", col='black',lty=5,  main="LM: Insample Lassso Actual vs Predicted",
     xlab = "Actual Sales",
     ylab="Predicted Sales")
############################################################################
###### BEST SUBSET, BACKWARD, FORWARD #############################################################
############################################################################
library(ISLR)
library(knitr)
library(printr)
attach(dd)
library(leaps)
regfit.full = regsubsets(OFF1WeekNum ~ ., data = dd_train)
summary(regfit.full)
plot(summary(regfit.full)$cp)
regfit.full = regsubsets(OFF1WeekNum ~ ., data = dd_train, nvmax = 37, method ="exhaustive" )
reg.summary = summary(regfit.full)
names(reg.summary)
reg.summary$rsq

library(ggvis)
rsq <- as.data.frame(reg.summary$rsq)
names(rsq) <- "R2"
rsq %>% 
  ggvis(x=~ c(1:nrow(rsq)), y=~R2 ) %>%
  layer_points(fill = ~ R2 ) %>%
  add_axis("y", title = "R2") %>% 
  add_axis("x", title = "Number of variables")

par(mfrow=c(2,2))
plot(reg.summary$rss ,xlab="Number of Variables ",ylab="RSS",type="l")
plot(reg.summary$adjr2 ,xlab="Number of Variables ", ylab="Adjusted RSq",type="l")
a=which.max(reg.summary$adjr2)
points(a,reg.summary$adjr2[a], col="red",cex=2,pch=20)
plot(reg.summary$cp ,xlab="Number of Variables ",ylab="Cp", type='l')
b=which.min(reg.summary$cp )
points(b,reg.summary$cp [b],col="red",cex=2,pch=20)
plot(reg.summary$bic ,xlab="Number of Variables ",ylab="BIC",type='l')
c=which.min(reg.summary$bic )
points(c,reg.summary$bic [c],col="red",cex=2,pch=20)
par(mfrow=c(1,1))
plot(regfit.full,scale="bic")
coef(regfit.full ,6)


num_var <- which.min(summary(regfit.full)$cp)
feat.cp <- which(summary(regfit.full)$which[num_var,] == TRUE)
subset.mod <- lm(OFF1WeekNum ~ ., data=dd_train[,feat.cp])
subset.pred <- predict(subset.mod, newdata=dd_test[,feat.cp])
subset.mse <- mean((subset.pred - dd_test[,3])^2)
subset.pred.train <- predict(subset.mod, newdata=dd_train[,feat.cp])
subset.train.mse <- mean((subset.pred.train - dd_train[,3])^2)
print(subset.mse)



regfit.fwd = regsubsets(OFF1WeekNum ~. , data=dd_train,nvmax=37, method ="forward")
regfit.bwd = regsubsets(OFF1WeekNum ~. , data=dd_train,nvmax=9, method ="backward")
summary(regfit.fwd)
coef(regfit.full ,9)
coef(regfit.fwd ,9)
coef(regfit.bwd ,9)

plot(regfit.fwd, scale = "Cp")


reg.summary = summary(regfit.fwd)
num_var <- which.min(summary(regfit.fwd)$cp)
feat.cp <- which(summary(regfit.fwd)$which[num_var,] == TRUE)
forward.mod <- lm(OFF1WeekNum ~ ., data=dd_train[,feat.cp])
forward.pred <- predict(forward.mod, newdata=dd_test[,feat.cp])
forward.mse <- mean((forward.pred - dd_test[,3])^2)
forward.predt <- predict(forward.mod, newdata=dd_train[,feat.cp])
forward.train.mse <- mean((forward.predt - dd_train[,3])^2)


print(forward.mse)


reg.summary = summary(regfit.bwd)
num_var <- which.min(summary(regfit.bwd)$cp)
feat.cp <- which(summary(regfit.bwd)$which[num_var,] == TRUE)
backward.mod <- lm(OFF1WeekNum ~ ., data=dd_train[,feat.cp])
backward.pred <- predict(backward.mod, newdata=dd_test[,feat.cp])
backward.mse <- mean((backward.pred - dd_test[,3])^2)
backward.predt <- predict(backward.mod, newdata=dd_train[,feat.cp])
backward.train.mse <- mean((backward.predt - dd_train[,3])^2)
print(backward.mse)

############################################################################
###### DECISION TREE-CLASSIFICATION ########################################
############################################################################

library(rpart)
dtm <- rpart(OFF1WeekNum~., dd_train, method = 'class')
dtm
plot(dtm)
text(dtm)

library(rpart.plot)
rpart.plot(dtm, type = 4, extra = 101)
tpredict <- predict(dtm, dd_test,type = 'class')
table(dd_test[,3],tpredict)

############################################################################
###### DECISION TREE - REGRESSION ##########################################
############################################################################
library(MASS)
library(tree)
set.seed(62)
training = sample(1:nrow(dd), nrow(dd)*0.8)
traint = tree(OFF1WeekNum~. ,dd, subset = training)
yhat = predict(traint, newdata = dd[-training,])
yhat
dd.test = dd[-training,'OFF1WeekNum']
mean((yhat-dd.test)^2)
cv.dd = cv.tree(traint)
plot(cv.dd$size, cv.dd$dev, type = 'b')
prune.dd = prune.tree(traint,best = 9)
yhat= predict(prune.dd, newdata = dd[-training,])
tree.MSE = mean((yhat-dd.test)^2)
yhat.train= predict(prune.dd, newdata = dd[training,])
tree.train.mse = mean((yhat.train-dd[training,'OFF1WeekNum'])^2)
############################################################################
###### BAGGING #############################################################
############################################################################
tree.random <- tree(OFF1WeekNum~.,dd_train)
rmse_reg <- function(model_obj, testing = NULL, target = NULL) {
  #Calculates rmse for a regression decision tree
  yhat <- predict(model_obj, newdata = testing)
  actual <- testing[[target]]
  sqrt(mean((yhat-actual)^2))
}
rmse_reg(tree.random, dd_test, "OFF1WeekNum")
library(randomForest)
bag.dd = randomForest(OFF1WeekNum~., data = dd, subset = training, mtry = 37, importance = TRUE, ntree = 500)
plot(bag.dd, type= 'l', main="MSE by ntree for Bagging")
bag.dd = randomForest(OFF1WeekNum~., data = dd, subset = training, mtry = 37, importance = TRUE, ntree = 170)
summary(bag.dd)
plot(bag.dd, type= 'l', main="MSE by ntree for Bagging")
rmse_reg(bag.dd, dd_test, 'OFF1WeekNum')
varImpPlot(bag.dd)
bag.mse = mean((yhat-dd.test)^2)

bag.predict.train <- predict(bag.dd,dd_train,type="response")
plot(dd_test$OFF1WeekNum, yhat, pch="o", col='red',lty=5,  main="Bagging: Actual vs Fitted(Test Data)",
     xlab = "Actual", 
     ylab="Predicted")
bag.train.mse <-mean((bag.predict.train - dd_train$OFF1WeekNum)^2)
bag.train.mse <- sqrt(bag.train.mse)
residuals.bag.train <- (dd_train$OFF1WeekNum-bag.predict.train)
residuals.bag.test <- (dd_test$OFF1WeekNum-yhat)
par(mfrow=c(1,1))
qqPlot(residuals.bag.train, main = "Bagging: Residual Plot(Train Data)") 
qqPlot(residuals.bag.test, main = "Bagging: Residual Plot(Test Data)") 
############################################################################
###### RANDOM FOREST #######################################################
############################################################################
rf.mse <- c()
for(i in 1:(ncol(dd_train)-1)){
  rf <- randomForest(OFF1WeekNum~., data=dd_train, mtry=i, importance=TRUE, ntree=150)
  rf.mse[i] <- rf$mse[150]
}
plot(rf.mse, main='Training Error by m', xlab='Number of Predictors', ylab='MSE')
#Select final model, 9 predictors per tree.
rf.OFF1WeekNum <- randomForest(OFF1WeekNum~., data=dd_train, mtry=9, importance=TRUE, ntree=100)
#Out of bag error
yhat.rf = predict(rf, newdata = dd_test, n.trees = 150)
rmse_reg(rf.OFF1WeekNum, dd_test, "OFF1WeekNum")
rf.mse = mean((yhat.rf - dd.test[])^2)

rf.predict.train <- predict(rf.OFF1WeekNum,dd_train,type="response")
rf.train.mse <-mean((rf.predict.train - dd_train$OFF1WeekNum)^2)
rf.train.rmse <- sqrt(rf.train.mse)
varImpPlot(rf.OFF1WeekNum)
############################################################################
###### BOOSTING ################################################################
############################################################################
library(gbm)
boost.dd = gbm(OFF1WeekNum~., data = dd[training,], distribution = 'gaussian',
               n.trees = 5000, interaction.depth = 4)
yhat.boost = predict(boost.dd, newdata = dd[-training,], n.trees = 5000)
mean((yhat.boost-dd.test)^2)

boost.dd = gbm(OFF1WeekNum~., data = dd[training,], distribution = 'gaussian',
               n.trees = 5000, interaction.depth = 4, shrinkage = 0.2)
yhat.boost = predict(boost.dd, newdata = dd[-training,], n.trees = 5000)
boost.mse= mean((yhat.boost-dd.test)^2)

yhat.boost.train = predict(boost.dd, newdata = dd_train[], n.trees = 5000)
boost.train.mse= mean((yhat.boost.train-dd_train[,3])^2)


##### NULL MODEL MSE ######
null.mse = mean((mean(dd_test[,3])-dd_test[,3])^2)
null.train.mse = mean((mean(dd_train[,3])-dd_train[,3])^2)


# Construct partial dependence plots
#install.packages("pdp")##
library(pdp)
p1 <- partial(bag.dd, pred.var = "OFF1ExpStart", grid.resolution = 10) %>%
  autoplot()
p2 <- partial(bag.dd, pred.var = "OFF1UnpredProp", grid.resolution = 10) %>%
  autoplot()
p3 <- partial(bag.dd, pred.var = "OFF1ImpactPhys",grid.resolution = 10) %>%
  autoplot()
p4 <- partial(bag.dd, pred.var ="OFF1WeekDur",grid.resolution = 10) %>%
  autoplot()
p5 <- partial(bag.dd, pred.var = "OFF1ImpactDay", grid.resolution = 10) %>%
  autoplot()
p6 <- partial(bag.dd, pred.var = "OFF1ImpactComm", grid.resolution = 10) %>%
  autoplot()
p7 <- partial(bag.dd, pred.var = "OFF1SympHand1",grid.resolution = 10) %>%
  autoplot()
p8 <- partial(bag.dd, pred.var ="OFF1ImpactIndep",grid.resolution = 10) %>%
  autoplot()
p9 <- partial(bag.dd, pred.var = "OFF1ImpactGroom", grid.resolution = 10) %>%
  autoplot()
p10 <- partial(bag.dd, pred.var = "OFF1SympBladd1", grid.resolution = 10) %>%
  autoplot()
p11 <- partial(bag.dd, pred.var = "age",grid.resolution = 10) %>%
  autoplot()
p12 <- partial(bag.dd, pred.var ="OFF1ImpactActiv",grid.resolution = 10) %>%
  autoplot()
p13 <- partial(bag.dd, pred.var = "OFF1ImpactRelat",grid.resolution = 10) %>%
  autoplot()
p14 <- partial(bag.dd, pred.var ="OFF1ImpactDrive",grid.resolution = 10) %>%
  autoplot()
p15 <- partial(bag.dd, pred.var = "OFF1SympSad1", grid.resolution = 10) %>%
  autoplot()
p16 <- partial(bag.dd, pred.var = "OFF1SympThink1", grid.resolution = 10) %>%
  autoplot()
p17 <- partial(bag.dd, pred.var = "OFF1ImpactFriend",grid.resolution = 10) %>%
  autoplot()
p18 <- partial(bag.dd, pred.var ="OFF1SympGait1",grid.resolution = 10) %>%
  autoplot()
p19 <- partial(bag.dd, pred.var ="OFF1SympThink1",grid.resolution = 10) %>%
  autoplot()





# Display plots side by side
gridExtra::grid.arrange(p1, p2, ncol = 2)
gridExtra::grid.arrange(p3, p4, ncol = 2)
gridExtra::grid.arrange(p5, p6, ncol = 2)
gridExtra::grid.arrange(p7, p8, ncol = 2)
gridExtra::grid.arrange(p9, p10, ncol = 2)
gridExtra::grid.arrange(p11, p12, ncol = 2)
gridExtra::grid.arrange(p13, p14, ncol = 2)
gridExtra::grid.arrange(p15, p16, ncol = 2)
gridExtra::grid.arrange(p17, p18, ncol = 2)
gridExtra::grid.arrange(p19, ncol = 2)

par.all <- partial(bag.dd, pred.var = c("OFF1ExpStart", "OFF1UnpredProp"), chull = TRUE)
plot.all <- autoplot(par.all, contour = TRUE, 
                     legend.title = "Partial\ndependence")

grid.arrange(p1, p2,plot.all)

##saveRDS(dd, file = "Group#13.ParkinsonData.RData")##
save(fit2,b2,mars.model,ridge.fit,lasso.fit,regfit.full,regfit.bwd,regfit.fwd,bag.dd,prune.dd,rf.OFF1WeekNum,boost.dd, file = "Group#13_RData_Models.RData")

save(list = ls(all=T),file = "./Group#13_RData_FinalData.RData")

