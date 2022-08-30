install.packages("dplyr")	
library(dplyr)
install.packages("knitr")
library(knitr)
install.packages("tidyverse")
library(tidyverse)
install.packages("ggplot2")
library(ggplot2)
install.packages("mice")
library(mice)
install.packages("lattice")
library(lattice)
install.packages("reshape2")
library(reshape2)
install.packages("DataExplorer") # if the following package is not available
library(DataExplorer)
install.packages("ggplot2")
library(ggplot2)
install.packages("ggpubr")
library(ggpubr)
library(car)
library(MASS)
library(corrplot) #for visualisation of correlation
library(lattice) #for visualisation
library(ggplot2) #for visualisation
library(caTools) #for splittind data into testing and training data
library(dplyr) #manipulating dataframe


setwd("C:\\NC-Personal\\IPBA Jigsaw\\Training Materials for Class\\R Basics\\Delhi House Price Prediction")
train <- read.csv("IndoreHP_Train.csv")
test <- read.csv("IndoreHP_Test.csv")
names(test)[6] <- 'nox' 
names(test)[7] <- 'xrd'
head(test)
str(test)
head(test)
summary(test)
names(train)[6] <- 'nox' 
names(train)[7] <- 'xrd'
head(train)
sum(is.na(train))

#Data Visualization
par(mfrow = c(2, 3))
plot1=plot(train$INDUS)
plot2=plot(train$CRIM)
plot3=plot(train$ZN)
plot4=plot(train$RIVER_FLG)
plot5=plot(train$nox)
plot6=plot(train$xrd)
plot7=plot(train$AGE)
plot8=plot(train$DIS)
plot9=plot(train$RAD)
plot10=plot(train$TAX)
plot11=plot(train$PTRATIO)
plot12=plot(train$B)
plot13=plot(train$LSTAT)
plot14=plot(train$MEDV)

# checking plots for log
par(mfrow = c(2, 3))
plot(log(train$INDUS))
plot(log(train$CRIM))
plot(log(train$ZN))
plot(log(train$RIVER_FLG))
plot(log(train$nox))
plot(log(train$xrd))
plot(log(train$AGE))
plot(log(train$DIS))
plot(log(train$RAD))
plot(log(train$TAX))
plot(log(train$PTRATIO))
plot(log(train$B))
plot(log(train$LSTAT))
plot(log(train$MEDV))

# finding correlation
corrplot(cor(train))

#Create Dummy Variable
train$RIVER_FLG0<- ifelse(train$RIVER_FLG == 0,1,0)
train$RAD2<- ifelse(train$RAD == 2,1,0)
train$RAD3<- ifelse(train$RAD == 3,1,0)
train$RAD4<- ifelse(train$RAD == 4,1,0)
train$RAD5<- ifelse(train$RAD == 5,1,0)
train$RAD6<- ifelse(train$RAD == 6,1,0)
train$RAD7<- ifelse(train$RAD == 7,1,0)
train$RAD8<- ifelse(train$RAD == 8,1,0)

str(train)


#Create Dummy Variable
test$RIVER_FLG0<- ifelse(test$RIVER_FLG == 0,1,0)
test$RAD2<- ifelse(test$RAD == 2,1,0)
test$RAD3<- ifelse(test$RAD == 3,1,0)
test$RAD4<- ifelse(test$RAD == 4,1,0)
test$RAD5<- ifelse(test$RAD == 5,1,0)
test$RAD6<- ifelse(test$RAD == 6,1,0)
test$RAD7<- ifelse(test$RAD == 7,1,0)
test$RAD8<- ifelse(test$RAD == 8,1,0)

str(test)


#applying Multiple Linear Regression Modelling

model1<-lm(MEDV~.,data=train)
model1
summary(model1)

# applying Multiple Linear Regression Modelling using LOG()

model2<-lm(log(MEDV)~. ,data=train)
model2
summary(model2)

# applying Multiple Linear Regression Modelling

model3<-lm(log(MEDV)~.- RIVER_FLG-ID-DIS-INDUS-CRIM-ZN-TAX-LSTAT-PTRATIO-(nox*DIS)-xrd
           +I(log(LSTAT))+I(log(DIS))+I(log(TAX))+I(log(PTRATIO)),data=train)
model3
summary(model3)

#Step Wise Regression Computation
library(leaps) #leaps, for computing stepwise regression
library(MASS)  #stepAIC() [MASS package], which choose the best model by AIC

step.model <- stepAIC(model3, direction = "both", 
                      trace = FALSE)
summary(step.model)


#Corelation between Independent Variables
car::vif(step.model)

#Residuals Plots
residuals <- data.frame('Residuals' = step.model$residuals)
residuals_histogram <- ggplot(residuals, aes(x=Residuals)) + geom_histogram(color='black', fill='orange') + ggtitle('Histogram of Residuals')
residuals_histogram

#Outlier Identification
car::outlierTest(step.model)  #The function outlierTest from car package gives the most extreme observation based on the given model
influenceIndexPlot(step.model)


#Plotting Model
par(mfrow = c(2, 2))
plot(step.model,col='skyblue')

#Predicting data for Test Dataset
predicted_data <- predict(step.model,test)
test$MEDV <- exp(predicted_data)
test$MEDV
head(test)


#Plotting the Model with alpha=0.05

plot1 <-test %>% 
  ggplot(aes((xrd),MEDV)) +
  geom_point(alpha=0.5) + 
  stat_smooth(aes(colour='black')) +
  xlab('XRD') +
  ylab('Predicted value of medv') +
  theme_bw()

ggplotly(plot1)

 

colnames(test)
colnames(train)
head(test)
test[,c(1,15)]
write.csv(test[c(1,15)], "saibal_submission_file.csv") 










