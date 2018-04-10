setwd("D:/ML/stock price")
train=read.csv("train.csv")
test=read.csv("C:\\Users\\lenovo\\Desktop\\winton stock\\test_2.csv")

#write csv
#write.csv(train.mean, file='train.mean.csv')
#write.csv(train.meanbc, file='train.meanbc.csv')
#write.csv(train.knn, file='train.knn.csv')
#write.csv(train0, file='train0.csv')

dim(train)  #40000   211
str(train)
summary(train)
dim(test)  #120000    147
library(mice)
md.pattern(train) #5,7;1,2,.....

train0=train   #replace with zero
train0[is.na(train0)]=0  
plot(density(train0$Feature_3), col='blue')
lines(density(train$Feature_3, na.rm = T),col='red')
legend("topright", c("before imputation", "after imputation"), lwd = 1,
       lty = 1, col = c("red", "blue"))

train.mean=train  #replace with mean
library(Hmisc)
set.seed(0)
for (i in 1:ncol(train)){
  train.mean[i] =Hmisc:: impute(train[i], what=c("mean"))
}
plot(density(train.mean$Feature_3), col='blue')
lines(density(train$Feature_3, na.rm = T),col='red')
legend("topright", c("before imputation", "after imputation"), lwd = 1,
       lty = 1, col = c("red", "blue"))

test.median = test
library(Hmisc)
set.seed(0)
for (i in 1:ncol(test)){
  test.median[i] =Hmisc:: impute(test[i], what=c("median"))
}
plot(density(test.median$Feature_3), col='blue')
lines(density(test$Feature_3, na.rm = T),col='red')
legend("topright", c("before imputation", "after imputation"), lwd = 1,
       lty = 1, col = c("red", "blue"))
write.csv(test.median,file = 'C:\\Users\\lenovo\\Desktop\\winton stock\\stestmed.csv')
help(write.csv)
#traink=train[,27:211]  #knn
traink=train
train.knn=train
library(DMwR)
trainka=knnImputation(traink,k=5)
train.knn=trainka
#train.knn[,27:211]=trainka

#correlation
library(corrplot)
feature=train.mean[,2:26]
M.fm <- cor(feature)   #the correlation matrix of features
corrplot(M.fm, method="circle")
feature.r=train.mean[,29:207]   #the correlation matrix of ret_2 to ret_180
M.rm <- cor(feature.r)
corrplot(M.rm, method="circle")
corrplot(cor(train.mean[,37:57]), method="circle")

# histogram
train.meanbc=train.mean
library(ggplot2)
#histogram plot of Feature_1
ggplot(train.mean, aes(x = Feature_1)) +
  geom_histogram(stat = "bin", binwidth=0.1)
qqnorm(train.mean$Feature_1)
qqline(train.mean$Feature_1)

#histogram plot of Feature_2
ggplot(train.mean, aes(x = Feature_2)) +
  geom_histogram(stat = "bin", binwidth=0.1)
qqnorm(train.mean$Feature_2)
qqline(train.mean$Feature_2)

#histogram plot of Feature_3
ggplot(train.mean, aes(x = Feature_3)) +
  geom_histogram(stat = "bin", binwidth=0.1)
qqnorm(train.mean$Feature_3)
qqline(train.mean$Feature_3)

#histogram plot of Feature_4
ggplot(train.mean, aes(x = Feature_4)) +
  geom_histogram(stat = "bin", binwidth=0.1)
qqnorm(train.mean$Feature_4)
qqline(train.mean$Feature_4)

#histogram plot of Feature_5
ggplot(train.mean, aes(x = Feature_5)) +
  geom_histogram(stat = "bin", binwidth=0.1)
qqnorm(train.mean$Feature_5)
qqline(train.mean$Feature_5)

qqnorm(train.meanbc$Feature_5)
qqline(train.meanbc$Feature_5)
#need box cox
library(forecast)
BoxCox.lambda(train.mean$Feature_5,method = "loglik") #lambda=0.9
f5.bc <- BoxCox(train.mean$Feature_5, lambda =0.9)
train.meanbc$Feature_5<-f5.bc

#histogram plot of Feature_6
ggplot(train.mean, aes(x = Feature_6)) +
  geom_histogram(stat = "bin", binwidth=0.1)
qqnorm(train.mean$Feature_6)
qqline(train.mean$Feature_6)
#need box cox transformation
library(forecast)
BoxCox.lambda(train.mean$Feature_6) #lambda=0.6333059
f6.bc <- BoxCox(train.mean$Feature_6, lambda =0.6333059)
train.meanbc$Feature_6<-f6.bc

qqnorm(train.meanbc$Feature_6)
qqline(train.meanbc$Feature_6)

#histogram plot of Feature_7
hist(train.mean$Feature_7)
qqnorm(train.mean$Feature_7)
qqline(train.mean$Feature_7)
#bc transformation
BoxCox.lambda(train.mean$Feature_7) #lambda=0.5186312
f7.bc <- BoxCox(train.mean$Feature_7, lambda =0.5186312)
train.meanbc$Feature_7<-f7.bc

qqnorm(train.meanbc$Feature_7)
qqline(train.meanbc$Feature_7)

#histogram plot of Feature_8
ggplot(train.mean, aes(x = Feature_8)) +
  geom_histogram(stat = "bin", binwidth=0.01)
qqnorm(train.mean$Feature_8)
qqline(train.mean$Feature_8)

#histogram plot of Feature_9
ggplot(train.mean, aes(x = Feature_9)) +
  geom_histogram(stat = "bin", binwidth=0.5)
qqnorm(train.mean$Feature_9)
qqline(train.mean$Feature_9)
#bc transformation
BoxCox.lambda(train.mean$Feature_9) #lambda=0.2218287
f9.bc <- BoxCox(train.mean$Feature_9, lambda =0.2218287)
train.meanbc$Feature_9<-f9.bc

qqnorm(train.meanbc$Feature_9)
qqline(train.meanbc$Feature_9)


#line plot from ret_2 t ret_120
time=c(seq(1,179))
r=train.mean[1:10,29:207]  #1-10 
tr=t(r)
treturn=data.frame(time,tr)
plot(x=time,y=treturn$X1,type="l",lty = 1)
library(reshape2)
mtreturn <- melt(treturn, 'time')
ggplot(mtreturn, aes(x = time, y = value))+
  geom_line(aes(color = variable))
  