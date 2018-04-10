
train=read.csv("C:\\Users\\lenovo\\Desktop\\winton stock\\strainmed.csv")
result = read.csv("D:\\MIN\\kaggle output\\output1.csv")
dim(train)

View(result)

sum_in_day = train[151]
weight_in_day = train[213]
weight_day = train[214]

for(i in 152:210){
  
  sum_in_day = train[,i]+sum_in_day
  
}

day1 = train[211]
day2 = train[212]
daySum = day1+day2

sum_all = (sum_in_day*weight_in_day+daySum*weight_day)/62
sum_intraday = sum_in_day*weight_in_day/60
sum_daily = daySum*weight_day/2

sum_new = cbind(sum_all,sum_intraday,sum_daily)
names(sum_new) = c('sum_all','sum_inday','sum_daily')
train_new = cbind(train, sum_new)

write.csv(train_new, file='C:\\Users\\lenovo\\Desktop\\winton stock\\train_mean.csv')
write.csv(sum_new, file='C:\\Users\\lenovo\\Desktop\\winton stock\\sum.csv')
