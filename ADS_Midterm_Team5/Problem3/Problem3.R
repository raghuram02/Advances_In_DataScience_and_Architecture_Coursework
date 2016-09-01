wind1 <- read.csv("train.csv", header = T)
library(sqldf)

wf1 <- read.csv("windforecasts_wf1.csv", header = T)

#Date separation and appending "-"
wf1<-sqldf("select 
           date,hors,u,v,ws,wd,
           substr(date,1,4)||'-'||substr(date,5,2)||'-'||substr(date,7,2) as dt,
           substr(date,9,2) as hr
           from wf1")
wf1$hr<-as.numeric(wf1$hr)
wf1$dt<-as.Date(wf1$dt)


# Date Manipulation
wf1<-sqldf("select date,hors,u,v,ws,wd,dt,hr,
           case when (hors+hr)==24 then '1.00' 
           when (hors+hr)==0 then '0.00'
           when (hors+hr)==23 then '0.23'
           when (hors+hr)<23 and (hors+hr)%23!=0 then (case when substr(substr('0.'||(hors+hr),1,4),4,1)=='.' then substr('0.'||(hors+hr),1,3)
           else substr('0.'||(hors+hr),1,4) end)
           when (hors+hr)==48 then '2.00' 
           when (hors+hr)==47 then '1.23' 
           when ((hors+hr)>=25 and (hors+hr)<=46) then (case when substr(substr('1.'||((hors+hr)%24),1,4),4,1)=='.' then substr('1.'||((hors+hr)%24),1,3)
           else substr('1.'||((hors+hr)%24),1,4) end)
           when (hors+hr)==71 then '2.23'
           when (hors+hr)==72 then '3.00' 
           when ((hors+hr)>=49 and (hors+hr)<=70) then (case when substr(substr('2.'||((hors+hr)%24),1,4),4,1)=='.' then substr('2.'||((hors+hr)%24),1,3)
           else substr('2.'||((hors+hr)%24),1,4) end) end as tot_hr from wf1")

wf2<-wf1
View(wf2)

wf2$tot_hr<-as.character(wf2$tot_hr)
wf2<-sqldf("select date,hors,u,v,ws,wd,dt,hr,tot_hr,
           substr(tot_hr,1,1) as tt,substr(tot_hr,3,2) as hh,hors+hr as net_hr from wf2")

wf2$tt<-as.numeric(wf2$tt)

wf2<-sqldf("select date,hors,u,v,ws,wd,dt+tt as dt,hr,tot_hr,hh from wf2")
wf2$dt1<-as.character(wf2$dt)
wf2
sqldf("select substr(dt1,1,4) from wf2")
wf2<-sqldf("select date,hors,u,v,ws,wd,tot_hr,
           case when length(hh)==1 then
           substr(dt1,1,4)||substr(dt1,6,2)||substr(dt1,9,2)||'0'||hh else 
           substr(dt1,1,4)||substr(dt1,6,2)||substr(dt1,9,2)||hh end as upd_dt from wf2")

View(wf2)
wf3<-wf2

library(lubridate)

#Prediction

#Merging
merging<-merge(x = wf3[,c("upd_dt","hors","u","v","ws","wd","tot_hr")], y = wind1[ , c("date", "wp1")], by.x ='upd_dt',by.y = 'date', all.x=TRUE)
write.csv(merging,"mergingnew.csv")

View(merging)

for(i in 1:nrow(merging))
{
  if(!(is.na(merging$wp1[i])))
    if(merging$wp1[i]==0)
    {
      merging$wp1[i]<-NA
    }
}

library(zoo)
library(data.table)
merged2<-merging

unareplaced<-na.locf(merged2$u,na.rm = FALSE)
vnareplaced<-na.locf(merged2$v,na.rm = FALSE)
wsreplaced<-na.locf(merged2$ws,na.rm = FALSE)
wdrepalced<-na.locf(merged2$wd,na.rm = FALSE)
nareplacedwp1 <- na.locf(merged2$wp1,na.rm = FALSE)

write.csv(merged2,"merged2.csv")
final<-cbind(merged2$upd_dt,merged2$hors,unareplaced,vnareplaced,wsreplaced,wdrepalced,merged2$tot_hr,nareplacedwp1)
final2<-as.data.frame(final)
setnames(final2,old="V1",new="Date")
setnames(final2,old="V2",new="hors")
setnames(final2,old="unareplaced",new="u")
setnames(final2,old="vnareplaced",new="v")
setnames(final2,old="wsreplaced",new="ws")
setnames(final2,old="wdrepalced",new="wd")
setnames(final2,old="V7",new="tot_hr")
setnames(final2,old="nareplacedwp1",new="wp1")

final_data <- final2

smp_size <- floor(0.75 * nrow(final3))

#Set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(final3)), size = smp_size)

#Split the data into training and testing
train3 <- final3[train_ind, ]
test3 <- final3[-train_ind, ]

#Logistic regression

write.csv(train3,"train3.csv")
View(train3)
class(train3$u)


final3$newwp1 <- NA

fit3 <- lm(newwp1~ u + v + ws + wd + hors + wp1, data=train3)
summary(fit3)

final3$u<-as.numeric(final3$u)
final3$v<-as.numeric(final3$v)
final3$ws<-as.numeric(final3$ws)
final3$wd<-as.numeric(final3$wd)
final3$hors<-as.numeric(final3$hors)
final3$wp1<-as.numeric(final3$wp1)



class(final2$u)
pred = predict(fit3, final3)
pred1 <- as.data.frame(pred)

final4 <- cbind(final3,pred1)
write.csv(final4,"finalwf7.csv")
accuracy(pred, final4$wp1)


weather1 <- read.csv("finalwf1.csv", header = T)
weather2 <- read.csv("finalwf2.csv", header = T)
weather3 <- read.csv("finalwf3.csv", header = T)
weather4 <- read.csv("finalwf4.csv", header = T)
weather5 <- read.csv("finalwf5.csv", header = T)
weather6 <- read.csv("finalwf6.csv", header = T)
weather7 <- read.csv("finalwf7.csv", header = T)


weather8 <- cbind(merging$upd_dt, weather1$pred, weather2$pred, weather3$pred, weather4$pred, weather5$pred, weather6$pred, weather7$pred)
weather8 <- as.data.frame(weather8)
setnames(weather8, old=c("V1"), new=c("Date"))
setnames(weather8, old=c("V2"), new=c("wp1"))
setnames(weather8, old=c("V3"), new=c("wp2"))
setnames(weather8, old=c("V4"), new=c("wp3"))
setnames(weather8, old=c("V5"), new=c("wp4"))
setnames(weather8, old=c("V6"), new=c("wp5"))
setnames(weather8, old=c("V7"), new=c("wp6"))
setnames(weather8, old=c("V8"), new=c("wp7"))

write.csv(weather8, "Benchmark.csv")
View(weather8)

# Regression Trees
tree_data = read.csv("hh.csv", header = T)

#Set the seed to make your partition reproductible
set.seed (1)

#Split the data into training and testing
train = sample (1:nrow(tree_data), nrow(tree_data)/2)

#Fit in a regression tree
tree.energy = tree(wp1 ~ u + v + ws + wd + hors,tree_data,subset=train)

#Summary of the tree
summary (tree.energy)

#Plotting the tree
plot (tree.energy)
text (tree.energy, pretty = 0)
cv.wind = cv.tree (tree.energy)
plot (cv.wind$size, cv.wind$dev, type='b')

# Pruning a tree
prune.energy =prune.tree(tree.energy, best = 5)
plot(prune.energy)
text(prune.energy, pretty = 0)

#Predicting the wp1 value
yhat=predict (tree.energy, newdata = tree_data[-train,])

pred2 <- as.data.frame(yhat)
View(pred2)

tree_data1 <- cbind(tree_data,pred2)

View(tree_data1)

for(i in 1:nrow(tree_data1)){
  if(is.na(tree_data1$wp1[i])){
    tree_data1$wp1[i]<-round(tree_data1$yhat[i],digits=4)
  }
}

write.csv(tree_data1,"tree_data1.csv")


energy.test=tree_data1 [-train, "wp1"]
plot(yhat,energy.test)
abline (0,1)
mean((yhat -energy.test)^2)
pred_tree <- accuracy(yhat, energy.test)


# Neural Networks
library(neuralnet)
library(MASS)
library(nnet)

# Read csv data
neural1 = read.csv("hh1.csv", header = T)
temp1 = read.csv("hh.csv", header = T)
View(neural1)

#Set the seed to make your partition reproductible
set.seed(500)

# Neglecting the unwanted columns
neural2 <- neural1[,-c(1,7)]
View(neural2)

# To check that there are no columns with Nas
apply(neural2,2,function(x) sum(is.na(x)))

# Setting the max and min values
maxs <- apply(neural2, 2, max) 
mins <- apply(neural2, 2, min)

# Converting the columns to numeric
neural2$hors <- as.numeric(neural2$hors)
neural2$u <- as.numeric(neural2$u)
neural2$v <- as.numeric(neural2$v)
neural2$ws <- as.numeric(neural2$ws)
neural2$wd <- as.numeric(neural2$wd)
neural2$wp1 <- as.numeric(neural2$wp1)

# Scale the data
scaled <- as.data.frame(scale(neural2, center = mins, scale = maxs - mins))

#Split the data into training and testing
index <- sample(1:nrow(neural2),round(0.75*nrow(neural2)))
train5 <- scaled[index,]
test5 <- scaled[-index,]

#Fit in neural networks model
n <- names(train5)
f <- as.formula(paste("wp1 ~", paste(n[!n %in% "wp1"], collapse = " + ")))
# nn <- nnet(f, train5, size=3,rang = 0.1, decay = 5e-4, maxit = 350,MaxNWts = 5000)

nn <- neuralnet(f,data=neural2,hidden=3,linear.output=F, threshold = 0.1)
plot(nn)

# Compute the values
temp2 <- temp1[,-c(1,7)]
pr.nn <- compute(nn,temp2[,1:5])

write.csv(pr.nn, "neu1.csv")

final5 <- cbind(temp2,pr.nn$net.result)
write.csv(final5,"final5.csv")

View(final5)
final5$wp1<-as.numeric(final5$wp1)

setnames(final5, old=c("pr.nn$net.result"), new=c("pred"))

for(i in 1:nrow(final5)){
  if(is.na(final5$wp1[i])){
    final5$wp1[i]<-round(final5$pred[i],digits=4)
  }
}
View(final5)

pr.nn_ <- pr.nn$net.result*(max(final5$wp1)-min(final5$wp1))+min(final5$wp1)
test.r <- (final5$wp1)*(max(final5$wp1)-min(final5$wp1))+min(final5$wp1)
MSE.nn <- sqrt(sum((test.r - pr.nn_)^2)/nrow(final5))

MAE.nn <- mean(abs(test.r - pr.nn_))
MAPE <- mean(abs((test.r - pr.nn_)/test.r))
RMSE.nn <- sqrt(sum((test.r - pr.nn_)^2)/nrow(final5))