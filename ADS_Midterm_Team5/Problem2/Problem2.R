# "sqldf" package for using sql functions in R
install.packages("sqldf")
library(sqldf)

# "data.table" for performing group functions, joins and list columns
install.packages("data.table")
library(data.table)

# "readxl" to read excel files
install.packages("readxl")
library(readxl)

# "caret" for confusion Matrix
install.packages("caret")
install.packages('e1071', dependencies=TRUE)
library(caret)

# gtools package
install.packages("gtools")
library(gtools)

# Read the ad.data file
ad1 <- read.table(file.choose(), header = F, fileEncoding = "UTF-8", sep = ",")
ad1$V1559<-NULL

# Read the ad.names file
ad2 <- read.csv("ad.csv", header = F)
# Removing 1st 2 rows



ad3 <- t(ad2)
ad4 <- ad3[,-c(1,0)]
ad5 <- as.data.frame(ad4)
View(ad5)

setDT(ad5, keep.rownames=TRUE)

# Removing the unwanted texts that starts with "|"
ad6 <- sqldf('select ad4 from ad5 where ad4 not like "%|%"')
ad7 <- t(ad6)
View(ad7)

# Binding the columns

l3 <- list()
ad7<-as.data.frame(ad7)
l3[[1]] <- ad7
l3[[2]] <- ad1

l4 <- rbindlist(l3)
l4 <- as.matrix(l4)

colnames(l4) <- l4[1,]
l4 <- l4[-c(1),]

l7<-as.data.frame(l4)

#replace ? with NA
l8<-as.data.frame(sapply(l7,sub,pattern='\\?',replacement=NA))

#separating heightcolumn and converting to numeric for mean
heightad<-as.numeric(as.character(l8$height))

#Mean
mean_height<-mean(heightad,na.rm = TRUE)

library(gtools)

#Replace NA with mean of column
heightad<-na.replace(heightad,mean_height)
l8$height<-heightad

#separating width and converting to numeric for mean
widthad<-as.numeric(as.character(l8$width))

#Mean
mean_width<-mean(widthad,na.rm = TRUE)

l9<-data.frame()
l9<-l8

#Replace NA with mean of column (width)
widthad<-na.replace(widthad,mean_width)
l8$width<-widthad

#ratio

aratioad<-as.numeric(as.character(l9$aratio))
l9$aratio<-aratioad
pos<-which(is.na(l9$aratio), arr.ind=TRUE)
l9$aratio[pos]<-l9$width[pos]/l9$height[pos]
View(l9)

write.csv(l9,"l9.csv")

#Remove local column NA's

l10<-l9[!is.na(l9$local),]

ad11 <- read.table("ad.data", header = F, fileEncoding = "UTF-8", sep = ",")

l12 <- cbind(l9, ad11$V1559)

setnames(l12, old=c("ad11$V1559"), new=c("status")) 

write.csv(l12, "adver.csv",row.names = FALSE)

# Logistic Regression

l12 <- read.csv("adver.csv", header=T)

l12$status_updated <- NA
l12$status_updated[l12$status == "ad."] <- 1
l12$status_updated[l12$status == "nonad."] <- 0

#Taking the sample data
smp_size <- floor(0.75 * nrow(l12))

#Set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(l12)), size = smp_size)

#Split the data into training and testing
train <- l12[train_ind, ]
test <- l12[-train_ind, ]

# Fitting data into model
fit2 <- glm(status~.,data=train, family=binomial(link="logit"))
summary(fit2)

#Predicting values
predict2 <- predict(fit2,test,type='response')
pred <- rep("nonad.",length(predict2))
pred[predict2<=0.5] <- "ad."

# Error and Confusion Matrix
error <- table(pred, test$status)
confusionMatrix(test$status, pred)

#ROC curve
install.packages("ROCR")
library(ROCR)
prediction <- prediction(predict2, test$status)
performance <- performance(prediction, measure = "tpr", x.measure = "fpr")
plot(performance, main="ROC curve", xlab="1-Specificity", ylab="Sensitivity")

#Lift curve
test$probs=predict2
prob=sort(test$probs,decreasing = T)
lift <- lift(status ~ prob, data = test)
lift
xyplot(lift,plot = "gain")
perf <- performance(prediction,"lift","rpp")
plot(perf, main="lift curve")


# Classification trees

library (ISLR)
library(tree)

adv = read.csv("adver.csv", header = T)
adverisement <- data.frame(adv)
View(adverisement)
tree <- tree(status~.,adverisement)
summary(tree)
plot(tree)
text(tree, pretty =0)

#Split the dataset into a training set and a test set

smp_size <- floor(0.75*nrow(adv))
set.seed (123)
train <- sample(seq_len(nrow(adv)), size = smp_size)
adv.test = adv [-train,]
status.test <- adv$status[-train]
#Build the tree based on the training set
tree.train = tree(status ~., adv, subset = train)
summary(tree.train)
#Evaluate its performance on the test data
tree.pred = predict(tree.train, adv.test, type = "class")

# Prediction and Confusion Matrix
class_tree <- table(tree.pred, status.test)
confusionMatrix(status.test, tree.pred)
View(class_tree)

#ROC curve
install.packages("ROCR")
library(ROCR)
prediction <- prediction(tree.pred, status.test)
performance <- performance(prediction, measure = "tpr", x.measure = "fpr")
plot(performance, main="ROC curve", xlab="1-Specificity", ylab="Sensitivity")

#Lift curve
perf <- performance(prediction,"lift","rpp")
plot(perf, main="lift curve", colorize=T)

# Neural Networks

install.packages("nnet")
library(nnet)

test_csv <- read.csv("adver.csv", head = TRUE)

#75% of the sample size
smp_size <- floor(0.50 * nrow(test_csv))

#Set the seed to make your partition reproductible
set.seed(123)
train <- sample(seq_len(nrow(test_csv)), size = smp_size)

#Split the data into training and testing
test <- test_csv[-train, ]

test_csv$status_updated <- NA
test_csv$status_updated[test_csv$status == "ad."] <- 0

test_csv$status_updated[test_csv$status == "nonad."] <- 1

seedsANN = nnet(status_updated~.,test_csv[train,], size=3,rang = 0.1, decay = 5e-4, maxit = 350,MaxNWts = 5000)

pr$pred <- predict(seedsANN, test_csv[-train,])

write.csv(pr$pred, "predpr.csv")

pred1 <- rep("1", length(pr$pred))
pred1[pr$pred<=0.5] <- "0"

# Prediction and Confusion Matrix
table(pred1, test$status_updated)

confusionMatrix(test$status_updated, pred1)

#ROC curve
install.packages("ROCR")
library(ROCR)
prediction <- prediction(pred1, test$status_updated)
performance <- performance(prediction, measure = "tpr", x.measure = "fpr")
plot(performance, main="ROC curve", xlab="1-Specificity", ylab="Sensitivity")

#Lift curve
perf <- performance(prediction,"lift","rpp")
plot(perf, main="lift curve", colorize=T)