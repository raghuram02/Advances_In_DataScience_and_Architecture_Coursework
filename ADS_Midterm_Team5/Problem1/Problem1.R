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

prob1 <- function(){
  
  #Reading the Excel file
  data_Creditcard <- read_excel(file.choose(), skip = 1)
  
  View(data_Creditcard)
  
  #Renaming an existing values
  setnames(data_Creditcard,old=c("default payment next month"), new=c("default_payment"))
  
  # Performing sanitization(checking 0) using sqldf
  sqldf('SELECT count(AGE) FROM data_Creditcard WHERE AGE =0')
  sqldf('SELECT count(PAY_0) FROM data_Creditcard WHERE PAY_0 =0')
  sqldf('SELECT count(PAY_2) FROM data_Creditcard WHERE PAY_2 =0')
  sqldf('SELECT count(PAY_3) FROM data_Creditcard WHERE PAY_3 =0')
  sqldf('SELECT count(PAY_4) FROM data_Creditcard WHERE PAY_4 =0')
  sqldf('SELECT count(PAY_5) FROM data_Creditcard WHERE PAY_5 =0')
  sqldf('SELECT count(PAY_6) FROM data_Creditcard WHERE PAY_6 =0')
  sqldf('SELECT count(BILL_AMT1) FROM data_Creditcard WHERE BILL_AMT1 =0')
  sqldf('SELECT count(BILL_AMT2) FROM data_Creditcard WHERE BILL_AMT2 =0')
  sqldf('SELECT count(BILL_AMT3) FROM data_Creditcard WHERE BILL_AMT3 =0')
  sqldf('SELECT count(BILL_AMT4) FROM data_Creditcard WHERE BILL_AMT4 =0')
  sqldf('SELECT count(BILL_AMT5) FROM data_Creditcard WHERE BILL_AMT5 =0')
  sqldf('SELECT count(BILL_AMT6) FROM data_Creditcard WHERE BILL_AMT6 =0')
  sqldf('SELECT count(PAY_AMT1) FROM data_Creditcard WHERE PAY_AMT1 =0')
  sqldf('SELECT count(PAY_AMT2) FROM data_Creditcard WHERE PAY_AMT2 =0')
  sqldf('SELECT count(PAY_AMT3) FROM data_Creditcard WHERE PAY_AMT3 =0')
  sqldf('SELECT count(PAY_AMT4) FROM data_Creditcard WHERE PAY_AMT4 =0')
  sqldf('SELECT count(PAY_AMT5) FROM data_Creditcard WHERE PAY_AMT5 =0')
  sqldf('SELECT count(PAY_AMT6) FROM data_Creditcard WHERE PAY_AMT6 =0')
  sqldf('SELECT count(default_payment) FROM data_Creditcard WHERE default_payment =0')
  
  
  # Performing sanitization(checking NULL) using sqldf
  sqldf('SELECT count(AGE) FROM data_Creditcard WHERE AGE IS NULL')
  sqldf('SELECT count(PAY_0) FROM data_Creditcard WHERE PAY_0 IS NULL')
  sqldf('SELECT count(PAY_2) FROM data_Creditcard WHERE PAY_2 IS NULL')
  sqldf('SELECT count(PAY_3) FROM data_Creditcard WHERE PAY_3 IS NULL')
  sqldf('SELECT count(PAY_4) FROM data_Creditcard WHERE PAY_4 IS NULL')
  sqldf('SELECT count(PAY_5) FROM data_Creditcard WHERE PAY_5 IS NULL')
  sqldf('SELECT count(PAY_6) FROM data_Creditcard WHERE PAY_6 IS NULL')
  sqldf('SELECT count(BILL_AMT1) FROM data_Creditcard WHERE BILL_AMT1 IS NULL')
  sqldf('SELECT count(BILL_AMT2) FROM data_Creditcard WHERE BILL_AMT2 IS NULL')
  sqldf('SELECT count(BILL_AMT3) FROM data_Creditcard WHERE BILL_AMT3 IS NULL')
  sqldf('SELECT count(BILL_AMT4) FROM data_Creditcard WHERE BILL_AMT4 IS NULL')
  sqldf('SELECT count(BILL_AMT5) FROM data_Creditcard WHERE BILL_AMT5 IS NULL')
  sqldf('SELECT count(BILL_AMT6) FROM data_Creditcard WHERE BILL_AMT6 IS NULL')
  sqldf('SELECT count(PAY_AMT1) FROM data_Creditcard WHERE PAY_AMT1 IS NULL')
  sqldf('SELECT count(PAY_AMT2) FROM data_Creditcard WHERE PAY_AMT2 IS NULL')
  sqldf('SELECT count(PAY_AMT3) FROM data_Creditcard WHERE PAY_AMT3 IS NULL')
  sqldf('SELECT count(PAY_AMT4) FROM data_Creditcard WHERE PAY_AMT4 IS NULL')
  sqldf('SELECT count(PAY_AMT5) FROM data_Creditcard WHERE PAY_AMT5 IS NULL')
  sqldf('SELECT count(PAY_AMT6) FROM data_Creditcard WHERE PAY_AMT6 IS NULL')
  sqldf('SELECT count(default_payment) FROM data_Creditcard WHERE default_payment IS NULL')
  
  # Changing the column values accordingly
  
  data_Creditcard$default_payment_str[data_Creditcard$default_payment == 1] <- "Yes"
  
  data_Creditcard$default_payment_str[data_Creditcard$default_payment == 0] <- "No"
  
  
  data_Creditcard$count_2 <- apply(data_Creditcard[7:12],1,function(x) length(which(x==-2)))
  data_Creditcard$count_1 <- apply(data_Creditcard[7:12],1,function(x) length(which(x==-1)))
  data_Creditcard$count0 <- apply(data_Creditcard[7:12],1,function(x) length(which(x==0)))
  data_Creditcard$count1 <- apply(data_Creditcard[7:12],1,function(x) length(which(x==1)))
  data_Creditcard$count2 <- apply(data_Creditcard[7:12],1,function(x) length(which(x==2)))
  data_Creditcard$count3 <- apply(data_Creditcard[7:12],1,function(x) length(which(x==3)))
  data_Creditcard$count4 <- apply(data_Creditcard[7:12],1,function(x) length(which(x==4)))
  data_Creditcard$count5 <- apply(data_Creditcard[7:12],1,function(x) length(which(x==5)))
  data_Creditcard$count6 <- apply(data_Creditcard[7:12],1,function(x) length(which(x==6)))
  data_Creditcard$count7 <- apply(data_Creditcard[7:12],1,function(x) length(which(x==7)))
  data_Creditcard$count8 <- apply(data_Creditcard[7:12],1,function(x) length(which(x==8)))
  
  
  
  # Logistic Regression Model
  
  # Taking the sample
  smp_size1 <- floor(0.60 * nrow(data_Creditcard))
  
  #Set the seed to make your partition reproductible
  set.seed(123)
  
  #Split the data into training and testing
  train_ind1 <- sample(seq_len(nrow(data_Creditcard)), size = smp_size1)
  train1 <- data_Creditcard[train_ind1, ]
  test1 <- data_Creditcard[-train_ind1, ]
  
  #Construct a logistic regression model using the variables 
  fit1 <- glm(default_payment~LIMIT_BAL+
                SEX+
                EDUCATION+
                MARRIAGE+
                AGE+
                count_2+
                count_1+
                count0+
                count1+
                count2+
                count3+
                count4+
                count5+
                count6+
                count7+
                count8+
                BILL_AMT1+
                BILL_AMT2+
                BILL_AMT3+
                BILL_AMT4+
                BILL_AMT5+
                BILL_AMT6+
                PAY_AMT1+
                PAY_AMT2+
                PAY_AMT3+
                PAY_AMT4+
                PAY_AMT5+
                PAY_AMT6, data=train1, family=binomial(link="logit"))
  
  #Summary of the fit
  summary(fit1)
  
  #Measures of prediction
  predict1 <- predict(fit1,test1,type='response')
  write.csv(pred1, "pred.csv")
  
  pred1 <- rep("No",length(predict1))
  pred1[predict1>=0.5] <- "Yes"
  
  library(caret)
  
  error <- table(pred1, test1$default_payment_str)
  
  
  confusionMatrix(test1$default_payment_str, pred1)
  
  #ROC curve
  install.packages("ROCR")
  library(ROCR)
  prediction <- prediction(predict1, test1$default_payment_str)
  performance <- performance(prediction, measure = "tpr", x.measure = "fpr")
  plot(performance, main="ROC curve", xlab="1-Specificity", ylab="Sensitivity")
  
  #Lift curve
  perf <- performance(prediction,"lift","rpp")
  plot(perf, main="lift curve", colorize=T)
  
  # Neural Networks
  
  install.packages("nnet")
  library(nnet)
  
  seedsANN1 = nnet(default_payment~LIMIT_BAL+
                     SEX+
                     EDUCATION+
                     MARRIAGE+
                     AGE+
                     count_2+
                     count_1+
                     count0+
                     count1+
                     count2+
                     count3+
                     count4+
                     count5+
  1                     count6+
                     count7+
                     count8+
                     BILL_AMT1+
                     BILL_AMT2+
                     BILL_AMT3+
                     BILL_AMT4+
                     BILL_AMT5+
                     BILL_AMT6+
                     PAY_AMT1+
                     PAY_AMT2+
                     PAY_AMT3+
                     PAY_AMT4+
                     PAY_AMT5+
                     PAY_AMT6, data=train1, size=20, hidden =5,rang = 0.1, maxit = 350,MaxNWts = 5000, threshold = 0.01)
  
  sqldf('select "0",count(status_updated) from test_csv where status_updated = 0')
  sqldf('select "0",count(pred) from pr where pred = 0')
  
  predict2 <- predict(seedsANN1, test1)
  pred2 <- rep("No", length(predict2))
  pred2[predict2>=0.5] <- "Yes"
  
  
  table(pred2, test1$default_payment_str)
  
  confusionMatrix(test1$default_payment_str, pred2)
  
  #ROC curve
  install.packages("ROCR")
  library(ROCR)
  prediction <- prediction(predict2, test1$default_payment_str)
  performance <- performance(prediction, measure = "tpr", x.measure = "fpr")
  plot(performance, main="ROC curve", xlab="1-Specificity", ylab="Sensitivity")
  
  #Lift curve
  perf <- performance(prediction,"lift","rpp")
  plot(perf, main="lift curve", colorize=T)
  
  # Classification Tree
  
  library (ISLR)
  library(tree)
  
  View(data_Creditcard)
  
  tree2 <- tree(default_payment~ LIMIT_BAL+
                  SEX+
                  EDUCATION+
                  MARRIAGE+
                  AGE+
                  PAY_0+
                  PAY_2+
                  PAY_3+
                  PAY_4+
                  PAY_5+
                  PAY_6+
                  BILL_AMT1+
                  BILL_AMT2+
                  BILL_AMT3+
                  BILL_AMT4+
                  BILL_AMT5+
                  BILL_AMT6+
                  PAY_AMT1+
                  PAY_AMT2+
                  PAY_AMT3+
                  PAY_AMT4+
                  PAY_AMT5+
                  PAY_AMT6,data_Creditcard)
  summary(tree2)
  plot(tree2)
  text(tree2, pretty =0)
  
  #Split the dataset into a training set and a test set
  
  smp_size <- floor(0.50*nrow(data_Creditcard))
  set.seed (123)
  train <- sample(seq_len(nrow(data_Creditcard)), size = smp_size)
  credit2.test = data_Creditcard [-train,]
  payment.test <- data_Creditcard$default_payment[-train]
  #Build the tree based on the training set
  tree.train = tree(as.factor(default_payment)~ LIMIT_BAL+
                      SEX+
                      EDUCATION+
                      MARRIAGE+
                      AGE+
                      PAY_0+
                      PAY_2+
                      PAY_3+
                      PAY_4+
                      PAY_5+
                      PAY_6+
                      BILL_AMT1+
                      BILL_AMT2+
                      BILL_AMT3+
                      BILL_AMT4+
                      BILL_AMT5+
                      BILL_AMT6+
                      PAY_AMT1+
                      PAY_AMT2+
                      PAY_AMT3+
                      PAY_AMT4+
                      PAY_AMT5+
                      PAY_AMT6, train1)
  summary(tree.train)
  #Evaluate its performance on the test data
  tree.pred = predict(tree.train, credit2.test, type = "class")
  class
  confusionMatrix(payment.test, tree.pred)
  View(class_tree)
  
  #ROC curve
  install.packages("ROCR")
  library(ROCR)
  prediction <- prediction(tree.pred, payment.test)
  performance <- performance(prediction, measure = "tpr", x.measure = "fpr")
  plot(performance, main="ROC curve", xlab="1-Specificity", ylab="Sensitivity")
  
  #Lift curve
  perf <- performance(prediction,"lift","rpp")
  plot(perf, main="lift curve", colorize=T)
}