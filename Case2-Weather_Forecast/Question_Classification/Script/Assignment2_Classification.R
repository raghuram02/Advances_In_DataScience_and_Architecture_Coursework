install.packages("nnet")
library(nnet)

findkwh <- function(){
  
  # Linear Regression Model
  classification1 <- read.csv("Hourly_Filled_Data_Tree.csv", head = T)
  
  avg_kwh <- mean(classification1$kWh)
  
  smp_size <- floor(0.75 * nrow(classification1))
  
  #Set the seed to make your partition reproductible
  set.seed(123)
  train_ind <- sample(seq_len(nrow(classification1)), size = smp_size)
  
  #Split the data into training and testing
  train <- classification1[train_ind, ]
  test <- classification1[-train_ind, ]
  
  #Fit a linear regression model 
  lm.fit5 = lm(kWh ~ Weekday + Temperature + peakhour + hour, data = train)
  
  #Summary of the fit
  summary(lm.fit5)
  
  #Measures of predictive accuracy
  install.packages("forecast")
  library(forecast)
  pred = predict(lm.fit5, classification1)
  
  cbin1 <- cbind(classification1, pred)
  
  cbin1$KWh_Class<-NA
  for(i in 1:nrow(cbin1)){
    if(cbin1$pred[i]>avg_kwh){
      cbin1$KWh_Class[i]<- "Above_Normal"
    }
    else
    {
      cbin1$KWh_Class[i]<-"Optimal"
    }
  }
  accuracy(pred, test$kWh)
  write.csv(pred, "ClassificationPerformanceMetrics.csv")
  
  # Classification Trees
  
  classification2 <- read.csv("Assignment2/Hourly_Filled_Data_Tree.csv", head = T)
  avg_kwh <- mean(classification2$kWh)
  kWh_Class = ifelse (classification2$kWh >avg_kwh, "Above_Normal ", "Optimal")
  classification3 = data.frame(classification2 ,kWh_Class)
  
  library(tree)
  tree = tree(kWh_Class ~ Temperature + hour + day + DayOfWeek, classification3)
  summary(tree)
  plot(tree)
  text(tree,pretty=0)
  set.seed (2)
  train = sample (1: nrow(classification3), 200)
  classification3.test = classification3[-train,]
  kWh_Class.test = classification3[-train]
  #Build the tree based on the training set
  classification3.train = tree(kWh_Class ~ kWh, classification3, subset = train)
  #Evaluate its performance on the test data
  tree.pred = predict(classification3.train, classification3.test, type ="class")
  table(tree.pred, classification3)
  
  # Neural Networks
  seeds <- read.csv("Assignment2/Hourly_Filled_Data_Tree.csv", head = T)
  seeds$X<-NULL
  seedstrain<- sample(1:210,1)
  seedstest <- setdiff(1:210,seedstrain)
  install.packages("nnet")
  library(nnet)
  ideal <- class.ind(seeds$kWh)
  train<- sample(1:nrow(seeds),5359)
  test<-setdiff(1:nrow(seeds),train)
  seedsANN = nnet(class.ind(kWh_Class)~Temperature+hour+month+day+year+peakhour+Weekday, seeds[train,], size=10,  softmax=TRUE,maxit=10)
  predict(seedsANN,seeds[train,-12], type="class")
  output<-table(predict(seedsANN, seeds[test,-12], type="class"),seeds[test,]$KWh_Class)
  
}