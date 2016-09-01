n <- readline("Enter the file name: ")
return_data <- function2(n)

function2 <- function(n){
  test_csv_data <- read.csv(n, head = TRUE)
  smp_size <- floor(0.75 * nrow(test_csv_data))
  
  set.seed(123)
  train_ind <- sample(seq_len(nrow(test_csv_data)), size = smp_size)
  
  train <- test_csv_data[train_ind, ]
  test <- test_csv_data[-train_ind, ]
  
  lm.fit = lm(kwh ~ Weekday + Temperature + peakhour + DayOfWeek + hour , data = train)
  
  
  m = summary(lm.fit)
  
  m$coefficients[,1]
  cooef <- m$coefficients[,1]
  
  Account <- test_csv_data$account[1]
  
  cooef_data <- as.matrix(cooef)
  cooef_data <- rbind(cooef_data, Account)
  pred = predict(lm.fit, test)
  predict <- accuracy(pred, test$kwh)
  
  
  write.csv(cooef_data, file="RegressionOutputs.csv")
  write.csv(predict, file="PerformanceMetrics.csv")
}