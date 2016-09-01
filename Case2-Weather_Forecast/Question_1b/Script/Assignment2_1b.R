replaceFunction <- function(n){
  # Getting the CSV File
  csv1 <- readline("Enter the file name for training the model: ")
  csv2 <- readline("Enter the file name for prediction: ")
  
  # Reading the CSV File
  test_csv_data <- read.csv(csv1, head = TRUE)
  test_csv4 <- read.csv(csv2, head = TRUE)
  
  #75% of the sample size
  smp_size <- floor(0.75 * nrow(test_csv_data))
  
  #Set the seed to make your partition reproductible
  set.seed(123)
  train_ind <- sample(seq_len(nrow(test_csv_data)), size = smp_size)
  
  #Split the data into training and testing
  train <- test_csv_data[train_ind, ]
  test <- test_csv_data[-train_ind, ]
  
  #Construct a logistic regression model using the variables 
  lm.fit = lm(kWh ~ Weekday + peakhour + DayOfWeek + hour + day + month , data = train)
  
  #Summary of the fit
  m = summary(lm.fit)
  
  #Measures of predictive accuracy
  pred1 = predict(lm.fit, test_csv4)
  predict <- accuracy(pred1, test_csv4$kWh)
  
  # Converting the predicted value to a data frame  
  preddataframe <- as.data.frame(pred1)
  
  # Column bind the predicted value with the existing dataset
  predictionWithoutTemp <- cbind(test_csv4,preddataframe$pred1)
  
  # Change the column name
  setnames(predictionWithoutTemp, old=c("preddataframe$pred1"), new=c("newkwh"))
  
  # Replacing the 0s with the predicted value
  for(i in 1:nrow(predictionWithoutTemp)){
    if(predictionWithoutTemp$kWh[i]==0){
      predictionWithoutTemp$kWh[i] = predictionWithoutTemp$newkwh[i]
    }
  }
  
  # Neglecting the newly generated column
  predictionWithoutTemp$newkwh <- NULL
  
  # Writing into CSV the filled data
  write.csv(predictionWithoutTemp, file="Hourly_filled_Data_1b.csv")
  
}


computeFunction <- function(){
  # Getting the CSV File
  n <- readline("Enter the file name: ")
  
  # Reading the CSV File
  test_csv_data = read.csv(n, head = TRUE)
  
  #75% of the sample size
  smp_size <- floor(0.75 * nrow(test_csv_data))
  
  #Set the seed to make your partition reproductible
  set.seed(123)
  train_ind <- sample(seq_len(nrow(test_csv_data)), size = smp_size)
  
  #Split the data into training and testing
  train <- test_csv_data[train_ind, ]
  test <- test_csv_data[-train_ind, ]
  
  #Construct a logistic regression model using the variables 
  lm.fit = lm(kWh ~ Weekday + peakhour + DayOfWeek + hour + day + month , data = train)
  
  #Summary of the fit
  m = summary(lm.fit)
  
  #Measures of predictive accuracy
  pred1 = predict(lm.fit, test)
  predict <- accuracy(pred1, test$kWh)
  
  write.csv(predict, "Metrics_RawData_1b.csv")
}