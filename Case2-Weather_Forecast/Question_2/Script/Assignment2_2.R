library(weatherData)
library(lubridate)
library(timeDate)
library(forecast)
library(data.table)


treeFunction <- function(){
  
  # Getting the CSV File
  n <- readline("Enter the file name: ")
  
  # Reading the CSV File
  test_data = read.csv("NewData.csv", head = TRUE)
  
  # Assigning the CSV file to data frame
  test_df <- data.frame(test_data)
  
  # Removing the Units Power Factor and kVARh as it is not required
  test_df_new <- test_df[test_df$Units!= 'Power Factor' & test_df$Units!= 'kVARh', ]
  
  # Extracting the day,month, weekday, DayOfWeek from Date
  test_date_format <- as.POSIXlt(test_df_new$Date,format="%m/%d/%Y")
  test_dayOfWeek = weekdays(test_date_format)
  test_weekday <- isWeekday(test_date_format,wday=1:5)
  test_dayOfWeek <- test_date_format$wday
  
  # Getting the hourly data
  test_hourData <- sapply(seq(5,292,by=12),function(i) rowSums(test_df_new[,i:(i+11)]))
  test_powerData <- as.data.frame(test_hourData)
  
  # Creating the structure for the output
  test_newdata <- data.frame(account = test_df_new$Account, 
                             date = test_date_format,
                             year = year(test_date_format),
                             month = month(test_date_format),
                             day = day(test_date_format),
                             DayOfWeek = test_dayOfWeek,
                             Weekday = as.numeric(test_weekday))
  # Getting the hour
  x <- t(c(0:23))
  x1 <- as.data.frame(x)
  
  # Creating a new data frame and populating it with the determined values
  test_data_new <- data.frame()
  test_datalist = list()
  for (i in 1:nrow(test_df_new)){
    test_data_new <- data.frame(test_newdata[i, 1:7, drop = FALSE], hour = t(x1), kwh <- t(test_powerData[i, 1:24, drop = FALSE]), row.names = NULL)
    test_datalist[[i]] <- test_data_new
  }
  test_big_data = rbindlist(test_datalist)
  setnames(test_big_data, old=c("X1"), new=c("kWh"))
  
  # Calculating the peak hour data and appending to the existing data frame
  peakhourData = c(7:19)
  test_big_data$peakhour<-NA
  for(i in 1:nrow(test_big_data)){
    if(test_big_data$hour[i] %in% peakhourData){
      test_big_data$peakhour[i] = 1
    }
    else
    {
      test_big_data$peakhour[i] = 0 
    }
  }
  
  # Getting the weather data for a year in intervals and combining them as a data frame
  d1 <- getWeatherForDate("KBOS", start_date="2014-01-01",
                          end_date = "2014-03-31",
                          opt_detailed = TRUE,
                          opt_all_columns = TRUE)
  
  
  d2 <- getWeatherForDate("KBOS", start_date="2014-04-01",
                          end_date = "2014-06-30",
                          opt_detailed = TRUE,
                          opt_all_columns = TRUE)
  
  d3 <- getWeatherForDate("KBOS", start_date="2014-07-01",
                          end_date = "2014-09-30",
                          opt_detailed = TRUE,
                          opt_all_columns = TRUE)
  
  d4 <- getWeatherForDate("KBOS", start_date="2014-10-01",
                          end_date = "2014-12-31",
                          opt_detailed = TRUE,
                          opt_all_columns = TRUE)
  
  d5 <- data.frame(d1$Time, d1$TemperatureF)
  d6 <- data.frame(d2$Time, d2$TemperatureF)
  d7 <- data.frame(d3$Time, d3$TemperatureF)
  d8 <- data.frame(d4$Time, d4$TemperatureF)
  d9 <- list(d5,d6,d7,d8)
  d10 <- rbindlist(d9)
  d10 <- d10[!(is.na(d10$d1.TemperatureF))] 
  d11 <- data.table(date=format(d10$d1.Time, "%Y/%m/%d"), time=format(d10$d1.Time, "%H:%M"), hour = (as.POSIXlt(d10$d1.Time))$hour, temperature = d10$d1.TemperatureF)
  d12 <- d11[, list(Temperature = mean(temperature)),by="date,hour"]
  
  # Handling the temperature outlier
  for(i in 1:nrow(d10)){
    if(d10$d1.TemperatureF[i] < 0){
      d10$d1.TemperatureF[i] = NA
    }
  }
  
  # Changing the date format accordingly
  d12$date <- as.POSIXct(d12$date)
  test_big_data$date <- as.POSIXct(test_big_data$date)
  
  # Performing the left join so that 2 data frames are merged
  d15test <- merge(x = test_big_data, y = d12, by = c("date", "hour"), all.x = TRUE)
  
  # Considering the data set with temperature as 0 and NA
  d16_test <- d15test[((d15test$Temperature > 0) | is.na(d15test$Temperature))] 
  
  # Considering the data set by neglecting NA in kWh
  d17_test <- d16_test[!(is.na(d16_test$kWh))] 
  
  tree_data = read.csv("Hourly_Filled_Data_Zoo_1c.csv", head = TRUE)
  
  #Set the seed to make your partition reproductible
  set.seed (1)
  
  #Split the data into training and testing
  train = sample (1:nrow(tree_data), nrow(tree_data)/2)
  
  install.packages("MASS")
  install.packages("grid")
  install.packages("tree")
  install.packages("neuralnet")
  library(MASS)
  library(grid)
  library(tree)
  library(neuralnet)
  #Fit in a regression tree
  tree.energy = tree(kWh ~ Temperature + Weekday + DayOfWeek + peakhour,tree_data,subset=train)
  
  #Summary of the tree
  summary (tree.energy)
  
  #Plotting the tree
  plot (tree.energy)
  text (tree.energy, pretty = 0)
  cv.energy = cv.tree (tree.energy)
  plot (cv.energy$size, cv.energy$dev, type='b')
  
  # Pruning a tree
  prune.energy =prune.tree(tree.energy, best = 5)
  plot(prune.energy)
  text(prune.energy, pretty = 0)
  
  #Predicting the kWh value
  yhat=predict (tree.energy, newdata4 = tree_data[-train,])
  energy.test=tree_data [-train, "kWh"]
  plot(yhat,energy.test)
  abline (0,1)
  mean((yhat -energy.test)^2)
  pred_tree <- accuracy(yhat, energy.test)
  
  write.csv(pred_tree, "PredictionPerformanceMetrics_Tree2.csv")
  
}

#Neural

neuralnetFunction <- function(){
  library(neuralnet)
  library(MASS)
  
  #Get file name
  n1 <- readline("Enter the file name: ")
  
  # Read csv data
  tree1 = read.csv(n1, head = TRUE)
  
  #Set the seed to make your partition reproductible
  set.seed(500)
  
  # Neglecting the unwanted columns
  tree2 <- tree1
  tree2 <- tree2[,-c(1,2,4,5)]
  
  # To check that there are no columns with 0s
  apply(tree2,2,function(x) sum(is.na(x)))
  
  # Setting the max and min values
  maxs <- apply(tree2, 2, max) 
  mins <- apply(tree2, 2, min)
  
  # Converting the columns to numeric
  tree2$hour <- as.numeric(tree2$hour)
  tree2$month <- as.numeric(tree2$month)
  tree2$day <- as.numeric(tree2$day)
  tree2$DayOfWeek <- as.numeric(tree2$DayOfWeek)
  tree2$Weekday <- as.numeric(tree2$Weekday)
  tree2$kWh <- as.numeric(tree2$kWh)
  tree2$peakhour <- as.numeric(tree2$peakhour)
  tree2$Temperature <- as.numeric(tree2$Temperature)
  
  # Scale the data
  scaled <- as.data.frame(scale(tree2, center = mins, scale = maxs - mins))
  
  #Split the data into training and testing
  index <- sample(1:nrow(tree2),round(0.75*nrow(tree2)))
  train5 <- scaled[index,]
  test5 <- scaled[-index,]
  
  #Fit in neural networks model
  n <- names(train5)
  f <- as.formula(paste("kWh ~", paste(n[!n %in% "kWh"], collapse = " + ")))
  nn <- neuralnet(f,data=train5,hidden=5,linear.output=F, threshold = 0.5)
  plot(nn)
  
  # Compute the values
  test6 <- test5[,-7]
  pr.nn <- compute(nn,test6[,1:7])
  pr.nn_ <- pr.nn$net.result*(max(tree2$kWh)-min(tree2$kWh))+min(tree2$kWh)
  test.r <- (test5$kWh)*(max(tree2$kWh)-min(tree2$kWh))+min(tree2$kWh)
  MSE.nn <- sqrt(sum((test.r - pr.nn_)^2)/nrow(test5))
  
  MAE.nn <- mean(abs(test.r - pr.nn_))
  MAPE <- mean(abs((test.r - pr.nn_)/test.r))
  RMSE.nn <- sqrt(sum((test.r - pr.nn_)^2)/nrow(test5))
  
  error<-data.frame()
  error<-c(MAE.nn,MAPE,RMSE.nn)
  error_data<-as.data.frame(error)
  error_data_t<-t(error_data)
  colnames(error_data_t)<-c("MAE","MAPE","RMSE")
  
  write.csv(error_data_t, file="PredictionPerformanceMetrics_NeuralNetworks2.csv")
  
}


