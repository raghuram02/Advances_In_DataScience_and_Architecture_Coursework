library(weatherData)
library(lubridate)
library(timeDate)
library(forecast)
library(data.table)


zooFunction <- function(){
  
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
  setnames(d15test,old=c("X1"), new=c("kWh"))
  
  # Neglecting the NAs data in the kWh column
  d16_test <- d15test[!(is.na(d15test$kWh))] 
  
  # Neglecting 0s in Kwh by Changing the 0s to NA in Kwh and not considering it
  for(i in 1:nrow(d16_test)){
    if(d16_test$kWh[i] == 0){
      d16_test$kWh[i] = NA
    }
  }
  
  # Replacing NAs using function 
  d16_test<-na.locf(d16_test,na.rm=FALSE)
  
  # Writing the values in CSV
  write.csv(d16_test,"Hourly_Filled_Data_Zoo_1c.csv")
  
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
  
  write.csv(predict, "Metrics_Hourly_Filled_Data_Zoo_1c.csv")
}