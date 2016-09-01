library(lubridate)
library(timeDate)

forecastFunction <- function(){
  
  # Getting the CSV File
  n3 <- readline("Enter the filled data file name: ")
  
  # Reading the CSV File
  hourly_data <- read.csv(n3, head = TRUE)
  
  #Taking the sample data
  smp_size <- floor(0.75 * nrow(hourly_data))
  
  #Set the seed to make your partition reproductible
  set.seed(123)
  train_ind <- sample(seq_len(nrow(hourly_data)), size = smp_size)
  
  #Split the data into training and testing
  train = sample (1:nrow(hourly_data), nrow(hourly_data)/2)
  
  # Fit in tree model
  tree.energy = tree(kWh ~ Temperature + hour + DayOfWeek + peakhour ,hourly_data,subset=train)
  
  # Summary of tree
  summary (tree.energy)
  
  #Set the seed to make your partition reproductible
  set.seed(500)
  
  # Neglecting the unwanted columns
  tree2 <- hourly_data
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
  tree2$Dayofyear <- as.numeric(tree2$Dayofyear)
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
  
  
  
  
  forecast_data <- read.csv("Assignment2/forecastNewData.csv", header = F)
  colnames(forecast_data)<-c("Day","Hour","Temp")
  View(forecast_data)
  
  date <-parse_date_time(as.character(forecast_data$Day),"%y/%m/%d")
  newdate2<- format(as.Date(date),"%m/%d/%Y")
  forecast_dateformat<-as.POSIXlt(newdate2,format="%m/%d/%Y")
  forecast_dayOfWeek = forecast_dateformat$wday
  forecast_Weekday <- isWeekday(forecast_dateformat,wday=1:5)
  
  
  newdata <- data.frame(Date = forecast_data$Day,
                        month = month(forecast_dateformat),
                        day = day(forecast_dateformat),
                        year = year(forecast_dateformat),
                        hour = forecast_data$Hour,
                        DayOfWeek = forecast_dayOfWeek,
                        Weekday = as.numeric(forecast_Weekday),
                        Temperature = forecast_data$Temp)
  
  
  peakhourData = c(7:19)
  
  newdata$peakhour<-NA
  for(i in 1:nrow(newdata)){
    if(newdata$hour[i] %in% peakhourData){
      newdata$peakhour[i] = 1
    }
    else
    {
      newdata$peakhour[i] = 0 
    }
  }
  
  
  # Compute the values using regression tree
  pred = predict(tree.energy, newdata)
  forecast_output1 <- cbind(forecast_data, pred)
  write.csv(forecast_output1, "forecastOutput_Output_regressionTree.csv")
  
  
  # Neglecting the unwanted columns
  tree3 <- newdata[,-c(1,4)]
  
  # Converting the columns to numeric
  tree3$hour <- as.numeric(tree3$hour)
  tree3$month <- as.numeric(tree3$month)
  tree3$day <- as.numeric(tree3$day)
  tree3$DayOfWeek <- as.numeric(tree3$DayOfWeek)
  tree3$Weekday <- as.numeric(tree3$Weekday)
  tree3$peakhour <- as.numeric(tree3$peakhour)
  tree3$Temperature <- as.numeric(tree3$Temperature)
  
  # Compute the values using neural networks
  pr.nn <- compute(nn,tree3[,1:7])
  pr.nn_ <- pr.nn$net.result*(max(tree2$kWh)-min(tree2$kWh))+min(tree2$kWh)
  forecast_output2 <- cbind(forecast_data, pred)
  write.csv(forecast_output2, "forecastOutput_Output_neuralNetwork.csv")
  
  
}