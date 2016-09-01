n <- readline("Enter the file name: ")

data <- function3(n)

function3 <- function(n){
  forecast_csv_data <- read.csv(n, head = TRUE)
  forecast = data.frame(forecast_csv_data)
  forecast_dateformat<-as.POSIXlt(forecast$Day,format="%m/%d/%Y")
  forecast_dayOfWeek = forecast_dateformat$wday
  forecast_Weekday <- isWeekday(forecast_dateformat,wday=1:5)
  
  
  newdata <- data.frame(Date = forecast_csv_data$Day,
                        month = month(forecast_dateformat),
                        day = day(forecast_dateformat),
                        year = year(forecast_dateformat),
                        hour = forecast_csv_data$Hour,
                        DayOfWeek = forecast_dayOfWeek,
                        Weekday = as.numeric(forecast_Weekday),
                        Temperature = forecast_csv_data$Temp)
  
  
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
  
  write.csv(newdata, file="forecastInput.csv")
  
  regression_data = read.csv("RegressionOutputs.csv", head = TRUE)
  
  constant <- regression_data[1,2]
  m1_weekday <-  regression_data[2,2]
  m2_temp <- regression_data[3,2]
  m3_peakhour <- regression_data[4,2]
  m4_day_of_week <- regression_data[5,2]
  m5_hour <- regression_data[6,2]
  
  kwh <- ((m1_weekday*newdata$Weekday) + (m2_temp*newdata$Temperature) + (m3_peakhour*newdata$peakhour) + (m4_day_of_week*newdata$DayOfWeek) + (m5_hour*newdata$hour) + constant)
  
  forecast_newdata <- data.frame(Day = forecast$Day, 
                                 Hr = forecast$Hour,
                                 Temp = forecast$Temp,
                                 KWH = kwh)
  
  
  sql<-as.numeric(sqldf('select v1 from regression_data where X="Account"')) 
  sql
  
  write.csv(forecast_newdata, "forecast_Output.csv" )
}