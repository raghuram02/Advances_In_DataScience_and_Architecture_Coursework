n <- readline("Enter the file name: ")

return_data <- function1(n)

function1 <- function(n){
  
  test_data = read.csv(n, head = TRUE)
  
  test_df <- data.frame(test_data)
  
  test_df_new <- test_df[test_df$Units!= 'Power Factor' & test_df$Units!= 'kVARh', ]
  
  test_date_format <- as.POSIXlt(test_df_new$Date,format="%m/%d/%Y")
  test_dayOfWeek = weekdays(test_date_format)
  
  test_weekday <- isWeekday(test_date_format,wday=1:5)
  as.numeric(test_weekday)
  
  test_dayOfWeek <- test_date_format$wday
  
  test_dayofyear <- test_date_format$yday
  
  test_hourData <- sapply(seq(5,292,by=12),function(i) rowSums(test_df_new[,i:(i+11)]))
  
  test_powerData <- as.data.frame(test_hourData)
  
  
  test_newdata <- data.frame(account = test_df_new$Account, 
                             date = test_date_format,
                             year = year(test_date_format),
                             month = month(test_date_format),
                             day = day(test_date_format),
                             DayOfWeek = test_dayOfWeek,
                             Dayofyear = test_dayofyear,
                             Weekday = as.numeric(test_weekday))
  x <- t(c(0:23))
  
  x1 <- as.data.frame(x)
  
  test_data_new <- data.frame()
  
  test_datalist = list()
  
  for (i in 1:nrow(test_df_new)){
    test_data_new <- data.frame(test_newdata[i, 1:8, drop = FALSE], hour = t(x1), kwh <- t(test_powerData[i, 1:24, drop = FALSE]), row.names = NULL)
    test_datalist[[i]] <- test_data_new
  }
  
  
  test_big_data = rbindlist(test_datalist)
  
  setnames(test_big_data, old=c("X1"), new=c("kWh"))
  
  
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
  
  head(d10)
  
  library(data.table)
  
  d11 <- data.table(date=format(d10$d1.Time, "%Y/%m/%d"), time=format(d10$d1.Time, "%H:%M"), hour = (as.POSIXlt(d10$d1.Time))$hour, temperature = d10$d1.TemperatureF)
  
  
  d12 <- d11[, list(Temperature = mean(temperature)),by="date,hour"]
  
  d12$date <- as.POSIXct(d12$date)
  test_big_data$date <- as.POSIXct(big_data$date)
  
  d15test <- merge(x = test_big_data, y = d12, by = c("date", "hour"), all.x = TRUE)
  
  d16_test <- d15test[!(is.na(d15test$kWh))] 
  
  d17_test<-sqldf('
                  
                  select 
                  date,
                  hour,
                  account,
                  year,
                  month,
                  day,
                  DayOfWeek,
                  Dayofyear,
                  Weekday,
                  peakhour,
                  Temperature,
                  case when kwh=0 
                  then (nakwh+pakwh+nakwh2+pakwh2+nakwh3+pakwh3)/6 else kwh end as kwh from (
                  
                  
                  
                  
                  select date,
                  hour,
                  account,
                  year,
                  month,
                  day,
                  DayOfWeek,
                  a.Dayofyear,
                  Weekday,
                  a.peakhour,
                  Temperature,
                  case when kwh=0 
                  then akwh else kwh end as kwh,
                  c.nakwh,d.pakwh,e.nakwh2,f.pakwh2,g.nakwh3,h.pakwh3
                  from d16_test a
                  
                  left join
                  
                  
                  (select 
                  Dayofyear,
                  peakhour,
                  avg(kwh) as akwh from d16_test group by
                  
                  Dayofyear,peakhour) b on a.Dayofyear=b.Dayofyear and a.peakhour=b.peakhour
                  
                  left join
                  
                  (select 
                  Dayofyear,
                  peakhour,
                  avg(kwh) as nakwh from d16_test group by
                  
                  Dayofyear,peakhour) c on a.Dayofyear=c.Dayofyear+1 and a.peakhour=c.peakhour
                  
                  left join
                  
                  (select 
                  Dayofyear,
                  peakhour,
                  avg(kwh) as pakwh from d16_test group by
                  
                  Dayofyear,peakhour) d on a.Dayofyear=d.Dayofyear-1 and a.peakhour=d.peakhour
                  left join
                  (select 
                  Dayofyear,
                  peakhour,
                  avg(kwh) as nakwh2 from d16_test group by
                  
                  Dayofyear,peakhour) e on a.Dayofyear=e.Dayofyear+2 and a.peakhour=e.peakhour
                  
                  left join
                  
                  (select 
                  Dayofyear,
                  peakhour,
                  avg(kwh) as pakwh2 from d16_test group by
                  
                  Dayofyear,peakhour) f on a.Dayofyear=f.Dayofyear-2 and a.peakhour=f.peakhour
                  
                  
                  left join
                  (select 
                  Dayofyear,
                  peakhour,
                  avg(kwh) as nakwh3 from d16_test group by
                  
                  Dayofyear,peakhour) g on a.Dayofyear=g.Dayofyear+3 and a.peakhour=g.peakhour
                  
                  left join
                  
                  (select 
                  Dayofyear,
                  peakhour,
                  avg(kwh) as pakwh3 from d16_test group by
                  
                  Dayofyear,peakhour) h on a.Dayofyear=h.Dayofyear-3 and a.peakhour=h.peakhour
                  
                  
                  
                  ) a
                  ')
  
  for(i in 1:nrow(d17_test))
  {
    if(d17_test$kwh[i] == 0){
      d17_test$kwh[i] = NA
    }
    else{
      
    }
    
  }
  head(d17_test)
  d17_test1<-sqldf('select date,
                   hour,
                   account,
                   year,
                   month,
                   day,
                   DayOfWeek,
                   Dayofyear,
                   Weekday,
                   peakhour,
                   Temperature,
                   kwh 
                   from d17_test where kwh!=0')
  
  write.csv(d17_test1,file="sampleformat.csv")
}
