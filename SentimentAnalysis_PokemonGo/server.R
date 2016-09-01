




server<-(function(input, output, session) {
 
  #Authorizing with credentials
  
  consumerKey <- "13Ne9Qrt8yEbyRO5qcQz01eZD"
  consumerSecret <- "CVliAkYCptee2niyK0Wt8M27MWOGNJeEGIW1FEXRupFvKBY5KW"
  accessToken <- "988672262-wAHE6iW16wYznXLjBhl3Geg751o015Vmpeqfj9TT"
  accessTokenSecret <- "lfFwGnWbdDEB9N4IlDENUzXUaVOISMooM8q071azXaXyg"
  
  setup_twitter_oauth(consumerlibrary(shiny)
library(tm)
library(wordcloud)
library(twitteR)
library(methods)
library(ROAuth)
require(RCurl)
library(stringr)
library(ggmap)
library(plyr)
library(dplyr)
library(RColorBrewer)
library(rsconnect)
library(devtools)Key, consumerSecret, accessToken, accessTokenSecret)
  
  
  token <- get("oauth_token", twitteR:::oauth_cache)
  token$cache()
  
  output$currentTime <- renderText({invalidateLater(1000, session) 
  paste("Current time is: ",Sys.time())})
  
  observe({
    
  invalidateLater(60000,session)
    
    count_positive = 0
    count_negative = 0
    count_neutral = 0
    
    positive_text <- vector()
    negative_text <- vector()
    neutral_text <- vector()
    
    vector_users <- vector()
    vector_sentiments <- vector()
    
    tweets_result = ""
    
    #Search by current trends
    tweets_result = searchTwitter("DallasPoliceShootings", n=25, lang = "en")
    for (tweet in tweets_result){
      print(paste(tweet$screenName, ":", tweet$text))
      
      #Distinguishing tweets as positive, negative and neutral
      vector_users <- c(vector_users, as.character(tweet$screenName));
      
      if (grepl("together", tweet$text, ignore.case = TRUE) == TRUE | grepl("prayers", tweet$text, ignore.case = TRUE)){
        count_positive = count_positive + 1
        
        vector_sentiments <- c(vector_sentiments, "Positive")
        positive_text <- c(positive_text, as.character(tweet$text))
        
      } else if (grepl("nonsense", tweet$text, ignore.case = TRUE) | grepl("shootings", tweet$text, ignore.case = TRUE)) { 
        count_negative = count_negative + 1
        
        vector_sentiments <- c(vector_sentiments, "Negative")
        negative_text <- c(negative_text, as.character(tweet$text))
        
      } else {
        count_neutral = count_neutral + 1
        vector_sentiments <- c(vector_sentiments, "Neutral")
        neutral_text <- c(neutral_text, as.character(neutral_text))
      }
    }
    
    df_users_sentiment <- data.frame(vector_users, vector_sentiments)
    
    # Displaying number of tweets handled and output Bar chart
     output$tweets_table = renderDataTable({
      df_users_sentiment
    })
    
    
    output$distPlot <- renderPlot({
      
      results = data.frame(tweets = c("Positive", "Negative", "Neutral"), numbers = c(count_positive,count_negative,count_neutral))
      
      barplot(results$numbers, names = results$tweets, xlab = "Sentiment", ylab = "Counts", col = c("Green","Red","Blue"))
      
      
      # Plotting a wordcloud (bag of words) from the tweets
      
      if (length(positive_text) > 0){
        
        output$positive_wordcloud <- renderPlot({ wordcloud(paste(positive_text, collapse=" "), min.freq = 0,  random.color=TRUE, max.words=100 ,colors=brewer.pal(8, "Dark2")) }) 
      }
      
      if (length(negative_text) > 0) {
        
        output$negative_wordcloud <- renderPlot({ wordcloud(paste(negative_text, collapse=" "), min.freq = 0, random.color=TRUE, max.words=100 ,colors=brewer.pal(8,"Set3")) }) 
      }
      
      
      if (length(neutral_text) > 0){
        
        output$neutral_wordcloud <- renderPlot({ wordcloud(paste(neutral_text, collapse=" "),min.freq = 0, random.color=TRUE , max.words=100 ,colors=brewer.pal(8, "Dark2")) }) 
      }
      
    })
  })
})
shinyApp(ui = ui, server = server)


configureApp(appName="Sentiment_Analysis_Team5",account="raghuram02", size="xlarge")


setwd("C:/Users/Raghuram/Desktop/Midterm_ADS\SentimentAnalysis_Team5")










