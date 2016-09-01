library(shiny)
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
library(devtools)

ui<-(fluidPage( 
  titlePanel("Sentiment Analysis"), #Title
  textOutput("currentTime"),   #Here, I show a real time clock
  h4("Tweets:"),   #Sidebar title
  sidebarLayout(
    sidebarPanel(
      dataTableOutput('tweets_table') #Here I show the users and the sentiment
    ),
    
    mainPanel(
      plotOutput("distPlot"), #Here I will show the bars graph
      sidebarPanel(
        plotOutput("positive_wordcloud") #Cloud for positive words
      ),
      sidebarPanel(
        plotOutput("negative_wordcloud") #Cloud for negative words
      ),
      sidebarPanel(
        plotOutput("neutral_wordcloud") #Cloud for neutral words
      )))))



configureApp("Team5_TwitterSentimentAnalysis", size="xlarge")

server<-(function(input, output, session) {
  memory.limit(size=5120)
  
  consumerKey <- "13Ne9Qrt8yEbyRO5qcQz01eZD"
  consumerSecret <- "CVliAkYCptee2niyK0Wt8M27MWOGNJeEGIW1FEXRupFvKBY5KW"
  accessToken <- "988672262-wAHE6iW16wYznXLjBhl3Geg751o015Vmpeqfj9TT"
  accessTokenSecret <- "lfFwGnWbdDEB9N4IlDENUzXUaVOISMooM8q071azXaXyg"
  
  setup_twitter_oauth(consumerKey, consumerSecret, accessToken, accessTokenSecret)
  
  
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
    
    tweets_result = searchTwitter("DallasPoliceShootings")
    for (tweet in tweets_result){
      print(paste(tweet$screenName, ":", tweet$text))
      
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
    
    output$tweets_table = renderDataTable({
      df_users_sentiment
    })
    
    output$distPlot <- renderPlot({
      
      results = data.frame(tweets = c("Positive", "Negative", "Neutral"), numbers = c(count_positive,count_negative,count_neutral))
      
      barplot(results$numbers, names = results$tweets, xlab = "Sentiment", ylab = "Counts", col = c("Green","Red","Blue"))
      
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


rsconnect::configureApp(appName="Team5_TwitterSentimentAnalysis123",account="raghuram02", size="xlarge")

shinyApp(ui = ui, server = server)

rsconnect::setAccountInfo(name='raghuram02', token='B658086D5E728A1021F6CCCABE36332C', secret='gRUHNF0KUvr5cMXevssPjcwoFklR1qxtMRJzpoKx')