install.packages("twitteR")
install.packages("ROAuth")
install.packages("RCurl")
install.packages("stringr")
install.packages("tm")
install.packages("ggmap")
install.packages("dplyr")
install.packages("plyr")
install.packages("wordcloud")
install.packages(c("devtools", "rjson", "bit64", "httr"))
install_github("twitteR", username="geoffjentry")

library(twitteR)
library(ROAuth)
require(RCurl)
library(stringr)
library(tm)
library(ggmap)
library(plyr)
library(dplyr)
library(tm)
library(wordcloud)
library(devtools)
library(twitteR)

# Setting the working directory
setwd("C:/Users/Lakshmi/Documents/text_mining_and_web_scraping")

# Setting the authentication
api_key <- "ClWMbfS8GuqlSg3TRrhaRqz9v"
api_secret <- "tPXXRu0HnatNYf4mBtbqZ4YEmd3bq3Y839U8Y9bDzprRmCBoTk"
access_token <- "748605873782665220-YJQbnK0s2GzDdu0wKU3KRY3iZDL2mbs"
access_token_secret <- "tlDMjBupP01fBVib9SOFDRf3dcTdkNKb5Tz6zqyqdooiS"

# Authentication
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)
save(setup_twitter_oauth, file="twitter authentication.Rdata")

N=2000  # tweets to request from each query
S=200  # radius in miles

#cities=DC,New York,San Fransisco,Colorado,Mountainview,Tampa,Austin,Boston,
#       Seatle,Vegas,Montgomery,Phoenix,Little Rock,Atlanta,Springfield,
#       Cheyenne,Bisruk,Helena,Springfield,Madison,Lansing,Salt Lake City,Nashville
#       Jefferson City,Raleigh,Harrisburg,Boise,Lincoln,Salem,St. Paul
# Setting the latitudes and longitudes
lats=c(38.9,40.7,37.8,39,37.4,28,30,42.4,48,36,32.3,33.5,34.7,33.8,37.2,41.2,46.8,46.6,37.2,
       43,42.7,40.8,36.2,38.6,35.8,40.3,43.6,40.8,44.9,44.9)
lons=c(-77,-74,-122,-105.5,-122,-82.5,-98,-71,-122,-115,-86.3,-112,-92.3,-84.4,-93.3,-104.8,
       -100.8,-112, -93.3,-89,-84.5,-111.8,-86.8,-92.2,-78.6,-76.8,-116.2,-98.7,-123,-93)

# Getting the twitter data
donald=do.call(rbind,lapply(1:length(lats), function(i) searchTwitter('pokemon+go',
lang="en",n=N,resultType="recent", geocode=paste(lats[i],lons[i],paste0(S,"mi"),sep=","))))

# Getting the latitude and longitude of the tweet, the tweet, re-twitted and favorited count, 
# the date and time it was twitted
donaldlat=sapply(donald, function(x) as.numeric(x$getLatitude()))
donaldlat=sapply(donaldlat, function(z) ifelse(length(z)==0,NA,z))  
donaldlon=sapply(donald, function(x) as.numeric(x$getLongitude()))
donaldlon=sapply(donaldlon, function(z) ifelse(length(z)==0,NA,z))  
donalddate=lapply(donald, function(x) x$getCreated())
donalddate=sapply(donalddate,function(x) strftime(x, format="%Y-%m-%d %H:%M:%S",tz = "UTC"))
donaldtext=sapply(donald, function(x) x$getText())
donaldtext=unlist(donaldtext)
isretweet=sapply(donald, function(x) x$getIsRetweet())
retweeted=sapply(donald, function(x) x$getRetweeted())
retweetcount=sapply(donald, function(x) x$getRetweetCount())
favoritecount=sapply(donald, function(x) x$getFavoriteCount())
favorited=sapply(donald, function(x) x$getFavorited())

# Data Formation
data=as.data.frame(cbind(tweet=donaldtext,date=donalddate,lat=donaldlat,lon=donaldlon,
                         isretweet=isretweet,retweeted=retweeted, retweetcount=retweetcount,
                         favoritecount=favoritecount,favorited=favorited))

usableText=str_replace_all(data$tweet,"[^[:graph:]]", " ") 

# Create corpus
corpus=Corpus(VectorSource(usableText))

# Convert to lower-case
corpus=tm_map(corpus,tolower)

# Remove stopwords
corpus=tm_map(corpus,function(x) removeWords(x,stopwords()))

# convert corpus to a Plain Text Document
corpus=tm_map(corpus,PlainTextDocument)

col=brewer.pal(6,"Dark2")
wordcloud(corpus, min.freq=25, scale=c(5,2),rot.per = 0.25,
          random.color=T, max.word=45, random.order=F,colors=col)

# Plotting a bar plot
count_positive = 0
count_negative = 0
count_neutral = 0

positive_text <- vector()
negative_text <- vector()
neutral_text <- vector()

vector_users <- vector()
vector_sentiments <- vector()



for (tweet in donald){
  
  vector_users <- c(vector_users, as.character(tweet$screenName));
  
  if (grepl("playing", tweet$text, ignore.case = TRUE) == TRUE | grepl("liked", tweet$text, 
                                                                       ignore.case = TRUE)){
    count_positive = count_positive + 1
    vector_sentiments <- c(vector_sentiments, "Positive")
    positive_text <- c(positive_text, as.character(tweet$text))
    
  } else if (grepl("catch", tweet$text, ignore.case = TRUE) | grepl("sick", tweet$text, 
                                                                    ignore.case = TRUE)) { 
    count_negative = count_negative + 1
    vector_sentiments <- c(vector_sentiments, "Negative")
    negative_text <- c(negative_text, as.character(tweet$text))
    
  } else {
    count_neutral = count_neutral + 1
    vector_sentiments <- c(vector_sentiments, "Neutral")
    neutral_text <- c(neutral_text, as.character(neutral_text))
  }
}

# Grouped Bar Plot
counts <- table(vector_sentiments)
barplot(counts, main="Sentiments of people on  PokemonGo", 
        xlab="Sentiments", ylab="Number of tweets", col=c("red","skyblue", "green"), 
        legend = rownames(counts), beside=TRUE)
