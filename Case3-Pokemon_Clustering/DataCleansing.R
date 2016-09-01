install.packages("Rstem")
install.packages("~/text_mining_and_web_scraping/sentiment_0.2.tar.gz", repos = NULL, type = "source")

library(twitteR)
library(sentiment)
library(plyr)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)



# harvest some tweets
# some_tweets = searchTwitter("starbucks", n=10, lang="en")
some_tweets <- searchTwitter("pokemon+Go", n=30000, lang="en")



some.df <- do.call("rbind", lapply(some_tweets, data.frame))
# some_tweets = read.csv("tweets_new.csv", head= T)
# tweet_list <- as.list(as.data.frame(t(some_tweets)))

# get the text
some_txt = sapply(some_tweets, function(x) x$getText())


# remove retweet entities
some_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", some_txt)
# remove at people
some_txt = gsub("@\\w+", "", some_txt)
# remove punctuation
some_txt = gsub("[[:punct:]]", "", some_txt)
# remove numbers
some_txt = gsub("[[:digit:]]", "", some_txt)
# remove html links
some_txt = gsub("http\\w+", "", some_txt)
# remove unnecessary spaces
some_txt = gsub("[ \t]{2,}", "", some_txt)
some_txt = gsub("^\\s+|\\s+$", "", some_txt)

# define "tolower error handling" function 
try.error = function(x)
{
  # create missing value
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error=function(e) e)
  # if not an error
  if (!inherits(try_error, "error"))
    y = tolower(x)
  # result
  return👍
}
# lower case using try.error with sapply 
some_txt = sapply(some_txt, try.error)

# remove NAs in some_txt
some_txt = some_txt[!is.na(some_txt)]
names(some_txt) = NULL



# classify emotion
class_emo = classify_emotion(some_txt, algorithm="bayes", prior=1.0)
# get emotion best fit
emotion = class_emo[,7]
# substitute NA's by "unknown"
emotion[is.na(emotion)] = "unknown"

# classify polarity
class_pol = classify_polarity(some_txt, algorithm="bayes")
# get polarity best fit
polarity = class_pol[,4]



# data frame with results
sent_df = data.frame(text=some_txt, emotion=emotion,
                     polarity=polarity, stringsAsFactors=FALSE)


sent_df = data.frame(text = some_txt, )

# sort data frame
sent_df = within(sent_df,
                 emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))


# plot distribution of emotions
ggplot(sent_df, aes(x=emotion)) +
  geom_bar(aes(y=..count.., fill=emotion)) +
  scale_fill_brewer(palette="Dark2") +
  labs(x="emotion categories", y="number of tweets")


sent_df$pol <- NA

for(i in 1:nrow(sent_df)){
  if(sent_df$polarity[i] == "positive"){
    sent_df$pol[i] = 4
  } else if(sent_df$polarity[i] == "negative"){
    sent_df$pol[i] = 0
  } else if(sent_df$polarity[i] == "neutral"){
    sent_df$pol[i] = 2
  }
}


sent1_df <- sent_df[sent_df$pol!=2, ]
write.csv(sent1_df, "sent1.csv")