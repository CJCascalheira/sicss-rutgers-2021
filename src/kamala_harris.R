# Load dependencies
library(rtweet)
library(tidyverse)
library(wordcloud)
library(tidytext)
library(textdata)
library(SnowballC)
library(tm)

# Pull sentiment library
senti <- get_sentiments("afinn")
tweets <- readRDS(file = "tweet_data.rds")

# Sentiment analysis prepare
tweet_sent_1 <- tweets %>%
  unnest_tokens(tweets_1, text) %>%
  select(user_id, retweet_count, tweets_1) %>%
  mutate(tweets_1 = wordStem((tweets_1), language = "en")) %>%
  left_join(senti, by = c("tweets_1" = "word")) %>%
  filter(!is.na(value)) 

# Average pos vs. nega
tweet_sent_1 %>%
  mutate(
    pos_neg = if_else(value > 0, "pos", "neg")
  ) %>%
  group_by(pos_neg) %>%
  summarize(
    mean = mean(value)
  )

# Retweets
tweet_sent_1 %>% 
  mutate(
    pos_neg = if_else(value > 0, "pos", "neg")
  ) %>%
  group_by(pos_neg) %>%
  distinct(user_id, .keep_all = TRUE) %>%
  summarize(
    sum_of_rt = sum(retweet_count)
  )

# Distribution of positive and negative sentiment
tweet_sent_1 %>%
  ggplot(aes(value)) +
  geom_bar() +
  theme_bw() +
  labs(y = "", x = "Sentiment Value", 
       title = "Distribution of Sentiment in Replies to Kamala Harris",
       subtitle = "June 7th, 2021")

# Create a word cloud
wordcloud::wordcloud(tweets$text, min.freq = 5)
