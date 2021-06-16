# SICSS-Rutgers 2021 rtweet Tutorial
# Package documentation: https://cran.r-project.org/web/packages/rtweet/rtweet.pdf
# Demo: https://mkearney.github.io/nicar_tworkshop/#1
# This tutorial is also adapted from the rtweet tutorial in the Computational Social Science class offered by Dr. Katherine Ognyanova. 

# With Twitter data, we can explore the texts for sentiment analysis and topic modeling, 
# or explore the relationship between users using network analysis.
# We can also combine Twitter data with other datasets for matching and more complicated analysis.
# Let's first look at how to collect Twitter data for different research objectives!

# install.packages("rtweet")     # Collect Twitter data
# install.packages("wordcloud")  # Create word clouds
rm(list = ls())

library(rtweet)
library(wordcloud)

# ================  ~~ Getting a Twitter authorization ================
# Follow the instructions below to get authorization data from Twitter
# https://cran.r-project.org/web/packages/rtweet/vignettes/auth.html 
vignette("auth", package = "rtweet") 

# Once your application for Twitter API gets approved, you can go to https://developer.twitter.com/en/apps and click on "Create App."
# Name the app. Once created, copy the following information:
#  -- API Key
#  -- API Secret     
#  -- Access token (you may have to generate it separately)
#  -- Access secret (generated along with the token)

API_key  <- Sys.getenv("TWITTER_KEY")
API_secret <- Sys.getenv("TWITTER_SECRET")
acc_token <- Sys.getenv("TWITTER_ACC")
acc_secret <- Sys.getenv("TWITTER_ACC_SECRET")

# Create a token that will be used to get you authorized:

my_token <- create_token(app = "ms_features",
                          consumer_key = API_key,
                          consumer_secret = API_secret,
                          access_token = acc_token,
                          access_secret = acc_secret) 

# ================  ~~ Twitter search data ================

# Get tweets from search: returns tweets from the last ~7 days
# Rate limit: ~18,000 tweets every 15 minutes.
# Searches for all the included words -- so both "covid" and "vaccine". 
# If we wanted one or the other, we could use "covid" OR "vaccine"
# If we wanted the exact phrase, we could use '"covid vaccine"'
# (basically put in the q parameter whatever it is you'd put in the 
# Twitter search box in order to find the tweets you want to collect)

# Below, n is the number of tweets we want to get (if available)
# include_rts specifies if we want to include retweets in the results
cv_tweets <- search_tweets(q = '"covid vaccine" lang:en', 
                           n = 100, 
                           include_rts = FALSE,
                           token = my_token, 
                           language = "en")

# Take a look at the data:
head(cv_tweets)
colnames(cv_tweets)
dim(cv_tweets)

# Look at the text of the tweets
cv_tweets$text

# preview users data
users_data(cv_tweets)

# Search tweets about SICSS
sicss_tweets <- search_tweets(q = 'SICSS lang:en', 
                              n = 100, 
                              include_rts = FALSE,
                              token = my_token, 
                              language = "en")

## plot time series (if ggplot2 is installed)
ts_plot(sicss_tweets, by = 'days')

## plot time series of tweets
ts_plot(sicss_tweets, "1 day") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of 'covid vaccine' Twitter statuses from past 7 days",
    subtitle = "Twitter status (tweet) counts aggregated using three-hour intervals",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

# For large numbers of tweets, we would need to set parameter retryonratelimit=TRUE
# Doing that forces R to try again if we reach Twitter rate limits.
# For example (don't run now!):

# tweets <- search_tweets(q = "politics",
#                         n = 500000, 
#                         include_rts = FALSE,
#                         token = my_token, 
#                         retryonratelimit=TRUE)
#

# Both in the streaming API and in the search API, we can request Tweets based on geographic location. 

# Search for 1,000 tweets sent from the US
rt <- search_tweets("lang:en", geocode = lookup_coords("usa"), n = 1000, token = my_token)
head(rt$text)

# We can also specify longitude & latitude for a point on the map along with a radius around it.
# Search tweets from within 25 miles of Rutgers:
ru50_tweets <- search_tweets(q ="", # Advanced search function
                             geocode = "40.5051,-74.4530,25mi",
                             n = 100, 
                             include_rts = FALSE,
                             token = my_token, 
                             language = "en")

head(ru50_tweets) 
dim(ru50_tweets)
head(ru50_tweets$text)

# Return only tweets sent from the president's account @POTUS:
pres_tweets <- search_tweets(q = "from:POTUS",
                             n = 100,  
                             token = my_token)
head(pres_tweets) 
dim(pres_tweets)
pres_tweets$text

# Return only tweets sent to @POTUS:
# (note those are replies, if we wanted mentions
# we could simply search for "@POTUS OR @CNN")
pres_replies <- search_tweets(q = "to:POTUS",
                              n = 100,  
                              token = my_token)
head(pres_replies) 
dim(pres_replies)
head(pres_replies$text)

# Useful additional specifications in your search 
# (based on search operators Twitter offers):

# Return only tweets with links:         "filter:links"
# Return only tweets with links to news: "filter:news"
# Return only tweets with media:         "filter:media"
# Return only tweets with video:         "filter:native_video"
# Return only verified tweets:           "filter:verified"
# Exclude verified tweets                "-filter:verified
# Exclude retweets:  "-filter:retweets"
# Exclude quotes:    "-filter:quote"
# Exclude replies:   "-filter:replies" 
# Minimum X number of replies:   "min_replies:X"
# Minimum X number of likes:     "min_faves:X"
# Minimum X number of retweets:  "min_retweets:X"

# Tweets with links to news.
news_tweets <- search_tweets(q = '"covid vaccine" filter:news min_faves:100 lang:en',
                             n = 100, 
                             include_rts = FALSE,
                             token = my_token, 
                             language = "en")
head(news_tweets) 
dim(news_tweets)

# Tweet text:
head(news_tweets$text)
wordcloud::wordcloud(news_tweets$text, min.freq=3)

# News URL:
news_tweets$urls_expanded_url

# Favorite & retweet counts
hist(news_tweets$favorite_count)
hist(news_tweets$retweet_count)

# Tweets with over 1,000 retweets:
news_tweets[news_tweets$retweet_count>1000, c("screen_name", "text" )] 

# ================  ~~ Twitter users ================

# The Twitter API lets us get the most recent tweets from a user's timeline
# The limit is a maximum of 3,200 tweets.
# Parameter home=FALSE gets user timeline; TRUE gets their home feed.

jack_tweets <- get_timeline(user="jack",
                            n=100,
                            home=FALSE,
                            token=my_token)

jack_tweets$text
wordcloud::wordcloud(jack_tweets$text, min.freq=3)


# Multiple users' timelines:
media_tweets <- get_timeline(user= c("cnn", "foxnews"),
                             n=100,
                             home=FALSE,
                             token=my_token)
media_tweets$text

# Get the users followed by the president's @POTUS account:
# Note that Twitter's default limit for following is 5,000
pres_fr <- get_friends(users="POTUS", n=100, token=my_token)

pres_fr

# Note that the accounts are returned as user IDs. 
# We can use those go get user data:

pres_fr_acc <- lookup_users(pres_fr$user_id, token=my_token)

pres_fr_acc

pres_fr_acc$screen_name 

# Get the users who follow the president on Twitter:

pres_fol <- get_followers(user="POTUS", n=100, token=my_token)

pres_fol_acc  <- lookup_users(pres_fol$user_id, token=my_token)

pres_fol_acc

pres_fol_acc$screen_name 

# If you want to get a large number of followers, you would need to include parameter retryonratelimit = TRUE  
# and be prepared to wait for a long time.

## How many total follows does cnn have?
cnn <- lookup_users("cnn", token = my_token)

## Get them all (this would take a little over 5 days)
#cnn_flw <- get_followers("cnn", n = cnn$followers_count, retryonratelimit = TRUE, token = my_token)

# ================  ~~ Twitter streaming API ================

# The streaming API randomly samples approx. 1% of all live tweets
# The following code will get live tweets over a period of 30 seconds,
# looking for ones with the that contain the term "network" in them.
# If those happen to exceed 1% of all tweets, they will be capped at 1%.

live_net <- stream_tweets(q="vaccine", 
                          timeout=30, #timeout = 60
                          token=my_token,
                          language = "en")

# We can also save the Twitter data to a file and read it back:
stream_tweets(q="vaccine", 
              timeout=30,
              token=my_token,
              language = "en",
              file_name="my_file.json",
              parse = FALSE)

# Read the file back and parse it to get a nice format:
live_vaccine <- parse_stream("my_file.json")
live_vaccine$text

# Plot time series of tweets
ts_plot(sicss_tweets, by = 'minutes')
