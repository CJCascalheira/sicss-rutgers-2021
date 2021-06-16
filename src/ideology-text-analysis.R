# --------------- Setup ------------------
# This script estimates topic models 3 times: (1) for the entire dataset, (2) for the left and (3) for the right
# Also, includes estimation of ideological density plot 
# load packages
library(readr) # load data
library(tidyverse) # just because
library(tm) # text analysis
library(topicmodels) # topic models
library(wordcloud2) # topic mod. viz 

# read dta
tweets <- read_csv("data/blm-tweets-ideology.csv")

# subset dta based on ideology 
tweets_gop <-filter(tweets, theta>0)
tweets_dem <-filter(tweets, theta<0)

# ------------------ Topic modeling for entire dataset ------------------------ 
# Clean
tweets <- tweets$text
tweets <- iconv(tweets, to = "ASCII", sub = " ")  
tweets <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tweets)  # Remove the "RT" (retweet) and usernames 
tweets = gsub("http.+ |http.+$", " ", tweets)  # Remove html links
tweets = gsub("http[[:alnum:]]*", "", tweets)
tweets = gsub("[[:punct:]]", " ", tweets)  # Remove punctuation
tweets = gsub("[ |\t]{2,}", " ", tweets)  # Remove tabs
tweets = gsub("^ ", "", tweets)  # Leading blanks
tweets = gsub(" $", "", tweets)  # Lagging blanks
tweets = gsub(" +", " ", tweets) # General spaces  
tweets = tolower(tweets)
tweets = unique(tweets) 
corpus <- Corpus(VectorSource(tweets)) 
corpus <- tm_map(corpus, removeWords, stopwords("english"))  
corpus <- tm_map(corpus, removeNumbers) 
corpus <- tm_map(corpus, stemDocument)
corpus = tm_map(corpus, removeWords, c("bangladesh","amp", "will", 'get', 'can', '/', 'n'))

# create document term matrix 
dtm = DocumentTermMatrix(corpus)
doc.length = apply(dtm, 1, sum)
dtm = dtm[doc.length > 0,]
inspect(dtm[1:2,10:15])

freq = colSums(as.matrix(dtm))
ord = order(freq, decreasing = TRUE)
freq[head(ord, n = 20)]

# LDA model with 5 topics selected
lda_5 = LDA(dtm, k = 5, method = 'Gibbs', 
            control = list(nstart = 5, seed = list(1505,99,36,56,88), best = TRUE, 
                           thin = 500, burnin = 4000, iter = 2000))

topicModel <- lda_5
tmResult <- posterior(topicModel)
beta <- tmResult$terms   # get beta from results
theta <- tmResult$topics 

top5termsPerTopic <- terms(topicModel, 5)
topicNames <- apply(top5termsPerTopic, 2, paste, collapse=" ")

# visualize topics as word cloud
topicToViz <- 3 # change for your own topic of interest
topicToViz <- grep('polic', topicNames)[1] # Or select a topic by a term contained in its name
# select to 40 most probable terms from the topic by sorting the term-topic-probability vector in decreasing order
top40terms <- sort(tmResult$terms[topicToViz,], decreasing=TRUE)[1:40]
words <- names(top40terms)
# extract the probabilites of each of the 40 terms
probabilities <- sort(tmResult$terms[topicToViz,], decreasing=TRUE)[1:40]
# visualize the terms as wordcloud
wordcloud2(data.frame(words, probabilities), shuffle = FALSE, size = 0.8)

############### DEMOCRATS #################
tweets <- tweets_dem$text
tweets <- iconv(tweets, to = "ASCII", sub = " ")  
tweets <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tweets)  # Remove the "RT" (retweet) and usernames 
tweets = gsub("http.+ |http.+$", " ", tweets)  # Remove html links
tweets = gsub("http[[:alnum:]]*", "", tweets)
tweets = gsub("[[:punct:]]", " ", tweets)  # Remove punctuation
tweets = gsub("[ |\t]{2,}", " ", tweets)  # Remove tabs
tweets = gsub("^ ", "", tweets)  # Leading blanks
tweets = gsub(" $", "", tweets)  # Lagging blanks
tweets = gsub(" +", " ", tweets) # General spaces  
tweets = tolower(tweets)
tweets = unique(tweets) 
corpus <- Corpus(VectorSource(tweets)) 
corpus <- tm_map(corpus, removeWords, stopwords("english"))  
corpus <- tm_map(corpus, removeNumbers) 
corpus <- tm_map(corpus, stemDocument)
corpus = tm_map(corpus, removeWords, c("bangladesh","amp", "will", 'get', 'can', '/', 'n'))

dtm = DocumentTermMatrix(corpus)
dtm
doc.length = apply(dtm, 1, sum)
dtm = dtm[doc.length > 0,]
dtm 

inspect(dtm[1:2,10:15])

freq = colSums(as.matrix(dtm))
ord = order(freq, decreasing = TRUE)
freq[head(ord, n = 20)]

#LDA model with 5 topics selected
lda_5 = LDA(dtm, k = 5, method = 'Gibbs', 
            control = list(nstart = 5, seed = list(1505,99,36,56,88), best = TRUE, 
                           thin = 500, burnin = 4000, iter = 2000))

topicModel <- lda_5
tmResult <- posterior(topicModel)
beta <- tmResult$terms   # get beta from results
theta <- tmResult$topics 

top5termsPerTopic <- terms(topicModel, 5)
topicNames <- apply(top5termsPerTopic, 2, paste, collapse=" ")

# visualize topics as word cloud
topicToViz <- 3 # change for your own topic of interest
topicToViz <- grep('support', topicNames)[1] # Or select a topic by a term contained in its name
# select to 40 most probable terms from the topic by sorting the term-topic-probability vector in decreasing order
top40terms <- sort(tmResult$terms[topicToViz,], decreasing=TRUE)[1:40]
words <- names(top40terms)
# extract the probabilites of each of the 40 terms
probabilities <- sort(tmResult$terms[topicToViz,], decreasing=TRUE)[1:40]
# visualize the terms as wordcloud
wordcloud2(data.frame(words, probabilities), shuffle = FALSE, size = 0.8)

######### REPUBLICANS ###########
tweets <- tweets_gop$text
tweets <- iconv(tweets, to = "ASCII", sub = " ")  
tweets <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tweets)  # Remove the "RT" (retweet) and usernames 
tweets = gsub("http.+ |http.+$", " ", tweets)  # Remove html links
tweets = gsub("http[[:alnum:]]*", "", tweets)
tweets = gsub("[[:punct:]]", " ", tweets)  # Remove punctuation
tweets = gsub("[ |\t]{2,}", " ", tweets)  # Remove tabs
tweets = gsub("^ ", "", tweets)  # Leading blanks
tweets = gsub(" $", "", tweets)  # Lagging blanks
tweets = gsub(" +", " ", tweets) # General spaces  
tweets = tolower(tweets)
tweets = unique(tweets) 
corpus <- Corpus(VectorSource(tweets)) 
corpus <- tm_map(corpus, removeWords, stopwords("english"))  
corpus <- tm_map(corpus, removeNumbers) 
corpus <- tm_map(corpus, stemDocument)
corpus = tm_map(corpus, removeWords, c("bangladesh","amp", "will", 'get', 'can', '/', 'n'))

dtm = DocumentTermMatrix(corpus)
dtm
doc.length = apply(dtm, 1, sum)
dtm = dtm[doc.length > 0,]
dtm 

inspect(dtm[1:2,10:15])

freq = colSums(as.matrix(dtm))
ord = order(freq, decreasing = TRUE)
freq[head(ord, n = 20)]

#LDA model with 5 topics selected
lda_5 = LDA(dtm, k = 5, method = 'Gibbs', 
            control = list(nstart = 5, seed = list(1505,99,36,56,88), best = TRUE, 
                           thin = 500, burnin = 4000, iter = 2000))

topicModel <- lda_5
tmResult <- posterior(topicModel)
beta <- tmResult$terms   # get beta from results
theta <- tmResult$topics 

top5termsPerTopic <- terms(topicModel, 5)
topicNames <- apply(top5termsPerTopic, 2, paste, collapse=" ")

# visualize topics as word cloud
topicToViz <- 4 # change for your own topic of interest
topicToViz <- grep('antifa', topicNames)[1] # Or select a topic by a term contained in its name
# select to 40 most probable terms from the topic by sorting the term-topic-probability vector in decreasing order
top40terms <- sort(tmResult$terms[topicToViz,], decreasing=TRUE)[1:40]
words <- names(top40terms)
# extract the probabilites of each of the 40 terms
probabilities <- sort(tmResult$terms[topicToViz,], decreasing=TRUE)[1:40]
# visualize the terms as wordcloud
wordcloud2(data.frame(words, probabilities), shuffle = FALSE, size = 0.8)

# ------------------- Ideological density plot -----------------------
# Ideological density
x <- na.omit(tweets$theta)
y <- density(na.omit(x), n = 2^12)

ggplot(data.frame(x = y$x, y = y$y), aes(x, y)) + geom_line() + 
  geom_segment(aes(xend = x, yend = 0, colour = x)) + 
  scale_color_gradient(low = 'dodgerblue2', high = 'firebrick2') + 
  labs(x = 'Ideology Estimate', y = 'Density', title = 'Ideological point estimates of Twitter users', caption = 'Negative values are left-leaning users, positive values are right-leaning users. Point estimates calculated with the tweetscore R package.') + 
  cowplot::theme_cowplot() + guides(color = FALSE)