# Load dependencies
library(rtweet)
library(wordcloud)
library(tidytext)
# library(textdata)
library(SnowballC)
library(tm)
library(lubridate)
library(viridis)
library(tidyverse)

# Pull sentiment library
senti_emo <- get_sentiments("nrc")
senti_pos_neg <- get_sentiments("bing")
senti_number <- get_sentiments("afinn")

# Load daya
blm <- read_csv("data/blm-data.csv")
blm_ideology <- read_csv("data/blm-tweets-ideology.csv")

# Select ideology variables
blm_ideology_1 <- blm_ideology %>%
  select(id_str, id_str.y, theta)

# Clean the dates
blm_1 <- blm %>%
  mutate(
    created_at = str_extract(created_at, "Jun [[:digit:]]{2} [[:digit:]]{2}:[[:digit:]]{2}:[[:digit:]]{2}"),
    created_at = paste0("2021 ", created_at),
    created_at = ymd_hms(created_at)
  )

# Sentiment analysis prepare
blm_2 <- blm_1 %>%
  unnest_tokens(blm_tweets, text) %>%
  select(id_str, created_at, blm_tweets) %>%
  left_join(senti_emo, by = c("blm_tweets" = "word")) %>%
  rename(sentiment_emo = sentiment) %>%
  left_join(senti_pos_neg, by = c("blm_tweets" = "word")) %>%
  rename(sentiment_pos_neg = sentiment) %>%
  left_join(senti_number, by = c("blm_tweets" = "word")) %>%
  filter(
    !is.na(sentiment_emo),
    !is.na(sentiment_pos_neg)
  )

# Merge BLM with ideology
ideologies <- left_join(blm_2, blm_ideology_1, by = c("id_str" = "id_str.y")) %>%
  filter(!is.na(theta))

# SENTIMENT OVER TIME -----------------------------------------------------

# Valence of sentiment by day
blm_2 %>%
  mutate(day = day(created_at)) %>%
  count(day, sentiment_pos_neg) %>%
  mutate(sentiment_pos_neg = recode(sentiment_pos_neg, 
                                    "negative" = "Negative Sentiment",
                                    "positive" = "Positive Sentiment")) %>%
  ggplot(aes(y = n, x = day)) +
  geom_line(size = 2) +
  facet_wrap(~ sentiment_pos_neg) +
  theme_bw() +
  labs(y = "", x = "June 2nd - June 9th, 2020", 
       title = "#BLM Tweets: Valence of Sentiment Over Time",
       subtitle = "Source: Twitter")

# Valence of sentiment by day, per tweet
blm_valence_plot <- blm_2 %>%
  filter(!is.na(value)) %>%
  group_by(id_str) %>%
  summarise(sum = sum(value)) %>%
  left_join(blm_2) %>%
  filter(!is.na(sum)) %>%
  distinct(id_str, .keep_all = TRUE) %>%
  mutate(pos_neg = if_else(sum < 0, "Negative Sentiment", "Positive Sentiment")) %>%
  mutate(day = day(created_at)) %>%
  count(day, pos_neg) %>%
  mutate(pos_neg = recode(pos_neg,
                          "negative" = "Negative Sentiment",
                          "positive" = "Positive Sentiment")) %>%
  ggplot(aes(y = n, x = day)) +
  geom_line(size = 2) +
  facet_wrap(~ pos_neg) +
  geom_vline(xintercept = 8, color = "red", size = 1.5) +
  theme_bw() +
  labs(y = "", x = "June 2nd - June 9th, 2020", 
       title = "#BLM Tweets: Valence of Sentiment Over Time",
       subtitle = "Source: Twitter (N = 11,901 tweets)") +
  annotate(geom = "text", x = 7, y = 600, label = "March on D.C.",
           color = "red") +
  theme(strip.text.x = element_text(size = 15))

# Show plot
blm_valence_plot

# Save plot
ggsave("data/results/blm_valence_plot.png", plot = blm_valence_plot)

# IDEOLOGY AND VALENCE ----------------------------------------------------

# Valence by tweet and ideology
ideologies_plot <- ideologies %>%
  filter(!is.na(value)) %>%
  group_by(id_str) %>%
  summarise(sum = sum(value)) %>%
  left_join(ideologies) %>%
  filter(!is.na(sum)) %>%
  distinct(id_str, .keep_all = TRUE) %>%
  mutate(pos_neg = if_else(sum < 0, "Negative", "Positive")) %>%
  mutate(day = day(created_at)) %>%
  mutate(ideology = if_else(theta < 0, "Left", "Right")) %>%
  count(day, ideology, pos_neg) %>%
  mutate(pos_neg = recode(pos_neg,
                          "negative" = "Negative",
                          "positive" = "Positive")) %>%
  ggplot(aes(y = n, x = day)) +
  geom_line(size = 2) +
  #facet_wrap(~ pos_neg) +
  facet_grid(vars(pos_neg), vars(ideology)) +
  geom_vline(xintercept = 8, color = "red", size = 1.5) +
  theme_bw() +
  labs(y = "Number of Tweets", x = "June 2nd - June 9th, 2020", 
       title = "#BLM Tweets: Valence of Sentiment Over Time",
       subtitle = "Source: Twitter (N = 5,941 tweets)") +
  annotate(geom = "text", x = 7, y = 600, label = "March on D.C.",
           color = "red") +
  theme(strip.text.x = element_text(size = 15),
        strip.text.y = element_text(size = 15))

# Show plot
ideologies_plot

# Save plot
ggsave("data/results/ideologies_plot.png", plot = ideologies_plot)

# EMOTIONS BY DAY ---------------------------------------------------------

# Emotions by day
blm_emotions <- blm_2 %>%
  mutate(day = day(created_at)) %>%
  count(day, sentiment_emo) %>%
  ggplot(aes(y = n, x = day, group = sentiment_emo, color = sentiment_emo)) +
  geom_line(size = 2) +
  theme_bw() +
  scale_color_viridis(discrete = TRUE, option = "D", name = "Emotions") +
  labs(y = "Number of Emotion-Laden Words Across Tweets", x = "June 2nd - June 9th, 2020", 
       title = "#BLM Tweets: Emotions Over Time",
       subtitle = "Source: Twitter")
blm_emotions 

# Save plot
ggsave("data/results/blm_emotions .png", plot = blm_emotions )
